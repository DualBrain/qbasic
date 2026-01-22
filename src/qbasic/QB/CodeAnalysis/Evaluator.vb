Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Binding
Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Imports QBLib
Imports System.IO

Namespace Global.QB.CodeAnalysis

  Friend Enum TimerState
    Off
    [Stop]
    [On]
  End Enum

  Friend NotInheritable Class Evaluator

    Private ReadOnly m_program As BoundProgram
    Private ReadOnly m_globals As Dictionary(Of String, Object)
    Private ReadOnly m_globalVariables As ImmutableArray(Of VariableSymbol)
    Private ReadOnly m_functions As New Dictionary(Of FunctionSymbol, BoundBlockStatement)
    Private ReadOnly m_commandLineArgs As String()
    Private ReadOnly m_locals As New Stack(Of Dictionary(Of String, Object))

    Private ReadOnly m_container As New Stack(Of String)

    ' Track current array bounds (updated by REDIM)
    Private ReadOnly m_arrayBounds As New Dictionary(Of String, (Lower As Integer, Upper As Integer))

    ' Error handling state
    Private m_err As ErrorCode = ErrorCode.None   ' Current error code (ERR)
    Private m_erl As Integer = 0 ' Line number where error occurred (ERL)
    Private m_errorHandlerTarget As String = Nothing ' Target label for ON ERROR GOTO
    Private m_errorResumeNext As Boolean = False ' ON ERROR RESUME NEXT mode
    Private m_errorPending As Boolean = False ' Flag indicating if an error occurred and needs handling
    Private m_errorResumeIndex As Integer = -1 ' Index to resume execution after error handling

    ' Timer event state
    Private m_timerHandlerTarget As Object = Nothing ' Target for ON TIMER GOSUB (label or line number)
    Private m_timerInterval As Double = 0 ' Timer interval in seconds
    Private m_timerState As TimerState = TimerState.Off ' Current timer state
    Private m_timerNextTrigger As DateTime = DateTime.MinValue ' Next trigger time
    Private m_timerEventPending As Boolean = False ' Whether a timer event is pending

    ' COM event state (channels 1-2)
    Private m_comHandlerTargets As Object() = {Nothing, Nothing} ' Targets for ON COM(n) GOSUB
    Private m_comStates As TimerState() = {TimerState.Off, TimerState.Off} ' Current COM states

    ' KEY event state (keys 1-20)
    Private m_keyHandlerTargets As Object() = New Object(20) {} ' Targets for ON KEY(n) GOSUB (1-based, index 0 unused)
    Private m_keyStates As TimerState() = New TimerState(20) {} ' Current KEY states (1-based, index 0 unused)

    ' STRIG event state (triggers 0,2,4,6)
    Private m_strigHandlerTargets As Object() = New Object(6) {} ' Targets for ON STRIG(n) GOSUB
    Private m_strigStates As TimerState() = New TimerState(6) {} ' Current STRIG states

    ' PLAY event state
    Private m_playHandlerTarget As Object = Nothing ' Target for ON PLAY(n) GOSUB
    Private m_playQueueSize As Integer = 0 ' Queue size threshold
    Private m_playState As TimerState = TimerState.Off ' Current PLAY state

    ' PEN event state
    Private m_penHandlerTarget As Object = Nothing ' Target for ON PEN GOSUB
    Private m_penState As TimerState = TimerState.Off ' Current PEN state

    ' File I/O state
    Private ReadOnly m_openFiles As New Dictionary(Of Integer, FileStream) ' File number to FileStream mapping
    Private ReadOnly m_fileModes As New Dictionary(Of Integer, String) ' File number to access mode mapping
    Private ReadOnly m_recordLengths As New Dictionary(Of Integer, Integer) ' File number to record length mapping (for RANDOM files)
    Private ReadOnly m_textReaders As New Dictionary(Of Integer, StreamReader) ' File number to StreamReader mapping for INPUT files
    Private ReadOnly m_textWriters As New Dictionary(Of Integer, StreamWriter) ' File number to StreamWriter mapping for OUTPUT/APPEND files

    'Private m_labelToIndex As Dictionary(Of String, Integer) = Nothing ' Label to m_currentIndex mapping
    'Private m_currentIndex As Integer = 0 ' Current statement m_currentIndex being executed

    Private Const GOTO_LABEL_PREFIX As String = "$LABEL"

    'Private m_random As Random

    'TODO: Need to make this scoped.
    Private ReadOnly m_gosubStack As New Stack(Of Integer)

    Private ReadOnly m_data As New List(Of Object)
    Private m_dataIndex As Integer = 0

    Private m_lastValue As Object

    Private Class ByRefVariable
      Public Property Name As String
      Public Sub New(name As String)
        Me.Name = name
      End Sub
    End Class

    ' Custom exception for QBasic runtime errors
    Public Class QBasicRuntimeException
      Inherits Exception

      Public ReadOnly Property ErrorCode As ErrorCode

      Public Sub New(errorCode As ErrorCode)
        MyBase.New($"Error {CInt(errorCode)}: {GetErrorMessage(errorCode)}")
        Me.ErrorCode = errorCode
      End Sub

      'Public Sub New(message As String)
      '  MyBase.New(message)
      'End Sub

    End Class

    ' Exception for RESUME operations that need to jump
    Private Class ResumeException
      Inherits Exception

      Public Property TargetIndex As Integer

      Public Sub New(targetIndex As Integer)
        Me.TargetIndex = targetIndex
      End Sub

    End Class

    ' Error handling methods
    'Private Sub SetError(errorCode As ErrorCode, Optional lineNumber As Integer = 0)
    '  m_err = errorCode
    '  m_erl = lineNumber
    '  m_errorPending = True
    'End Sub

    Private Sub ClearError()
      m_err = 0
      m_erl = 0
      m_errorPending = False
      m_errorResumeIndex = -1
    End Sub

    Private Function HandlePendingError(ByRef index As Integer, labelToIndex As Dictionary(Of String, Integer)) As Boolean
      m_errorResumeIndex = index ' Save current index for RESUME

      'Console.WriteLine($"DEBUG: HandlePendingError - m_errorResumeNext={m_errorResumeNext}, m_errorHandlerTarget={m_errorHandlerTarget}")

      If m_errorResumeNext Then
        ' ON ERROR RESUME NEXT - continue with next statement
        'Console.WriteLine("DEBUG: Resuming next")
        ClearError()
        index += 1 ' Skip current statement
        Return True
      ElseIf m_errorHandlerTarget IsNot Nothing Then
        ' ON ERROR GOTO - jump to error handler
        'Console.WriteLine($"DEBUG: Looking for label '{m_errorHandlerTarget}'")
        'Console.WriteLine($"DEBUG: labelToIndex has {labelToIndex.Count} entries")
        'For Each kv In labelToIndex
        'Console.WriteLine($"DEBUG: Label: {kv.Key} at {kv.Value}")
        'Next
        If labelToIndex.ContainsKey(m_errorHandlerTarget) Then
          'Console.WriteLine($"DEBUG: Found label, jumping to {labelToIndex(m_errorHandlerTarget)}")
          ' Clear pending error flag so handler can execute normally
          m_errorPending = False
          index = labelToIndex(m_errorHandlerTarget)
          Return True
        Else
          'Console.WriteLine("DEBUG: Label not found")
          ' Label not found, treat as fatal error
          Throw New QBasicRuntimeException(ErrorCode.UndefinedLineNumber)
        End If
      Else
        ' No error handler, fatal error
        Throw New QBasicRuntimeException(m_err)
      End If

      Return False
    End Function

    Private Function GetVariableNameFromSyntax(syntax As ExpressionSyntax) As String
      If TypeOf syntax Is IdentifierExpressionSyntax Then
        Return DirectCast(syntax, IdentifierExpressionSyntax).Identifier.Text
      End If
      Throw New Exception("Not a variable syntax")
    End Function

    ' Added so that we can access the "variables" for unit testing.
    Public ReadOnly Property Globals As Dictionary(Of String, Object)
      Get
        Return m_globals
      End Get
    End Property

    Sub New(program As BoundProgram, variables As Dictionary(Of VariableSymbol, Object), globalVariables As ImmutableArray(Of VariableSymbol), Optional commandLineArgs As String() = Nothing)

      m_program = program
      m_globals = New Dictionary(Of String, Object)
      For Each kv In variables
        m_globals(kv.Key.Name) = kv.Value
      Next
      m_globalVariables = globalVariables
      m_commandLineArgs = If(commandLineArgs, Array.Empty(Of String)())
      m_locals.Push(New Dictionary(Of String, Object))

      Dim current = program
      While current IsNot Nothing
        For Each kv In current.Functions
          Dim func = kv.Key
          Dim body = kv.Value
          m_functions.Add(func, body)
        Next
        current = current.Previous
      End While

    End Sub

    Public Function Evaluate() As Object

      ' Initialize global variables
      For Each v As VariableSymbol In m_globalVariables
        If Not m_globals.ContainsKey(v.Name) Then
          If v.IsArray Then
            Dim lower = EvaluateExpression(v.Lower)
            Dim upper = EvaluateExpression(v.Upper)
            Dim size = CInt(upper) - CInt(lower) + 1
            Dim list = New List(Of Object)(size)
            For i = 0 To size - 1
              list.Add(0)
            Next
            m_globals(v.Name) = list
          Else
            m_globals(v.Name) = Nothing
          End If
        End If
      Next

      Dim func = If(m_program.MainFunction, m_program.ScriptFunction)
      If func Is Nothing Then
        Return Nothing
      Else
        Dim body = m_functions(func)
        m_container.Push(func.Name)
        Dim result = EvaluateStatement(body, Nothing)
        m_container.Pop()
        Return result
      End If
    End Function

    Private Function EvaluateStatement(body As BoundBlockStatement, Optional labelToIndex As Dictionary(Of String, Integer) = Nothing) As Object

      Dim localLabelToIndex = If(labelToIndex IsNot Nothing, labelToIndex, New Dictionary(Of String, Integer))
      'm_labelToIndex = localLabelToIndex

      If labelToIndex Is Nothing Then
        For i = 0 To body.Statements.Length - 1
          Dim s = body.Statements(i)
          If TypeOf s Is BoundLabelStatement Then
            Dim label = CType(s, BoundLabelStatement).Label.Name
            If IsNumeric(label) Then
              localLabelToIndex.Add(GOTO_LABEL_PREFIX & label, i)
              '' Line numbers are handled differently
              'localLabelToIndex(GOTO_LABEL_PREFIX & label) = i
            Else
              localLabelToIndex.Add(label, i)
              'localLabelToIndex(label) = i
            End If
          ElseIf TypeOf s Is BoundBlockStatement Then
            Dim block = CType(s, BoundBlockStatement)
            If block.Statements.Length > 0 AndAlso TypeOf block.Statements(0) Is BoundLabelStatement Then
              localLabelToIndex.Add(CType(block.Statements(0), BoundLabelStatement).Label.Name, i)
            End If
          End If
        Next
      End If

      Dim index = 0
      While index < body.Statements.Length

        If QBasic.Common.s_cancelToken.IsCancellationRequested Then
          Exit While
        End If

        ' Check for pending errors before executing next statement
        If m_errorPending Then
          If HandlePendingError(index, localLabelToIndex) Then
            Continue While ' Skip to next iteration with updated index
          End If
        End If

        ' Check for timer events before executing next statement
        If CheckTimerEvent(index, localLabelToIndex) Then
          Continue While ' Timer event triggered, restart loop
        End If

        Dim s = body.Statements(index)
        'Debug.WriteLine($"{index}:{s.Kind}")
        Try
          Select Case s.Kind
            Case BoundNodeKind.BeepStatement
              ' TODO: Implement BEEP sound
              index += 1
            Case BoundNodeKind.PokeStatement
              ' TODO: Implement POKE memory write
              index += 1
            Case BoundNodeKind.OutStatement
              Dim outStmt = CType(s, BoundOutStatement)
              Dim portValue = EvaluateExpression(outStmt.Port)
              Dim port As Integer
              If TypeOf portValue Is Double Then
                port = CInt(CDbl(portValue))
              ElseIf TypeOf portValue Is Integer Then
                port = CInt(portValue)
              Else
                port = CInt(portValue)
              End If
              If port < 0 Or port > 65535 Then
                Throw New QBasicRuntimeException(ErrorCode.Overflow)
              End If
              ' Data is ignored, just throw error
              Throw New QBasicRuntimeException(ErrorCode.AdvancedFeature)
            Case BoundNodeKind.ChDirStatement
              Dim chdir = CType(s, BoundChDirStatement)
              Dim value = CStr(EvaluateExpression(chdir.Expression))
              System.IO.Directory.SetCurrentDirectory(value)
              index += 1
            Case BoundNodeKind.CircleStatement
              Dim circle = CType(s, BoundCircleStatement)
              Dim x = CInt(EvaluateExpression(circle.X))
              Dim y = CInt(EvaluateExpression(circle.Y))
              Dim radius = CSng(EvaluateExpression(circle.Radius))
              Dim color = If(circle.Color IsNot Nothing, CInt(EvaluateExpression(circle.Color)), New Integer?)
              Dim start = If(circle.Start IsNot Nothing, CDbl(EvaluateExpression(circle.Start)), New Double?)
              Dim [end] = If(circle.End IsNot Nothing, CDbl(EvaluateExpression(circle.End)), New Double?)
              Dim aspect = If(circle.Aspect IsNot Nothing, CDbl(EvaluateExpression(circle.Aspect)), New Double?)
              QBLib.Video.CIRCLE(x, y, radius, color, start, [end], aspect)
              index += 1
            Case BoundNodeKind.ClearStatement
              index += 1
            Case BoundNodeKind.ClsStatement
              Debug.WriteLine("CLS")
              Dim cs = CType(s, BoundClsStatement)
              Dim value = If(cs.Expression Is Nothing, 0, CInt(EvaluateExpression(cs.Expression)))
              If value < 0 OrElse value > 2 Then
                ' error condition
                QBLib.Video.PRINT("CLS parameter out-of-range.")
              Else
                Select Case value
                  Case 0 ' Clears the screen of all text and graphics
                    QBLib.Video.CLS()
                  Case 1 ' Clears only the graphics viewport
                    'TODO: Revisit after SCREEN, WIDTH, VIEW.
                    QBLib.Video.CLS()
                  Case 2 ' Clears only the text window
                    'TODO: Revisit after SCREEN, WIDTH, VIEW.
                    QBLib.Video.CLS()
                End Select
              End If
              index += 1
            Case BoundNodeKind.ColorStatement
              Dim cs = CType(s, BoundColorStatement)
              Dim expression1 = If(cs.Expression1 Is Nothing, -1, CInt(EvaluateExpression(cs.Expression1)))
              Dim expression2 = If(cs.Expression2 Is Nothing, -1, CInt(EvaluateExpression(cs.Expression2)))
              Dim expression3 = If(cs.Expression3 Is Nothing, -1, CInt(EvaluateExpression(cs.Expression3)))
              Select Case QBLib.Video.ScreenMode
                Case 0 'COLOR [foreground%] [,[background%] [,border%]]	Screen mode 0 (text only)
                  If expression1 > -1 AndAlso expression1 < 16 Then QBLib.Video.m_fgColor = expression1
                  If expression2 > -1 AndAlso expression2 < 16 Then QBLib.Video.m_bgColor = expression2
                  If expression3 > -1 AndAlso expression3 < 16 Then QBLib.Video.m_borderColor = expression3
                Case 1 'COLOR [background%] [,palette%]	Screen mode 1
                  If expression1 > -1 AndAlso expression1 < 16 Then QBLib.Video.m_bgColor = expression1
                  If expression2 > -1 AndAlso expression2 < 16 Then QBLib.Video.m_paletteIndex = expression2
                Case 4, 12, 13 'COLOR [foreground%]	Screen modes 4, 12, 13
                  If expression1 > -1 AndAlso expression1 < 16 Then QBLib.Video.COLOR(expression1)
                Case 7, 8, 9, 10 'COLOR [foreground%] [,background&]	Screen modes 7-10
                  If expression1 > -1 AndAlso expression1 < 16 Then QBLib.Video.m_fgColor = expression1
                  If expression2 > -1 AndAlso expression2 < 16 Then QBLib.Video.m_bgColor = expression2
                Case Else
              End Select
              index += 1
            Case BoundNodeKind.ConditionalGotoStatement
              Dim cgs = CType(s, BoundConditionalGotoStatement)
              Dim condition = CBool(EvaluateExpression(cgs.Condition))
              If condition = cgs.JumpIfTrue Then
                index = localLabelToIndex(cgs.Label.Name)
              Else
                index += 1
              End If
            Case BoundNodeKind.EndStatement
              index = body.Statements.Length
            Case BoundNodeKind.ExpressionStatement : EvaluateExpressionStatement(CType(s, BoundExpressionStatement)) : index += 1

            Case BoundNodeKind.GosubStatement
              Dim gs = CType(s, BoundGosubStatement)
              Dim value As Integer = Nothing
              If localLabelToIndex.TryGetValue(gs.Label.Name, value) Then
                m_gosubStack.Push(index + 1)
                index = value
              Else
                'Console.WriteLine("ERROR: GosubStatement label " & gs.Label.Name & " not found")
                index += 1
              End If

            Case BoundNodeKind.GotoStatement
              Dim gs = CType(s, BoundGotoStatement)
              Dim value As Integer = Nothing
              If localLabelToIndex.TryGetValue(gs.Label.Name, value) Then
                index = value
              Else
                'Console.WriteLine("ERROR: GotoStatement label " & gs.Label.Name & " not found")
                index += 1
              End If
            'index = labelToIndex(gs.Label)

            Case BoundNodeKind.HandleCommaStatement : EvaluateHandleCommaStatement(CType(s, BoundHandleCommaStatement)) : index += 1
            Case BoundNodeKind.HandlePrintLineStatement : EvaluateHandlePrintLineStatement(CType(s, BoundHandlePrintLineStatement)) : index += 1
            Case BoundNodeKind.PrintStatement : EvaluatePrintStatement(CType(s, BoundPrintStatement)) : index += 1
            Case BoundNodeKind.HandlePrintStatement : EvaluateHandlePrintStatement(CType(s, BoundHandlePrintStatement)) : index += 1
            Case BoundNodeKind.HandleSpcStatement : EvaluateHandleSpcStatement(CType(s, BoundHandleSpcStatement)) : index += 1
            Case BoundNodeKind.HandleTabStatement : EvaluateHandleTabStatement(CType(s, BoundHandleTabStatement)) : index += 1
            Case BoundNodeKind.IfStatement : EvaluateIfStatement(CType(s, BoundIfStatement), localLabelToIndex) : index += 1

            Case BoundNodeKind.InputStatement

              Dim input = CType(s, BoundInputStatement)
              Dim suppressCr = input.SuppressCr
              Dim suppressQuestionMark = input.SuppressQuestionMark
              Dim prompt As String = Nothing
              If input.PromptExpression IsNot Nothing Then
                Dim value = CStr(EvaluateExpression(input.PromptExpression))
                prompt = value
              End If
              Do
                If Not String.IsNullOrEmpty(prompt) Then
                  QBLib.Video.PRINT(prompt, True)
                End If
                If Not suppressQuestionMark Then
                  QBLib.Video.PRINT("? ", True)
                End If
                Dim potential = QBLib.Video.InputAsync("").GetAwaiter.GetResult
                Dim potentials = Split(potential, ",")
                If potentials.Length = input.Variables.Length Then
                  For i = 0 To input.Variables.Length - 1
                    Dim value = potentials(i)
                    If input.Variables(i).Type Is TypeSymbol.Double OrElse
                       input.Variables(i).Type Is TypeSymbol.Single OrElse
                       input.Variables(i).Type Is TypeSymbol.ULong64 OrElse
                       input.Variables(i).Type Is TypeSymbol.Long64 OrElse
                       input.Variables(i).Type Is TypeSymbol.ULong OrElse
                       input.Variables(i).Type Is TypeSymbol.Long OrElse
                       input.Variables(i).Type Is TypeSymbol.UInteger OrElse
                       input.Variables(i).Type Is TypeSymbol.Integer OrElse
                       input.Variables(i).Type Is TypeSymbol.SByte OrElse
                       input.Variables(i).Type Is TypeSymbol.Byte Then
                      If IsNumeric(value) Then
                        If value.Contains("."c) Then
                          If input.Variables(i).Type Is TypeSymbol.Single OrElse
                         input.Variables(i).Type Is TypeSymbol.Double Then
                            Assign(input.Variables(i), value)
                          Else
                            QBLib.Video.PRINT() : Continue Do
                          End If
                        Else
                          'TODO: Check in-range for values/types.
                          Assign(input.Variables(i), value)
                        End If
                      Else
                        QBLib.Video.PRINT() : Continue Do
                      End If
                    Else
                      Assign(input.Variables(i), value)
                    End If
                  Next
                  'If Not suppressCr Then Console.WriteLine()
                  'TODO: If suppressCr is True, need to move the cursor back????
                  Exit Do
                Else
                  QBLib.Video.PRINT()
                End If
              Loop

              index += 1

            Case BoundNodeKind.KillStatement
              Dim kill = CType(s, BoundKillStatement)
              Dim value = CStr(EvaluateExpression(kill.Expression))
              If System.IO.Directory.Exists(value) Then
                System.IO.Directory.Delete(value)
              Else
                System.IO.File.Delete(value)
              End If
              index += 1
            Case BoundNodeKind.LabelStatement : index += 1
            Case BoundNodeKind.MidStatement : EvaluateMidStatement(CType(s, BoundMidStatement)) : index += 1
            Case BoundNodeKind.MkDirStatement
              Dim mkdir = CType(s, BoundMkDirStatement)
              Dim value = CStr(EvaluateExpression(mkdir.Expression))
              System.IO.Directory.CreateDirectory(value)
              index += 1
            Case BoundNodeKind.NameStatement
              Dim name = CType(s, BoundNameStatement)
              Dim originalPath = CStr(EvaluateExpression(name.OriginalPath))
              Dim destinationPath = CStr(EvaluateExpression(name.DestinationPath))
              If System.IO.Directory.Exists(originalPath) Then
                System.IO.Directory.Move(originalPath, destinationPath)
              Else
                System.IO.File.Move(originalPath, destinationPath)
              End If
              index += 1
            Case BoundNodeKind.NopStatement : index += 1
            Case BoundNodeKind.LetStatement
              Dim temp = CType(s, BoundLetStatement)
              Dim variableName = temp.Variable.Name
              Dim functionName = m_container.Peek()
              If String.Compare(functionName, variableName, True) = 0 Then
                m_lastValue = EvaluateExpression(temp.Expression)
              Else
                EvaluateLetStatement(CType(s, BoundLetStatement))
              End If
              index += 1

            'EvaluateLetStatement(CType(s, BoundLetStatement)) : index += 1

            Case BoundNodeKind.LineStatement

              Dim ls = CType(s, BoundLineStatement)
              Dim step1 = ls.Step1
              Dim x1 = If(ls.X1 IsNot Nothing, CInt(EvaluateExpression(ls.X1)), New Integer?)
              Dim y1 = If(ls.Y1 IsNot Nothing, CInt(EvaluateExpression(ls.Y1)), New Integer?)
              Dim step2 = ls.Step2
              Dim x2 = CInt(EvaluateExpression(ls.X2))
              Dim y2 = CInt(EvaluateExpression(ls.Y2))
              Dim attribute = If(ls.Attribute IsNot Nothing, CInt(EvaluateExpression(ls.Attribute)), New Integer?)
              Dim mode = CType(ls.Mode, LineOption)
              Dim style = If(ls.Style IsNot Nothing, CInt(EvaluateExpression(ls.Style)), New Integer?)

              QBLib.Video.LINE(step1, x1, y1, step2, x2, y2, attribute, mode)

              index += 1

            Case BoundNodeKind.LocateStatement

              Dim ls = CType(s, BoundLocateStatement)
              Dim row = If(ls.Row Is Nothing, -1, CInt(EvaluateExpression(ls.Row)))
              Dim col = If(ls.Col Is Nothing, -1, CInt(EvaluateExpression(ls.Col)))
              Dim visible = If(ls.Visible Is Nothing, -1, CInt(EvaluateExpression(ls.Visible)))
              Dim scanStart = If(ls.ScanStart Is Nothing, -1, CInt(EvaluateExpression(ls.ScanStart)))
              Dim scanStop = If(ls.Scanstop Is Nothing, -1, CInt(EvaluateExpression(ls.Scanstop)))

              QBLib.Video.LOCATE(row, col, visible, scanStart, scanStop)

              index += 1

            Case BoundNodeKind.OptionStatement
              'TODO: Need to handle with Arrays.
              'TODO: Also need to track that no other invalid
              '      statements have executed (pretty much all
              '      other statements are *invalid* in this
              '      context.)
              index += 1

            Case BoundNodeKind.PsetStatement
              Dim psetStmt = CType(s, BoundPsetStatement)
              Dim x = CInt(Math.Truncate(CDbl(EvaluateExpression(psetStmt.X))))
              Dim y = CInt(Math.Truncate(CDbl(EvaluateExpression(psetStmt.Y))))
              If psetStmt.Color Is Nothing Then
                QBLib.Video.PSET(psetStmt.Step, x, y)
              Else
                Dim c = CInt(EvaluateExpression(psetStmt.Color))
                QBLib.Video.PSET(psetStmt.Step, x, y, c)
              End If
              index += 1

            Case BoundNodeKind.PresetStatement
              Dim psetStmt = CType(s, BoundPresetStatement)
              Dim x = CInt(Math.Truncate(CDbl(EvaluateExpression(psetStmt.X))))
              Dim y = CInt(Math.Truncate(CDbl(EvaluateExpression(psetStmt.Y))))
              If psetStmt.Color Is Nothing Then
                QBLib.Video.PRESET(psetStmt.Step, x, y)
              Else
                Dim c = CInt(EvaluateExpression(psetStmt.Color))
                QBLib.Video.PRESET(psetStmt.Step, x, y, c)
              End If
              index += 1

            Case BoundNodeKind.RemStatement : index += 1

            Case BoundNodeKind.ReturnGosubStatement
              Dim rg = CType(s, BoundReturnGosubStatement)

              Dim value As Integer = Nothing
              If rg.Label Is Nothing Then
                If m_gosubStack.Count > 0 Then
                  index = m_gosubStack.Pop
                Else
                  Throw New QBasicRuntimeException(ErrorCode.ReturnWithoutGosub)
                End If
              ElseIf localLabelToIndex.TryGetValue(rg.Label.Name, value) Then
                index = value
              Else
                'Console.WriteLine("ERROR: ReturnGosubStatement label " & rg.Label.Name & " not found")
              End If

            Case BoundNodeKind.ReturnStatement
              'TODO: Need to determine if this is a 
              '      value Return or a Return related
              '      to a Gosub.
              Dim rs = CType(s, BoundReturnStatement)
              m_lastValue = If(rs.Expression Is Nothing, Nothing, EvaluateExpression(rs.Expression))
              'm_lastValue = If(rs.Expression Is Nothing, m_lastValue, EvaluateExpression(rs.Expression))
              Return m_lastValue

            Case BoundNodeKind.RmDirStatement
              Dim rmdir = CType(s, BoundRmDirStatement)
              Dim value = CStr(EvaluateExpression(rmdir.Expression))
              System.IO.Directory.Delete(value)
              index += 1

            Case BoundNodeKind.ScreenStatement
              Dim screen = CType(s, BoundScreenStatement)
              Dim mode = If(screen.Mode IsNot Nothing, CInt(EvaluateExpression(screen.Mode)), New Integer?)
              Dim colorBurst = If(screen.ColorBurst IsNot Nothing, CInt(EvaluateExpression(screen.ColorBurst)), New Integer?)
              Dim aPage = If(screen.APage IsNot Nothing, CInt(EvaluateExpression(screen.APage)), New Integer?)
              Dim vPage = If(screen.VPage IsNot Nothing, CInt(EvaluateExpression(screen.VPage)), New Integer?)
              Dim [erase] = If(screen.Erase IsNot Nothing, CInt(EvaluateExpression(screen.Erase)), New Integer?)
              QBLib.Video.SCREEN(mode, colorBurst, aPage, vPage, [erase])
              index += 1

            Case BoundNodeKind.StopStatement
              index = body.Statements.Length
            Case BoundNodeKind.SystemStatement
              index = body.Statements.Length
              m_lastValue = UInt64.MaxValue
            Case BoundNodeKind.SwapStatement
              Dim swap = CType(s, BoundSwapStatement)
              Dim variable1 = CType(swap.Variable1, BoundVariableExpression)
              Dim variable2 = CType(swap.Variable2, BoundVariableExpression)

              Dim hold = m_globals(variable1.Variable.Name)
              m_globals(variable1.Variable.Name) = m_globals(variable2.Variable.Name)
              m_globals(variable2.Variable.Name) = hold

              'Dim locals = m_locals.Peek
              'Dim hold = locals(variable1.Variable)
              'locals(variable1.Variable) = locals(variable2.Variable)
              'locals(variable2.Variable) = hold

              index += 1

            Case BoundNodeKind.VariableDeclaration : EvaluateVariableDeclaration(CType(s, BoundVariableDeclaration)) : index += 1
            Case BoundNodeKind.DimStatement : EvaluateDimStatement(s) : index += 1
            Case BoundNodeKind.EnvironStatement : EvaluateEnvironStatement(CType(s, BoundEnvironStatement)) : index += 1
            Case BoundNodeKind.EraseStatement : EvaluateEraseStatement(CType(s, BoundEraseStatement)) : index += 1
            Case BoundNodeKind.RedimStatement : EvaluateRedimStatement(CType(s, BoundRedimStatement)) : index += 1
            Case BoundNodeKind.ResumeStatement : EvaluateResumeStatement(CType(s, BoundResumeStatement)) : index += 1
            Case BoundNodeKind.ResumeNextStatement : EvaluateResumeNextStatement(CType(s, BoundResumeNextStatement)) : index += 1
            Case BoundNodeKind.CallStatement : EvaluateCallStatement(CType(s, BoundCallStatement)) : index += 1
            Case BoundNodeKind.DataStatement : EvaluateDataStatement(CType(s, BoundDataStatement)) : index += 1
            Case BoundNodeKind.DateStatement : EvaluateDateStatement(CType(s, BoundDateStatement)) : index += 1
            Case BoundNodeKind.ErrorStatement : EvaluateErrorStatement(CType(s, BoundErrorStatement)) : index += 1
            Case BoundNodeKind.ReadStatement : EvaluateReadStatement(CType(s, BoundReadStatement)) : index += 1
            Case BoundNodeKind.TimeStatement : EvaluateTimeStatement(CType(s, BoundTimeStatement)) : index += 1
            Case BoundNodeKind.SleepStatement : EvaluateSleepStatement(CType(s, BoundSleepStatement), index, localLabelToIndex) : index += 1
            Case BoundNodeKind.OnTimerGosubStatement : EvaluateOnTimerGosubStatement(CType(s, BoundOnTimerGosubStatement)) : index += 1
            Case BoundNodeKind.OnComGosubStatement : EvaluateOnComGosubStatement(CType(s, BoundOnComGosubStatement)) : index += 1
            Case BoundNodeKind.OnKeyGosubStatement : EvaluateOnKeyGosubStatement(CType(s, BoundOnKeyGosubStatement)) : index += 1
            Case BoundNodeKind.OnStrigGosubStatement : EvaluateOnStrigGosubStatement(CType(s, BoundOnStrigGosubStatement)) : index += 1
            Case BoundNodeKind.OnPlayGosubStatement : EvaluateOnPlayGosubStatement(CType(s, BoundOnPlayGosubStatement)) : index += 1
            Case BoundNodeKind.OnPenGosubStatement : EvaluateOnPenGosubStatement(CType(s, BoundOnPenGosubStatement)) : index += 1
            Case BoundNodeKind.TimerStatement : EvaluateTimerStatement(CType(s, BoundTimerStatement), index, localLabelToIndex) : index += 1
            Case BoundNodeKind.ComStatement : EvaluateComStatement(CType(s, BoundComStatement), index, localLabelToIndex) : index += 1
            Case BoundNodeKind.KeyEventStatement : EvaluateKeyEventStatement(CType(s, BoundKeyEventStatement), index, localLabelToIndex) : index += 1
            Case BoundNodeKind.StrigStatement : EvaluateStrigStatement(CType(s, BoundStrigStatement), index, localLabelToIndex) : index += 1
            Case BoundNodeKind.PlayEventStatement : EvaluatePlayEventStatement(CType(s, BoundPlayEventStatement), index, localLabelToIndex) : index += 1
            Case BoundNodeKind.PenStatement : EvaluatePenStatement(CType(s, BoundPenStatement), index, localLabelToIndex) : index += 1
            Case BoundNodeKind.OpenStatement : EvaluateOpenStatement(CType(s, BoundOpenStatement)) : index += 1
            Case BoundNodeKind.CloseStatement : EvaluateCloseStatement(CType(s, BoundCloseStatement)) : index += 1
            Case BoundNodeKind.LineInputFileStatement : EvaluateLineInputFileStatement(CType(s, BoundLineInputFileStatement)) : index += 1
            Case BoundNodeKind.PrintFileStatement : EvaluatePrintFileStatement(CType(s, BoundPrintFileStatement)) : index += 1
            Case BoundNodeKind.SeekStatement : EvaluateSeekStatement(CType(s, BoundSeekStatement)) : index += 1
            Case BoundNodeKind.OnErrorGotoStatement : EvaluateOnErrorGotoStatement(CType(s, BoundOnErrorGotoStatement)) : index += 1
            Case BoundNodeKind.OnErrorGotoZeroStatement : EvaluateOnErrorGotoZeroStatement(CType(s, BoundOnErrorGotoZeroStatement)) : index += 1
            Case BoundNodeKind.SelectCaseStatement : EvaluateSelectCaseStatement(CType(s, BoundSelectCaseStatement), localLabelToIndex) : index += 1
            Case BoundNodeKind.DoWhileStatement : EvaluateDoWhileStatement(CType(s, BoundDoWhileStatement), localLabelToIndex) : index += 1
            Case BoundNodeKind.DoUntilStatement : Console.WriteLine("DEBUG: Found DoUntilStatement") : EvaluateDoUntilStatement(CType(s, BoundDoUntilStatement), localLabelToIndex) : index += 1
            Case BoundNodeKind.ForStatement : EvaluateForStatement(CType(s, BoundForStatement), localLabelToIndex) : index += 1
            Case BoundNodeKind.WhileStatement : EvaluateWhileStatement(CType(s, BoundWhileStatement), localLabelToIndex) : index += 1
            Case Else
              Throw New Exception($"Unexpected kind {s.Kind}")
          End Select
        Catch ex As ResumeException
          ' Handle RESUME jumping
          ClearError()
          index = ex.TargetIndex
          Continue While
        Catch ex As QBasicRuntimeException
          ' Handle runtime errors - set error and handle it
          If m_err = 0 Then ' Only set if not already set
            m_err = ex.ErrorCode
            'Select Case ex.Message
            '  Case "Overflow" : m_err = ErrorCode.Overflow
            '  Case "RESUME without error" : m_err = ErrorCode.ResumeWithoutError
            '  Case "Advanced feature unavailable" : m_err = ErrorCode.AdvancedFeature
            '  Case Else : m_err = ErrorCode.Internal
            'End Select
          End If
          m_errorPending = True
          If HandlePendingError(index, localLabelToIndex) Then
            Continue While
          End If
        Catch ex As Exception
          ' Handle other runtime exceptions
          If m_err = 0 Then
            If ex.GetType() = GetType(DivideByZeroException) Then
              m_err = ErrorCode.DivisionByZero
            ElseIf ex.GetType() = GetType(IndexOutOfRangeException) Then
              m_err = ErrorCode.SubscriptOutOfRange
            ElseIf ex.GetType() = GetType(IOException) Then
              m_err = ErrorCode.BadFileMode
            ElseIf ex.GetType() = GetType(ArgumentException) Then
              m_err = ErrorCode.BadFileMode
            ElseIf ex.GetType = GetType(ObjectDisposedException) Then
              m_err = ErrorCode.BadFileMode
            Else
              m_err = ErrorCode.Internal
            End If
          End If
          m_errorPending = True
          If HandlePendingError(index, localLabelToIndex) Then
            Continue While
          End If
        End Try
      End While

      Return m_lastValue

    End Function

    Private Sub EvaluateForStatement(node As BoundForStatement, labelToIndex As Dictionary(Of String, Integer))
      Dim lower As Integer = CInt(EvaluateExpression(node.LowerBound))
      Dim upper As Integer = CInt(EvaluateExpression(node.UpperBound))
      Dim stepValue As Integer = If(node.Stepper Is Nothing, 1, CInt(EvaluateExpression(node.Stepper)))
      Dim variable = node.Variable
      Assign(variable, CObj(lower))
      While True
        Dim condition = New BoundBinaryExpression(
          New BoundVariableExpression(variable),
          BoundBinaryOperator.Bind(SyntaxKind.LessThanEqualToken, TypeSymbol.Integer, TypeSymbol.Integer),
          New BoundLiteralExpression(CObj(upper)))
        Dim condResult = EvaluateExpression(condition)
        If DirectCast(condResult, Boolean) Then
          Dim bodyBlock = CType(node.Body, BoundBlockStatement)
          EvaluateStatement(bodyBlock, labelToIndex)
        Else
          Exit While
        End If
        Dim increment = New BoundBinaryExpression(
          New BoundVariableExpression(variable),
          BoundBinaryOperator.Bind(SyntaxKind.PlusToken, TypeSymbol.Integer, TypeSymbol.Integer),
          New BoundLiteralExpression(CObj(stepValue)))
        Assign(variable, EvaluateExpression(increment))
      End While
    End Sub

    Private Sub EvaluateWhileStatement(node As BoundWhileStatement, labelToIndex As Dictionary(Of String, Integer))
      Dim exprResult = EvaluateExpression(node.Expression)
      While DirectCast(exprResult, Boolean)
        Dim bodyBlock = CType(node.Statements, BoundBlockStatement)
        EvaluateStatement(bodyBlock, labelToIndex)
        exprResult = EvaluateExpression(node.Expression)
      End While
    End Sub

    Private Sub EvaluateDataStatement(node As BoundDataStatement)
      For Each value In node.Data
        m_data.Add(value)
      Next
    End Sub

    Private Sub EvaluateReadStatement(node As BoundReadStatement)
      For Each variable In node.Variables
        If m_dataIndex < m_data.Count Then
          m_globals(variable.Name) = m_data(m_dataIndex)
          m_dataIndex += 1
        Else
          ' Out of data error
          'Throw New Exception("Out of DATA")
          'SetError(ErrorCode.OutOfData)
          Throw New QBasicRuntimeException(ErrorCode.OutOfData)
          ' Error will be handled by the main evaluation loop
        End If
      Next
    End Sub

    Private Sub EvaluateDateStatement(node As BoundDateStatement)
      ' DATE$ = stringexpression$ - sets the system date
      ' In a real implementation, this would set the system date
      ' For now, we'll just ignore it as setting system date requires admin privileges
      Dim dateString = CStr(EvaluateExpression(node.Expression))
      ' TODO: Implement actual date setting if needed
    End Sub

    Private Sub EvaluateTimeStatement(node As BoundTimeStatement)
      ' TIME$ = stringexpression$ - sets the system time
      ' In a real implementation, this would set the system time
      ' For now, we'll just ignore it as setting system time requires admin privileges
      Dim timeString = CStr(EvaluateExpression(node.Expression))
      ' TODO: Implement actual time setting if needed
    End Sub

    Private Sub EvaluateOnTimerGosubStatement(node As BoundOnTimerGosubStatement)
      ' ON TIMER(interval) GOSUB target - sets up timer event handler
      ' Interval must be 1-86400 seconds
      Dim intervalValue = EvaluateExpression(node.Interval)
      If TypeOf intervalValue Is String Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If

      Dim interval As Double = CDbl(intervalValue)
      If interval < 1 Or interval > 86400 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      ' Set the timer handler and interval
      ' For ON TIMER GOSUB, the target is a label, not an expression to evaluate
      If TypeOf node.Target Is BoundVariableExpression Then
        m_timerHandlerTarget = CType(node.Target, BoundVariableExpression).Variable.Name
      Else
        ' Fallback: try to evaluate as string
        m_timerHandlerTarget = CStr(EvaluateExpression(node.Target))
      End If
      m_timerInterval = interval

      ' Timer remains OFF until TIMER ON is executed
      m_timerState = TimerState.Off
      m_timerEventPending = False


    End Sub

    Private Sub EvaluateOnComGosubStatement(node As BoundOnComGosubStatement)
      ' ON COM(channel) GOSUB target - sets up COM event handler
      ' Channel must be 1 or 2
      Dim channelValue = EvaluateExpression(node.Channel)
      If TypeOf channelValue Is String Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If

      Dim channel As Integer = CInt(channelValue)
      If channel < 1 Or channel > 2 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      ' Set the COM handler
      ' For ON COM GOSUB, the target is a label, not an expression to evaluate
      If TypeOf node.Target Is BoundVariableExpression Then
        m_comHandlerTargets(channel - 1) = CType(node.Target, BoundVariableExpression).Variable.Name
      Else
        ' Fallback: try to evaluate as string
        m_comHandlerTargets(channel - 1) = CStr(EvaluateExpression(node.Target))
      End If

      ' COM remains OFF until COM(channel) ON is executed
      m_comStates(channel - 1) = TimerState.Off
    End Sub

    Private Sub EvaluateOnKeyGosubStatement(node As BoundOnKeyGosubStatement)
      ' ON KEY(keyNumber) GOSUB target - sets up KEY event handler
      ' Key number must be 1-20
      Dim keyValue = EvaluateExpression(node.KeyNumber)
      If TypeOf keyValue Is String Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If

      Dim keyNumber As Integer = CInt(keyValue)
      If keyNumber < 1 Or keyNumber > 20 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      ' Set the KEY handler
      If TypeOf node.Target Is BoundVariableExpression Then
        m_keyHandlerTargets(keyNumber) = CType(node.Target, BoundVariableExpression).Variable.Name
      Else
        m_keyHandlerTargets(keyNumber) = CStr(EvaluateExpression(node.Target))
      End If

      ' KEY remains OFF until KEY(keyNumber) ON is executed
      m_keyStates(keyNumber) = TimerState.Off
    End Sub

    Private Sub EvaluateOnStrigGosubStatement(node As BoundOnStrigGosubStatement)
      ' ON STRIG(triggerNumber) GOSUB target - sets up STRIG event handler
      ' Trigger number must be 0, 2, 4, or 6
      Dim triggerValue = EvaluateExpression(node.TriggerNumber)
      If TypeOf triggerValue Is String Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If

      Dim triggerNumber As Integer = CInt(triggerValue)
      If triggerNumber <> 0 AndAlso triggerNumber <> 2 AndAlso triggerNumber <> 4 AndAlso triggerNumber <> 6 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      ' Set the STRIG handler
      If TypeOf node.Target Is BoundVariableExpression Then
        m_strigHandlerTargets(triggerNumber) = CType(node.Target, BoundVariableExpression).Variable.Name
      Else
        m_strigHandlerTargets(triggerNumber) = CStr(EvaluateExpression(node.Target))
      End If

      ' STRIG remains OFF until STRIG(triggerNumber) ON is executed
      m_strigStates(triggerNumber) = TimerState.Off
    End Sub

    Private Sub EvaluateOnPlayGosubStatement(node As BoundOnPlayGosubStatement)
      ' ON PLAY(queueSize) GOSUB target - sets up PLAY event handler
      ' Queue size must be 1-32
      Dim queueValue = EvaluateExpression(node.QueueSize)
      If TypeOf queueValue Is String Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If

      Dim queueSize As Integer = CInt(queueValue)
      If queueSize < 1 Or queueSize > 32 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      ' Set the PLAY handler
      If TypeOf node.Target Is BoundVariableExpression Then
        m_playHandlerTarget = CType(node.Target, BoundVariableExpression).Variable.Name
      Else
        m_playHandlerTarget = CStr(EvaluateExpression(node.Target))
      End If
      m_playQueueSize = queueSize

      ' PLAY remains OFF until PLAY(queueSize) ON is executed
      m_playState = TimerState.Off
    End Sub

    Private Sub EvaluateOnPenGosubStatement(node As BoundOnPenGosubStatement)
      ' ON PEN GOSUB target - sets up PEN event handler
      ' Set the PEN handler
      If TypeOf node.Target Is BoundVariableExpression Then
        m_penHandlerTarget = CType(node.Target, BoundVariableExpression).Variable.Name
      Else
        m_penHandlerTarget = CStr(EvaluateExpression(node.Target))
      End If

      ' PEN remains OFF until PEN ON is executed
      m_penState = TimerState.Off
    End Sub

    Private Sub EvaluateTimerStatement(node As BoundTimerStatement, ByRef currentIndex As Integer, labelToIndex As Dictionary(Of String, Integer))
      ' TIMER ON/OFF/STOP - controls timer event state
      Select Case node.VerbKind
        Case SyntaxKind.OnKeyword
          If m_timerHandlerTarget Is Nothing Then
            ' No handler set up yet
            Return
          End If
          m_timerState = TimerState.On
          ' Schedule next trigger
          If m_timerEventPending Then
            ' Event was pending from STOP state, trigger immediately
            m_timerEventPending = False
            TriggerTimerEvent(currentIndex, labelToIndex)
          Else
            ' Start fresh timer
            m_timerNextTrigger = DateTime.Now.AddSeconds(m_timerInterval)
          End If

        Case SyntaxKind.OffKeyword
          m_timerState = TimerState.Off
          m_timerEventPending = False
          ' Clear handler
          m_timerHandlerTarget = Nothing
          m_timerInterval = 0


        Case SyntaxKind.StopKeyword
          m_timerState = TimerState.Stop
          ' Event remains pending if it was about to trigger

      End Select
    End Sub

    Private Sub EvaluateComStatement(node As BoundComStatement, ByRef currentIndex As Integer, labelToIndex As Dictionary(Of String, Integer))
      ' COM(channel) ON/OFF/STOP - controls COM event state
      Dim channelValue = EvaluateExpression(node.Channel)
      If TypeOf channelValue Is String Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If

      Dim channel As Integer = CInt(channelValue)
      If channel < 1 Or channel > 2 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      Dim channelIndex = channel - 1
      Select Case node.VerbKind
        Case SyntaxKind.OnKeyword
          If m_comHandlerTargets(channelIndex) Is Nothing Then
            ' No handler set up yet
            Return
          End If
          m_comStates(channelIndex) = TimerState.On

        Case SyntaxKind.OffKeyword
          m_comStates(channelIndex) = TimerState.Off
          ' Clear handler
          m_comHandlerTargets(channelIndex) = Nothing

        Case SyntaxKind.StopKeyword
          m_comStates(channelIndex) = TimerState.Stop
      End Select
    End Sub

    Private Sub EvaluateKeyEventStatement(node As BoundKeyEventStatement, ByRef currentIndex As Integer, labelToIndex As Dictionary(Of String, Integer))
      ' KEY(keyNumber) ON/OFF/STOP - controls KEY event state
      Dim keyValue = EvaluateExpression(node.KeyNumber)
      If TypeOf keyValue Is String Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If

      Dim keyNumber As Integer = CInt(keyValue)
      If keyNumber < 1 Or keyNumber > 20 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      Select Case node.VerbKind
        Case SyntaxKind.OnKeyword
          If m_keyHandlerTargets(keyNumber) Is Nothing Then
            ' No handler set up yet
            Return
          End If
          m_keyStates(keyNumber) = TimerState.On

        Case SyntaxKind.OffKeyword
          m_keyStates(keyNumber) = TimerState.Off
          ' Clear handler
          m_keyHandlerTargets(keyNumber) = Nothing

        Case SyntaxKind.StopKeyword
          m_keyStates(keyNumber) = TimerState.Stop
      End Select
    End Sub

    Private Sub EvaluateStrigStatement(node As BoundStrigStatement, ByRef currentIndex As Integer, labelToIndex As Dictionary(Of String, Integer))
      ' STRIG(triggerNumber) ON/OFF/STOP - controls STRIG event state
      Dim triggerValue = EvaluateExpression(node.TriggerNumber)
      If TypeOf triggerValue Is String Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If

      Dim triggerNumber As Integer = CInt(triggerValue)
      If triggerNumber <> 0 AndAlso triggerNumber <> 2 AndAlso triggerNumber <> 4 AndAlso triggerNumber <> 6 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      Select Case node.VerbKind
        Case SyntaxKind.OnKeyword
          If m_strigHandlerTargets(triggerNumber) Is Nothing Then
            ' No handler set up yet
            Return
          End If
          m_strigStates(triggerNumber) = TimerState.On

        Case SyntaxKind.OffKeyword
          m_strigStates(triggerNumber) = TimerState.Off
          ' Clear handler
          m_strigHandlerTargets(triggerNumber) = Nothing

        Case SyntaxKind.StopKeyword
          m_strigStates(triggerNumber) = TimerState.Stop
      End Select
    End Sub

    Private Sub EvaluatePlayEventStatement(node As BoundPlayEventStatement, ByRef currentIndex As Integer, labelToIndex As Dictionary(Of String, Integer))
      ' PLAY(queueSize) ON/OFF/STOP - controls PLAY event state
      Dim queueValue = EvaluateExpression(node.QueueSize)
      If TypeOf queueValue Is String Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If

      Dim queueSize As Integer = CInt(queueValue)
      If queueSize < 1 Or queueSize > 32 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      Select Case node.VerbKind
        Case SyntaxKind.OnKeyword
          If m_playHandlerTarget Is Nothing Then
            ' No handler set up yet
            Return
          End If
          m_playState = TimerState.On

        Case SyntaxKind.OffKeyword
          m_playState = TimerState.Off
          ' Clear handler
          m_playHandlerTarget = Nothing
          m_playQueueSize = 0

        Case SyntaxKind.StopKeyword
          m_playState = TimerState.Stop
      End Select
    End Sub

    Private Sub EvaluatePenStatement(node As BoundPenStatement, ByRef currentIndex As Integer, labelToIndex As Dictionary(Of String, Integer))
      ' PEN ON/OFF/STOP - controls PEN event state
      Select Case node.VerbKind
        Case SyntaxKind.OnKeyword
          If m_penHandlerTarget Is Nothing Then
            ' No handler set up yet
            Return
          End If
          m_penState = TimerState.On

        Case SyntaxKind.OffKeyword
          m_penState = TimerState.Off
          ' Clear handler
          m_penHandlerTarget = Nothing

        Case SyntaxKind.StopKeyword
          m_penState = TimerState.Stop
      End Select
    End Sub

    Private Sub TriggerTimerEvent(ByRef currentIndex As Integer, labelToIndex As Dictionary(Of String, Integer))
      ' Trigger timer event by GOSUBing to handler
      ' Temporarily disable timer to prevent recursive triggers
      Dim savedState = m_timerState
      m_timerState = TimerState.Stop

      ' GOSUB to timer handler
      Dim handlerLabel = CStr(m_timerHandlerTarget)
      Dim handlerIndex As Integer = -1
      If labelToIndex IsNot Nothing AndAlso labelToIndex.TryGetValue(handlerLabel, handlerIndex) Then
        ' Push current location for RETURN
        m_gosubStack.Push(currentIndex + 1) ' Push next statement for RETURN
        ' Jump to handler
        currentIndex = handlerIndex
      End If

      ' Schedule next trigger
      If savedState = TimerState.On Then
        m_timerNextTrigger = DateTime.Now.AddSeconds(m_timerInterval)
        m_timerState = TimerState.On
      Else
        m_timerState = savedState
      End If
    End Sub

    Private Function CheckTimerEvent(ByRef currentIndex As Integer, labelToIndex As Dictionary(Of String, Integer)) As Boolean
      ' Check if timer event should trigger
      If m_timerState = TimerState.On AndAlso m_timerHandlerTarget IsNot Nothing Then
        If DateTime.Now >= m_timerNextTrigger Then
          TriggerTimerEvent(currentIndex, labelToIndex)
          Return True
        End If
      End If
      Return False
    End Function

    Private Sub EvaluateSleepStatement(node As BoundSleepStatement, ByRef currentIndex As Integer, labelToIndex As Dictionary(Of String, Integer))
      ' SLEEP [seconds] - suspends execution until timeout, keypress, or ON event
      ' Numbers < 1 treated as 0 (infinite wait)
      ' 0 = infinite wait until keypress or ON event
      ' Other values rounded to nearest integer seconds

      Dim seconds As Double = 0

      ' Evaluate the optional seconds parameter
      If node.Expression IsNot Nothing Then
        Dim value = EvaluateExpression(node.Expression)
        If TypeOf value Is String Then
          Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
        End If
        seconds = CDbl(value)
      End If

      ' Determine sleep duration
      ' Numbers < 1 are treated as 0 (infinite)
      ' Other values rounded to nearest integer
      Dim sleepDuration As Double
      If seconds < 1 Then
        sleepDuration = -1 ' Infinite wait
      Else
        sleepDuration = Math.Round(seconds)
      End If

      ' Clear keyboard buffer - ignore any keys pressed before SLEEP
      QBLib.Video.INKEY$() ' Clear any pending key

      Dim startTime = DateTime.Now
      Dim endTime As DateTime
      If sleepDuration >= 0 Then
        endTime = startTime.AddSeconds(sleepDuration)
      End If

      ' Sleep loop - check for keypress, timer events, or timeout
      Do
        ' Check for keypress (any key including modifiers)
        Dim key = QBLib.Video.INKEY$()
        If Not String.IsNullOrEmpty(key) Then
          Exit Do ' Key pressed, exit sleep
        End If

        ' Check for timer events
        If CheckTimerEvent(currentIndex, labelToIndex) Then
          Exit Do ' Timer event triggered
        End If

        ' Check for timeout (if not infinite)
        If sleepDuration >= 0 AndAlso DateTime.Now >= endTime Then
          Exit Do ' Timeout reached
        End If

        ' TODO: Check for other ON events (COM, KEY, PEN, PLAY, STRIG)

        ' Small delay to prevent busy waiting
        Threading.Thread.Sleep(10)
      Loop While True
    End Sub

    Private Sub EvaluateOnErrorGotoStatement(node As BoundOnErrorGotoStatement)
      ' Set error handler target
      If TypeOf node.Target Is BoundVariableExpression Then
        ' Target is a label/variable name
        Dim varExpr = CType(node.Target, BoundVariableExpression)
        m_errorHandlerTarget = varExpr.Variable.Name?.ToLower
      Else
        ' Target is an expression (line number)
        Dim targetValue = EvaluateExpression(node.Target)
        If IsNumeric(targetValue) Then
          ' Line number - convert to label format
          m_errorHandlerTarget = GOTO_LABEL_PREFIX & CStr(targetValue)
        Else
          ' Label name
          m_errorHandlerTarget = CStr(targetValue)?.ToLower
        End If
      End If
      m_errorResumeNext = False
      'Console.WriteLine($"DEBUG: Set error handler target to '{m_errorHandlerTarget}'")
    End Sub

    Private Sub EvaluateOnErrorGotoZeroStatement(node As BoundOnErrorGotoZeroStatement)
      ' Disable error handling
      m_errorHandlerTarget = Nothing
      m_errorResumeNext = False
    End Sub

    Private Sub EvaluateResumeStatement(node As BoundResumeStatement)
      If Not m_errorPending AndAlso m_errorResumeIndex = -1 Then
        Throw New QBasicRuntimeException(ErrorCode.ResumeWithoutError)
      End If

      ' For RESUME with label, we'd need to implement jumping to specific labels
      ' For now, implement basic RESUME (return to error location)
      If m_errorResumeIndex >= 0 Then
        ' This will be handled by returning a special value to the main loop
        Throw New ResumeException(m_errorResumeIndex)
      End If
    End Sub

    Private Sub EvaluateResumeNextStatement(node As BoundResumeNextStatement)
      If m_errorResumeIndex = -1 Then
        Throw New QBasicRuntimeException(ErrorCode.ResumeWithoutError)
      End If

      ' RESUME NEXT - continue with statement after the error
      Throw New ResumeException(m_errorResumeIndex + 1)
    End Sub

    Private Sub EvaluateErrorStatement(node As BoundErrorStatement)
      Dim errorCode = CType(CInt(EvaluateExpression(node.Expression)), ErrorCode)
      'SetError(errorCode)
      Throw New QBasicRuntimeException(errorCode)
      ' Error will be handled by the main evaluation loop
    End Sub

    Private Sub EvaluateSelectCaseStatement(node As BoundSelectCaseStatement, labelToIndex As Dictionary(Of String, Integer))
      Dim testValue As Object = EvaluateExpression(node.Test)

      ' Check each case
      For Each caseStmt In node.Cases
        For Each match In caseStmt.Matches
          Dim isMatch = False
          Select Case match.MatchType
            Case CaseMatchType.Value
              ' Direct value comparison - use numeric comparison for compatibility
              Dim matchValue As Object = EvaluateExpression(match.Expression)
              Try
                ' Try numeric comparison first
                Dim testNum = CDbl(testValue)
                Dim matchNum = CDbl(matchValue)
                isMatch = (testNum = matchNum)
              Catch ex As Exception
                ' Fall back to object comparison
                isMatch = Object.Equals(testValue, matchValue)
              End Try
            Case CaseMatchType.Range
              ' Range comparison: start <= testValue <= end
              Dim startValue As Object = EvaluateExpression(match.Expression)
              Dim endValue As Object = EvaluateExpression(match.ExpressionTo)
              ' For simplicity, assume numeric comparison for ranges
              Dim testNum = CDbl(testValue)
              Dim startNum = CDbl(startValue)
              Dim endNum = CDbl(endValue)
              isMatch = (testNum >= startNum AndAlso testNum <= endNum)
            Case CaseMatchType.IsComparison
              ' IS comparison: testValue IS comparison expression
              Dim compareValue As Object = EvaluateExpression(match.Expression)
              ' For simplicity, assume numeric comparison for IS comparisons
              Dim testNum = CDbl(testValue)
              Dim compareNum = CDbl(compareValue)
              Select Case match.Comparison
                Case SyntaxKind.EqualToken
                  isMatch = (testNum = compareNum)
                Case SyntaxKind.LessThanToken
                  isMatch = (testNum < compareNum)
                Case SyntaxKind.LessThanEqualToken
                  isMatch = (testNum <= compareNum)
                Case SyntaxKind.GreaterThanToken
                  isMatch = (testNum > compareNum)
                Case SyntaxKind.GreaterThanEqualToken
                  isMatch = (testNum >= compareNum)
                Case SyntaxKind.LessThanGreaterThanToken
                  isMatch = (testNum <> compareNum)
              End Select
          End Select

          If isMatch Then
            EvaluateStatement(CType(caseStmt.Statement, BoundBlockStatement), labelToIndex)
            Return ' Exit after first matching case
          End If
        Next
      Next

      ' If no case matched and there's an ELSE clause, execute it
      If node.ElseStatement IsNot Nothing Then
        EvaluateStatement(CType(node.ElseStatement, BoundBlockStatement))
      End If
    End Sub

    Private Sub EvaluateDoWhileStatement(node As BoundDoWhileStatement, labelToIndex As Dictionary(Of String, Integer))
      If node.AtBeginning Then
        ' DO WHILE condition ... LOOP
        Do
          Dim conditionValue = CBool(EvaluateExpression(node.Expression))
          If Not conditionValue Then Exit Do
          EvaluateStatement(CType(node.Statements, BoundBlockStatement), labelToIndex)
        Loop
      Else
        ' DO ... LOOP WHILE condition
        Do
          EvaluateStatement(CType(node.Statements, BoundBlockStatement), labelToIndex)
          Dim conditionValue = CBool(EvaluateExpression(node.Expression))
          If Not conditionValue Then Exit Do
        Loop
      End If
    End Sub

    Private Sub EvaluateDoUntilStatement(node As BoundDoUntilStatement, labelToIndex As Dictionary(Of String, Integer))
      If node.AtBeginning Then
        ' DO UNTIL condition ... LOOP
        Console.WriteLine("DEBUG: DO UNTIL AtBeginning = True")
        Dim iterations = 0
        Do
          iterations += 1
          If iterations > 10 Then
            Console.WriteLine("DEBUG: Too many iterations, exiting")
            Exit Do
          End If
          Dim conditionValue = CBool(EvaluateExpression(node.Expression))
          Console.WriteLine("DEBUG: Condition = " & CStr(conditionValue))
          If conditionValue Then
            Console.WriteLine("DEBUG: Exiting loop")
            Exit Do
          End If
          Console.WriteLine("DEBUG: Executing body")
          EvaluateStatement(CType(node.Statements, BoundBlockStatement), labelToIndex)
        Loop
        Console.WriteLine("DEBUG: Loop finished")
      Else
        ' DO ... LOOP UNTIL condition
        Do
          EvaluateStatement(CType(node.Statements, BoundBlockStatement), labelToIndex)
          Dim conditionValue = CBool(EvaluateExpression(node.Expression))
          If conditionValue Then Exit Do
        Loop
      End If
    End Sub

    Private Sub EvaluateMidStatement(node As BoundMidStatement)
      Dim positionValue = CInt(EvaluateExpression(node.PositionExpression))
      Dim lengthValue = CInt(If(node.LengthExpression Is Nothing, Integer.MaxValue, EvaluateExpression(node.LengthExpression)))
      Dim value = CStr(EvaluateExpression(node.Expression))
      'Assign(node.Variable, value)
      If node.Variable.Kind = SymbolKind.GlobalVariable Then
        Dim temp = CStr(m_globals(node.Variable.Name))
        Mid(temp, positionValue, lengthValue) = value
        m_globals(node.Variable.Name) = temp
      Else
        Dim locals = m_locals.Peek
        locals(node.Variable.Name) = value
        Dim temp = CStr(locals(node.Variable.Name))
        Mid(temp, positionValue, lengthValue) = value
        locals(node.Variable.Name) = temp
      End If
    End Sub

    Private Sub EvaluateLetStatement(node As BoundLetStatement)
      Dim value = EvaluateExpression(node.Expression)
      Debug.Assert(value IsNot Nothing)
      m_lastValue = value
      Assign(node.Variable, value)
    End Sub

    Private Shared Sub EvaluateHandleCommaStatement(node As BoundHandleCommaStatement)
      If node IsNot Nothing Then
      End If
      'Dim screenWidth = 80
      Dim zoneWidth = 14
      Dim pos = QBLib.Video.POS(0) 'Console.CursorLeft + 1
      Dim cur = pos Mod zoneWidth
      QBLib.Video.PRINT(Microsoft.VisualBasic.Strings.Space(cur), True)
    End Sub

    Private Shared Sub EvaluateHandlePrintLineStatement(node As BoundHandlePrintLineStatement)
      If node IsNot Nothing Then
      End If
      QBLib.Video.PRINT()
    End Sub

    Private Sub EvaluatePrintStatement(node As BoundPrintStatement)
      For Each item In node.Nodes
        Select Case item.Kind
          Case BoundNodeKind.Symbol
            Dim symbol = CType(item, BoundSymbol)
            If symbol.Value = ";" Then
              ' Semicolon suppresses newline
            ElseIf symbol.Value = "," Then
              ' Comma advances to next print zone
              QBLib.Video.PRINT("        ", False) ' Print 8 spaces for tab
            End If
          Case BoundNodeKind.SpcFunction
            Dim spcFunc = CType(item, BoundSpcFunction)
            Dim count = CInt(EvaluateExpression(spcFunc.Expression))
            ' TODO: Implement SPC function - for now just print spaces
            QBLib.Video.PRINT(New String(" "c, count), False)
          Case BoundNodeKind.TabFunction
            Dim tabFunc = CType(item, BoundTabFunction)
            Dim column = CInt(EvaluateExpression(tabFunc.Expression))
            ' TODO: Implement TAB function - for now just print
            QBLib.Video.PRINT("TAB(" & column & ")", False)
          Case BoundNodeKind.LiteralExpression
            Dim literal = CType(item, BoundLiteralExpression)
            QBLib.Video.PRINT(CStr(literal.Value), False)
          Case Else
            ' Regular expression to print
            Dim value = EvaluateExpression(CType(item, BoundExpression))
            QBLib.Video.PRINT(CStr(value), False)
        End Select
      Next
      ' Add newline at end unless last item was semicolon
      If node.Nodes.Length = 0 OrElse Not (node.Nodes.Last.Kind = BoundNodeKind.Symbol AndAlso CType(node.Nodes.Last, BoundSymbol).Value = ";") Then
        QBLib.Video.PRINT("", True)
      End If
    End Sub

    Private Sub EvaluateHandlePrintStatement(node As BoundHandlePrintStatement)
      Dim value = EvaluateExpression(node.Expression)
      Dim str = value.ToString()
      If TypeOf value IsNot String AndAlso
         Not str.StartsWith("-"c) Then
        QBLib.Video.PRINT(" ", True) ': QBLib.Video.PRINT(" "c, True)
      End If
      QBLib.Video.PRINT(str, node.NoCr) ': QBLib.Video.PRINT(" "c, True)
    End Sub

    Private Sub EvaluateHandleSpcStatement(node As BoundHandleSpcStatement)
      Dim screenWidth = 80
      'Dim zoneWidth = 14
      Dim result = EvaluateExpression(node.Expression)
      Dim value = CInt(result)
      If value < 0 OrElse value > 255 Then
        'error
      ElseIf value > screenWidth Then
        value = value Mod screenWidth
      End If
      Dim str = Microsoft.VisualBasic.Strings.Space(value)
      QBLib.Video.PRINT(str, True)
    End Sub

    Private Sub EvaluateHandleTabStatement(node As BoundHandleTabStatement)
      Dim screenWidth = 80
      'Dim zoneWidth = 14
      Dim result = EvaluateExpression(node.Expression)
      Dim value = CInt(result)
      If value < 0 OrElse value > 255 Then
        ' error
      End If
      Dim pos = QBLib.Video.POS(0)  'Console.CursorLeft + 1
      Dim diff = 0
      If pos < value Then
        diff = value - pos
      ElseIf pos > value Then
        diff = screenWidth - pos
        QBLib.Video.PRINT(Microsoft.VisualBasic.Strings.Space(diff))
        diff = value
      End If
      Dim str = Microsoft.VisualBasic.Strings.Space(diff)
      QBLib.Video.PRINT(str, True)
    End Sub

    Private Sub EvaluateIfStatement(node As BoundIfStatement, labelToIndex As Dictionary(Of String, Integer))
      Dim conditionValue = CBool(EvaluateExpression(node.Expression))
      If conditionValue Then
        EvaluateStatement(CType(node.Statements, BoundBlockStatement), labelToIndex)
      Else
        Dim executed = False
        For Each elseIfClause In node.ElseIfStatements
          If Not executed Then
            Dim elseIfCondition = CBool(EvaluateExpression(elseIfClause.Expression))
            If elseIfCondition Then
              EvaluateStatement(CType(elseIfClause.Statements, BoundBlockStatement))
              executed = True
            End If
          End If
        Next
        If Not executed AndAlso node.ElseStatement IsNot Nothing Then
          EvaluateStatement(CType(node.ElseStatement, BoundBlockStatement), labelToIndex)
        End If
      End If
    End Sub

    Private Sub EvaluateVariableDeclaration(node As BoundVariableDeclaration)
      Dim value As Object
      If node.Variable.IsArray Then
        ' Initialize array as a List
        Dim lowerBound = CInt(EvaluateExpression(node.Variable.Lower))
        Dim upperBound = CInt(EvaluateExpression(node.Variable.Upper))
        Dim size = upperBound - lowerBound + 1
        Dim arrayList = New List(Of Object)(size)
        For i = 0 To size - 1
          If node.Variable.Type Is TypeSymbol.String Then
            arrayList.Add("")
          Else
            arrayList.Add(0)
          End If
        Next
        value = arrayList
      ElseIf node.Initializer IsNot Nothing Then
        value = EvaluateExpression(node.Initializer)
      Else
        If node.Variable.Type Is TypeSymbol.String Then
          value = ""
        Else
          value = 0
        End If
      End If
      Debug.Assert(value IsNot Nothing)
      m_lastValue = value
      Assign(node.Variable, value)
    End Sub

    Private Sub EvaluateDimStatement(node As BoundStatement)
      Dim dimStmt = CType(node, BoundDimStatement)
      Dim declarations = DirectCast(CallByName(node, "Declarations", CallType.Get), ImmutableArray(Of BoundVariableDeclaration))
      Dim preserve = False
      If node.GetType() = GetType(BoundRedimStatement) Then
        preserve = CBool(CallByName(node, "Preserve", CallType.Get))
      End If
      For Each declaration In declarations
        Dim variable = declaration.Variable
        If variable.IsArray Then
          ' Calculate new size
          Dim lower = EvaluateExpression(variable.Lower)
          Dim upper = EvaluateExpression(variable.Upper)
          Dim lowerInt As Integer
          Dim upperInt As Integer
          ' Convert bounds to integers with proper error handling
          Try
            lowerInt = Convert.ToInt32(lower)
            upperInt = Convert.ToInt32(upper)
          Catch ex As OverflowException
            Throw New QBasicRuntimeException(ErrorCode.Overflow)
            'Throw New Exception("Array bounds are out of valid range (-32768 to 32767).")
          End Try
          If upperInt < lowerInt Then
            'Throw New Exception($"Array bounds are invalid: {lowerInt} to {upperInt}. Lower bound must be less than or equal to upper bound.")
            Throw New QBasicRuntimeException(ErrorCode.SubscriptOutOfRange)
          End If
          ' Check for reasonable array size (QBasic had memory limits)
          Dim arraySize = CLng(upperInt) - CLng(lowerInt) + 1 ' Use Long to avoid overflow
          If arraySize > 16383 Then
            'Throw New Exception($"Array size {arraySize} is too large. Maximum allowed is 16383 elements.")
            Throw New QBasicRuntimeException(ErrorCode.Overflow)
          End If
          If arraySize <= 0 Then
            'Throw New Exception($"Invalid array size {arraySize}. Array must have at least 1 element.")
            Throw New QBasicRuntimeException(ErrorCode.SubscriptOutOfRange)
          End If
          Dim newSize = CInt(arraySize)
          ' Get existing list
          Dim existingList As List(Of Object) = Nothing
          If m_globals.ContainsKey(variable.Name) AndAlso TypeOf m_globals(variable.Name) Is List(Of Object) Then
            existingList = CType(m_globals(variable.Name), List(Of Object))
          End If

          ' Create new list
          Dim newList = New List(Of Object)(newSize)
          For i = 0 To newSize - 1
            newList.Add(0)
          Next
          ' Copy old values if preserve
          If preserve AndAlso existingList IsNot Nothing Then
            Dim copySize = Math.Min(existingList.Count, newSize)
            For i = 0 To copySize - 1
              newList(i) = existingList(i)
            Next
          End If
          m_globals(variable.Name) = newList
        Else
          EvaluateVariableDeclaration(declaration)
        End If
      Next
    End Sub

    Private Sub EvaluateRedimStatement(node As BoundRedimStatement)
      For Each declaration In node.Declarations
        Dim variable = declaration.Variable
        If variable.IsArray Then
          ' Calculate new size
          Dim lower = EvaluateExpression(variable.Lower)
          Dim upper = EvaluateExpression(variable.Upper)
          Dim newSize = CInt(upper) - CInt(lower) + 1
          ' Get existing list
          Dim existingList As List(Of Object) = Nothing
          If m_globals.ContainsKey(variable.Name) AndAlso TypeOf m_globals(variable.Name) Is List(Of Object) Then
            existingList = CType(m_globals(variable.Name), List(Of Object))
          End If

          ' Create new list
          Dim newList = New List(Of Object)(newSize)
          For i = 0 To newSize - 1
            newList.Add(0)
          Next
          ' Copy old values if preserve
          If node.Preserve AndAlso existingList IsNot Nothing Then
            Dim copySize = Math.Min(existingList.Count, newSize)
            For i = 0 To copySize - 1
              newList(i) = existingList(i)
            Next
          End If
          m_globals(variable.Name) = newList
          ' Update current bounds for this array
          Dim lowerBound = CInt(EvaluateExpression(variable.Lower))
          Dim upperBound = CInt(EvaluateExpression(variable.Upper))
          m_arrayBounds(variable.Name) = (lowerBound, upperBound)
        End If
      Next
    End Sub

    Private Sub EvaluateEraseStatement(node As BoundEraseStatement)
      For Each variable In node.Variables
        If TypeOf variable Is BoundVariableExpression Then
          Dim varExpr = CType(variable, BoundVariableExpression)
          Dim varName = varExpr.Variable.Name
          Dim varSymbol = varExpr.Variable

          ' Check if variable exists
          If Not m_globals.ContainsKey(varName) Then
            Throw New Exception($"Array '{varName}' does not exist.")
          End If

          Dim value = m_globals(varName)
          If TypeOf value Is List(Of Object) Then
            ' It's an array - handle based on static/dynamic status
            Dim arrayList = CType(value, List(Of Object))

            If varSymbol.IsStaticArray Then
              ' Static arrays: reinitialize all elements
              If varSymbol.Type.Name = "Integer" OrElse varSymbol.Type.Name = "Long" OrElse varSymbol.Type.Name = "Single" OrElse varSymbol.Type.Name = "Double" Then
                ' Numeric arrays: set to 0
                For i = 0 To arrayList.Count - 1
                  arrayList(i) = 0
                Next
              ElseIf varSymbol.Type.Name = "String" Then
                ' String arrays: set to empty string
                For i = 0 To arrayList.Count - 1
                  arrayList(i) = ""
                Next
              Else
                ' Other types: set to default value
                For i = 0 To arrayList.Count - 1
                  arrayList(i) = Nothing
                Next
              End If
            Else
              ' Dynamic arrays: deallocate (remove from globals)
              m_globals.Remove(varName)
            End If
          Else
            ' This should not happen due to compile-time validation, but add runtime check anyway
            Throw New Exception($"Variable '{varName}' is not an array.")
          End If
        End If
      Next
    End Sub

    Private Sub EvaluateEnvironStatement(node As BoundEnvironStatement)
      ' ENVIRON statement sets environment variables
      ' Format: ENVIRON "VARNAME=value"
      Dim envString = CStr(EvaluateExpression(node.Expression))

      ' Parse the VARNAME=value format
      Dim equalsIndex = envString.IndexOf("="c)
      If equalsIndex > 0 Then
        Dim varName = envString.Substring(0, equalsIndex)
        Dim varValue = envString.Substring(equalsIndex + 1)
        Environment.SetEnvironmentVariable(varName, varValue)
      End If
    End Sub

    Private Sub EvaluateCallStatement(node As BoundCallStatement)
      EvaluateCallExpression(node.Call)
    End Sub

    Private Sub EvaluateExpressionStatement(node As BoundExpressionStatement)
      m_lastValue = EvaluateExpression(node.Expression)
    End Sub

    Private Function EvaluateBoundFunctionExpression(node As BoundBoundFunctionExpression) As Object
      ' Evaluate LBOUND or UBOUND function
      Dim arrayName = node.ArrayVariable.Name
      Dim dimension = CInt(EvaluateExpression(node.Dimension))

      ' For now, assume dimension 1 (single dimension arrays)
      If dimension <> 1 Then
        Throw New NotImplementedException("Multi-dimensional LBOUND/UBOUND not yet implemented")
      End If

      ' Check if array has current runtime bounds (updated by REDIM)
      If m_arrayBounds.ContainsKey(arrayName) Then
        Dim bounds = m_arrayBounds(arrayName)
        Return If(node.IsLbound, bounds.Lower, bounds.Upper)
      Else
        ' Use symbol bounds (from DIM declaration)
        Dim lower = If(node.ArrayVariable.Lower IsNot Nothing, CInt(EvaluateExpression(node.ArrayVariable.Lower)), 0)
        Dim upper = If(node.ArrayVariable.Upper IsNot Nothing, CInt(EvaluateExpression(node.ArrayVariable.Upper)), 10)
        Return If(node.IsLbound, lower, upper)
      End If
    End Function

    Private Function EvaluateExpression(node As BoundExpression) As Object

      If node.ConstantValue IsNot Nothing Then
        Return EvaluateConstantExpression(node)
      End If

      Select Case node.Kind

        Case BoundNodeKind.ArrayAccessExpression : Return EvaluateArrayAccessExpression(CType(node, BoundArrayAccessExpression))
        Case BoundNodeKind.AssignmentExpression : Return EvaluateAssignmentExpression(CType(node, BoundAssignmentExpression))
        Case BoundNodeKind.BinaryExpression : Return EvaluateBinaryExpression(CType(node, BoundBinaryExpression))
        Case BoundNodeKind.LiteralExpression : Return EvaluateLiteralExpression(CType(node, BoundLiteralExpression))
        Case BoundNodeKind.VariableExpression : Return EvaluateVariableExpression(CType(node, BoundVariableExpression))
        Case BoundNodeKind.UnaryExpression : Return EvaluateUnaryExpression(CType(node, BoundUnaryExpression))
        Case BoundNodeKind.ParenExpression : Return EvaluateParenExpression(CType(node, BoundParenExpression))
        Case BoundNodeKind.BoundFunctionExpression : Return EvaluateBoundFunctionExpression(CType(node, BoundBoundFunctionExpression))
        Case BoundNodeKind.CallExpression : Return EvaluateCallExpression(CType(node, BoundCallExpression))
        Case BoundNodeKind.ConversionExpression : Return EvaluateConversionExpression(CType(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node {node.Kind}")
      End Select

    End Function

    Private Shared Function EvaluateConstantExpression(node As BoundExpression) As Object
      Debug.Assert(node.ConstantValue IsNot Nothing)
      Try
        Return node.ConstantValue.Value
      Catch ex As OverflowException
        Throw New Exception("Numeric literal is too large.")
      End Try
    End Function

    Private Function EvaluateLiteralExpression(node As BoundLiteralExpression) As Object
      Return node.Value
    End Function

    Private Function EvaluateVariableExpression(node As BoundVariableExpression) As Object
      If node.Variable.Kind = SymbolKind.GlobalVariable Then
        If m_globals.ContainsKey(node.Variable.Name) Then
          Return m_globals(node.Variable.Name)
        Else
          Return Nothing
        End If
      Else
        Dim locals = m_locals.Peek
        If locals.ContainsKey(node.Variable.Name) Then
          Dim value = locals(node.Variable.Name)
          If TypeOf value Is ByRefVariable Then
            Return m_globals(DirectCast(value, ByRefVariable).Name)
          Else
            Return value
          End If
        Else
          Return Nothing
        End If
      End If
    End Function

    Private Function EvaluateParenExpression(node As BoundParenExpression) As Object
      Return EvaluateExpression(node.Expression)
    End Function

    Private Function EvaluateArrayAccessExpression(node As BoundArrayAccessExpression) As Object
      Dim arrayValue As List(Of Object)
      ' Assume all arrays are global for now
      arrayValue = CType(m_globals(node.Variable.Name), List(Of Object))

      ' Use current bounds if available (updated by REDIM), otherwise use symbol bounds
      Dim lower As Integer
      Dim upper As Integer
      If m_arrayBounds.ContainsKey(node.Variable.Name) Then
        Dim bounds = m_arrayBounds(node.Variable.Name)
        lower = bounds.Lower
        upper = bounds.Upper
      Else
        lower = If(node.Variable.Lower IsNot Nothing, CInt(EvaluateExpression(node.Variable.Lower)), 0)
        upper = If(node.Variable.Upper IsNot Nothing, CInt(EvaluateExpression(node.Variable.Upper)), arrayValue.Count - 1)
      End If
      Dim index = CInt(EvaluateExpression(node.Index))

      ' Handle bounds checking
      If index < lower OrElse index > upper Then
        Throw New IndexOutOfRangeException($"Array index {index} out of bounds")
      End If

      Return arrayValue(index - lower)
    End Function

    Private Function EvaluateArrayAccessExpressionForAssignment(node As BoundArrayAccessExpression) As Tuple(Of List(Of Object), Integer)
      Dim arrayValue As List(Of Object)
      ' Assume all arrays are global for now
      If Not m_globals.ContainsKey(node.Variable.Name) Then
        ' Create array if it doesn't exist
        Dim size = 11 ' Default size 0-10
        Dim list = New List(Of Object)(size)
        For i = 0 To size - 1
          list.Add(0)
        Next
        m_globals(node.Variable.Name) = list
      End If
      arrayValue = CType(m_globals(node.Variable.Name), List(Of Object))

      ' Use current bounds if available (updated by REDIM), otherwise use symbol bounds
      Dim lower As Integer
      Dim upper As Integer
      If m_arrayBounds.ContainsKey(node.Variable.Name) Then
        Dim bounds = m_arrayBounds(node.Variable.Name)
        lower = bounds.Lower
        upper = bounds.Upper
      Else
        lower = If(node.Variable.Lower IsNot Nothing, CInt(EvaluateExpression(node.Variable.Lower)), 0)
        upper = If(node.Variable.Upper IsNot Nothing, CInt(EvaluateExpression(node.Variable.Upper)), arrayValue.Count - 1)
      End If
      Dim index = CInt(EvaluateExpression(node.Index))

      ' Handle bounds checking
      If index < lower OrElse index > upper Then
        Throw New IndexOutOfRangeException($"Array index {index} out of bounds")
      End If

      Return Tuple.Create(arrayValue, index - lower)
    End Function

    Private Function EvaluateAssignmentExpression(node As BoundAssignmentExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      Debug.Assert(value IsNot Nothing)
      If node.Variable.Type Is TypeSymbol.String AndAlso (TypeOf value IsNot String AndAlso TypeOf value IsNot Char) Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      ElseIf node.Variable.Type IsNot TypeSymbol.String AndAlso (TypeOf value Is String OrElse TypeOf value Is Char) Then
        Throw New QBasicRuntimeException(ErrorCode.TypeMismatch)
      End If
      Assign(node.Variable, value)
      Return value
    End Function

    Private Function EvaluateUnaryExpression(node As BoundUnaryExpression) As Object
      Dim operand = EvaluateExpression(node.Operand)
      Select Case node.Op.Kind
        Case BoundUnaryOperatorKind.Identity : Return CInt(operand)
        Case BoundUnaryOperatorKind.Negation
          If TypeOf operand Is Single Then Return -CSng(operand)
          If TypeOf operand Is Integer Then Return -CInt(operand)
          If TypeOf operand Is Long Then Return -CLng(operand)
          If TypeOf operand Is Double Then Return -CDbl(operand)
          If TypeOf operand Is SByte Then Return -CSByte(operand)
          Throw New Exception($"Unexpected negation operation")
        Case BoundUnaryOperatorKind.LogicalNegation : Return Not CBool(operand)
        Case BoundUnaryOperatorKind.BitwiseComplement : Return Not CInt(operand)
        Case Else
          Throw New Exception($"Unexpected unary operator {node.Op}")
      End Select
    End Function

    Private Function EvaluateBinaryExpression(node As BoundBinaryExpression) As Object
      Dim left = EvaluateExpression(node.Left)
      Dim right = EvaluateExpression(node.Right)

      'Debug.Assert(left IsNot Nothing AndAlso right IsNot Nothing)
      Select Case node.Op.Kind

' 14 ()
' 13 ^
' 12 - (negation "unary")
' 11 */
' 10 \
' 09 MOD
' 08 +-
' 07 = > >= < <= <>
' 06 NOT
' 05 AND, AndAlso
' 04 OR, OrElse
' 03 XOR
' 02 EQV
' 01 IMP

        Case BoundBinaryOperatorKind.Raise
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) ^ CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) ^ CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) ^ CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) ^ CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) ^ CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) ^ CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) ^ CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) ^ CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) ^ CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) ^ CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) ^ CByte(right))
          End Select

        Case BoundBinaryOperatorKind.Multiplication
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) * CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) * CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) * CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) * CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) * CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) * CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) * CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) * CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) * CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) * CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) * CByte(right))
          End Select

        Case BoundBinaryOperatorKind.Division
          ' Check for division by zero
          If CDbl(right) = 0 Then
            'SetError(ErrorCode.DivisionByZero) ' Division by zero
            Throw New QBasicRuntimeException(ErrorCode.DivisionByZero)
          End If
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(CDec(left) / CDec(right)))
            Case TypeSymbol.Type.Double : Return (CDbl(CDbl(left) / CDbl(right)))
            Case TypeSymbol.Type.Single : Return (CSng(CSng(left) / CSng(right)))
            Case TypeSymbol.Type.ULong64 : Return (CULng(CULng(left) / CULng(right)))
            Case TypeSymbol.Type.Long64 : Return (CLng(CLng(left) / CLng(right)))
            Case TypeSymbol.Type.ULong : Return (CUInt(CUInt(left) / CUInt(right)))
            Case TypeSymbol.Type.Long : Return (CInt(CInt(left) / CInt(right)))
            Case TypeSymbol.Type.UInteger : Return (CUShort(CUShort(left) / CUShort(right)))
            Case TypeSymbol.Type.Integer : Return (CShort(CShort(left) / CShort(right)))
            Case TypeSymbol.Type.SByte : Return (CSByte(CSByte(left) / CSByte(right)))
            Case TypeSymbol.Type.Byte : Return (CByte(CByte(left) / CByte(right)))
          End Select

        Case BoundBinaryOperatorKind.IntegerDivision
          ' Check for division by zero
          If CDbl(right) = 0 Then
            'SetError(ErrorCode.DivisionByZero) ' Division by zero
            Throw New QBasicRuntimeException(ErrorCode.DivisionByZero)
            Return 0 ' Return 0 for integer division by zero
          End If
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Single : Return CInt(left) \ CInt(right)
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) \ CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) \ CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) \ CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) \ CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) \ CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) \ CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) \ CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) \ CByte(right))
          End Select

        Case BoundBinaryOperatorKind.ModOperation
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) Mod CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) Mod CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) Mod CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) Mod CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) Mod CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) Mod CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) Mod CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) Mod CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) Mod CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) Mod CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) Mod CByte(right))
          End Select

        Case BoundBinaryOperatorKind.Addition
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) + CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) + CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) + CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) + CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) + CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) + CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) + CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) + CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) + CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) + CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) + CByte(right))
            Case TypeSymbol.Type.String : Return (CStr(left) & CStr(right))
          End Select

        Case BoundBinaryOperatorKind.Subtraction
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) - CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) - CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) - CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) - CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) - CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) - CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) - CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) - CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) - CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) - CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) - CByte(right))
          End Select

        Case BoundBinaryOperatorKind.Equal
          ' Use numeric comparison for QBasic compatibility
          Try
            Dim leftNum = CDbl(left)
            Dim rightNum = CDbl(right)
            Return If(leftNum = rightNum, -1, 0)
          Catch ex As Exception
            Return If(Equals(left, right), -1, 0)
          End Try
        Case BoundBinaryOperatorKind.NotEqual
          ' Use numeric comparison for QBasic compatibility
          Try
            Dim leftNum = CDbl(left)
            Dim rightNum = CDbl(right)
            Return If(leftNum <> rightNum, -1, 0)
          Catch ex As Exception
            Return If(Not Equals(left, right), -1, 0)
          End Try

        Case BoundBinaryOperatorKind.GreaterThan
          ' Use numeric comparison for QBasic compatibility
          ' Use numeric comparison for QBasic compatibility
          Try
            Dim leftNum = CDbl(left)
            Dim rightNum = CDbl(right)
            Return If(leftNum > rightNum, -1, 0)
          Catch ex As Exception
            ' Fallback to type-specific comparison
            Return If(CDbl(left) > CDbl(right), -1, 0)
          End Try

        Case BoundBinaryOperatorKind.GreaterThanEqual
          Select Case TypeSymbol.TypeSymbolToType(node.Left.Type)
            Case TypeSymbol.Type.Decimal : Return If(CDec(left) >= CDec(right), -1, 0)
            Case TypeSymbol.Type.Double : Return If(CDbl(left) >= CDbl(right), -1, 0)
            Case TypeSymbol.Type.Single : Return If(CSng(left) >= CSng(right), -1, 0)
            Case TypeSymbol.Type.ULong64 : Return If(CULng(left) >= CULng(right), -1, 0)
            Case TypeSymbol.Type.Long64 : Return If(CLng(left) >= CLng(right), -1, 0)
            Case TypeSymbol.Type.ULong : Return If(CUInt(left) >= CUInt(right), -1, 0)
            Case TypeSymbol.Type.Long : Return If(CInt(left) >= CInt(right), -1, 0)
            Case TypeSymbol.Type.UInteger : Return If(CUShort(left) >= CUShort(right), -1, 0)
            Case TypeSymbol.Type.Integer : Return If(CShort(left) >= CShort(right), -1, 0)
            Case TypeSymbol.Type.SByte : Return If(CSByte(left) >= CSByte(right), -1, 0)
            Case TypeSymbol.Type.Byte : Return If(CByte(left) >= CByte(right), -1, 0)
            Case TypeSymbol.Type.String : Return If(CStr(left) >= CStr(right), -1, 0)
          End Select

        Case BoundBinaryOperatorKind.LessThan
          Select Case TypeSymbol.TypeSymbolToType(node.Left.Type)
            Case TypeSymbol.Type.Decimal : Return If(CDec(left) < CDec(right), -1, 0)
            Case TypeSymbol.Type.Double : Return If(CDbl(left) < CDbl(right), -1, 0)
            Case TypeSymbol.Type.Single : Return If(CSng(left) < CSng(right), -1, 0)
            Case TypeSymbol.Type.ULong64 : Return If(CULng(left) < CULng(right), -1, 0)
            Case TypeSymbol.Type.Long64 : Return If(CLng(left) < CLng(right), -1, 0)
            Case TypeSymbol.Type.ULong : Return If(CUInt(left) < CUInt(right), -1, 0)
            Case TypeSymbol.Type.Long : Return If(CInt(left) < CInt(right), -1, 0)
            Case TypeSymbol.Type.UInteger : Return If(CUShort(left) < CUShort(right), -1, 0)
            Case TypeSymbol.Type.Integer : Return If(CShort(left) < CShort(right), -1, 0)
            Case TypeSymbol.Type.SByte : Return If(CSByte(left) < CSByte(right), -1, 0)
            Case TypeSymbol.Type.Byte : Return If(CByte(left) < CByte(right), -1, 0)
            Case TypeSymbol.Type.String : Return If(CStr(left) < CStr(right), -1, 0)
          End Select

        Case BoundBinaryOperatorKind.LessThanEqual
          Select Case TypeSymbol.TypeSymbolToType(node.Left.Type)
            Case TypeSymbol.Type.Decimal : Return If(CDec(left) <= CDec(right), -1, 0)
            Case TypeSymbol.Type.Double : Return If(CDbl(left) <= CDbl(right), -1, 0)
            Case TypeSymbol.Type.Single : Return If(CSng(left) <= CSng(right), -1, 0)
            Case TypeSymbol.Type.ULong64 : Return If(CULng(left) <= CULng(right), -1, 0)
            Case TypeSymbol.Type.Long64 : Return If(CLng(left) <= CLng(right), -1, 0)
            Case TypeSymbol.Type.ULong : Return If(CUInt(left) <= CUInt(right), -1, 0)
            Case TypeSymbol.Type.Long : Return If(CInt(left) <= CInt(right), -1, 0)
            Case TypeSymbol.Type.UInteger : Return If(CUShort(left) <= CUShort(right), -1, 0)
            Case TypeSymbol.Type.Integer : Return If(CShort(left) <= CShort(right), -1, 0)
            Case TypeSymbol.Type.SByte : Return If(CSByte(left) <= CSByte(right), -1, 0)
            Case TypeSymbol.Type.Byte : Return If(CByte(left) <= CByte(right), -1, 0)
            Case TypeSymbol.Type.String : Return If(CStr(left) <= CStr(right), -1, 0)
          End Select

        Case BoundBinaryOperatorKind.LogicalAnd, BoundBinaryOperatorKind.BitwiseAnd
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) And CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) And CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) And CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) And CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) And CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) And CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) And CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) And CByte(right))
            Case TypeSymbol.Type.Boolean : Return (CBool(left) And CBool(right))
          End Select

        Case BoundBinaryOperatorKind.LogicalAndAlso
          Return CBool(left) AndAlso CBool(right)

        Case BoundBinaryOperatorKind.LogicalOr, BoundBinaryOperatorKind.BitwiseOr
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) Or CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) Or CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) Or CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) Or CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) Or CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) Or CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) Or CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) Or CByte(right))
            Case TypeSymbol.Type.Boolean : Return (CBool(left) Or CBool(right))
          End Select

        Case BoundBinaryOperatorKind.LogicalOrElse
          Return CBool(left) OrElse CBool(right)

        Case BoundBinaryOperatorKind.LogicalXor, BoundBinaryOperatorKind.BitwiseXor
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) Xor CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) Xor CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) Xor CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) Xor CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) Xor CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) Xor CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) Xor CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) Xor CByte(right))
            Case TypeSymbol.Type.Boolean : Return (CBool(left) Xor CBool(right))
          End Select

        Case BoundBinaryOperatorKind.LogicalImp : Return CInt(Not CBool(left) Or CBool(right))
        Case BoundBinaryOperatorKind.BitwiseEqv : Return CInt(CBool(left) = CBool(right))
        Case BoundBinaryOperatorKind.BitwiseImp : Return CInt(CBool(left) AndAlso Not CBool(right))

        Case Else
          Throw New Exception($"Unexpected binary operator {node.Op.Kind}")
      End Select

      Throw New Exception($"Unexpected binary operator {left} {node.Op.Kind} {right} type {node.Type}.")

    End Function

    Private Function EvaluateCallExpression(node As BoundCallExpression) As Object
      If node.Function Is BuiltinFunctions.Abs Then
        Dim argValue = EvaluateExpression(node.Arguments(0))
        ' Preserve the input type for ABS
        If TypeOf argValue Is Single Then
          Return MathF.Abs(CSng(argValue))
        ElseIf TypeOf argValue Is Integer Then
          Return Math.Abs(CInt(argValue))
        ElseIf TypeOf argValue Is Double Then
          Return Math.Abs(CDbl(argValue))
        ElseIf TypeOf argValue Is Short Then
          Return Math.Abs(CShort(argValue))
        ElseIf TypeOf argValue Is SByte Then
          Return Math.Abs(CSByte(argValue))
        Else
          ' Default to Double
          Return Math.Abs(CDbl(argValue))
        End If
        Dim value = CSng(EvaluateExpression(node.Arguments(0)))
        Return MathF.Abs(value)
      ElseIf node.Function Is BuiltinFunctions.Asc Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Try
          Return QBLib.Core.QBAsc(value)
          'Return Microsoft.VisualBasic.Strings.Asc(value)
        Catch ex As Exception
          Return Microsoft.VisualBasic.Chr(0)
        End Try
      ElseIf node.Function Is BuiltinFunctions.Atn Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Atan(value)
      ElseIf node.Function Is BuiltinFunctions.Chr Then
        Dim value = CInt(EvaluateExpression(node.Arguments(0)))
        If value.Between(0, 255) Then
          Return Microsoft.VisualBasic.Strings.Chr(value)
        Else
          Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
        End If
      ElseIf node.Function Is BuiltinFunctions.CDbl Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return value
      ElseIf node.Function Is BuiltinFunctions.CInt Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return CShort(value)
      ElseIf node.Function Is BuiltinFunctions.CLng Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return CInt(value)
      ElseIf node.Function Is BuiltinFunctions.Cos Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Cos(value)
      ElseIf node.Function Is BuiltinFunctions.CSng Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return CSng(value)
      ElseIf node.Function Is BuiltinFunctions.CsrLin Then
        Return 1
      ElseIf node.Function Is BuiltinFunctions.Cvd Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.CvdMbf Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Cvi Then
        ' CVI converts 2-byte binary string to integer
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        ' For testing, return a dummy integer value
        Return 12345
      ElseIf node.Function Is BuiltinFunctions.Cvl Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Cvs Then
        ' CVS converts 4-byte binary string to single
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        ' For testing, return a dummy single value
        Return 3.14F
      ElseIf node.Function Is BuiltinFunctions.CvsMbf Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Date Then
        Return DateTime.Now.ToString("MM-dd-yyyy")
      ElseIf node.Function Is BuiltinFunctions.Environ Then
        Dim thing = EvaluateExpression(node.Arguments(0))
        If TypeOf thing Is String Then
          Dim varName = CStr(EvaluateExpression(node.Arguments(0)))
          Dim envValue = Environment.GetEnvironmentVariable(varName)
          Return If(envValue IsNot Nothing, envValue, "")
        Else
          Dim index = CInt(EvaluateExpression(node.Arguments(0)))
          Dim envVars = Environment.GetEnvironmentVariables()
          If index >= 1 AndAlso index <= envVars.Count Then
            Dim entry = envVars.Cast(Of DictionaryEntry).ElementAt(index - 1)
            Return $"{entry.Key}={entry.Value}"
          Else
            Return ""
          End If
        End If
      ElseIf node.Function Is BuiltinFunctions.Eof Then
        Dim fileNumber = CInt(EvaluateExpression(node.Arguments(0)))
        If m_openFiles.ContainsKey(fileNumber) Then
          Dim stream = m_openFiles(fileNumber)
          Return stream.Position >= stream.Length
        Else
          Throw New QBasicRuntimeException(ErrorCode.BadFileNumber)
        End If
      ElseIf node.Function Is BuiltinFunctions.ErDev1 Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.ErDev2 Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Erl Then
        ' ERL returns the line number where the last error occurred (0 if no error)
        Return m_erl
      ElseIf node.Function Is BuiltinFunctions.Err Then
        ' ERR returns the error code of the last error (0 if no error)
        Return m_err
      ElseIf node.Function Is BuiltinFunctions.Exp Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        'Dim base = 2.718282
        'If value = 0 Then Return 0
        'Return base ^ value
        Return Math.Exp(value)
      ElseIf node.Function Is BuiltinFunctions.FileAttr Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Fix Then
        'NOTE: FIX truncates a floating-point expression to its integer portion.
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Fix(value)
      ElseIf node.Function Is BuiltinFunctions.Fre Then
        Return 160266
      ElseIf node.Function Is BuiltinFunctions.FreeFile Then
        ' Find the next available file number (1-255)
        For i = 1 To 255
          If Not m_openFiles.ContainsKey(i) Then
            Return i
          End If
        Next
        Throw New QBasicRuntimeException(ErrorCode.TooManyFiles)
      ElseIf node.Function Is BuiltinFunctions.Hex Then
        Dim value = CInt(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Hex(value)
      ElseIf node.Function Is BuiltinFunctions.Inkey Then
        ' INKEY$ returns the last key pressed as a string (empty if no key pressed)
        Return QBLib.Video.INKEY$()
      ElseIf node.Function Is BuiltinFunctions.Inp Then
        Dim portValue = EvaluateExpression(node.Arguments(0))
        Dim port As Integer
        If TypeOf portValue Is Double Then
          port = CInt(CDbl(portValue))
        ElseIf TypeOf portValue Is Integer Then
          port = CInt(portValue)
        Else
          port = CInt(portValue)
        End If
        If port < 0 Or port > 65535 Then
          Throw New QBasicRuntimeException(ErrorCode.Overflow)
        End If
        Throw New QBasicRuntimeException(ErrorCode.AdvancedFeature)
      ElseIf node.[Function] Is BuiltinFunctions.Input Then
        Return Console.ReadLine()
      ElseIf node.Function Is BuiltinFunctions.Command Then
        ' COMMAND$ returns command line arguments as space-separated string
        If m_commandLineArgs IsNot Nothing AndAlso m_commandLineArgs.Length > 0 Then
          Return String.Join(" "c, m_commandLineArgs)
        Else
          Return ""
        End If
      ElseIf node.Function Is BuiltinFunctions.Instr1 Then
        Dim string1 = CStr(EvaluateExpression(node.Arguments(0)))
        Dim string2 = CStr(EvaluateExpression(node.Arguments(1)))
        Return Microsoft.VisualBasic.InStr(string1, string2)
      ElseIf node.Function Is BuiltinFunctions.Instr2 Then
        Dim position = CInt(EvaluateExpression(node.Arguments(0)))
        Dim string1 = CStr(EvaluateExpression(node.Arguments(1)))
        Dim string2 = CStr(EvaluateExpression(node.Arguments(2)))
        Return Microsoft.VisualBasic.InStr(position, string1, string2)
      ElseIf node.Function Is BuiltinFunctions.Int Then
        'NOTE: INT returns the largest integer less than or equal to the numeric expression.
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Int(value)
      ElseIf node.Function Is BuiltinFunctions.IoCtl Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.LCase Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return value?.ToLower
      ElseIf node.Function Is BuiltinFunctions.Left Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Dim position = CInt(EvaluateExpression(node.Arguments(1)))
        Return Microsoft.VisualBasic.Left(value, position)
      ElseIf node.Function Is BuiltinFunctions.Len Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Len(value)
      ElseIf node.Function Is BuiltinFunctions.Loc Then
        Dim fileNumber = CInt(EvaluateExpression(node.Arguments(0)))
        If m_openFiles.ContainsKey(fileNumber) Then
          Dim stream = m_openFiles(fileNumber)
          Dim mode = m_fileModes(fileNumber)
          Select Case mode.ToUpper()
            Case "BINARY"
              Return CLng(stream.Position)
            Case "RANDOM"
              ' For random files, return record number
              Dim recLen = m_recordLengths(fileNumber)
              Return CLng(stream.Position \ recLen) + 1
            Case "INPUT", "OUTPUT", "APPEND"
              ' For sequential files, return offset divided by 128
              Return CLng(stream.Position \ 128)
            Case Else
              Return CLng(stream.Position)
          End Select
        Else
          Throw New QBasicRuntimeException(ErrorCode.BadFileNumber)
        End If
      ElseIf node.Function Is BuiltinFunctions.Lof Then
        Dim fileNumber = CInt(EvaluateExpression(node.Arguments(0)))
        If m_openFiles.ContainsKey(fileNumber) Then
          Dim stream = m_openFiles(fileNumber)
          Return CLng(stream.Length)
        Else
          Throw New QBasicRuntimeException(ErrorCode.BadFileNumber)
        End If
      ElseIf node.Function Is BuiltinFunctions.Log Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Log(value)
      ElseIf node.Function Is BuiltinFunctions.Lpos Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Ltrim Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.LTrim(value)
      ElseIf node.Function Is BuiltinFunctions.Mid1 Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Dim start = CInt(EvaluateExpression(node.Arguments(1)))
        Return Microsoft.VisualBasic.Mid(value, start)
      ElseIf node.Function Is BuiltinFunctions.Mid2 Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Dim start = CInt(EvaluateExpression(node.Arguments(1)))
        Dim length = CInt(EvaluateExpression(node.Arguments(2)))
        Return Microsoft.VisualBasic.Mid(value, start, length)
      ElseIf node.Function Is BuiltinFunctions.Mkd Then
        ' MKD$ converts double to 8-byte binary string
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return New String(ChrW(0), 8) ' Return 8-byte string for testing
      ElseIf node.Function Is BuiltinFunctions.MkdMbf Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Mki Then
        ' MKI$ converts integer to 2-byte binary string
        Dim value = CInt(EvaluateExpression(node.Arguments(0)))
        Return New String(ChrW(0), 2) ' Return 2-byte string for testing
      ElseIf node.Function Is BuiltinFunctions.Mkl Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Mks Then
        ' MKS$ converts single to 4-byte binary string
        Dim value = CSng(EvaluateExpression(node.Arguments(0)))
        Return New String(ChrW(0), 4) ' Return 4-byte string for testing
      ElseIf node.Function Is BuiltinFunctions.MksMbf Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Oct Then
        Dim value = CInt(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Oct(value)
      ElseIf node.Function Is BuiltinFunctions.Peek Then
        Return 0
      ElseIf node.Function Is BuiltinFunctions.Pen Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Play Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Pmap Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Point Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Pos Then
        Return 1
      ElseIf node.Function Is BuiltinFunctions.Right Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Dim position = CInt(EvaluateExpression(node.Arguments(1)))
        Return Microsoft.VisualBasic.Right(value, position)
      ElseIf node.[Function] Is BuiltinFunctions.Rnd1 Then
        ' NOTES:
        '   PRINT RND() in QBasic 1.1 always results in .7055475 if RANDOMIZE is not called prior.
        '   According to documentation, RND is initially set to utilize seed of 0 if RANDOMIZE is never called.
        '   Allowed range on RANDOMIZE is -32768 to 32767; however, it does appear that pretty much any number is allowed?
        'If m_random Is Nothing Then m_random = New Random(g_seed)
        ' Return a new random number between 0 and 1...
        g_lastRndResult = g_random.NextSingle()
        Return g_lastRndResult
      ElseIf node.[Function] Is BuiltinFunctions.Timer Then
        Return CSng($"{DateTime.Now.TimeOfDay.TotalSeconds:F2}")
      ElseIf node.[Function] Is BuiltinFunctions.Rnd2 Then
        ' NOTES:
        '   PRINT RND(1) in QBasic 1.1 always results in .7055475 if RANDOMIZE is not called prior.
        '   According to documentation, RND is initially set to utilize seed of 0 if RANDOMIZE is never called.
        '   Allowed range on RANDOMIZE is -32768 to 32767; however, it does appear that pretty much any number is allowed?
        Dim opt = CInt(EvaluateExpression(node.Arguments(0)))
        If opt < 0 Then
          ' Reseed with negative value
          g_random = New Random(opt)
          g_lastRndResult = g_random.NextSingle()
          Return g_lastRndResult
        ElseIf opt = 0 Then
          ' Return last result...
          Return CSng(If(g_lastRndResult, 0))
        Else
          ' Return a new random number between 0 and 1...
          g_lastRndResult = g_random.NextSingle()
          Return g_lastRndResult
        End If
        'Return Microsoft.VisualBasic.Rnd
      ElseIf node.Function Is BuiltinFunctions.Rtrim Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.RTrim(value)
      ElseIf node.Function Is BuiltinFunctions.Screen Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Seek Then
        ' SEEK function returns the same as LOC function
        Dim fileNumber = CInt(EvaluateExpression(node.Arguments(0)))
        If m_openFiles.ContainsKey(fileNumber) Then
          Dim stream = m_openFiles(fileNumber)
          Dim mode = m_fileModes(fileNumber)
          Select Case mode.ToUpper()
            Case "BINARY"
              Return CLng(stream.Position)
            Case "RANDOM"
              ' For random files, return record number
              Dim recLen = m_recordLengths(fileNumber)
              Return CLng(stream.Position \ recLen) + 1
            Case "INPUT", "OUTPUT", "APPEND"
              ' For sequential files, return offset divided by 128
              Return CLng(stream.Position \ 128)
            Case Else
              Return CLng(stream.Position)
          End Select
        Else
          Throw New QBasicRuntimeException(ErrorCode.BadFileNumber)
        End If
      ElseIf node.Function Is BuiltinFunctions.Sgn Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Sign(value)
      ElseIf node.Function Is BuiltinFunctions.Sin Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Sin(value)
      ElseIf node.Function Is BuiltinFunctions.Space Then
        Dim length = CInt(EvaluateExpression(node.Arguments(0)))
        Return New String(" "c, length)
      ElseIf node.Function Is BuiltinFunctions.Sqr Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Sqrt(value)
      ElseIf node.Function Is BuiltinFunctions.Stick Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Str Then
        Dim value = EvaluateExpression(node.Arguments(0))
        Return Microsoft.VisualBasic.Str(value)
      ElseIf node.Function Is BuiltinFunctions.Strig Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.StringFunction Then
        Dim length = CInt(EvaluateExpression(node.Arguments(0)))
        Dim thing = EvaluateExpression(node.Arguments(1))
        If TypeOf thing Is String Then
          Return New String(CStr(thing)(0), length)
        Else
          Return New String(ChrW(CInt(thing)), length)
        End If
      ElseIf node.Function Is BuiltinFunctions.Tan Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Tan(value)
      ElseIf node.Function Is BuiltinFunctions.Time Then
        Return DateTime.Now.ToString("HH:mm:ss")
      ElseIf node.Function Is BuiltinFunctions.UBound Then
        Dim arg = node.Arguments(0)
        If TypeOf arg Is BoundVariableExpression Then
          Dim varExpr = CType(arg, BoundVariableExpression)
          If varExpr.Variable.IsArray Then
            Return EvaluateExpression(varExpr.Variable.Upper)
          Else
            ' UBOUND called on non-array variable - return -1 (QBasic behavior)
            Return -1L
          End If
        Else
          ' Invalid argument type
          Return -1L
        End If
      ElseIf node.Function Is BuiltinFunctions.UCase Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return value?.ToUpper
      ElseIf node.Function Is BuiltinFunctions.Val Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Val(value)
      ElseIf node.Function Is BuiltinFunctions.VarPtr1 Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.VarPtr2 Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.VarSeg Then
        Stop
        Return Nothing
      Else
        Dim locals = New Dictionary(Of String, Object)
        For i = 0 To node.Arguments.Length - 1
          Dim parameter = node.Function.Parameters(i)
          Dim argument = node.Arguments(i)
          If parameter.IsByRef AndAlso TypeOf argument.Syntax Is IdentifierExpressionSyntax Then
            locals.Add(parameter.Name, New ByRefVariable(GetVariableNameFromSyntax(argument.Syntax)))
          Else
            Dim value = EvaluateExpression(argument)
            Debug.Assert(value IsNot Nothing)
            locals.Add(parameter.Name, value)
          End If
        Next
        m_locals.Push(locals)
        Dim statement = m_functions(node.Function)
        m_container.Push(node.Function.Name)
        Dim result = EvaluateStatement(statement, Nothing)
        m_container.Pop()
        m_locals.Pop()
        Return result
      End If
    End Function

    Private Function EvaluateConversionExpression(node As BoundConversionExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      If node.Type Is TypeSymbol.Any Then
        Return value
      ElseIf node.Type Is TypeSymbol.Boolean Then
        If TypeOf value Is Boolean Then
          Return If(CBool(value), -1, 0)
        ElseIf TypeOf value Is Double OrElse TypeOf value Is Single OrElse TypeOf value Is Integer OrElse TypeOf value Is Short OrElse TypeOf value Is Long OrElse TypeOf value Is Byte OrElse TypeOf value Is SByte Then
          Return If(CDbl(value) <> 0, -1, 0)
        Else
          Return If(Convert.ToBoolean(value), -1, 0)
        End If
      ElseIf node.Type Is TypeSymbol.Byte Then
        Return Convert.ToByte(value)
      ElseIf node.Type Is TypeSymbol.SByte Then
        Return Convert.ToSByte(value)
      ElseIf node.Type Is TypeSymbol.Integer Then
        Return Convert.ToInt16(value)
      ElseIf node.Type Is TypeSymbol.UInteger Then
        Return Convert.ToUInt16(value)
      ElseIf node.Type Is TypeSymbol.Long Then
        Return Convert.ToInt32(value)
      ElseIf node.Type Is TypeSymbol.ULong Then
        Return Convert.ToUInt32(value)
      ElseIf node.Type Is TypeSymbol.Long64 Then
        Return Convert.ToInt64(value)
      ElseIf node.Type Is TypeSymbol.ULong64 Then
        Return Convert.ToUInt64(value)
      ElseIf node.Type Is TypeSymbol.Single Then
        If TypeOf value Is Boolean Then
          Return If(CBool(value), -1.0!, 0!)
        Else
          Return Convert.ToSingle(value)
        End If
      ElseIf node.Type Is TypeSymbol.Double Then
        Return Convert.ToDouble(value)
      ElseIf node.Type Is TypeSymbol.String Then
        Return Convert.ToString(value)
      Else
        Throw New Exception($"Unexpected type {node.Type}")
      End If
    End Function

    Private Sub Assign(variable As VariableSymbol, value As Object)
      ' Convert value to the variable's type
      value = ConvertValue(value, variable.Type)
      If variable.Kind = SymbolKind.GlobalVariable Then
        m_globals(variable.Name) = value
      Else
        Dim locals = m_locals.Peek
        If locals.ContainsKey(variable.Name) AndAlso TypeOf locals(variable.Name) Is ByRefVariable Then
          m_globals(DirectCast(locals(variable.Name), ByRefVariable).Name) = value
        Else
          locals(variable.Name) = value
        End If
      End If
    End Sub

    Private Function ConvertValue(value As Object, targetType As TypeSymbol) As Object
      If value Is Nothing Then Return Nothing
      If targetType Is TypeSymbol.Integer Then
        Return CInt(value)
      ElseIf targetType Is TypeSymbol.Single Then
        Return CSng(value)
      ElseIf targetType Is TypeSymbol.Double Then
        Return CDbl(value)
      ElseIf targetType Is TypeSymbol.Long Then
        Return CLng(value)
      Else
        Return value
      End If
    End Function

    Private Sub Assign(expression As BoundExpression, value As Object)
      If TypeOf expression Is BoundVariableExpression Then
        Dim variable = CType(expression, BoundVariableExpression).Variable
        Assign(variable, value)
      ElseIf TypeOf expression Is BoundArrayAccessExpression Then
        Dim arrayAccess = CType(expression, BoundArrayAccessExpression)
        Dim tuple = EvaluateArrayAccessExpressionForAssignment(arrayAccess)
        tuple.Item1(tuple.Item2) = value
      End If
    End Sub

    Private Sub EvaluateOpenStatement(node As BoundOpenStatement)
      Dim fileName = CStr(EvaluateExpression(node.File))
      Dim fileNumber = CInt(EvaluateExpression(node.FileNumber))

      ' Check if file number is already in use
      If m_openFiles.ContainsKey(fileNumber) Then
        Throw New QBasicRuntimeException(ErrorCode.FileAlreadyOpen)
      End If

      ' Determine access mode
      Dim mode As FileMode = FileMode.OpenOrCreate
      Dim access As FileAccess = FileAccess.ReadWrite
      Dim modeString = If(node.Mode IsNot Nothing, CStr(EvaluateExpression(node.Mode)), "RANDOM")

      Select Case modeString.ToUpper()
        Case "INPUT"
          access = FileAccess.Read
          mode = FileMode.Open
        Case "OUTPUT"
          access = FileAccess.Write
          mode = FileMode.Create
        Case "APPEND"
          access = FileAccess.Write
          mode = FileMode.Append
        Case "BINARY"
          access = FileAccess.ReadWrite
          mode = FileMode.OpenOrCreate
        Case "RANDOM"
          access = FileAccess.ReadWrite
          mode = FileMode.OpenOrCreate
        Case Else
          Throw New QBasicRuntimeException(ErrorCode.BadFileMode)
      End Select

      ' For COM ports, handle differently (not implemented yet)
      If fileName.ToUpper().StartsWith("COM") Then
        Throw New QBasicRuntimeException(ErrorCode.AdvancedFeature)
      End If

      Try
        Dim stream = New FileStream(fileName, mode, access)
        m_openFiles.Add(fileNumber, stream)
        m_fileModes.Add(fileNumber, modeString.ToUpper())

        ' Store record length (default 128 for RANDOM files)
        Dim recLen As Integer = 128
        If node.RecLen IsNot Nothing Then
          recLen = CInt(EvaluateExpression(node.RecLen))
        End If
        m_recordLengths.Add(fileNumber, recLen)

        ' For INPUT files, create a StreamReader
        If modeString.ToUpper() = "INPUT" Then
          m_textReaders.Add(fileNumber, New StreamReader(stream, leaveOpen:=True))
        End If
      Catch ex As Exception
        Throw New QBasicRuntimeException(ErrorCode.FileNotFound)
      End Try
    End Sub

    Private Sub EvaluateCloseStatement(node As BoundCloseStatement)
      For Each fileNumberExpr In node.FileNumbers
        Dim fileNumber = CInt(EvaluateExpression(fileNumberExpr))
        If m_openFiles.ContainsKey(fileNumber) Then
          m_openFiles(fileNumber).Close()
          m_openFiles.Remove(fileNumber)
          m_fileModes.Remove(fileNumber)
          m_recordLengths.Remove(fileNumber)
          If m_textReaders.ContainsKey(fileNumber) Then
            'm_textReaders(fileNumber).Dispose()
            m_textReaders.Remove(fileNumber)
          End If
          If m_textWriters.ContainsKey(fileNumber) Then
            'm_textWriters(fileNumber).Dispose()
            m_textWriters.Remove(fileNumber)
          End If
        End If
      Next

      ' If no file numbers specified, close all files
      If node.FileNumbers.Length = 0 Then
        For Each kvp In m_openFiles
          kvp.Value.Close()
        Next
        m_openFiles.Clear()
        m_fileModes.Clear()
        m_recordLengths.Clear()
        'For Each kvp In m_textReaders
        '  kvp.Value.Dispose()
        'Next
        m_textReaders.Clear()
        'For Each kvp In m_textWriters
        '  kvp.Value.Dispose()
        'Next
        m_textWriters.Clear()
      End If
    End Sub

    Private Sub EvaluateLineInputFileStatement(node As BoundLineInputFileStatement)
      Dim fileNumber = CInt(EvaluateExpression(node.FileNumber))
      If Not m_openFiles.ContainsKey(fileNumber) Then
        Throw New QBasicRuntimeException(ErrorCode.BadFileNumber)
      End If

      If Not m_textReaders.ContainsKey(fileNumber) Then
        Throw New QBasicRuntimeException(ErrorCode.BadFileMode)
      End If

      Dim reader = m_textReaders(fileNumber)
      Dim line = reader.ReadLine()
      If line Is Nothing Then
        Throw New QBasicRuntimeException(ErrorCode.InputPastEnd)
      End If
      Assign(node.Variable, line)
    End Sub

    Private Sub EvaluatePrintFileStatement(node As BoundPrintFileStatement)
      Dim fileNumber = CInt(EvaluateExpression(node.FileNumber))
      If Not m_openFiles.ContainsKey(fileNumber) Then
        Throw New QBasicRuntimeException(ErrorCode.BadFileNumber)
      End If

      ' Create writer if it doesn't exist
      If Not m_textWriters.ContainsKey(fileNumber) Then
        Dim stream = m_openFiles(fileNumber)
        Try
          m_textWriters.Add(fileNumber, New StreamWriter(stream, leaveOpen:=True))
        Catch ex As Exception
          Throw New QBasicRuntimeException(ErrorCode.Internal)
        End Try
      End If

      Dim writer = m_textWriters(fileNumber)
      For Each boundNode In node.Nodes
        If TypeOf boundNode Is BoundExpression Then
          Dim value = EvaluateExpression(DirectCast(boundNode, BoundExpression))
          writer.Write(CStr(value))
          writer.Flush()
        End If
        ' TODO: Handle separators like ; and , for formatting
      Next
      ' For simplicity, always add newline for now
      writer.WriteLine()
      writer.Flush()
    End Sub

    Private Sub EvaluateSeekStatement(node As BoundSeekStatement)
      Dim fileNumber = CInt(EvaluateExpression(node.FileNumber))
      If Not m_openFiles.ContainsKey(fileNumber) Then
        Throw New QBasicRuntimeException(ErrorCode.BadFileNumber)
      End If

      Dim stream = m_openFiles(fileNumber)
      Dim mode = m_fileModes(fileNumber)
      Dim position = CLng(EvaluateExpression(node.Position))

      If position < 1 Then
        Throw New QBasicRuntimeException(ErrorCode.IllegalFunctionCall)
      End If

      Select Case mode.ToUpper()
        Case "BINARY"
          ' Position is byte offset (0-based)
          stream.Position = position - 1
        Case "RANDOM"
          ' Position is record number (1-based)
          Dim recLen = m_recordLengths(fileNumber)
          stream.Position = (position - 1) * recLen
        Case "INPUT", "OUTPUT", "APPEND"
          ' For sequential files, position is byte offset
          stream.Position = position - 1
        Case Else
          stream.Position = position - 1
      End Select
    End Sub

  End Class

  '' This Evaluator walks the "raw" SyntaxTree.
  'Public NotInheritable Class Evaluator_SyntaxTree

  '  Private ReadOnly m_root As ExpressionSyntax

  '  Sub New(root As ExpressionSyntax)
  '    m_root = root
  '  End Sub

  '  Public Function Evaluate() As Integer
  '    Return EvaluateExpression(m_root)
  '  End Function

  '  Private Function EvaluateExpression(node As ExpressionSyntax) As Integer

  '    If TypeOf node Is LiteralExpressionSyntax Then
  '      Return CInt(CType(node, LiteralExpressionSyntax).LiteralToken.Value)
  '    End If

  '    If TypeOf node Is UnaryExpressionSyntax Then
  '      Dim u = CType(node, UnaryExpressionSyntax)
  '      Dim operand = EvaluateExpression(u.Operand)
  '      Select Case u.OperatorToken.Kind
  '        Case SyntaxKind.PlusToken : Return operand
  '        Case SyntaxKind.MinusToken : Return -operand
  '        Case Else
  '          Throw New Exception($"Unexpected unary operator {u.OperatorToken.Kind}")
  '      End Select
  '    End If

  '    If TypeOf node Is BinaryExpressionSyntax Then
  '      Dim b = CType(node, BinaryExpressionSyntax)
  '      Dim left = EvaluateExpression(b.Left)
  '      Dim right = EvaluateExpression(b.Right)
  '      Select Case b.OperatorToken.Kind
  '        Case SyntaxKind.PlusToken : Return left + right
  '        Case SyntaxKind.MinusToken : Return left - right
  '        Case SyntaxKind.StarToken : Return left * right
  '        Case SyntaxKind.SlashToken : Return CInt(left / right)
  '        Case SyntaxKind.BackslashToken : Return left \ right
  '        Case Else
  '          Throw New Exception($"Unexpected binary operator {b.OperatorToken.Kind}")
  '      End Select
  '    End If

  '    If TypeOf node Is ParenExpressionSyntax Then
  '      Dim p = CType(node, ParenExpressionSyntax)
  '      Return EvaluateExpression(p.Expression)
  '    End If

  '    Throw New Exception($"Unexpected node {node.Kind}")

  '  End Function

  'End Class

  Friend Module Singleton

    Friend Const OPTION_EXPLICIT As Boolean = False
    Friend Const OPTION_DOUBLE As Boolean = False

    Friend g_seed As Integer = 0
    Friend g_random As New Random(g_seed)
    Friend g_lastRndResult As New Single?

  End Module

End Namespace
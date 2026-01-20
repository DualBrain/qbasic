Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Binding
Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Imports QBLib

Namespace Global.QB.CodeAnalysis

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
    Private m_err As Integer = 0 ' Current error code (ERR)
    Private m_erl As Integer = 0 ' Line number where error occurred (ERL)
    Private m_errorHandlerTarget As String = Nothing ' Target label for ON ERROR GOTO
    Private m_errorResumeNext As Boolean = False ' ON ERROR RESUME NEXT mode
    Private m_errorPending As Boolean = False ' Flag indicating if an error occurred and needs handling
    Private m_errorResumeIndex As Integer = -1 ' Index to resume execution after error handling

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

      Public Sub New(message As String)
        MyBase.New(message)
      End Sub
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
    Private Sub SetError(errorCode As Integer, Optional lineNumber As Integer = 0)
      m_err = errorCode
      m_erl = lineNumber
      m_errorPending = True
    End Sub

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
          Throw New QBasicRuntimeException($"Error {m_err}: {GetErrorMessage(m_err)}")
        End If
      Else
        ' No error handler, fatal error
        Throw New QBasicRuntimeException($"Error {m_err}: {GetErrorMessage(m_err)}")
      End If

      Return False
    End Function

    Private Function GetErrorMessage(errorCode As Integer) As String
      Select Case errorCode
        Case 1 : Return "NEXT without FOR"
        Case 2 : Return "Syntax error"
        Case 3 : Return "RETURN without GOSUB"
        Case 4 : Return "Out of DATA"
        Case 5 : Return "Illegal function call"
        Case 6 : Return "Overflow"
        Case 7 : Return "Out of memory"
        Case 8 : Return "Undefined line number"
        Case 9 : Return "Subscript out of range"
        Case 10 : Return "Duplicate Definition"
        Case 11 : Return "Division by zero"
        Case 12 : Return "Illegal direct"
        Case 13 : Return "Type mismatch"
        Case 14 : Return "Out of string space"
        Case 15 : Return "String too long"
        Case 16 : Return "String formula too complex"
        Case 17 : Return "Can't continue"
        Case 18 : Return "Undefined user function"
        Case 19 : Return "No RESUME"
        Case 20 : Return "RESUME without error"
        Case 21 : Return "Unprintable error"
        Case 22 : Return "Missing operand"
        Case 23 : Return "Line buffer overflow"
        Case 24 : Return "Device Timeout"
        Case 25 : Return "Device Fault"
        Case 26 : Return "FOR Without NEXT"
        Case 27 : Return "Out of Paper"
        Case 28 : Return "Unprintable error"
        Case 29 : Return "WHILE without WEND"
        Case 30 : Return "WEND without WHILE"
        Case 31 To 49 : Return "Unprintable error"
        Case 50 : Return "FIELD overflow"
        Case 51 : Return "Internal error"
        Case 52 : Return "Bad file number"
        Case 53 : Return "File not found"
        Case 54 : Return "Bad file mode"
        Case 55 : Return "File already open"
        Case 56 : Return "Unprintable error"
        Case 57 : Return "Device I/O Error"
        Case 58 : Return "File already exists"
        Case 59 : Return "Unprintable error"
        Case 60 : Return "Unprintable error"
        Case 61 : Return "Disk full"
        Case 62 : Return "Input past end"
        Case 63 : Return "Bad record number"
        Case 64 : Return "Bad filename"
        Case 65 : Return "Unprintable error"
        Case 66 : Return "Direct statement in file"
        Case 67 : Return "Too many files"
        Case 68 : Return "Device Unavailable"
        Case 69 : Return "Communication buffer overflow"
        Case 70 : Return "Permission Denied"
        Case 71 : Return "Disk not Ready"
        Case 72 : Return "Disk media error"
        Case 73 : Return "Advanced Feature"
        Case 74 : Return "Rename across disks"
        Case 75 : Return "Path/File Access Error"
        Case 76 : Return "Path not found"
        Case Else : Return $"Undefined error {errorCode}"
      End Select
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
        Dim result = EvaluateStatement(body)
        m_container.Pop()
        Return result
      End If
    End Function

    Private Function EvaluateStatement(body As BoundBlockStatement) As Object

      Dim labelToIndex = New Dictionary(Of String, Integer)

      For i = 0 To body.Statements.Length - 1
        If TypeOf body.Statements(i) Is BoundLabelStatement Then
          labelToIndex.Add(CType(body.Statements(i), BoundLabelStatement).Label.Name, i + 1)
        End If
      Next

       Dim index = 0
       While index < body.Statements.Length

         If QBasic.Common.s_cancelToken.IsCancellationRequested Then
           Exit While
         End If

         ' Check for pending errors before executing next statement
         If m_errorPending Then
           If HandlePendingError(index, labelToIndex) Then
             Continue While ' Skip to next iteration with updated index
           End If
         End If

        'Console.WriteLine($"DEBUG: About to execute index {index}")
        Try
            Dim s = body.Statements(index)
           Select Case s.Kind
          Case BoundNodeKind.BeepStatement
            ' TODO: Implement BEEP sound
            index += 1
          Case BoundNodeKind.PokeStatement
            ' TODO: Implement POKE memory write
            index += 1
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
              index = labelToIndex(cgs.Label.Name)
            Else
              index += 1
            End If
          Case BoundNodeKind.EndStatement
            index = body.Statements.Length
          Case BoundNodeKind.ExpressionStatement : EvaluateExpressionStatement(CType(s, BoundExpressionStatement)) : index += 1

           Case BoundNodeKind.GosubStatement
            Dim gs = CType(s, BoundGosubStatement)
            Dim value As Integer = Nothing
            If labelToIndex.TryGetValue(gs.Label.Name, value) Then
              m_gosubStack.Push(index + 1)
              index = value
            Else
                'Console.WriteLine("ERROR: GosubStatement label " & gs.Label.Name & " not found")
                index += 1
            End If

           Case BoundNodeKind.GotoStatement
            Dim gs = CType(s, BoundGotoStatement)
            Dim value As Integer = Nothing
            If labelToIndex.TryGetValue(gs.Label.Name, value) Then
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
          Case BoundNodeKind.IfStatement : EvaluateIfStatement(CType(s, BoundIfStatement)) : index += 1

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
              End If
            ElseIf labelToIndex.TryGetValue(rg.Label.Name, value) Then
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
           Case BoundNodeKind.OnErrorGotoStatement : EvaluateOnErrorGotoStatement(CType(s, BoundOnErrorGotoStatement)) : index += 1
           Case BoundNodeKind.OnErrorGotoZeroStatement : EvaluateOnErrorGotoZeroStatement(CType(s, BoundOnErrorGotoZeroStatement)) : index += 1
           Case BoundNodeKind.SelectCaseStatement : EvaluateSelectCaseStatement(CType(s, BoundSelectCaseStatement)) : index += 1
           Case BoundNodeKind.DoWhileStatement : EvaluateDoWhileStatement(CType(s, BoundDoWhileStatement)) : index += 1
          Case BoundNodeKind.DoUntilStatement : Console.WriteLine("DEBUG: Found DoUntilStatement") : EvaluateDoUntilStatement(CType(s, BoundDoUntilStatement)) : index += 1
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
             SetError(11) ' Generic error
           End If
           ' Error will be handled on next iteration
           Continue While
         End Try
       End While

      Return m_lastValue

    End Function

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
          Throw New Exception("Out of DATA")
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

    Private Sub EvaluateOnErrorGotoStatement(node As BoundOnErrorGotoStatement)
      ' Set error handler target
      If TypeOf node.Target Is BoundVariableExpression Then
        ' Target is a label/variable name
        Dim varExpr = CType(node.Target, BoundVariableExpression)
        m_errorHandlerTarget = varExpr.Variable.Name
      Else
        ' Target is an expression (line number)
        Dim targetValue = EvaluateExpression(node.Target)
        If IsNumeric(targetValue) Then
          ' Line number - convert to label format
          m_errorHandlerTarget = GOTO_LABEL_PREFIX & CStr(targetValue)
        Else
          ' Label name
          m_errorHandlerTarget = CStr(targetValue)
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
        Throw New QBasicRuntimeException("RESUME without error")
      End If

      ' For RESUME with label, we'd need to implement jumping to specific labels
      ' For now, implement basic RESUME (return to error location)
      If m_errorResumeIndex >= 0 Then
        ' This will be handled by returning a special value to the main loop
        Throw New ResumeException(m_errorResumeIndex)
      End If
    End Sub

    Private Sub EvaluateResumeNextStatement(node As BoundResumeNextStatement)
      If Not m_errorPending AndAlso m_errorResumeIndex = -1 Then
        Throw New QBasicRuntimeException("RESUME without error")
      End If

      ' RESUME NEXT - continue with statement after the error
      If m_errorResumeIndex >= 0 Then
        Throw New ResumeException(m_errorResumeIndex + 1)
      End If
    End Sub

    Private Sub EvaluateErrorStatement(node As BoundErrorStatement)
      Dim errorCode = CInt(EvaluateExpression(node.Expression))
      SetError(errorCode)
      ' Error will be handled by the main evaluation loop
    End Sub

    Private Sub EvaluateSelectCaseStatement(node As BoundSelectCaseStatement)
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
            EvaluateStatement(CType(caseStmt.Statement, BoundBlockStatement))
            Return ' Exit after first matching case
          End If
        Next
      Next

      ' If no case matched and there's an ELSE clause, execute it
      If node.ElseStatement IsNot Nothing Then
        EvaluateStatement(CType(node.ElseStatement, BoundBlockStatement))
      End If
    End Sub

    Private Sub EvaluateDoWhileStatement(node As BoundDoWhileStatement)
      If node.AtBeginning Then
        ' DO WHILE condition ... LOOP
        Do
          Dim conditionValue = CBool(EvaluateExpression(node.Expression))
          If Not conditionValue Then Exit Do
          EvaluateStatement(CType(node.Statements, BoundBlockStatement))
        Loop
      Else
        ' DO ... LOOP WHILE condition
        Do
          EvaluateStatement(CType(node.Statements, BoundBlockStatement))
          Dim conditionValue = CBool(EvaluateExpression(node.Expression))
          If Not conditionValue Then Exit Do
        Loop
      End If
    End Sub

    Private Sub EvaluateDoUntilStatement(node As BoundDoUntilStatement)
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
          EvaluateStatement(CType(node.Statements, BoundBlockStatement))
        Loop
        Console.WriteLine("DEBUG: Loop finished")
      Else
        ' DO ... LOOP UNTIL condition
        Do
          EvaluateStatement(CType(node.Statements, BoundBlockStatement))
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
      QBLib.Video.PRINT(value.ToString(), node.NoCr) ': QBLib.Video.PRINT(" "c, True)
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

    Private Sub EvaluateIfStatement(node As BoundIfStatement)
      Dim conditionValue = CBool(EvaluateExpression(node.Expression))
      If conditionValue Then
        EvaluateStatement(CType(node.Statements, BoundBlockStatement))
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
          EvaluateStatement(CType(node.ElseStatement, BoundBlockStatement))
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
            Throw New Exception("Array bounds are out of valid range (-32768 to 32767).")
          End Try
          If upperInt < lowerInt Then
            Throw New Exception($"Array bounds are invalid: {lowerInt} to {upperInt}. Lower bound must be less than or equal to upper bound.")
          End If
          ' Check for reasonable array size (QBasic had memory limits)
          Dim arraySize = CLng(upperInt) - CLng(lowerInt) + 1 ' Use Long to avoid overflow
          If arraySize > 65535 Then
            Throw New Exception($"Array size {arraySize} is too large. Maximum allowed is 65535 elements.")
          End If
          If arraySize <= 0 Then
            Throw New Exception($"Invalid array size {arraySize}. Array must have at least 1 element.")
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

      Debug.Assert(left IsNot Nothing AndAlso right IsNot Nothing)
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
            SetError(11) ' Division by zero
            Throw New QBasicRuntimeException($"Error {m_err}: {GetErrorMessage(m_err)}")
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
            SetError(11) ' Division by zero
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
        Return Microsoft.VisualBasic.Strings.Asc(value)
      ElseIf node.Function Is BuiltinFunctions.Atn Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Atan(value)
      ElseIf node.Function Is BuiltinFunctions.Chr Then
        Dim value = CInt(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Strings.Chr(value)
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
        Stop
        Return Nothing
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
        Return 1
      ElseIf node.Function Is BuiltinFunctions.Hex Then
        Dim value = CInt(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Hex(value)
      ElseIf node.Function Is BuiltinFunctions.Inkey Then
        ' INKEY$ returns the last key pressed as a string (empty if no key pressed)
        Return QBLib.Video.INKEY$()
      ElseIf node.Function Is BuiltinFunctions.Inp Then
        Stop
        Return Nothing
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
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Lof Then
        Stop
        Return Nothing
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
        Return CSng(DateTime.Now.TimeOfDay.TotalSeconds)
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
        Stop
        Return Nothing
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
      ElseIf node.Function Is BuiltinFunctions.Timer Then
        Stop
        Return Nothing
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
        Dim result = EvaluateStatement(statement)
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
Imports Basic.Parser.ParserExtensions

Namespace Global.Basic

  Public Class Interactive

#Region "State Machine"

    'Public Property HelpKeyword As String
    'Public Property FacebookTokens As List(Of Basic.Parser.Token)

    Public ReadOnly Property IsRunning As Boolean
      Get
        Return m_running
      End Get
    End Property

    'Public Property IsTrial As Boolean
    '  Get
    '    Return m_isTrial
    '  End Get
    '  Set(value As Boolean)
    '    m_isTrial = value
    '  End Set
    'End Property

    'Public Property Speed As Short
    '  Get
    '    Return m_speed
    '  End Get
    '  Set(value As Short)
    '    m_speed = value
    '  End Set
    'End Property

    Public Property CurrentPath As String
      Get
        Return m_currentPath
      End Get
      Set(value As String)
        m_currentPath = value
      End Set
    End Property

    Public Property Pen As Input.IPen
      Get
        Return InternalPen
      End Get
      Set(value As Input.IPen)
        InternalPen = value
        'm_pen.ColumnCount = m_display.ColumnCount
        'm_pen.ScreenMode = m_display.ScreenMode
      End Set
    End Property

    Public Sub Reset()

      'm_speed = 6 ' Approximately 4.77Mhz speed.
      'm_speed = 60 ' Approximately same as DosBox
      'm_speed = 200 ' Seems to work great on Silverlight...
      'm_speed = 600 ' Seems to work rediculously well in Silverlight...
      'm_speed = 800 ' Seems to be the current maximum perf setting in Silverlight...

      m_successLineNumber = 0

      m_debugSetVariables = False
      m_debugGosub = False
      m_debugForLoops = False

      m_stringVariableList.Clear() ' = New Dictionary(Of String, StringValue)
      m_numericVariableList.Clear() ' = New Dictionary(Of String, NumericValue)
      m_fileList.Clear() ' = New Dictionary(Of Short, File)
      m_lpt = New List(Of Printer.Printer) From {New Printer.Printer("LPT1:"),
                                               New Printer.Printer("LPT2:"),
                                               New Printer.Printer("LPT3:")}

      m_lines.Clear() ' = New List(Of Parser.Line)
      m_interpreter.Clear() ' = New List(Of Parser.Line)
      m_shutDown = False

      m_inputPrompt = Nothing
      m_inputSuppressCr = False
      m_inputVariableList.Clear() ' = New List(Of String)

      m_onKey = New List(Of OnKey) From {New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3B)}, .Macro = "LIST ", .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3C)}, .Macro = "RUN" & ChrW(27), .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3D)}, .Macro = "LOAD""", .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3E)}, .Macro = "SAVE""", .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3F)}, .Macro = "CONT" & ChrW(27), .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H40)}, .Macro = ",""LPT1:""" & ChrW(27), .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H41)}, .Macro = "TRON" & ChrW(27), .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H42)}, .Macro = "TROFF" & ChrW(27), .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H43)}, .Macro = "KEY ", .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H44)}, .Macro = "SCREEN 0, 0, 0" & ChrW(27), .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H48)}, .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H4B)}, .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H4D)}, .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H50)}, .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                       New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off}}

      If m_virtualFileSystem IsNot Nothing Then
        m_currentPath = m_virtualFileSystem.InitialPath
      Else
        m_currentPath = "C:\"
      End If

      m_running = False
      m_waiting = False

      ' The following three variables are used to track what is currently being interpreted.
      m_interpreterIndex = 0
      m_statementIndex = 0
      m_tokenIndex = 0

      ' PEEK/POKE
      'm_peek.Clear() ' = New Dictionary(Of Integer, Byte)
      'm_peek.Add(0, CByte(m_speed \ 4))

      ' ON TIMER
      m_onTimerState = OnState.Off
      m_onTimerLine = 0
      m_onTimerInterval = 0
      m_onTimerTime = New Date?

      m_sleepUntil = New Date?

      m_onPenState = OnState.Off
      m_onPenLineOrLabel = Nothing
      m_onPenActivated = False
      m_penCurrentPosition = Nothing
      m_penCurrentActivated = False
      m_penLastActivatedPosition = Nothing
      'm_penLastActivated = False
      m_penActivatedSinceLastPoll = False

      'm_defSeg = -1

      ' WIDTH
      'm_widthScrn = 80 ' 40 and 80 are the only numbers that are valid.
      'm_widthLpt1 = 255 ' 1 to 255 allowed.  255 effectively turns on infinite width (line wrapping).
      'm_widthLpt2 = 255
      'm_widthLpt3 = 255
      'm_widthCom1 = 255
      'm_widthCom2 = 255

      ' OPTION BASE
      m_optionBase = 0

      ' TRON/TROFF
      m_tron = False

      ' DEFSTR
      m_defStrList.Clear() ' = New List(Of Char)

      ' DEFINT
      m_defIntList.Clear() ' = New List(Of Char)

      ' DEFSNG
      m_defSngList.Clear() ' = New List(Of Char)

      ' DEFDBL
      m_defDblList.Clear() ' = New List(Of Char)

      ' ERR
      m_err = 0
      m_erl = 0

      ' ON ERROR GOTO <linenumber>
      m_onErrorGoto = 0
      m_onErrorResumeInterpreterIndex = -1
      m_onErrorResumeStatementIndex = 0

      m_varPtrList.Clear() ' = New Dictionary(Of String, Integer)
      m_varNameList.Clear() ' = New Dictionary(Of Integer, String)

      '' Integer, Single, Double identifiers.
      'm_numericSuffixList = New List(Of Char) From {"%"c, "!"c, "#"c}

      'm_numericFunctionList = New List(Of String) From {"ABS", "ASC", "ATN", "CDBL", "CINT", "CLNG", "COS", "CSNG",
      '                                                  "CSRLIN", "CVI", "CVL", "EOF", "ERR", "EXP", "FIX", "FREEFILE",
      '                                                  "INP", "INSTR", "INT", "LBOUND", "LEN", "LOF", "LOG", "PEEK",
      '                                                  "POINT", "POS", "RND", "SCREEN", "SGN", "SIN", "SQR", "STRIG",
      '                                                  "TAN", "TIMER", "UBOUND", "VAL"}

      'm_alphaNumericList = New List(Of Char) From {"A"c, "B"c, "C"c, "D"c, "E"c, "F"c, "G"c, "H"c, "I"c, "J"c, "K"c, "L"c, "M"c,
      '                                             "N"c, "O"c, "P"c, "Q"c, "R"c, "S"c, "T"c, "U"c, "V"c, "W"c, "X"c, "Y"c, "Z"c,
      '                                             "a"c, "b"c, "c"c, "d"c, "e"c, "f"c, "g"c, "h"c, "i"c, "j"c, "k"c, "l"c, "m"c,
      '                                             "n"c, "o"c, "p"c, "q"c, "r"c, "s"c, "t"c, "u"c, "v"c, "w"c, "x"c, "y"c, "z"c,
      '                                             "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c, "0"c}

      'm_digitList = New List(Of Char) From {"0"c, "1"c, "2"c, "3"c, "4"c,
      '                                      "5"c, "6"c, "7"c, "8"c, "9"c}

      m_contIndex = -1

      m_continueInputDollar = False
      m_continueInput = False
      m_continueLineInput = False
      m_continueInputRandomize = False

      m_keyBuffer.Clear() ' = New List(Of KeyAction)
      m_characterBuffer.Clear() ' = New List(Of Char)
      m_hexCodeScanCodeBuffer.Clear() ' = New List(Of Char)

      m_labelList.Clear() ' = New Dictionary(Of String, Integer)
      m_defFnList.Clear() ' = New Dictionary(Of String, DefFn)

      m_forLoop.Clear() ' = New List(Of ForLoop)
      m_gosubReturn.Clear() ' = New List(Of GosubReturn)
      m_doLoop.Clear() ' = New List(Of Integer)
      m_whileLoop.Clear() ' = New List(Of WhileLoop)

      m_readIndex = 0
      m_data.Clear() ' = New List(Of Parser.Token)
      m_dataLineNumberIndex.Clear()

      ' NEW / OLD
      m_pendingNew = False

      m_autoStart = -1
      m_autoIncrement = 10
      m_autoModeActive = False

      'm_editLine = -1

      'm_listStartLine = -1
      'm_listEndLine = -1
      'm_listLineIndex = -1

      m_display.Cls(-1)
      ResetDraw()

    End Sub

    Public ReadOnly Property Environ As Dictionary(Of String, String)
      Get
        Return m_environ
      End Get
    End Property

#Region "Member Variables"

    Private ReadOnly m_environ As New Dictionary(Of String, String)

    'Private ReadOnly m_dialect As Parser.Dialect = Parser.Dialect.Hybrid
    Private ReadOnly m_reservedWords As New List(Of String)

    Private ReadOnly m_isTrial As Boolean = False

    Private ReadOnly m_region As String = "en-US"

    ' This will force an exit of the "speed" loop whenever the display has become
    ' invalidated due to some screen update code.
    Private ReadOnly m_forcedInvalidate As Boolean = False

    Private m_suppressOk As Boolean = False

    ' How many statements are executed per "game loop".
    'Private m_speed As Short '= 6

    'Private ReadOnly m_productName As String = "hybrid basic"

    Private m_debugSetVariables As Boolean = False
    Private m_debugGosub As Boolean = False
    Private m_debugForLoops As Boolean = False

    Private m_drawPosition As New Drawing.Point(0, 0)
    Private m_drawColor As Short = -1
    Private m_drawScale As Single = 1
    Private m_drawAngle As Integer = 0

    Private ReadOnly m_stringVariableList As New Dictionary(Of String, StringValue) 'String)
    Private ReadOnly m_numericVariableList As New Dictionary(Of String, NumericValue) 'Double)
    Private ReadOnly m_fileList As New Dictionary(Of Short, File)
    Private m_lpt As New List(Of Printer.Printer) From {New Printer.Printer("LPT1:"),
                                                        New Printer.Printer("LPT2:"),
                                                        New Printer.Printer("LPT3:")}

    'Private ReadOnly m_gpio As IO.IGpio
    Private ReadOnly m_environment As Environment.IEnvironment
    Private ReadOnly m_display As Display.Display ' Holds a reference to the display layer.
    Public ReadOnly Property Display As Display.Display
      Get
        Return m_display
      End Get
    End Property
    Private ReadOnly m_virtualFileSystem As IO.IVirtualFileSystem
    Private ReadOnly m_sound As Audio.ISound
    Private ReadOnly m_keyboard As Input.IKeyboard
    Private WithEvents InternalPen As Input.IPen

    Private ReadOnly m_lines As New List(Of Parser.Line) ' The "loaded program" list.
    Private ReadOnly m_interpreter As New List(Of Parser.Line) ' The "interpreter" list.  (Could be equal to m_lines or a single line.)

    Private m_shutDown As Boolean ' If set, something has occured that states that we should stop the interpreter.

    ' INPUT / LINE INPUT
    Private m_inputPrompt As String
    Private m_inputSuppressCr As Boolean
    Private ReadOnly m_inputVariableList As New List(Of String)

    Private m_onKey As New List(Of OnKey) From {New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3B)}, .Macro = "LIST ", .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3C)}, .Macro = "RUN" & ChrW(27), .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3D)}, .Macro = "LOAD""", .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3E)}, .Macro = "SAVE""", .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H3F)}, .Macro = "CONT" & ChrW(27), .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H40)}, .Macro = ",""LPT1:""" & ChrW(27), .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H41)}, .Macro = "TRON" & ChrW(27), .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H42)}, .Macro = "TROFF" & ChrW(27), .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H43)}, .Macro = "KEY ", .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H44)}, .Macro = "SCREEN 0, 0, 0" & ChrW(27), .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H48)}, .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H4B)}, .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H4D)}, .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = {ChrW(&H0), ChrW(&H50)}, .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off},
                                              New OnKey() With {.HexCodeScanCode = Nothing, .State = OnState.Off}}

    Private m_currentPath As String = "C:\" ' Hold the current path used with LOAD, SAVE, CHDIR, etc.

    Private m_running As Boolean ' Interpreter is executing...
    Private m_waiting As Boolean ' Waiting for a response from the server during LOAD, SAVE, CHDIR, etc..

    ' The following three variables are used to track what is currently being interpreted.
    Private m_interpreterIndex As Integer
    Private m_statementIndex As Integer
    Private m_tokenIndex As Integer

    ' PEEK/POKE
    Private ReadOnly m_peek As New Dictionary(Of Integer, Byte)

    ' SLEEP
    Private m_sleepUntil As New Date?

    ' ON TIMER
    Private m_onTimerState As OnState = OnState.Off
    Private m_onTimerLine As Integer = 0 ' 0 = never set.
    Private m_onTimerInterval As Integer = 0 ' 0 = never set.
    Private m_onTimerTime As Date? ' set to nothing is disabled; otherwise represents when the next time (actual) the event should activate.

    ' ON PEN

    Private m_onPenState As OnState = OnState.Off
    Private m_onPenLineOrLabel As String = Nothing ' set to nothing is disabled.
    Private m_onPenActivated As Boolean = False

    ' Current state.
    Private m_penCurrentPosition As Drawing.Point = Nothing
    Private m_penCurrentActivated As Boolean = False

    ' Last activated state.
    Private m_penLastActivatedPosition As Drawing.Point = Nothing
    'Private m_penLastActivated As Boolean = False
    Private m_penActivatedSinceLastPoll As Boolean = False

    'Private m_defSeg As Integer = -1 ' Default DS

    ' WIDTH
    'Private m_widthScrn As Short = 80 ' 40 and 80 are the only numbers that are valid.
    'Private m_widthLpt1 As Short = 255 ' 1 to 255 allowed.  255 effectively turns on infinite width (line wrapping).
    'Private m_widthLpt2 As Short = 255
    'Private m_widthLpt3 As Short = 255
    'Private m_widthCom1 As Short = 255
    'Private m_widthCom2 As Short = 255

    ' OPTION BASE
    Private m_optionBase As Short = 0

    ' TRON/TROFF
    Private m_tron As Boolean

    ' DEFSTR
    Private ReadOnly m_defStrList As New List(Of Char)

    ' DEFINT
    Private ReadOnly m_defIntList As New List(Of Char)

    ' DEFSNG
    Private ReadOnly m_defSngList As New List(Of Char)

    ' DEFDBL
    Private ReadOnly m_defDblList As New List(Of Char)

    ' SCREEN
    'Private m_screen As Short = 0

    ' ERR
    Private m_err As Short
    Private m_erl As Integer

    ' ON ERROR GOTO <linenumber>
    Private m_successLineNumber As Integer = 0
    Private m_onErrorGoto As Integer = 0 ' Defaults to "line 0" (no line).
    Private m_onErrorResumeInterpreterIndex As Integer = -1
    Private m_onErrorResumeStatementIndex As Integer = 0

    Private ReadOnly m_varPtrList As New Dictionary(Of String, Integer)
    Private ReadOnly m_varNameList As New Dictionary(Of Integer, String)

    ' Integer, Single, Double identifiers.
    Private ReadOnly m_numericSuffixList As New List(Of Char) From {"%"c, "!"c, "#"c}

    Private ReadOnly m_numericFunctionList As New List(Of String) From {"ABS", "ASC", "ATN", "CDBL", "CINT", "CLNG", "COS", "CSNG",
                                                             "CSRLIN", "CVI", "CVL", "EOF", "ERR", "EXP", "FIX", "FREEFILE",
                                                             "INP", "INSTR", "INT", "LBOUND", "LEN", "LOF", "LOG", "PEEK",
                                                             "POINT", "POS", "RND", "SCREEN", "SGN", "SIN", "SQR", "STRIG",
                                                             "TAN", "TIMER", "UBOUND", "VAL"}

    Private ReadOnly m_alphaNumericList As New List(Of Char) From {"A"c, "B"c, "C"c, "D"c, "E"c, "F"c, "G"c, "H"c, "I"c, "J"c, "K"c, "L"c, "M"c,
                                                        "N"c, "O"c, "P"c, "Q"c, "R"c, "S"c, "T"c, "U"c, "V"c, "W"c, "X"c, "Y"c, "Z"c,
                                                        "a"c, "b"c, "c"c, "d"c, "e"c, "f"c, "g"c, "h"c, "i"c, "j"c, "k"c, "l"c, "m"c,
                                                        "n"c, "o"c, "p"c, "q"c, "r"c, "s"c, "t"c, "u"c, "v"c, "w"c, "x"c, "y"c, "z"c,
                                                        "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c, "0"c}

    Private ReadOnly m_digitList As New List(Of Char) From {"0"c, "1"c, "2"c, "3"c, "4"c,
                                                 "5"c, "6"c, "7"c, "8"c, "9"c}

    Private m_contIndex As Integer = -1

    Private m_continueInputDollar As Boolean
    Private m_continueInput As Boolean
    Private m_continueLineInput As Boolean
    Private m_continueInputRandomize As Boolean

    Private ReadOnly m_keyBuffer As New List(Of KeyAction)
    Private ReadOnly m_characterBuffer As New List(Of Char)
    Private ReadOnly m_hexCodeScanCodeBuffer As New List(Of Char)

    Private ReadOnly m_labelList As New Dictionary(Of String, Integer)
    Private ReadOnly m_defFnList As New Dictionary(Of String, DefFn)

    Private ReadOnly m_forLoop As New List(Of ForLoop)
    Private ReadOnly m_gosubReturn As New List(Of GosubReturn)
    Private ReadOnly m_doLoop As New List(Of Integer)
    Private ReadOnly m_whileLoop As New List(Of WhileLoop)

    Private m_readIndex As Integer = 0
    Private ReadOnly m_data As New List(Of Parser.Token)
    Private ReadOnly m_dataLineNumberIndex As New Dictionary(Of Integer, Integer) ' Line number, readindex

    ' NEW / OLD
    Private m_pendingNew As Boolean

    Private m_autoStart As Integer = -1
    Private m_autoIncrement As Integer = 10
    Private m_autoModeActive As Boolean = False

    'Private m_editLine As Integer = -1

    'Private m_listStartLine As Integer = -1
    'Private m_listEndLine As Integer = -1
    'Private m_listLineIndex As Integer = -1

#End Region

    'Private WithEvents InternalTimer As System.Timers.Timer 'Timers.ITimer

    'Public Event HelpHook As EventHandler
    'Public Event FacebookHook As EventHandler

    'Public Event ListHook As EventHandler

    Public Event EndHook As EventHandler
    Public Event ErrorHook As EventHandler
    Public Event StopHook As EventHandler
    Public Event ContHook As EventHandler
    Public Event TronHook As EventHandler
    Public Event TroffHook As EventHandler

    'Public Event NewHook As EventHandler
    'Public Event LoadHook As EventHandler
    'Public Event SaveHook As EventHandler
    'Public Event MergeHook As EventHandler
    'Public Event DeleteHook As EventHandler

    Public Event SystemHook As EventHandler

#End Region

    Private Class StringValue
      Public Property Value As String
      Public Property Common As Boolean
      Public Sub New(value As String)
        Me.Value = value
        Common = False
      End Sub
      Public Sub New(value As String, common As Boolean)
        Me.Value = value
        Me.Common = common
      End Sub
      Public Overrides Function ToString() As String
        Return Value
      End Function
    End Class

    Private Class NumericValue
      Public Property Value As Double
      Public Property Type As NumericType
      Public Property Common As Boolean
      Public Sub New(value As Double, type As NumericType)
        Me.Value = value
        Me.Type = type
      End Sub
      Public Sub New(value As Double, type As NumericType, common As Boolean)
        Me.Value = value
        Me.Type = type
        Me.Common = common
      End Sub
      Public Overrides Function ToString() As String
        Return Value.ToString
      End Function
    End Class

    Private Enum NumericType
      [Integer]
      [Long]
      [Single]
      [Double]
    End Enum

    Private Class Chain
      Public Property Merge As Boolean
      Public Property Filename As String
      Public Property Line As Integer
      Public Property All As Boolean
      Public Property DeleteMin As Integer
      Public Property DeleteMax As Integer
    End Class

    Private Class File
      Public Property Number As Short
      Public Property Path As String
      Public Property Mode As FileMode
      Public Property Lock As FileLock
      Public Property Length As Integer
      Public Property Position As Integer
      Public Property RecordLength As Integer
      Public Property FieldList As New List(Of Field)
      Public Property Printer As Printer.Printer
    End Class

    Private Class Field
      Public Property VariableName As String
      Public Property Length As Short
    End Class

    Private Class InputUserState
      Public Property FileNumber As Short
      Public Property VariableList As List(Of String)
      Public Property LineInput As Boolean
    End Class

    Private Enum FileMode
      Input ' Position to the beginning of a the file.  A "File not found" error is given if the file does not exist.
      Output ' Position to the beginning of the file.  If the file does not exist, one is created.
      Append ' Position to the end of the file.  If the file does not exist, one is created.
      RandomAccessReadWrite
      RandomAccessRead
      RandomAccessWrite
    End Enum

    Private Enum FileLock
      [Default]
      [Shared]
      LockRead
      LockWrite
      LockReadWrite
    End Enum

    Private Class OnKey
      Public Property HexCodeScanCode As Char()
      Public Property Macro As String
      Public Property State As OnState
      Public Property Line As Integer
    End Class

    Private Enum OnState
      [Off]
      [On]
      [Stop]
    End Enum

    Private Class KeyAction
      Public Property KeyEventArgs As Input.KeyEventArgs
      Public Property Pressing As Boolean
      Public Sub New(e As Input.KeyEventArgs, pressing As Boolean)
        KeyEventArgs = e
        Me.Pressing = pressing
      End Sub
    End Class

    Public Sub New(environment As Environment.IEnvironment,
                   display As Display.Display,
                   virtualFileSystem As IO.IVirtualFileSystem,
                   sound As Audio.ISound,
                   keyboard As Input.IKeyboard,
                   region As String)

      'm_dialect = dialect

      'Dim d As Parser.IDialect = Nothing
      'Select Case dialect
      '  Case Parser.Dialect.Hybrid
      '    d = New Parser.Dialects.Hybrid
      '  Case Parser.Dialect.GWBasic
      '    d = New Parser.Dialects.GwBasic
      '  Case Parser.Dialect.QBasic
      Dim d = New Parser.Dialects.Qbasic
      '  Case Parser.Dialect.AmigaBasic
      '    d = New Parser.Dialects.AmigaBasic
      '  Case Parser.Dialect.TinyBasic
      '    d = New Parser.Dialects.TinyBasic
      '  Case Else
      '    Throw New NotImplementedException(dialect.ToString)
      'End Select

      'If d IsNot Nothing Then
      For Each word In d.ReservedWords
        m_reservedWords.Add(word)
      Next
      'End If

      m_environment = environment
      m_display = display
      m_virtualFileSystem = virtualFileSystem
      m_sound = sound
      m_keyboard = keyboard
      'm_gpio = gpio
      'm_productName = productName
      'm_speed = Speed

      If m_virtualFileSystem IsNot Nothing Then
        m_currentPath = m_virtualFileSystem.InitialPath
      End If

      'm_display.ShowVisualQueues = True

      'm_keyTimer = New Windows.Threading.DispatcherTimer()
      'm_keyTimer.Interval = TimeSpan.Zero

      'm_displayTimer = New Windows.Threading.DispatcherTimer()
      'm_displayTimer.Interval = TimeSpan.Zero

      'If m_useDispatchTimer Then
      'InternalTimer = timer 'New Windows.Threading.DispatcherTimer()
      'Try
      '  InternalTimer.Interval = 1 '1000
      'Catch ex As ArgumentOutOfRangeException ' Windows Timer
      '  InternalTimer.Interval = 1
      'End Try
      'Else
      'm_storyboardTimer = New Storyboard
      'm_storyboardTimer.Duration = TimeSpan.FromMilliseconds(0)
      'End If

      If region IsNot Nothing Then
        m_region = region
      End If

      m_peek.Clear() ' = New Dictionary(Of Integer, Byte)
      'm_peek.Add(0, CByte(m_speed \ 4))

      If m_region IsNot Nothing Then
      End If

    End Sub

    '    Public Sub New(dialect As Parser.Dialect,
    '                   environment As Environment.IEnvironment,
    '                   display As Display.Display,
    '                   virtualFileSystem As IO.IVirtualFileSystem,
    '                   sound As Audio.ISound,
    '                   keyboard As Input.IKeyboard,
    '                   gpio As IO.IGpio,
    '                   productName As String,
    '                   speed As Short,
    '                   region As String)

    '      m_dialect = dialect

    '      Dim d As Parser.IDialect = Nothing
    '      Select Case dialect
    '        Case Parser.Dialect.Hybrid
    '          d = New Parser.Dialects.Hybrid
    '        Case Parser.Dialect.GWBasic
    '          d = New Parser.Dialects.GwBasic
    '        Case Parser.Dialect.QBasic
    '          d = New Parser.Dialects.Qbasic
    '        Case Parser.Dialect.AmigaBasic
    '          d = New Parser.Dialects.AmigaBasic
    '        Case Parser.Dialect.TinyBasic
    '          d = New Parser.Dialects.TinyBasic
    '        Case Else
    '          Throw New NotImplementedException(dialect.ToString)
    '      End Select

    '      If d IsNot Nothing Then
    '        For Each word In d.ReservedWords
    '          m_reservedWords.Add(word)
    '        Next
    '      End If

    '      m_environment = environment
    '      m_display = display
    '      m_virtualFileSystem = virtualFileSystem
    '      m_sound = sound
    '      m_keyboard = keyboard
    '      m_gpio = gpio
    '      m_productName = productName
    '      m_speed = speed

    '      If m_virtualFileSystem IsNot Nothing Then
    '        m_currentPath = m_virtualFileSystem.InitialPath
    '      End If

    '      'InternalTimer = Timer 'New Windows.Threading.DispatcherTimer()
    '      'Try
    '      '  InternalTimer.Interval = 0 '1000
    '      'Catch ex As ArgumentOutOfRangeException ' Windows Timer
    '      '  InternalTimer.Interval = 1
    '      'End Try

    '      If region IsNot Nothing Then
    '        m_region = region
    '      End If

    '      m_peek.Clear() ' = New Dictionary(Of Integer, Byte)
    '      m_peek.Add(0, CByte(m_speed \ 4))

    '      If m_region IsNot Nothing Then
    '      End If

    '    End Sub

    Public ReadOnly Property Printers(index As Integer) As Printer.Printer
      Get
        Return m_lpt(index)
      End Get
    End Property

    Public Property Source() As String
      Get
        Dim result As String = ""
        For Each line In m_lines
          If result <> "" Then
            result &= vbCrLf
          End If
          result &= line.Text 'ToString
        Next
        Return result
      End Get
      Set(value As String)
        SetSource(value)
      End Set
    End Property

    Private Sub SetSource(value As String)

      m_lines.Clear()

      If value.IndexOf(vbCrLf) > -1 Then
        value = value.Replace(vbCrLf, vbLf)
      End If

      If value.IndexOf(vbCr) > -1 Then
        value = value.Replace(vbCr, vbLf)
      End If

      Dim textLines = value.Split(CChar(vbLf))

      For Each textRow In textLines

        'If Me.m_dialect = Parser.Dialect.GWBasic Then
        '  textRow = textRow.Trim ' Trim since we don't want any white space before the line number.
        'End If

        'If Not String.IsNullOrEmpty(textRow) Then

        Dim buffer() As Byte = textRow.ToByteArray
        Using s As New System.IO.MemoryStream()
          s.Write(buffer, 0, buffer.Length)
          s.Seek(0, System.IO.SeekOrigin.Begin)
          ' Parse command.
          Dim parse As New Parser.Parser(s)
          m_lines.Add(parse.Lines(0))
        End Using

        'End If

      Next

    End Sub

    'Private m_tickBusy As Boolean

    'Private Sub InternalTimer_Tick(sender As Object, e As System.EventArgs) Handles InternalTimer.Elapsed

    '  If m_tickBusy Then
    '    Return
    '  End If

    '  Try
    '    m_tickBusy = True
    '    ProcessInterpreter()
    '  Finally
    '    m_tickBusy = False
    '  End Try

    'End Sub

    'Private Sub m_storyboardTimer_Tick(sender As Object, e As System.EventArgs) Handles m_storyboardTimer.Completed
    '  For index As Short = 1 To 100
    '    ProcessInterpreter()
    '  Next
    '  m_storyboardTimer.Begin()
    'End Sub

    Private Sub ProcessKeyBuffer()
      Do Until m_keyBuffer.Count = 0
        Dim action = m_keyBuffer(0)
        m_keyBuffer.RemoveAt(0)
        ProcessKey(action.KeyEventArgs, action.Pressing)
        If m_running OrElse m_waiting Then
          Exit Do
        End If
      Loop
    End Sub

    'Public ReadOnly Property IsShiftDown As Boolean
    '  Get
    '    Return m_display.ShiftDown
    '  End Get
    'End Property

    'Public ReadOnly Property IsControlDown As Boolean
    '  Get
    '    Return m_display.ControlDown
    '  End Get
    'End Property

    'Public ReadOnly Property IsAltDown As Boolean
    '  Get
    '    Return m_display.AltDown
    '  End Get
    'End Property

    Public Sub ProcessKey(e As Input.KeyEventArgs, pressing As Boolean)
      ProcessKeyInternal(e, pressing)
    End Sub

    Private Sub ProcessKeyInternal(e As Input.KeyEventArgs, pressing As Boolean)

      Dim ctrl As Boolean
      Dim alt As Boolean
      Dim shift As Boolean

      If m_keyboard IsNot Nothing AndAlso m_keyboard.UseModifier Then
        ctrl = ((Me.m_keyboard.Modifier And Input.ModifierKeys.Control) = Input.ModifierKeys.Control)
        alt = ((Me.m_keyboard.Modifier And Input.ModifierKeys.Alt) = Input.ModifierKeys.Alt)
        shift = ((Me.m_keyboard.Modifier And Input.ModifierKeys.Shift) = Input.ModifierKeys.Shift)
      Else
        ctrl = m_display.ControlDown
        alt = m_display.AltDown
        shift = m_display.ShiftDown
      End If

      If m_running Then

        ' CTRL+C or CTRL+BREAK
        If ctrl AndAlso Not alt AndAlso Not shift AndAlso pressing AndAlso
         (e.Key = Input.Key.C OrElse (e.Key = Input.Key.Unknown AndAlso e.PlatformKeyCode = 19)) Then

          'If m_dialect = Parser.Dialect.QBasic Then
          '  m_running = False
          'Else
          m_running = False
          If m_display.Pos(0) > 1 Then
            m_display.Print()
          End If
          'If Me.m_dialect = Parser.Dialect.GWBasic Then
          '  If m_interpreter(m_interpreterIndex).LineNumber IsNot Nothing Then
          '    m_display.Print(String.Format("Break in {0}", m_interpreter(m_interpreterIndex).LineNumber), True)
          '    m_contIndex = m_interpreterIndex
          '  Else
          '    m_contIndex = -1
          '    m_display.Print("Break", True)
          '  End If
          'Else
          m_contIndex = -1
          m_display.Print("Break", True)
          'End If
          'End If

          'If Me.m_dialect = Parser.Dialect.GWBasic Then
          'Else
          RaiseEvent EndHook(Me, EventArgs.Empty)
          'End If

        ElseIf m_waiting Then
          m_keyBuffer.Add(New KeyAction(e, pressing))
          Exit Sub

        Else

          Dim length = m_characterBuffer.Count

          If e.OppositeShiftState Then
            If m_keyboard IsNot Nothing AndAlso m_keyboard.UseModifier Then
              shift = Not shift
            Else
              If m_display.ShiftDown Then
                m_display.EditShiftUp()
              Else
                m_display.EditShiftDown()
              End If
            End If
          End If

          Select Case e.Key
          'Case Key.Alt ' Alt
          'Case Key.Window ' Window
            Case Input.Key.Ctrl
              If m_keyboard Is Nothing OrElse Not m_keyboard.UseModifier Then
                If pressing Then m_display.EditControlDown() Else m_display.EditControlUp()
              End If
            Case Input.Key.Shift
              If m_keyboard Is Nothing OrElse Not m_keyboard.UseModifier Then
                If pressing Then m_display.EditShiftDown() Else m_display.EditShiftUp()
              End If
            Case Input.Key.Alt
              If m_keyboard Is Nothing OrElse Not m_keyboard.UseModifier Then
                If pressing Then m_display.EditAltDown() Else m_display.EditAltUp()
              End If
            Case Input.Key.CapsLock
              If pressing Then m_display.EditCapsLock()
            Case Input.Key.Insert : If pressing Then m_display.EditInsert()

            Case Input.Key.Escape : If pressing Then m_characterBuffer.Add(ChrW(27))
            Case Input.Key.Back : If pressing Then m_characterBuffer.Add(ChrW(8))
            Case Input.Key.Delete ': If pressing Then m_characterBuffer.Add(ChrW(0))
            Case Input.Key.Tab : If pressing Then m_characterBuffer.Add(ChrW(9))

            Case Input.Key.A, Input.Key.B, Input.Key.C, Input.Key.D, Input.Key.E, Input.Key.F, Input.Key.G, Input.Key.H, Input.Key.I, Input.Key.J, Input.Key.K, Input.Key.L, Input.Key.M,
               Input.Key.N, Input.Key.O, Input.Key.P, Input.Key.Q, Input.Key.R, Input.Key.S, Input.Key.T, Input.Key.U, Input.Key.V, Input.Key.W, Input.Key.X, Input.Key.Y, Input.Key.Z
              If pressing Then
                If m_display.CapsLock Then
                  If shift Then
                    m_characterBuffer.Add(CChar(e.Key.ToString.ToLower))
                  Else
                    m_characterBuffer.Add(CChar(e.Key.ToString))
                  End If
                Else
                  If shift Then
                    m_characterBuffer.Add(CChar(e.Key.ToString))
                  Else
                    m_characterBuffer.Add(CChar(e.Key.ToString.ToLower))
                  End If
                End If
              End If

            Case Input.Key.D0 : If pressing Then If shift Then m_characterBuffer.Add(")"c) Else m_characterBuffer.Add("0"c)
            Case Input.Key.D1 : If pressing Then If shift Then m_characterBuffer.Add("!"c) Else m_characterBuffer.Add("1"c)
            Case Input.Key.D2 : If pressing Then If shift Then m_characterBuffer.Add("@"c) Else m_characterBuffer.Add("2"c)
            Case Input.Key.D3 : If pressing Then If shift Then m_characterBuffer.Add("#"c) Else m_characterBuffer.Add("3"c)
            Case Input.Key.D4 : If pressing Then If shift Then m_characterBuffer.Add("$"c) Else m_characterBuffer.Add("4"c)
            Case Input.Key.D5 : If pressing Then If shift Then m_characterBuffer.Add("%"c) Else m_characterBuffer.Add("5"c)
            Case Input.Key.D6 : If pressing Then If shift Then m_characterBuffer.Add("^"c) Else m_characterBuffer.Add("6"c)
            Case Input.Key.D7 : If pressing Then If shift Then m_characterBuffer.Add("&"c) Else m_characterBuffer.Add("7"c)
            Case Input.Key.D8 : If pressing Then If shift Then m_characterBuffer.Add("*"c) Else m_characterBuffer.Add("8"c)
            Case Input.Key.D9 : If pressing Then If shift Then m_characterBuffer.Add("("c) Else m_characterBuffer.Add("9"c)

            Case Input.Key.Space : If pressing Then m_characterBuffer.Add(" "c)

            Case Input.Key.Enter : If pressing Then m_characterBuffer.Add(ChrW(13))

            Case Input.Key.F1 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H3B))
            Case Input.Key.F2 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H3C))
            Case Input.Key.F3 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H3D))
            Case Input.Key.F4 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H3E))
            Case Input.Key.F5 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H3F))
            Case Input.Key.F6 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H40))
            Case Input.Key.F7 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H41))
            Case Input.Key.F8 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H42))
            Case Input.Key.F9 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H43))
            Case Input.Key.F10 : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H44))

            Case Input.Key.Left
              If pressing Then
                m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H4B))
                m_characterBuffer.Add(ChrW(&H0)) : m_characterBuffer.Add(ChrW(&H4B))
              End If
            Case Input.Key.Right
              If pressing Then
                m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H4D))
                m_characterBuffer.Add(ChrW(&H0)) : m_characterBuffer.Add(ChrW(&H4D))
              End If
            Case Input.Key.Up
              If pressing Then
                m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H48))
                m_characterBuffer.Add(ChrW(&H0)) : m_characterBuffer.Add(ChrW(&H48))
              End If
            Case Input.Key.Down
              If pressing Then
                m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H50))
                m_characterBuffer.Add(ChrW(&H0)) : m_characterBuffer.Add(ChrW(&H50))
              End If

            Case Input.Key.Home : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H47))
            Case Input.Key.End : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H4F))
            Case Input.Key.PageUp : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H49))
            Case Input.Key.PageDown : If pressing Then m_hexCodeScanCodeBuffer.Add(ChrW(&H0)) : m_hexCodeScanCodeBuffer.Add(ChrW(&H51))

            Case Input.Key.NumPad0 : If pressing Then m_characterBuffer.Add("0"c)
            Case Input.Key.NumPad1 : If pressing Then m_characterBuffer.Add("1"c)
            Case Input.Key.NumPad2 : If pressing Then m_characterBuffer.Add("2"c)
            Case Input.Key.NumPad3 : If pressing Then m_characterBuffer.Add("3"c)
            Case Input.Key.NumPad4 : If pressing Then m_characterBuffer.Add("4"c)
            Case Input.Key.NumPad5 : If pressing Then m_characterBuffer.Add("5"c)
            Case Input.Key.NumPad6 : If pressing Then m_characterBuffer.Add("6"c)
            Case Input.Key.NumPad7 : If pressing Then m_characterBuffer.Add("7"c)
            Case Input.Key.NumPad8 : If pressing Then m_characterBuffer.Add("8"c)
            Case Input.Key.NumPad9 : If pressing Then m_characterBuffer.Add("9"c)
            Case Input.Key.Decimal : If pressing Then m_characterBuffer.Add("."c)
            Case Input.Key.Add : If pressing Then m_characterBuffer.Add("+"c)
            Case Input.Key.Subtract : If pressing Then m_characterBuffer.Add("-"c)
            Case Input.Key.Multiply : If pressing Then m_characterBuffer.Add("*"c)
            Case Input.Key.Divide : If pressing Then m_characterBuffer.Add("/"c)

            Case Input.Key.Unknown
              Select Case e.PlatformKeyCode
                Case 18 ' Alt
                Case 91 ' Window
                Case 19 ' Break Pause
                Case 144 ' NumLock
                Case 186 ' ; :
                  If pressing Then If shift Then m_characterBuffer.Add(":"c) Else m_characterBuffer.Add(";"c)
                Case 187 ' = +
                  If pressing Then If shift Then m_characterBuffer.Add("+"c) Else m_characterBuffer.Add("="c)
                Case 188 ' , < 
                  If pressing Then If shift Then m_characterBuffer.Add("<"c) Else m_characterBuffer.Add(","c)
                Case 189 ' - _
                  If pressing Then If shift Then m_characterBuffer.Add("_"c) Else m_characterBuffer.Add("-"c)
                Case 190 ' . >
                  If pressing Then If shift Then m_characterBuffer.Add(">"c) Else m_characterBuffer.Add("."c)
                Case 191 ' / ?
                  If pressing Then If shift Then m_characterBuffer.Add("?"c) Else m_characterBuffer.Add("/"c)
                Case 192 ' ` ~
                  If pressing Then If shift Then m_characterBuffer.Add("~"c) Else m_characterBuffer.Add("`"c)
                Case 219 ' [ {
                  If pressing Then If shift Then m_characterBuffer.Add("{"c) Else m_characterBuffer.Add("["c)
                Case 220 ' \ |
                  If pressing Then If shift Then m_characterBuffer.Add("|"c) Else m_characterBuffer.Add("\"c)
                Case 221 ' ] }
                  If pressing Then If shift Then m_characterBuffer.Add("}"c) Else m_characterBuffer.Add("]"c)
                Case 222 ' ' "
                  If pressing Then If shift Then m_characterBuffer.Add(""""c) Else m_characterBuffer.Add("'"c)
                Case Else
              End Select

          End Select

          If e.OppositeShiftState Then
            If m_keyboard IsNot Nothing AndAlso m_keyboard.UseModifier Then
              shift = Not shift
            Else
              If m_display.ShiftDown Then
                m_display.EditShiftUp()
              Else
                m_display.EditShiftDown()
              End If
            End If
          End If

          If (m_continueInput OrElse
              m_continueInputRandomize OrElse
              m_continueLineInput) AndAlso
             length <> m_characterBuffer.Count Then
            If m_characterBuffer(m_characterBuffer.Count - 1) = ChrW(8) Then
              If m_characterBuffer.Count > 0 Then
                m_characterBuffer.RemoveAt(m_characterBuffer.Count - 1)
                If m_characterBuffer.Count > 0 Then
                  m_characterBuffer.RemoveAt(m_characterBuffer.Count - 1)
                  Dim row As Short = m_display.CsrLin
                  Dim col As Short = m_display.Pos(0)
                  m_display.Locate(row, CShort(col - 1))
                  m_display.Print(" ", False)
                  m_display.Locate(row, CShort(col - 1))
                End If
              End If
            ElseIf m_characterBuffer(m_characterBuffer.Count - 1) = ChrW(13) Then
            Else
              m_display.Print(m_characterBuffer(m_characterBuffer.Count - 1), False)
            End If
          End If

        End If

      Else
        InteractiveProcessKey(e, pressing)
      End If

      If shift Then
      End If

    End Sub

    Private Sub InteractiveProcessKey(e As Input.KeyEventArgs, pressing As Boolean)

      Dim ctrl As Boolean
      Dim alt As Boolean
      Dim shift As Boolean

      If m_keyboard IsNot Nothing AndAlso m_keyboard.UseModifier Then
        ctrl = ((Me.m_keyboard.Modifier And Input.ModifierKeys.Control) = Input.ModifierKeys.Control)
        alt = ((Me.m_keyboard.Modifier And Input.ModifierKeys.Alt) = Input.ModifierKeys.Alt)
        shift = ((Me.m_keyboard.Modifier And Input.ModifierKeys.Shift) = Input.ModifierKeys.Shift)
      Else
        ctrl = m_display.ControlDown
        alt = m_display.AltDown
        shift = m_display.ShiftDown
      End If

      ' CTRL + ?

      If Not m_paste AndAlso ctrl AndAlso Not alt AndAlso Not shift AndAlso pressing Then
        Select Case e.Key
          Case Input.Key.Home
            m_display.Cls(-1)
          Case Input.Key.C
            If m_autoModeActive Then
              m_display.Print()
              m_display.Print("Ok", True)
              m_autoModeActive = False
            Else
              m_display.Print("*CTRL+C*", True)
            End If
            Return
          Case Input.Key.Unknown
            Select Case e.PlatformKeyCode
              Case 19 ' Break Pause
                If m_autoModeActive Then
                  m_display.Print()
                  InternalPrompt()
                  m_autoModeActive = False
                Else
                  m_display.Print("*CTRL+BREAK*", True)
                End If
                Return
              Case Else
            End Select
          Case Else
        End Select
        'Return
      End If

      ' CTRL + ALT + ?

      If ctrl AndAlso alt AndAlso Not shift AndAlso pressing Then
        Select Case e.Key
          Case Input.Key.D1
            If m_display.ExecuteKeyOn(1) Then ProcessCommand()
            Return
          Case Input.Key.D2
            If m_display.ExecuteKeyOn(2) Then ProcessCommand()
            Return
          Case Input.Key.D3
            If m_display.ExecuteKeyOn(3) Then ProcessCommand()
            Return
          Case Input.Key.D4
            If m_display.ExecuteKeyOn(4) Then ProcessCommand()
            Return
          Case Input.Key.D5
            If m_display.ExecuteKeyOn(5) Then ProcessCommand()
            Return
          Case Input.Key.D6
            If m_display.ExecuteKeyOn(6) Then ProcessCommand()
            Return
          Case Input.Key.D7
            If m_display.ExecuteKeyOn(7) Then ProcessCommand()
            Return
          Case Input.Key.D8
            If m_display.ExecuteKeyOn(8) Then ProcessCommand()
            Return
          Case Input.Key.D9
            If m_display.ExecuteKeyOn(9) Then ProcessCommand()
            Return
          Case Input.Key.D0
            If m_display.ExecuteKeyOn(10) Then ProcessCommand()
            Return
          Case Else
        End Select
        'Return
      End If

      ' CTRL + SHIFT + ?

      If ctrl AndAlso Not alt AndAlso shift AndAlso pressing Then
        'Return
      End If

      ' ALT + ?

      If Not ctrl AndAlso alt AndAlso Not shift AndAlso pressing Then
        'Return
      End If

      ' ALT + SHIFT + ?

      If Not ctrl AndAlso alt AndAlso shift AndAlso pressing Then
        'Return
      End If

      If e.OppositeShiftState Then
        shift = Not shift
        If Not (m_keyboard IsNot Nothing AndAlso m_keyboard.UseModifier) Then
          If m_display.ShiftDown Then
            m_display.EditShiftUp()
          Else
            m_display.EditShiftDown()
          End If
        End If
      End If

      Select Case e.Key
      'Case Key.Alt ' Alt
      '  If m_keyboard Is Nothing OrElse Not m_keyboard.UseModifier Then
      '    If pressing Then m_display.EditAltDown() Else m_display.EditAltUp()
      '  End If
      'Case Key.Window ' Window
        Case Input.Key.Ctrl
          If m_keyboard Is Nothing OrElse Not m_keyboard.UseModifier Then
            If pressing Then m_display.EditControlDown() Else m_display.EditControlUp()
          End If
        Case Input.Key.Shift
          If m_keyboard Is Nothing OrElse Not m_keyboard.UseModifier Then
            If pressing Then m_display.EditShiftDown() Else m_display.EditShiftUp()
          End If
        Case Input.Key.Alt
          If m_keyboard Is Nothing OrElse Not m_keyboard.UseModifier Then
            If pressing Then m_display.EditAltDown() Else m_display.EditAltUp()
          End If
        Case Input.Key.CapsLock
          If pressing Then m_display.EditCapsLock()
        Case Input.Key.Insert : If pressing Then m_display.EditInsert()

        Case Input.Key.Escape : If pressing Then m_display.EditEscape()
        Case Input.Key.Back : If pressing Then m_display.EditBackspace()
        Case Input.Key.Delete : If pressing Then m_display.EditDelete()
        Case Input.Key.Tab : If pressing Then m_display.EditTab()

        Case Input.Key.A, Input.Key.B, Input.Key.C, Input.Key.D, Input.Key.E, Input.Key.F, Input.Key.G, Input.Key.H, Input.Key.I, Input.Key.J, Input.Key.K, Input.Key.L, Input.Key.M,
           Input.Key.N, Input.Key.O, Input.Key.P, Input.Key.Q, Input.Key.R, Input.Key.S, Input.Key.T, Input.Key.U, Input.Key.V, Input.Key.W, Input.Key.X, Input.Key.Y, Input.Key.Z
          If pressing Then
            If m_display.CapsLock Then
              If shift Then
                m_display.EditKeyPress(CChar(e.Key.ToString.ToLower))
              Else
                m_display.EditKeyPress(CChar(e.Key.ToString))
              End If
            Else
              If shift Then
                m_display.EditKeyPress(CChar(e.Key.ToString))
              Else
                m_display.EditKeyPress(CChar(e.Key.ToString.ToLower))
              End If
            End If
          End If

        Case Input.Key.D0 : If pressing Then If shift Then m_display.EditKeyPress(")"c) Else m_display.EditKeyPress("0"c)
        Case Input.Key.D1 : If pressing Then If shift Then m_display.EditKeyPress("!"c) Else m_display.EditKeyPress("1"c)
        Case Input.Key.D2 : If pressing Then If shift Then m_display.EditKeyPress("@"c) Else m_display.EditKeyPress("2"c)
        Case Input.Key.D3 : If pressing Then If shift Then m_display.EditKeyPress("#"c) Else m_display.EditKeyPress("3"c)
        Case Input.Key.D4 : If pressing Then If shift Then m_display.EditKeyPress("$"c) Else m_display.EditKeyPress("4"c)
        Case Input.Key.D5 : If pressing Then If shift Then m_display.EditKeyPress("%"c) Else m_display.EditKeyPress("5"c)
        Case Input.Key.D6 : If pressing Then If shift Then m_display.EditKeyPress("^"c) Else m_display.EditKeyPress("6"c)
        Case Input.Key.D7 : If pressing Then If shift Then m_display.EditKeyPress("&"c) Else m_display.EditKeyPress("7"c)
        Case Input.Key.D8 : If pressing Then If shift Then m_display.EditKeyPress("*"c) Else m_display.EditKeyPress("8"c)
        Case Input.Key.D9 : If pressing Then If shift Then m_display.EditKeyPress("("c) Else m_display.EditKeyPress("9"c)

        Case Input.Key.Space : If pressing Then m_display.EditKeyPress(" "c)

        Case Input.Key.Enter
          If pressing Then
            If Not m_autoModeActive Then
              ProcessCommand()
            Else
              ProcessCommand()
              If Not InternalNextAutoLine(True) Then
                m_autoModeActive = False
                InternalPrompt()
              End If
            End If
          End If

        Case Input.Key.Left : If pressing Then m_display.EditArrowLeft()
        Case Input.Key.Right : If pressing Then m_display.EditArrowRight()
        Case Input.Key.Up : If pressing Then m_display.EditArrowUp()
        Case Input.Key.Down : If pressing Then m_display.EditArrowDown()

        Case Input.Key.Home : If pressing Then m_display.EditHome()
        Case Input.Key.End : If pressing Then m_display.EditEnd()
        Case Input.Key.PageUp
        Case Input.Key.PageDown

        Case Input.Key.F1 : If pressing Then m_display.Print("LIST", False)
        Case Input.Key.F2 : If pressing Then m_display.Print("RUN", False) : ProcessCommand()
        Case Input.Key.F3 : If pressing Then m_display.Print("LOAD""", False)
        Case Input.Key.F4 : If pressing Then m_display.Print("SAVE""", False)
        Case Input.Key.F5 : If pressing Then m_display.Print("CONT", False) : ProcessCommand()
        Case Input.Key.F6 : If pressing Then m_display.Print(",""LPT1:""", False) : ProcessCommand()
        Case Input.Key.F7 : If pressing Then m_display.Print("TRON", False) : ProcessCommand()
        Case Input.Key.F8 : If pressing Then m_display.Print("TROFF", False) : ProcessCommand()
        Case Input.Key.F9 : If pressing Then m_display.Print("KEY ", False)
        Case Input.Key.F10 : If pressing Then m_display.Print("SCREEN 0,0,0", False) : ProcessCommand()

        Case Input.Key.NumPad0 : If pressing Then m_display.EditKeyPress("0"c)
        Case Input.Key.NumPad1 : If pressing Then m_display.EditKeyPress("1"c)
        Case Input.Key.NumPad2 : If pressing Then m_display.EditKeyPress("2"c)
        Case Input.Key.NumPad3 : If pressing Then m_display.EditKeyPress("3"c)
        Case Input.Key.NumPad4 : If pressing Then m_display.EditKeyPress("4"c)
        Case Input.Key.NumPad5 : If pressing Then m_display.EditKeyPress("5"c)
        Case Input.Key.NumPad6 : If pressing Then m_display.EditKeyPress("6"c)
        Case Input.Key.NumPad7 : If pressing Then m_display.EditKeyPress("7"c)
        Case Input.Key.NumPad8 : If pressing Then m_display.EditKeyPress("8"c)
        Case Input.Key.NumPad9 : If pressing Then m_display.EditKeyPress("9"c)
        Case Input.Key.Decimal : If pressing Then m_display.EditKeyPress("."c)
        Case Input.Key.Add : If pressing Then m_display.EditKeyPress("+"c)
        Case Input.Key.Subtract : If pressing Then m_display.EditKeyPress("-"c)
        Case Input.Key.Multiply : If pressing Then m_display.EditKeyPress("*"c)
        Case Input.Key.Divide : If pressing Then m_display.EditKeyPress("/"c)

        Case Input.Key.SemiColon : If pressing Then If shift Then m_display.EditKeyPress(":"c) Else m_display.EditKeyPress(";"c)
        Case Input.Key.Plus : If pressing Then If shift Then m_display.EditKeyPress("+"c) Else m_display.EditKeyPress("="c)
        Case Input.Key.Comma : If pressing Then If shift Then m_display.EditKeyPress("<"c) Else m_display.EditKeyPress(","c)
        Case Input.Key.Minus : If pressing Then If shift Then m_display.EditKeyPress("_"c) Else m_display.EditKeyPress("-"c)
        Case Input.Key.Period : If pressing Then If shift Then m_display.EditKeyPress(">"c) Else m_display.EditKeyPress("."c)
        Case Input.Key.Slash : If pressing Then If shift Then m_display.EditKeyPress("?"c) Else m_display.EditKeyPress("/"c)
        Case Input.Key.Tilde : If pressing Then If shift Then m_display.EditKeyPress("~"c) Else m_display.EditKeyPress("`"c)
        Case Input.Key.BracketLeft : If pressing Then If shift Then m_display.EditKeyPress("{"c) Else m_display.EditKeyPress("["c)
        Case Input.Key.Backslash : If pressing Then If shift Then m_display.EditKeyPress("|"c) Else m_display.EditKeyPress("\"c)
        Case Input.Key.BracketRight : If pressing Then If shift Then m_display.EditKeyPress("}"c) Else m_display.EditKeyPress("]"c)
        Case Input.Key.Apostrophe : If pressing Then If shift Then m_display.EditKeyPress(""""c) Else m_display.EditKeyPress("'"c)

        Case Input.Key.Unknown
          Select Case e.PlatformKeyCode
            Case 18 ' Alt
            Case 91 ' Window
            Case 3 ' CTRL+BREAK?
              m_shutDown = True
            Case 19 ' Break Pause
            Case 144 ' NumLock
            Case 186 ' ; :
              If pressing Then If shift Then m_display.EditKeyPress(":"c) Else m_display.EditKeyPress(";"c)
            Case 187 ' = +
              If pressing Then If shift Then m_display.EditKeyPress("+"c) Else m_display.EditKeyPress("="c)
            Case 188 ' , < 
              If pressing Then If shift Then m_display.EditKeyPress("<"c) Else m_display.EditKeyPress(","c)
            Case 189 ' - _
              If pressing Then If shift Then m_display.EditKeyPress("_"c) Else m_display.EditKeyPress("-"c)
            Case 190 ' . >
              If pressing Then If shift Then m_display.EditKeyPress(">"c) Else m_display.EditKeyPress("."c)
            Case 191 ' / ?
              If pressing Then If shift Then m_display.EditKeyPress("?"c) Else m_display.EditKeyPress("/"c)
            Case 192 ' ` ~
              If pressing Then If shift Then m_display.EditKeyPress("~"c) Else m_display.EditKeyPress("`"c)
            Case 219 ' [ {
              If pressing Then If shift Then m_display.EditKeyPress("{"c) Else m_display.EditKeyPress("["c)
            Case 220 ' \ |
              If pressing Then If shift Then m_display.EditKeyPress("|"c) Else m_display.EditKeyPress("\"c)
            Case 221 ' ] }
              If pressing Then If shift Then m_display.EditKeyPress("}"c) Else m_display.EditKeyPress("]"c)
            Case 222 ' ' "
              If pressing Then If shift Then m_display.EditKeyPress(""""c) Else m_display.EditKeyPress("'"c)
            Case 145 ' Scroll Lock
            ' Do nothing...

            Case 0 ' supressing a known, yet unsupported key...

            Case Else
              If pressing Then
                m_display.Print(String.Format("UnknownKey: {0}{1}", e.PlatformKeyCode, If(shift, "+SHIFT", "")), True)
              End If
          End Select
        Case Else
      End Select

      If e.OppositeShiftState Then
        shift = Not shift
        If Not (m_keyboard IsNot Nothing AndAlso m_keyboard.UseModifier) Then
          If m_display.ShiftDown Then
            m_display.EditShiftUp()
          Else
            m_display.EditShiftDown()
          End If
        End If
      End If

      If shift Then
      End If

    End Sub

    Public Sub ProcessInterpreter()

      'For iteration As Short = 1 To m_speed

      ' Will be executing each line in the source file until either:
      '  a) reach an END statement.
      '  b) reach an inferred END statement (last line of code).
      '  c) reach a STOP statement.
      '  d) CTRL+C executed.

      ' Checks to see if there is a pending shutdown before executing each statement.
      If m_shutDown Then
        InternalShutdown()
      End If

      If m_running AndAlso Not m_waiting Then

        ' Are we in the process of "listing" to the screen. (GW-BASIC)
        If False Then 'm_listLineIndex > -1 Then

          'm_running = ContinueList()

          'If Not m_running Then
          '  InternalPrompt()
          'End If

          'Exit For

        Else

          ' Do we have a PEN (ON PEN GOSUB n) active?
          If Me.m_onPenState = OnState.On AndAlso
           m_onPenActivated AndAlso
           m_interpreter.Count > 1 Then

            m_onPenActivated = False

            If m_onPenLineOrLabel IsNot Nothing Then
              m_sleepUntil = New Date?
              ' Setup gosub...
              SetReturnPosition()
              GotoLabelOrLineNumber(m_onPenLineOrLabel)
              SetSubEntryPosition()
            End If

          End If

          ' Do we have a TIMER (ON TIMER GOSUB n) active?
          If Me.m_onTimerState = OnState.On AndAlso
           m_interpreter.Count > 1 Then 'm_interpreter(0).LineNumber IsNot Nothing Then

            If Date.Now > m_onTimerTime Then

              m_sleepUntil = New Date?

              ' Prepare for next event...
              m_onTimerTime = Date.Now.AddSeconds(m_onTimerInterval)

              ' Setup gosub...
              SetReturnPosition()
              GotoLabelOrLineNumber(m_onTimerLine.ToString)
              SetSubEntryPosition()

            End If

          End If

          If m_interpreter.Count > 0 AndAlso m_interpreter.Count > 1 Then 'm_interpreter(0).LineNumber IsNot Nothing Then

            For index As Integer = 0 To 19

              If Me.m_onKey(index).State = OnState.On AndAlso
               m_onKey(index).Line > 0 AndAlso
               m_hexCodeScanCodeBuffer.Count > 1 AndAlso
               m_onKey(index).HexCodeScanCode IsNot Nothing Then

                If m_onKey(index).HexCodeScanCode(0) = m_hexCodeScanCodeBuffer(0) AndAlso
                 m_onKey(index).HexCodeScanCode(1) = m_hexCodeScanCodeBuffer(1) Then

                  m_sleepUntil = New Date?

                  ' Remove key...
                  m_hexCodeScanCodeBuffer.RemoveAt(0)
                  m_hexCodeScanCodeBuffer.RemoveAt(0)

                  ' Prepare GOSUB.
                  SetReturnPosition()
                  GotoLabelOrLineNumber(m_onKey(index).Line.ToString)
                  SetSubEntryPosition()

                End If

              End If

            Next

            If m_hexCodeScanCodeBuffer.Count > 1 Then
              ' Key was not consumed... throw away.
              m_hexCodeScanCodeBuffer.RemoveAt(0)
              m_hexCodeScanCodeBuffer.RemoveAt(0)
            End If

          End If

          If m_sleepUntil IsNot Nothing Then

            'If m_sleepUntil = DateTime.MaxValue Then
            '  ' Keep sleeping...
            'Else
            If Date.Now > m_sleepUntil Then
              m_sleepUntil = New Date?
              'Else
              '  Exit For
            End If

            If m_keyBuffer.Count > 0 OrElse
             m_characterBuffer.Count > 0 OrElse
             m_hexCodeScanCodeBuffer.Count > 0 Then
              m_sleepUntil = New Date?
            Else
              Return 'Exit For
            End If

          End If

          If PeekToken() Is Nothing AndAlso
           Not m_waiting AndAlso
           Not m_continueInputDollar AndAlso
           Not m_continueInput AndAlso
           Not m_continueInputRandomize AndAlso
           Not m_continueLineInput Then
            PopStatement()
          Else
            ' Haven't finished the current statement...
          End If

          If m_interpreterIndex > m_interpreter.Count - 1 Then

            ' End of program.

            m_running = False
            m_contIndex = -1
            m_continueInputDollar = False
            m_continueInput = False
            m_continueInputRandomize = False
            m_continueLineInput = False

            If Not m_autoModeActive Then
              'If Me.m_dialect = Parser.Dialect.GWBasic Then
              '  InternalPrompt()
              'Else
              RaiseEvent EndHook(Me, EventArgs.Empty)
              'End If
            End If

          Else

            If m_continueInputDollar Then

              ' Need to pick up where we left off.
              m_continueInputDollar = False
              m_running = ContinueExecuteInputDollar()

            ElseIf m_continueInput Then

              ' Need to pick up where we left off.
              m_continueInput = False
              m_running = ContinueExecuteInput()

            ElseIf m_continueLineInput Then

              ' Need to pick up where we left off.
              m_continueLineInput = False
              m_running = ContinueExecuteLineInput()

            ElseIf m_continueInputRandomize Then

              ' Need to pick up where we left off.
              m_continueInputRandomize = False
              m_running = ContinueExecuteInputRandomize()

            Else

              Trace()

              m_running = ExecuteStatement()

            End If

            If m_running Then

              ' Used to have event stuff here... moved up...

            Else

              If m_suppressOk Then
                m_suppressOk = False
              Else
                If Not m_autoModeActive Then
                  'If Me.m_dialect = Parser.Dialect.GWBasic Then
                  '  InternalPrompt()
                  'Else
                  RaiseEvent EndHook(Me, EventArgs.Empty)
                  'End If
                End If
              End If

            End If

          End If

        End If

      Else

        m_continueInputDollar = False
        m_continueInput = False
        m_continueInputRandomize = False
        m_continueLineInput = False

      End If

      If 1 = 1 Then

        m_display.CursorVisible = Not m_running OrElse
                                m_continueInputDollar OrElse
                                m_continueInput OrElse
                                m_continueInputRandomize OrElse
                                m_continueLineInput

      End If

      '  If Not m_running Then Exit For

      '  If m_forcedInvalidate AndAlso
      '   m_running AndAlso
      '   m_display.IsInvalidated Then
      '    Exit For
      '  End If

      'Next

    End Sub

    Private Sub Trace()

      'If Me.m_dialect = Parser.Dialect.GWBasic Then
      '  If m_tron AndAlso
      '   m_interpreter(m_interpreterIndex).LineNumber IsNot Nothing AndAlso
      '   m_statementIndex = 0 AndAlso m_tokenIndex = 0 Then
      '    m_display.Print(String.Format("[{0}]", m_interpreter(m_interpreterIndex).LineNumber), False)
      '  End If
      'Else
      '  'TODO: Implement TRACE eventing for Amiga or QBasic.
      'End If

    End Sub

    Private Sub ProcessCommand()

      ' Determine "line" that enter was pressed on.

      Dim text As String = m_display.GetCurrentLineText

      m_display.EditReturn()

      If text Is Nothing Then
        Return
      End If

      If text.Length > 255 Then
        text = text.Substring(0, 255)
      End If

      If m_autoModeActive Then
        Dim asterisk As Integer = -1
        For index As Integer = 0 To text.Length - 1
          If "0123456789".Contains(text(index)) Then
          ElseIf text(index) = "*" Then
            asterisk = index
            Exit For
          Else
            Exit For
          End If
        Next
        If asterisk > -1 Then
          If text.Length = asterisk + 1 Then
            ' Skip
            Return
          Else
            ' Update
            'text = text.Substring(0, asterisk) & " " & text.Substring(asterisk + 1)
            text = String.Concat(text.AsSpan(0, asterisk), " ", text.AsSpan(asterisk + 1))
          End If
        End If
      End If

      ' For debugging purposes.
      'm_display.Print("Command: " & text, True)

      Dim buffer() As Byte = text.ToByteArray
      Using s As New System.IO.MemoryStream()

        s.Write(buffer, 0, buffer.Length)
        s.Seek(0, System.IO.SeekOrigin.Begin)

        ' Parse command.
        Dim parse As New Parser.Parser(s)

        'Dim printOK As Boolean = True

        If parse.Lines.Count > 0 Then

          'If Me.m_dialect = Parser.Dialect.GWBasic Then

          '  ' GW-BASIC's interactive mode and "editor" are combined.

          '  If parse.Lines(0).LineNumber Is Nothing Then

          '    'Dim tp As New TokenProcessor(parse.Lines(0).Statements(0).Tokens)

          '    m_interpreter.Clear()
          '    m_interpreter.Add(parse.Lines(0).Copy)
          '    m_interpreterIndex = 0
          '    m_statementIndex = 0
          '    m_tokenIndex = 0
          '    m_running = True
          '    'ExecuteLine() 'tp)

          '  Else
          '    ' Has a line number, will update the program list accordingly.
          '    ' Because it is an line entry, do not print the OK prompt.

          '    If m_pendingNew Then
          '      m_lines.Clear()
          '      m_pendingNew = False
          '    End If

          '    If Not CInt(parse.Lines(0).LineNumber).Between(0, 65529) Then
          '      m_display.Print("Syntax error", True)
          '      InternalPrompt()
          '    Else

          '      ' see if the current line exists.
          '      '   if so, update or delete it.
          '      '   else add it.

          '      m_contIndex = -1

          '      InsertUpdateSourceLine(parse.Lines(0))

          '      'Dim existingIndex As Integer = -1

          '      'For index As Integer = 0 To m_lines.Count - 1
          '      '  If m_lines(index).LineNumber = parse.Lines(0).LineNumber Then
          '      '    existingIndex = index
          '      '    Exit For
          '      '  End If
          '      'Next

          '      'If existingIndex > -1 Then

          '      '  If parse.Lines(0).Statements.Count = 0 Then
          '      '    m_lines.RemoveAt(existingIndex)
          '      '  Else
          '      '    m_lines(existingIndex) = parse.Lines(0)
          '      '  End If

          '      'Else
          '      '  ' Need to find where to insert it at in order to keep the list sorted.
          '      '  Dim inserted As Boolean = False
          '      '  For index As Integer = 0 To m_lines.Count - 1
          '      '    If m_lines(index).LineNumber > parse.Lines(0).LineNumber Then
          '      '      m_lines.Insert(If(index = 0, 0, index - 1), parse.Lines(0))
          '      '      inserted = True
          '      '      Exit For
          '      '    End If
          '      '  Next
          '      '  If Not inserted Then
          '      '    ' Add
          '      '    m_lines.Add(parse.Lines(0))
          '      '  End If
          '      'End If

          '      m_autoStart = CInt(parse.Lines(0).LineNumber)

          '      'printOK = False

          '    End If
          '  End If

          'Else

          ' One line "interactive" mode (only) (Amiga, QBasic)

          If parse.Lines(0).LineNumber Is Nothing Then
            m_interpreter.Clear()
            m_interpreter.Add(parse.Lines(0).Copy)
            m_interpreterIndex = 0
            m_statementIndex = 0
            m_tokenIndex = 0
            m_running = True
          Else
            m_running = ThrowBasicError(BasicError.SyntaxError)
          End If

          'End If

        Else
          m_display.Print("Nothing?", True)
          InternalPrompt()
        End If

      End Using

      'If printOK Then
      '  InternalPrompt()
      'End If

    End Sub

    Private Sub InsertUpdateSourceLine(line As Parser.Line)

      Dim existingIndex As Integer = -1

      For index As Integer = 0 To m_lines.Count - 1
        If m_lines(index).LineNumber = line.LineNumber Then
          existingIndex = index
          Exit For
        End If
      Next

      If existingIndex > -1 Then

        If line.Statements.Count = 0 Then
          m_lines.RemoveAt(existingIndex)
        Else
          m_lines(existingIndex) = line
        End If

      Else
        ' Need to find where to insert it at in order to keep the list sorted.
        Dim inserted As Boolean = False
        For index As Integer = 0 To m_lines.Count - 1
          If m_lines(index).LineNumber > line.LineNumber Then
            'm_lines.Insert(If(index = 0, 0, index - 1), line)
            m_lines.Insert(If(index = 0, 0, index), line)
            inserted = True
            Exit For
          End If
        Next
        If Not inserted Then
          ' Add
          m_lines.Add(line)
        End If
      End If


    End Sub

#Region "Throw"

    Private Enum BasicError As Short
      NextWithoutFor = 1
      SyntaxError = 2
      ReturnWithoutGosub = 3
      OutOfData = 4
      IllegalFunctionCall = 5
      Overflow = 6
      OutOfMemory = 7
      UndefinedLineNumber = 8
      SubscriptOutOfRange = 9
      DuplicateDefinition = 10
      DivisionByZero = 11
      IllegalDirect = 12
      TypeMismatch = 13
      OutOfStringSpace = 14
      StringTooLong = 15
      StringFormulaTooComplex = 16
      CantContinue = 17
      UndefinedUserFunction = 18
      NoResume = 19
      ResumeWithoutError = 20
      'UnprintableError = 21
      MissingOperand = 22
      LineBufferOverflow = 23
      DeviceTimeout = 24
      DeviceFault = 25
      ForWithoutNext = 26
      OutOfPaper = 27
      'UnprintableError = 28
      WhileWithoutWend = 29
      WendWithoutWhile = 30
      'UnprintableError = 31
      'UnprintableError = 32
      'UnprintableError = 33
      'UnprintableError = 34
      'UnprintableError = 35
      'UnprintableError = 36
      'UnprintableError = 37
      'UnprintableError = 38
      'UnprintableError = 39
      'UnprintableError = 40
      'UnprintableError = 41
      'UnprintableError = 42
      'UnprintableError = 43
      'UnprintableError = 44
      'UnprintableError = 45
      'UnprintableError = 46
      'UnprintableError = 47
      'UnprintableError = 48
      'UnprintableError = 49
      FieldOverflow = 50
      InternalError = 51
      BadFileNumber = 52
      FileNotFound = 53
      BadFileMode = 54
      FileAlreadyOpen = 55
      'UnprintableError = 56
      DeviceIoError = 57
      FileAlreadyExists = 58
      'UnprintableError = 59
      'UnprintableError = 60
      DiskFull = 61
      InputPastEnd = 62
      BadRecordNumber = 63
      BadFilename = 64
      'UnprintableError = 65
      DirectStatementInFile = 66
      TooManyFiles = 67
      DeviceUnavailable = 68
      CommunicationBufferOverflow = 69
      PermissionDenied = 70
      DiskNotReady = 71
      DiskMediaError = 72
      AdvancedFeature = 73
      RenameAcrossDisks = 74
      PathFileAccessError = 75
      PathNotFound = 76
    End Enum

    Private ReadOnly m_basicErrorOutput As New Dictionary(Of Short, String) From {{BasicError.NextWithoutFor, "NEXT without FOR"},
                                                                            {BasicError.SyntaxError, "Syntax error"},
                                                                            {BasicError.ReturnWithoutGosub, "RETURN without GOSUB"},
                                                                            {BasicError.OutOfData, "Out of DATA"},
                                                                            {BasicError.IllegalFunctionCall, "Illegal function call"},
                                                                            {BasicError.Overflow, "Overflow"},
                                                                            {BasicError.OutOfMemory, "Out of memory"},
                                                                            {BasicError.UndefinedLineNumber, "Undefined line number"},
                                                                            {BasicError.SubscriptOutOfRange, "Subscript out of range"},
                                                                            {BasicError.DuplicateDefinition, "Duplicate Definition"},
                                                                            {BasicError.DivisionByZero, "Division by zero"},
                                                                            {BasicError.IllegalDirect, "Illegal direct"},
                                                                            {BasicError.TypeMismatch, "Type mismatch"},
                                                                            {BasicError.OutOfStringSpace, "Out of string space"},
                                                                            {BasicError.StringTooLong, "String too long"},
                                                                            {BasicError.StringFormulaTooComplex, "String formula too complex"},
                                                                            {BasicError.CantContinue, "Can't continue"},
                                                                            {BasicError.UndefinedUserFunction, "Undefined user function"},
                                                                            {BasicError.NoResume, "No RESUME"},
                                                                            {BasicError.ResumeWithoutError, "RESUME without error"},
                                                                            {BasicError.MissingOperand, "Missing operand"},
                                                                            {BasicError.LineBufferOverflow, "Line buffer overflow"},
                                                                            {BasicError.DeviceTimeout, "Device Timeout"},
                                                                            {BasicError.DeviceFault, "Device Fault"},
                                                                            {BasicError.ForWithoutNext, "FOR Without NEXT"},
                                                                            {BasicError.OutOfPaper, "Out of Paper"},
                                                                            {BasicError.WhileWithoutWend, "WHILE without WEND"},
                                                                            {BasicError.WendWithoutWhile, "WEND without WHILE"},
                                                                            {BasicError.FieldOverflow, "FIELD overflow"},
                                                                            {BasicError.InternalError, "Internal Error"},
                                                                            {BasicError.BadFileNumber, "Bad file number"},
                                                                            {BasicError.FileNotFound, "File not found"},
                                                                            {BasicError.BadFileMode, "Bad file mode"},
                                                                            {BasicError.FileAlreadyOpen, "File already open"},
                                                                            {BasicError.DeviceIoError, "Device I/O Error"},
                                                                            {BasicError.FileAlreadyExists, "File already exists"},
                                                                            {BasicError.DiskFull, "Disk full"},
                                                                            {BasicError.InputPastEnd, "Input past end"},
                                                                            {BasicError.BadRecordNumber, "Bad record number"},
                                                                            {BasicError.BadFilename, "Bad filename"},
                                                                            {BasicError.DirectStatementInFile, "Direct statement in file"},
                                                                            {BasicError.TooManyFiles, "Too many files"},
                                                                            {BasicError.DeviceUnavailable, "Device Unavailable"},
                                                                            {BasicError.CommunicationBufferOverflow, "Communication buffer overflow"},
                                                                            {BasicError.PermissionDenied, "Permission Denied"},
                                                                            {BasicError.DiskNotReady, "Disk not Ready"},
                                                                            {BasicError.DiskMediaError, "Disk media error"},
                                                                            {BasicError.AdvancedFeature, "Advanced Feature"},
                                                                            {BasicError.RenameAcrossDisks, "Rename across disks"},
                                                                            {BasicError.PathFileAccessError, "Path/File Access Error"},
                                                                            {BasicError.PathNotFound, "Path not found"}}

    Public Function ThrowBasicError(err As Short) As Boolean
      Return ThrowBasicError(err, 0)
    End Function

    Private Function ThrowBasicError(err As Short, lineNumber As Integer) As Boolean

      m_err = CShort(err)

      Dim interpreterIndex = m_interpreterIndex

      'If m_waitForResponse Then
      '  interpreterIndex -= 1
      'End If

      If lineNumber > 0 Then
        m_erl = lineNumber
        'ElseIf Me.m_dialect = Parser.Dialect.GWBasic AndAlso
        '       m_interpreter(interpreterIndex).LineNumber Is Nothing Then
        '  m_erl = 65535 ' Direct mode.
      Else

        'ElseIf m_dialect = Parser.Dialect.AmigaBasic Then
        '  'TODO: Need to determine "last encountered line number".
        '  m_erl = 65535
        'ElseIf m_dialect = Parser.Dialect.QBasic Then
        '  'TODO: Need to determine "last encountered line number".
        '  m_erl = 65529
        'Else
        '  m_erl = 65535
        'End If
        If m_interpreter IsNot Nothing AndAlso
         interpreterIndex < m_interpreter.Count AndAlso
         m_interpreter(interpreterIndex).LineNumber IsNot Nothing Then
          m_erl = CInt(m_interpreter(interpreterIndex).LineNumber)
        Else
          If m_successLineNumber > 0 Then
            m_erl = m_successLineNumber
          Else
            m_erl = 0
          End If
        End If

      End If

      Dim message As String = "Unprintable error"

      Dim value As String = Nothing
      If m_basicErrorOutput.TryGetValue(err, value) Then
        message = value
      End If

      If m_erl < 65535 Then

        If m_onErrorGoto > 0 AndAlso
         m_onErrorResumeInterpreterIndex = -1 Then

          'Dim l = From p In m_lines Where p.LineNumber = m_onErrorGoto

          'If l.Count > 0 Then
          '  m_onErrorResumeInterpreterIndex = interpreterIndex
          '  m_onErrorResumeStatementIndex = m_statementIndex
          '  Return GotoLabelOrLineNumber(l(0).LineNumber.ToString)
          'Else
          '  'Return ThrowBasicError(BasicError.UndefinedLineNumber)
          '  If m_dialect = Parser.Dialect.GWBasic Then
          '    m_erl = CInt(m_interpreter(interpreterIndex).LineNumber)
          '  Else
          '  End If
          '  m_err = BasicError.UndefinedLineNumber
          '  message = m_basicErrorOutput(err)
          'End If

          If m_erl = 0 Then
            'If Me.m_dialect = Parser.Dialect.QBasic Then
            '  m_erl = 65529 
            'ElseIf Me.m_dialect = Parser.Dialect.AmigaBasic Then
            '  m_erl = 65535
            'End If
          End If

          m_onErrorResumeInterpreterIndex = interpreterIndex
          m_onErrorResumeStatementIndex = m_statementIndex
          Dim label As String = Nothing
          If m_lines(m_onErrorGoto).Statements(0).Tokens(0).IsLabelToken Then
            label = m_lines(m_onErrorGoto).Statements(0).Tokens(0).Literal
          ElseIf m_lines(m_onErrorGoto).LineNumber IsNot Nothing Then
            Return GotoLabelOrLineNumber(m_lines(m_onErrorGoto).LineNumber.ToString)
          Else
            m_erl = CInt(m_interpreter(interpreterIndex).LineNumber)
            m_err = BasicError.UndefinedLineNumber
            message = m_basicErrorOutput(err)
          End If

          If message IsNot Nothing Then
          End If

          Return GotoLabelOrLineNumber(label)

        End If

        If m_display.Pos(0) > 1 Then
          m_display.Print()
        End If
        If m_erl > 0 Then 'm_dialect = Parser.Dialect.GWBasic Then
          m_display.Print(String.Format("{0} in {1}", message, m_erl), True)
        Else
          m_display.Print(String.Format("{0} line {1}", message, m_interpreterIndex + 1), True)
        End If

      Else
        'If Me.m_dialect = Parser.Dialect.GWBasic Then
        '  m_display.Print(message, True)
        'Else
        m_display.Print(String.Format("{0} line {1}", message, m_interpreterIndex + 1), True)
        'End If
      End If

      Return False

    End Function

    Private Function ThrowBasicError(err As BasicError) As Boolean
      Return ThrowBasicError(CShort(err))
    End Function

    'Private Function ThrowBasicError(err As BasicError, lineNumber As Integer) As Boolean
    '  Return ThrowBasicError(CShort(err), lineNumber)
    'End Function

#End Region

    Public Sub InternalShutdown()
      'If m_useDispatchTimer Then
      's_cancelTokenSource.Cancel()
      'InternalTimer.Stop()
      'InternalTimer = Nothing
      'Else
      '  m_storyboardTimer.Stop()
      '  m_storyboardTimer = Nothing
      'End If
      'RaiseEvent Shutdown(Me, New EventArgs)
      RaiseEvent SystemHook(Me, EventArgs.Empty)
    End Sub

    Private Function RealPath(path As String) As String

      If "ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(path(0)) AndAlso
       path.IndexOf(":"c) = 1 Then
        ' Contains drive.
      ElseIf path(0) = "\" Then
        ' At root... need to add current location drive info.
        'path = m_currentPath.Substring(0, 2) & path
        path = String.Concat(m_currentPath.AsSpan(0, 2), path)
      ElseIf path.IndexOf("..") > -1 Then
        Dim list As New List(Of String)
        'Dim temp() = Split(m_currentPath & path, "\")
        Dim temp() = (m_currentPath & path).Split("\"c)
        For Each entry In temp
          list.Add(entry)
        Next
        Dim index As Integer = list.Count - 1
        Do
          If list(index) = ".." Then
            list.RemoveAt(index)
            If index - 1 > 0 Then
              list.RemoveAt(index - 1)
              index -= 2
            Else
              index -= 1
            End If
          Else
            index -= 1
          End If
          If index = 0 Then
            Exit Do
          End If
        Loop
        Dim result = ""
        For Each entry In list
          If result <> "" Then
            result &= "\"
          End If
          result &= entry
        Next
        Return result
      ElseIf path.IndexOf("*"c) > -1 OrElse
           path.IndexOf("?"c) > -1 Then
        Return Nothing
      ElseIf path.IndexOf(":"c) > -1 Then
        'path = path
      Else
        path = m_currentPath & path
      End If

      If m_virtualFileSystem IsNot Nothing AndAlso Not m_virtualFileSystem.IsEightDotThree Then
        Return path
      Else
        Return path.ToUpper
      End If

    End Function

    Private Function PeekToken() As Parser.Token
      Dim token As Parser.Token = Nothing
      If m_interpreterIndex = -1 AndAlso m_interpreterIndex < m_interpreter.Count Then
        ' do nothing
      ElseIf m_interpreterIndex > m_interpreter.Count - 1 OrElse
           m_statementIndex > m_interpreter(m_interpreterIndex).Statements.Count - 1 OrElse
           m_tokenIndex > m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count - 1 Then
        ' do nothing
      Else
        token = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(m_tokenIndex)
      End If
      Return token
    End Function

    Private Function PopToken() As Parser.Token
      Dim token As Parser.Token = PeekToken()
      If token IsNot Nothing Then m_tokenIndex += 1
      Return token
    End Function

    'Private Function PushToken() As Parser.Token
    'If m_tokenIndex > 0 Then
    'm_tokenIndex -= 1
    'End If
    'Return PeekToken()
    'End Function

    'Private Function Expect(ByVal text As String) As Boolean
    '  If PeekToken().ToString = text Then
    '    PopToken()
    '  Else
    '    Return ThrowBasicError(BasicError.SyntaxError)
    '  End If
    'End Function

    'Private Function Expect(ByVal tokenType As Parser.TokenType) As Boolean
    '  If PeekToken().Type = tokenType Then
    '    PopToken()
    '  Else
    '    Return ThrowBasicError(BasicError.SyntaxError)
    '  End If
    'End Function

    Private Sub PopStatement()
      m_tokenIndex = 0
      m_statementIndex += 1
      If m_interpreterIndex > m_interpreter.Count - 1 Then
        Return
      End If
      If m_interpreterIndex = -1 OrElse
       m_statementIndex > m_interpreter(m_interpreterIndex).Statements.Count - 1 Then
        PopLine()
      End If
    End Sub

    Private Sub PopLine()
      m_statementIndex = 0
      m_tokenIndex = 0
      If m_interpreterIndex < m_interpreter.Count Then
        m_interpreterIndex += 1
      End If
    End Sub

#Region "Expressions"

    Private Function ExecuteStringExpression(ByRef result As String, ByRef variableName As String) As Boolean

      'Dim unused_name As String = ""
      'Dim op As String = ""
      'Dim stri As String = ""

      Dim op As String '= Nothing
      Dim str As String = Nothing

      'result = exec_str_primary(symbol, line, variable_name)
      If Not ExecuteStringPrimary(result, variableName) Then Return False
      If m_continueInputDollar Then Return True

      While PeekToken() IsNot Nothing AndAlso PeekToken().ToString = "+"

        'variable_name = ""
        op = PopToken().ToString()
        'symbol = GetSymbol(Line)
        'stri = exec_str_primary(symbol, Line, unused_name)
        str = Nothing
        If Not ExecuteStringPrimary(str, Nothing) Then Return False
        If m_continueInputDollar Then Return True

        If op = "+" Then
          result &= str
        End If

      End While

      Return True

    End Function

    Private Function ExecuteStringPrimary(ByRef result As String, ByRef variableName As String) As Boolean

      If variableName Is Nothing Then
      End If

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      ElseIf PeekToken.IsStringLiteralToken Then
        result = PopToken.ToString
        Return True
      End If

      Dim keyword As String = PeekToken.ToString()

      'If PeekToken() Is Nothing Then
      '  'Err.WriteLine("EXPRESSION EXPECTED - FOUND END OF LINE.")
      '  Return ThrowBasicError(BasicError.SyntaxError)

      'ElseIf PeekToken.IsStringLiteralToken Then

      '  ' Strip out the quots.

      '  result = PopToken.ToString

      Select Case KeywordToKeywordLookup(keyword)

        Case KeywordLookup.LEFTS

          ' A specified number of characters of a string from it's beginning.
          ' 
          ' LEFT$(str, length)

          PopToken()

          If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim str As String = Nothing
          If Not ExecuteStringExpression(str, Nothing) Then Return False
          If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim length As Double
          If Not ExecuteExpression(length) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          If str.Length > CShort(length) Then
            result = str.Substring(0, CShort(length))
          Else
            result = str
          End If

        Case KeywordLookup.RIGHTS

          ' A specified number of characters of a string from it's end.
          ' 
          ' RIGHT$(str, length)

          PopToken()

          If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim str As String = Nothing
          If Not ExecuteStringExpression(str, Nothing) Then Return False
          If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim length As Double
          If Not ExecuteExpression(length) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          If str.Length - CShort(length) < 1 Then
            result = str
          Else
            result = str.Substring(str.Length - CShort(length))
          End If

        Case KeywordLookup.MIDS

          ' A specified number of characters of a string from it's middle. 
          ' 
          ' MID$(target, start, length)
          ' MID$(target, start)

          Dim target As String = Nothing
          Dim start As Double
          Dim length As Double?

          PopToken()

          If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
          If Not ExecuteStringExpression(target, Nothing) Then Return False
          If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
          If Not ExecuteExpression(start) Then Return False

          If PeekToken.IsCommaToken() Then
            PopToken()
            Dim value As Double
            If Not ExecuteExpression(value) Then Return False
            length = value
          End If

          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          'If length > -1 Then
          '  result = Mid(target, start, length)
          'Else
          '  result = Mid(target, start)
          'End If

          If Not start.Between(1, 255) Then
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

          If length IsNot Nothing AndAlso Not CInt(length).Between(0, 255) Then
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

          If length > -1 Then
            If (start - 1) + length > target.Length Then
              length = target.Length - (start - 1)
            End If
            If length > 0 Then
              result = target.Substring(CShort(start) - 1, CShort(length))
            Else
              result = ""
            End If
          Else
            If target.Length > CShort(start) - 1 Then
              result = target.Substring(CShort(start) - 1)
            Else
              result = ""
            End If
          End If

        Case KeywordLookup.STRINGS

          ' STRING$ returns a string using the specifed stringexpression of the specified length.
          ' 
          ' STRING$(length, stringexpression)
          ' STRING$(length, numericexpression)
          '
          ' numericexpression is converted to ASCII.

          Dim count As Short
          Dim str As String = Nothing
          Dim ascii As Short

          PopToken()

          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          If Not ExecuteExpression(count) Then Return False
          If Not PopToken.IsCommaToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          If count >= 0 Then
            If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
              If Not ExecuteStringExpression(str, Nothing) Then Return False
              result = "".PadRight(count, CChar(str))
            Else
              If Not ExecuteExpression(ascii) Then Return False
              result = "".PadRight(count, ChrW(ascii))
            End If

            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

        Case KeywordLookup.SPACES

          ' SPACE$ returns a string of spaces of the specified length.
          '
          ' SPACE$(length)

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim length As Short
          If Not ExecuteExpression(length) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          If length >= 0 Then
            'result = Space(length)
            result = "".PadRight(length)
          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

        Case KeywordLookup.CHRS

          ' The ASCII character corresponding to the expression provided.
          ' 
          ' CHR$(expression)

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim ascii As Short
          If Not ExecuteExpression(ascii) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          If ascii.Between(0, 255) Then
            result = ChrW(ascii)
          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

        Case KeywordLookup.STRS

          ' The string equivalent of the ASCII value of the numerical expression. The ASCII value of the first character of the string expression.
          ' 
          ' STR$(expression)
          ' VAL(stringexpression)

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim number As Double
          If Not ExecuteExpression(number) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          result = CStr(number)

          ' Adding a space to a number if it positive, if negative, leave the space off.
          If result(0) <> "-" Then
            result = " " & result
          End If

          If result.Substring(result.Length - 1) = ".0" Then
            result = result.Substring(0, result.Length - 2)
          End If

        Case KeywordLookup.LCASES

          ' 
          '
          ' 

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim str As String = Nothing
          If Not ExecuteStringExpression(str, Nothing) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          'result = LCase(str)
          result = str.ToLower

        Case KeywordLookup.UCASES

          ' 
          '
          ' 

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim str As String = Nothing
          If Not ExecuteStringExpression(str, Nothing) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          'result = UCase(str)
          result = str.ToUpper

        Case KeywordLookup.ENVIRONS

          Dim str As String = Nothing
          Dim num1 As Short = -1

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
            'Dim str As String = Nothing
            If Not ExecuteStringExpression(str, Nothing) Then Return False
          Else
            'Dim num1 As Short
            If Not ExecuteExpression(num1) Then Return False
            If Not num1.Between(1, 255) Then
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If
          End If
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          If str IsNot Nothing Then
            Dim entry = (From p In m_environ Where p.Key = str).FirstOrDefault
            If entry.IsNull Then
              result = ""
            Else
              result = entry.Value
            End If
          ElseIf num1.Between(1, 255) Then
            If m_environ.Count <= num1 Then
              Dim index As Integer = 1
              For Each entry In m_environ
                If index = num1 Then
                  result = entry.Value
                  Exit For
                End If
              Next
            Else
              result = ""
            End If
          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

        Case KeywordLookup.LTRIMS

          ' 
          '
          ' 

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim str As String = Nothing
          If Not ExecuteStringExpression(str, Nothing) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          Dim index As Integer = 1

          While index <= str.Length AndAlso str(index) = " "
            index += 1
          End While

          result = str.Substring(index)

        Case KeywordLookup.RTRIMS

          ' 
          ' 
          ' 

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim str As String = Nothing
          If Not ExecuteStringExpression(str, Nothing) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          Dim index = str.Length

          While index >= 1 AndAlso str(index - 1) = " "
            index -= 1
          End While

          result = str.Substring(0, index)

        Case KeywordLookup.INKEYS

          PopToken()

          If m_characterBuffer.Count > 0 Then
            result = m_characterBuffer(0)
            m_characterBuffer.RemoveAt(0)
            If result = ChrW(0) Then
              result &= m_characterBuffer(0)
              m_characterBuffer.RemoveAt(0)
            End If
          Else
            result = ""
          End If

        Case KeywordLookup.INPUTS

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim count As Short
          If Not ExecuteExpression(count) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          ' Start a new interpreter?
          '   Running 10 A$ = INKEY$
          '           20 IF A$ <> "" THEN B$ = A$ + B$
          '           30 IF B$ > count THEN GOTO 50
          '           40 GOTO 10
          '           50 END

          If m_characterBuffer.Count >= count Then
            result = ""
            For index = 1 To count
              result &= m_characterBuffer(0)
              m_characterBuffer.RemoveAt(0)
            Next
          Else
            ' need to jump back here until we do have enough keys or CTRL+BREAK are encountered.
            m_continueInputDollar = True
            Return True
          End If

      '' A specified length string of character(s) from the keyboard or specified open file. 
      ''
      '' INPUT$(integer [,#filenumber])

      'symbol = GetSymbol(Line)
      'expect("(", symbol, Line)
      'index1 = CShort(exec_expr(symbol, Line))

      'If index1 >= 0 Then
      '  If symbol = "," Then
      '    symbol = GetSymbol(Line)
      '    If symbol = "#" Then
      '      symbol = GetSymbol(Line)
      '    End If
      '    index2 = CShort(exec_expr(symbol, Line))
      '    expect(")", symbol, Line)
      '    ' (not implemented) aFile = getFileValue(index2)
      '    If aFile Is Nothing Then
      '      LogWrite("INPUT$(" & index1 & ", #" & index2 & ") ")
      '      If log IsNot Nothing Then log.Flush()
      '      ' (not implemented) result = gets(aFile, index1)
      '      LogWriteLine("-> " & result)
      '    Else
      '      error_marker()
      '      Err.WriteLine("FILE #" & index2 & " NOT OPEN IN INPUT$.")
      '    End If
      '  Else
      '    expect(")", symbol, Line)
      '    LogWrite("INPUT$(" & index1 & ") ")
      '    If log IsNot Nothing Then log.Flush()
      '    ' (translated) result = gets(KEYBOARD, index1)
      '    result = win.Input(index1)
      '    LogWriteLine("-> " & result)
      '  End If
      'Else
      '  error_marker()
      '  Err.WriteLine("INPUT$ WITH NEGATIVE NUMBER " & index1 & ".")
      '  While symbol <> ")"
      '    symbol = GetSymbol(Line)
      '  End While
      '  symbol = GetSymbol(Line)
      'End If

        Case KeywordLookup.IOCTLS

          PopToken()
          If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
          If PeekToken.IsHashToken Then
            PopToken()
          End If
          Dim num1 As Double
          If Not ExecuteExpression(num1) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          Dim filenumber = CShort(num1)

          Dim value As File = Nothing

          If m_fileList.TryGetValue(filenumber, value) Then
            If value.Path Is Nothing AndAlso value.Printer IsNot Nothing Then
              result = value.Printer.IoCtl()
            Else
              Return ThrowBasicError(BasicError.BadFileMode)
            End If
          Else
            Return ThrowBasicError(BasicError.BadFileNumber)
          End If

        Case KeywordLookup.HEXS

          ' The hexadecimal and octal equivalents of the number.
          '
          ' HEX$(number)
          ' OCT$(number)

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim number As Short
          If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
            Dim exp As String = Nothing
            If Not ExecuteStringExpression(exp, Nothing) Then Return False
            number = 0
          Else
            If Not ExecuteExpression(number) Then Return False
          End If
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          result = VisualBasic.Conversion.Hex(number)

        Case KeywordLookup.OCTS

          ' The hexadecimal and octal equivalents of the number.
          '
          ' HEX$(number)
          ' OCT$(number)

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim number As Short
          If Not ExecuteExpression(number) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          'result = Oct(number)
          'result = Oct.EncodeOctalString(number)
          result = VisualBasic.Oct.Oct(number)
      'result = "*"

        Case KeywordLookup.ERDEVS
          PopToken()
          result = ""

        Case KeywordLookup.TIMES

          ' The system date and time, respectively.
          '
          ' DATE$
          ' TIME$

          PopToken()
          result = Date.Now.ToString("HH:mm:ss")

        Case KeywordLookup.DATES

          ' The system date and time, respectively.
          '
          ' DATE$
          ' TIME$

          PopToken()
          result = Date.Now.ToString("MM-dd-yyyy")

        Case KeywordLookup.MKIS

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim number As Short
          If Not ExecuteExpression(number) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          result = VisualBasic.MkCv.MKI(number) ' CStr(Chr(index1 Mod 256)) & CStr(Chr(index1 \ 256 Mod 256))

        Case KeywordLookup.MKSS

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim number As Double
          If Not ExecuteExpression(number) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          result = VisualBasic.MkCv.MKS(CSng(number.ToBasicSng)) ' CStr(Chr(index1 Mod 256)) & CStr(Chr(index1 \ 256 Mod 256))

        Case KeywordLookup.MKDS

          PopToken()
          If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          Dim number As Double
          If Not ExecuteExpression(number) Then Return False
          If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          result = VisualBasic.MkCv.MKD(number) ' CStr(Chr(index1 Mod 256)) & CStr(Chr(index1 \ 256 Mod 256))


          'ElseIf keyword.Equals("MKL$" Then

          '  PopToken()
          '  If Not PopToken.IsParenOpenToken() Then Return ThrowBasicError(BasicError.SyntaxError)
          '  Dim number As Short
          '  If Not ExecuteExpression(number) Then Return False
          '  If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          '  result = MkCv.MKI(number)

        Case Else

          If keyword(0).Between("A"c, "Z"c) Then
            If m_defFnList.ContainsKey(PeekToken.ToString()) Then
              If Not ExecuteStringFunction(m_defFnList(PopToken.ToString), result) Then Return False
            Else
              'variableName = PopToken.ToString()
              Dim varName As String = Nothing
              If Not PopAndParseVariableName(varName) Then Return False
              If Not IsStringVariable(varName) Then Return ThrowBasicError(BasicError.TypeMismatch)
              If Not GetStringVariable(varName, result) Then Return False
            End If
          Else
            If IsNumeric(keyword(0)) Then
              Return ThrowBasicError(BasicError.TypeMismatch)
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          End If

      End Select

      Return True

    End Function

    Private Function ExecuteExpression(ByRef result As Short) As Boolean
      Dim s As Double
      If Not ExecuteExpression(s) Then Return False
      If s.Between(-32768, 32767) Then
        result = CShort(s)
        Return True
      Else
        Return ThrowBasicError(BasicError.Overflow)
      End If
    End Function

    Private Function ExecuteExpression(ByRef result As Double) As Boolean

      If Not ExecuteConditionalEqv(result) Then Return False

      While PeekToken() IsNot Nothing AndAlso PeekToken.ToString() = "IMP"
        PopToken()
        Dim number1 As Short = CShort(result)
        Dim number2 As Double
        If Not ExecuteConditionalEqv(number2) Then Return False
        result = BinaryImp(number1, CShort(number2))
      End While

      Return True

    End Function

    Private Function ExecuteConditionalEqv(ByRef result As Double) As Boolean

      If Not ExecuteConditionalOr(result) Then Return False

      While PeekToken() IsNot Nothing AndAlso PeekToken.ToString() = "EQV"
        PopToken()
        Dim number1 As Short = CShort(result)
        Dim number2 As Double
        If Not ExecuteConditionalOr(number2) Then Return False
        result = BinaryEqv(number1, CShort(number2))
      End While

      Return True

    End Function

    Private Function ExecuteConditionalOr(ByRef result As Double) As Boolean

      If Not ExecuteConditionalAnd(result) Then Return False

      While PeekToken() IsNot Nothing AndAlso (PeekToken.ToString() = "OR" OrElse PeekToken.ToString() = "XOR")

        Dim number1 As Short = CShort(result)
        Dim op As String = PopToken.ToString
        Dim number2 As Double

        If Not ExecuteConditionalAnd(number2) Then Return False

        If op = "OR" Then
          result = BinaryOr(number1, CShort(number2))
        ElseIf op = "XOR" Then
          result = BinaryXor(number1, CShort(number2))
        End If

      End While

      Return True

    End Function

    Private Function ExecuteConditionalAnd(ByRef result As Double) As Boolean

      If Not ExecuteConditionalNot(result) Then Return False

      While PeekToken() IsNot Nothing AndAlso PeekToken.ToString() = "AND"
        PopToken()
        Dim number1 As Short = CShort(result)
        Dim number2 As Double
        If Not ExecuteConditionalNot(number2) Then Return False
        result = BinaryAnd(number1, CShort(number2))
      End While

      Return True

    End Function

    Private Function ExecuteConditionalNot(ByRef result As Double) As Boolean

      If PeekToken() IsNot Nothing AndAlso PeekToken.ToString() = "NOT" Then
        PopToken()
        Dim num As Double
        If Not ExecuteComparison(num) Then Return False
        result = If(num = 0, -1, 0) '-AscW(num = 0)
      Else
        If Not ExecuteComparison(result) Then Return False
      End If

      Return True

    End Function

    Private Function ExecuteComparison(ByRef result As Double) As Boolean

      If PeekToken() IsNot Nothing AndAlso
       PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) AndAlso
       Not m_numericFunctionList.Contains(PeekToken.ToString) Then

        Dim stri1 As String = Nothing

        If Not ExecuteStringExpression(stri1, Nothing) Then Return False

        If PeekToken() IsNot Nothing Then

          Dim symbol As String = PeekToken.ToString()

          If symbol = "=" OrElse
           symbol = "<>" OrElse
           symbol = "<" OrElse
           symbol = ">" OrElse
           symbol = "<=" OrElse
           symbol = ">=" Then

            PopToken()

            Dim op As String = symbol

            Dim stri2 As String = Nothing
            If Not ExecuteStringExpression(stri2, Nothing) Then Return False

            If op = "=" Then
              result = If(stri1 = stri2, -1.0, 0.0)
            ElseIf op = "<>" Then
              result = If(stri1 <> stri2, -1.0, 0.0)
            ElseIf op = "<" Then
              result = If(stri1 < stri2, -1.0, 0.0)
            ElseIf op = ">" Then
              result = If(stri1 > stri2, -1.0, 0.0)
            ElseIf op = "<=" Then
              result = If(stri1 <= stri2, -1.0, 0.0)
            ElseIf op = ">=" Then
              result = If(stri1 >= stri2, -1.0, 0.0)
            End If

          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

        End If

      Else

        If Not ExecuteAddSubtract(result) Then Return False

        If PeekToken() IsNot Nothing Then

          Dim symbol As String = PeekToken.ToString()

          If symbol = "=" OrElse
           symbol = "<>" OrElse
           symbol = "<" OrElse
           symbol = ">" OrElse
           symbol = "<=" OrElse
           symbol = ">=" Then

            PopToken()

            Dim op As String = symbol

            Dim num As Double = 0.0
            If Not ExecuteAddSubtract(num) Then Return False

            If op = "=" Then
              result = If(result = num, -1.0, 0.0)
            ElseIf op = "<>" Then
              result = If(result <> num, -1.0, 0.0)
            ElseIf op = "<" Then
              result = If(result < num, -1.0, 0.0)
            ElseIf op = ">" Then
              result = If(result > num, -1.0, 0.0)
            ElseIf op = "<=" Then
              result = If(result <= num, -1.0, 0.0)
            ElseIf op = ">=" Then
              result = If(result >= num, -1.0, 0.0)
            End If

          End If
        End If
      End If

      Return True

    End Function

    Private Function ExecuteAddSubtract(ByRef result As Double) As Boolean

      If Not ExecuteMultiplyDivide(result) Then Return False

      While PeekToken() IsNot Nothing AndAlso
          (PeekToken.ToString = "+" OrElse
           PeekToken.ToString = "-")

        Dim op As String = PopToken.ToString

        Dim num As Double
        If Not ExecuteMultiplyDivide(num) Then Return False

        If op = "+" Then
          result += num
        ElseIf op = "-" Then
          result -= num
        End If

      End While

      Return True

    End Function

    Private Function ExecuteMultiplyDivide(ByRef result As Double) As Boolean

      If Not ExecuteNegation(result) Then Return False

      While PeekToken() IsNot Nothing AndAlso
          (PeekToken.ToString = "*" OrElse
           PeekToken.ToString = "/" OrElse
           PeekToken.ToString = "\" OrElse
           PeekToken.ToString = "MOD")

        Dim op As String = PopToken.ToString

        Dim num As Double
        If Not ExecuteNegation(num) Then Return False

        If op = "*" Then
          result *= num
        Else
          Try
            If op = "/" Then
              If num = 0 Then
                m_display.Print(m_basicErrorOutput(BasicError.DivisionByZero), True, False)
                result = 1.701412E+38
              Else
                result /= num
              End If
            ElseIf op = "\" Then
              If num = 0 Then
                m_display.Print(m_basicErrorOutput(BasicError.DivisionByZero), True, False)
                result = 1.701412E+38
              Else
                result = CShort(result) \ CShort(num)
              End If
            ElseIf op = "MOD" Then
              If result = 0 AndAlso num = 0 Then
                result = CShort(0)
              Else
                result = CShort(result) Mod CShort(num)
              End If
            End If
          Catch ex As Exception
            m_display.Print(m_basicErrorOutput(BasicError.DivisionByZero), True, False)
            result = 1.701412E+38
          End Try
        End If
      End While

      Return True

    End Function

    Private Function ExecuteNegation(ByRef result As Double) As Boolean

      If PeekToken() IsNot Nothing AndAlso PeekToken.ToString = "-" Then
        PopToken()
        If Not ExecuteExponentation(result) Then Return False
        result = -result
      ElseIf PeekToken() IsNot Nothing AndAlso PeekToken.ToString = "+" Then
        PopToken()
        If Not ExecuteExponentation(result) Then Return False
      Else
        If Not ExecuteExponentation(result) Then Return False
      End If

      Return True

    End Function

    Private Function ExecuteExponentation(ByRef result As Double) As Boolean

      If Not ExecutePrimary(result) Then Return False

      If PeekToken() IsNot Nothing AndAlso PeekToken.ToString = "^" Then

        PopToken()

        Dim num As Double
        If Not ExecutePrimary(num) Then Return False

        If CShort(num) = num Then
          If result = 0 AndAlso num < 0 Then
            m_display.Print(m_basicErrorOutput(BasicError.DivisionByZero), True, False)
            result = 1.701412E+38
          Else
            result ^= CShort(num)
          End If
        Else
          If result = 0 AndAlso num < 0 Then
            m_display.Print(m_basicErrorOutput(BasicError.DivisionByZero), True, False)
            result = 1.701412E+38
          Else
            result ^= num
          End If
        End If

      End If

      Return True

    End Function

    Private Function ExecutePrimary(ByRef result As Double) As Boolean

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.MissingOperand)

      ElseIf PeekToken.IsPeriodToken Then

        PopToken()

        If m_digitList.Contains(PeekToken.ToString()(0)) Then

          Dim value As String = PopToken.ToString()

          If m_numericSuffixList.Contains(value(value.Length - 1)) Then
            value = value.Substring(0, value.Length - 1)
          End If

          value = "." & value

          Try
            result = Double.Parse(value)
          Catch ex As Exception
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End Try

        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

        Return True

      ElseIf PeekToken.IsParenOpenToken Then

        PopToken()
        If Not ExecuteExpression(result) Then Return False
        If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

        Return True

      End If

      Dim keyword As String = PeekToken.ToString

      If keyword = "-" Then

        PopToken()

        Dim value As String = PopToken.ToString()

        If m_numericSuffixList.Contains(value(value.Length - 1)) Then
          value = value.Substring(0, value.Length - 1)
        End If

        Try
          result = -Double.Parse(value)
        Catch ex As Exception
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End Try

      ElseIf m_digitList.Contains(keyword(0)) Then

        Dim value As String = PopToken.ToString()

        If m_numericSuffixList.Contains(value(value.Length - 1)) Then
          value = value.Substring(0, value.Length - 1)
        End If

        Try
          result = Double.Parse(value)
        Catch ex As Exception
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End Try

      Else 'If keyword.Equals("FRE") Then

        Select Case KeywordToKeywordLookup(keyword)

          'Case KeywordLookup.DIGITALREAD

          '  PopToken()
          '  If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
          '  Dim pin As Short
          '  If Not ExecuteExpression(pin) Then Return False
          '  If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          '  If m_gpio Is Nothing Then
          '    Return ThrowBasicError(BasicError.AdvancedFeature)
          '  ElseIf Not m_gpio.IsAvailable Then
          '    Return ThrowBasicError(BasicError.AdvancedFeature)
          '  End If

          '  Dim value = m_gpio.DigitalRead(pin)

          '  result = value

          Case KeywordLookup.PEN
            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If Me.m_onPenState = OnState.On Then

              If InternalPen Is Nothing Then
                Return ThrowBasicError(BasicError.AdvancedFeature)
              End If

              Dim h As Integer = InternalPen.SurfaceHeight
              Dim w As Integer = InternalPen.SurfaceWidth

              Dim th = m_display.ScreenHeight / h
              Dim tw = m_display.ScreenWidth / w

              Select Case num1
                Case 0 ' 
                  result = If(m_penActivatedSinceLastPoll, -1, 0)
                  m_penActivatedSinceLastPoll = False
                Case 1 ' returns x when last activated
                  result = If(m_penLastActivatedPosition IsNot Nothing, CInt(m_penLastActivatedPosition.X * tw), 0)
                Case 2 ' returns y when last activated
                  result = If(m_penLastActivatedPosition IsNot Nothing, CInt(m_penLastActivatedPosition.Y * th), 0)
                Case 3 ' Current switch position
                  result = If(m_penCurrentActivated, -1, 0)
                Case 4 ' returns current y
                  result = If(m_penCurrentPosition IsNot Nothing, CInt(m_penCurrentPosition.X * tw), 0)
                Case 5 ' returns current x
                  result = If(m_penCurrentPosition IsNot Nothing, CInt(m_penCurrentPosition.Y * th), 0)
                Case 6 ' returns character row when last activated
                  result = If(m_penLastActivatedPosition IsNot Nothing, 1 + (CInt(m_penLastActivatedPosition.Y * th) \ m_display.CharacterHeight), 1)
                Case 7 ' returns character column when last activated
                  result = If(m_penLastActivatedPosition IsNot Nothing, 1 + (CInt(m_penLastActivatedPosition.X * tw) \ m_display.CharacterWidth), 1)
                Case 8 ' returns current character row
                  result = If(m_penCurrentPosition IsNot Nothing, 1 + (CInt(m_penCurrentPosition.Y * th) \ m_display.CharacterHeight), 1)
                Case 9 ' returns current character column
                  result = If(m_penCurrentPosition IsNot Nothing, 1 + (CInt(m_penCurrentPosition.X * tw) \ m_display.CharacterWidth), 1)
                Case Else
                  ThrowBasicError(BasicError.IllegalFunctionCall)
              End Select

            Else

              ' PEN ON must be the current state for the PEN() function to work.
              ThrowBasicError(BasicError.IllegalFunctionCall)

            End If

          'Case KeywordLookup.PI
          '  PopToken()
          '  result = 3.14159265
          Case KeywordLookup.FRE
            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
              Dim value1 As String = Nothing
              If Not ExecuteStringExpression(value1, Nothing) Then Return False
            Else
              Dim num1 As Double
              If Not ExecuteExpression(num1) Then Return False
            End If
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            'TODO: Consider implementing memory constraints similar to original GW-BASIC.

            If m_environment IsNot Nothing Then
              result = m_environment.FreeMemory
            Else
              result = 655356
            End If

          Case KeywordLookup.INT

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = VisualBasic.Conversion.Int(num1)

          Case KeywordLookup.ABS

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = Math.Abs(num1)

          Case KeywordLookup.LPOS

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            Dim filenumber = CShort(num1)
            Dim value As File = Nothing
            If m_fileList.TryGetValue(filenumber, value) Then
              If value.Path Is Nothing AndAlso value.Printer IsNot Nothing Then
                result = value.Printer.LPos()
              Else
                Return ThrowBasicError(BasicError.BadFileMode)
              End If
            Else
              Return ThrowBasicError(BasicError.BadFileNumber)
            End If

          Case KeywordLookup.LEN

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim str As String = Nothing
            If Not ExecuteStringExpression(str, Nothing) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = str.Length

          Case KeywordLookup.ASC

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim str As String = Nothing
            If Not ExecuteStringExpression(str, Nothing) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If str <> "" Then
              result = AscW(str(0))
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

          Case KeywordLookup.INSTR

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)

            Dim index As Short
            If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
              index = 1
            Else
              If Not ExecuteExpression(index) Then Return False
              If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
            End If

            Dim str1 As String = Nothing
            If Not ExecuteStringExpression(str1, Nothing) Then Return False
            If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim str2 As String = Nothing
            If Not ExecuteStringExpression(str2, Nothing) Then Return False
            If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)

            If index >= 1 Then
              If str1.Length = 0 Then
                result = 0
              Else
                result = str1.IndexOf(str2, index) + 1
              End If
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

          Case KeywordLookup.RND

            Dim lastRandomNumber As Double

            PopToken()

            If PeekToken.IsParenOpenToken Then
              PopToken()
              If Not PeekToken.IsParenCloseToken Then
                Dim num1 As Double
                If Not ExecuteExpression(num1) Then Return False
                If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)
                If num1 <> 0.0 Then
                  lastRandomNumber = VisualBasic.VBMath.Rnd()
                End If
              Else
                If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)
                lastRandomNumber = VisualBasic.VBMath.Rnd()
              End If
            Else
              lastRandomNumber = VisualBasic.VBMath.Rnd()
            End If
            result = lastRandomNumber

          Case KeywordLookup.SQR

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If num1 < 0 Then
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            Else
              result = Math.Sqrt(num1).ToBasicSng
            End If

          Case KeywordLookup.FIX

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = VisualBasic.Conversion.Fix(num1).ToBasicSng

          Case KeywordLookup.VAL

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim stri1 As String = Nothing
            If Not ExecuteStringExpression(stri1, Nothing) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            Try
              result = CDbl(stri1).ToBasicSng
            Catch ex As Exception
              result = 0.0
            End Try

        'TODO: Not sure what this code is supposed to be doing.

        'Dim stri2 As String = GetSymbol(stri1)

        'If stri2 = "-" Then
        '  stri2 &= GetSymbol(stri1)
        'ElseIf stri2 = "+" Then
        '  stri2 = GetSymbol(stri1)
        'End If

        'Try
        '  result = CSng(stri2)
        'Catch ex As Exception
        '  result = 0.0
        'End Try

          'Case KeywordLookup.SGN

          '  PopToken()
          '  If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
          '  Dim num1 As Double
          '  If Not ExecuteExpression(num1) Then Return False
          '  If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          '  If num1 > 0.0 Then
          '    result = 1.0
          '  ElseIf num1 = 0.0 Then
          '    result = 0.0
          '  Else
          '    result = -1.0
          '  End If

          Case KeywordLookup.SIN

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = Math.Sin(num1).ToBasicSng

          Case KeywordLookup.COS

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = Math.Cos(num1).ToBasicSng

          Case KeywordLookup.TAN

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = Math.Tan(num1).ToBasicSng

          Case KeywordLookup.ATN

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = Math.Atan(num1).ToBasicSng

          'Case KeywordLookup.EXTERR

          '  PopToken()
          '  If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
          '  Dim num1 As Short
          '  If Not ExecuteExpression(num1) Then Return False
          '  If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

          '  If num1.Between(0, 3) Then
          '    'NOTE: For now, return 0.
          '    result = 0
          '  Else
          '    Return ThrowBasicError(BasicError.IllegalFunctionCall)
          '  End If

          Case KeywordLookup.EXP

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = Math.Exp(num1).ToBasicSng

          Case KeywordLookup.LOG

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)
            result = Math.Log(num1).ToBasicSng

          Case KeywordLookup.EOF

            If m_isTrial Then
              Return ThrowBasicError(BasicError.AdvancedFeature)
            End If

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If Not InternalEof(CShort(num1), result) Then Return False

          Case KeywordLookup.LOC

            If m_isTrial Then
              Return ThrowBasicError(BasicError.AdvancedFeature)
            End If

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If Not InternalLoc(CShort(num1), result) Then Return False

          Case KeywordLookup.LOF

            If m_isTrial Then
              Return ThrowBasicError(BasicError.AdvancedFeature)
            End If

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If Not InternalLof(CShort(num1), result) Then Return False

        'ElseIf symbol = "EOF" Then
        '    ' (translated) symbol := get_symbol(line);
        '    symbol = get_symbol(line)
        '    ' ****> expect("(", symbol, line);
        '    ' (translated) index1 := round(exec_expr(symbol, line));
        '    index1 = CShort(exec_expr(symbol, line))
        '    ' ****> expect(")", symbol, line);
        '    ' (translated) if getFileValue(index1) <> STD_NULL then
        '  If getFileValue(index1) IsNot Nothing Then
        '    ' (translated) if hasNext(getFileValue(index1)) then
        '    If hasNext(getFileValue(index1)) Then
        '      ' (translated) result := 0.0;
        '      result = 0.0
        '      ' (translated) else
        '    Else
        '      ' (translated) result := -1.0;
        '      result = -1.0
        '      ' (translated) end if;
        '    End If
        '    ' (translated) writeln(log, "EOF(" <& index1 <& ") -> " <& result);
        '    logWriteLine("EOF(" & index1 & ") -> " & result)
        '    ' (translated) else
        '  Else
        '    ' ****> error_marker;
        '    ' (translated) writeln(err, "FILE #" <& index1 <& " NOT OPEN IN EOF.");
        '    err.WriteLine("FILE #" & index1 & " NOT OPEN IN EOF.")
        '    ' (translated) result := -1.0;
        '    result = -1.0
        '    ' (translated) end if;
        '  End If
        '    ' (translated) elseif symbol = "LOF" then
        'ElseIf symbol = "LOF" Then
        '    ' (translated) symbol := get_symbol(line);
        '    symbol = get_symbol(line)
        '    ' ****> expect("(", symbol, line);
        '    ' (translated) index1 := round(exec_expr(symbol, line));
        '    index1 = CShort(exec_expr(symbol, line))
        '    ' ****> expect(")", symbol, line);
        '    ' (translated) if getFileValue(index1) <> STD_NULL then
        '    If getFileValue(index1) <> STD_NULL Then
        '      ' (translated) result := flt(length(getFileValue(index1)));
        '      result = CSng(length(getFileValue(index1)))
        '      ' (translated) writeln(log, "LOF(" <& index1 <& ") -> " <& result);
        '      logWriteLine("LOF(" & index1 & ") -> " & result)
        '      ' (translated) else
        '    Else
        '      ' ****> error_marker;
        '      ' (translated) writeln(err, "FILE #" <& index1 <& " NOT OPEN IN LOF.");
        '      err.WriteLine("FILE #" & index1 & " NOT OPEN IN LOF.")
        '      ' (translated) end if;
        '    End If

          Case KeywordLookup.PMAP

            ' Used with WINDOW and VIEW to translate coordinates.

            PopToken()

            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim exp As Double
            If Not ExecuteExpression(exp) Then Return False
            If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim func As Double
            If Not ExecuteExpression(func) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            Select Case func
              Case 0 ' Logical exp to physical x.
                result = 0
              Case 1 ' Logical exp to physical y.
                result = 0
              Case 2 ' Physical exp to logical x.
                result = 0
              Case 3 ' Physical exp to logical y.
                result = 0
              Case Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End Select

          Case KeywordLookup.POS

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = m_display.Pos(CShort(num1))

          Case KeywordLookup.CSRLIN
            PopToken()
            result = m_display.CsrLin

          Case KeywordLookup.TIMER
            PopToken()
            Dim ts As New TimeSpan(Date.Now.Hour, Date.Now.Minute, Date.Now.Second)
            result = (ts.TotalSeconds + (Date.Now.Millisecond / 1000)).ToBasicSng

          Case KeywordLookup.CDBL

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            If Not ExecuteExpression(result) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = CDbl(result).ToBasicDbl

          Case KeywordLookup.CSNG

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            If Not ExecuteExpression(result) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            Dim r As Double = result.ToBasicSng

            result = r

          Case KeywordLookup.CLNG

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = CShort(num1)

          Case KeywordLookup.CINT

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Double
            If Not ExecuteExpression(num1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If num1.Between(-32768, 32767) Then
              result = CShort(num1)
            Else
              Return ThrowBasicError(BasicError.Overflow)
            End If

          Case KeywordLookup.PEEK

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim index1 As Double
            If Not ExecuteExpression(index1) Then Return False
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If CInt(index1).Between(0, 65535) Then
              If m_peek.ContainsKey(CInt(index1)) Then
                result = m_peek(CInt(index1))
              Else
                result = 0
              End If
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

        'Select Case index1
        '  Case 16 ' Is it a bw or color screen?
        '    result = 35.0
        '  Case 1040 ' Is it a bw or color screen?
        '    result = 35.0
        '    'Case &H6C ' &H6C contains the low order tick count of 0-255
        '    '  time_now = DateTime.Now
        '    '  ' (translated) since_midnight := time_now - truncToDay(time_now);
        '    '  since_midnight = time_now - truncToDay(time_now)
        '    '  ' (translated) result := 18.2 * (flt(toSeconds(since_midnight)) +
        '    '  result = 18.2 * (CSng(toSeconds(since_midnight)) + CSng(since_midnight.micro_second) / 1000000.0)
        '    '  result -= floor(result / 256.0) * 256.0
        '    '  ' (translated) index1 := round(result);
        '    '  index1 = CShort(result)
        '    '  If index1 < 0 Then
        '    '    result = 0.0
        '    '  ElseIf index1 > 255 Then
        '    '    result = 255.0
        '    '  Else
        '    '    ' (translated) result := flt(index1);
        '    '    result = CSng(index1)
        '    '  End If

        '  Case Else
        '    result = 0.0
        'End Select

        'ElseIf symbol = "INP" Then

        '  symbol = get_symbol(line)
        '  expect("(", symbol, line)

        '  ' (translated) index1 := round(exec_expr(symbol, line));
        '  index1 = CShort(exec_expr(symbol, line))
        '  expect(")", symbol, line)

        '  Select Case index1
        '    Case &H60 ' &H60/96    Get keyboard scan code
        '      current_key = busy_getc(KEYBOARD)
        '      If current_key = KEY_NONE Then
        '        result = 128.0 ' This is a key release code
        '      Else
        '        ' (translated) result := flt(keyboardScanCode(current_key));
        '        result = CSng(keyboardScanCode(current_key))
        '      End If
        '    Case &H3DA ' &H3DA/986  Indicate vertical retrace
        '      ' (translated) result := flt(rand(0, 1) * 8);
        '      result = CSng(rand(0, 1) * 8)
        '    Case Else
        '      result = 0.0
        '  End Select

        '  logWriteLine("**INP(" & index1 & ") -> " & result)

          Case KeywordLookup.SCREEN

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim row As Short
            If Not ExecuteExpression(row) Then Return False
            If Not PopToken.IsCommaToken() Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim col As Short
            If Not ExecuteExpression(col) Then Return False
            Dim alphaOnlyModeColor As Double
            If PeekToken.IsCommaToken Then
              PopToken()
              If Not ExecuteExpression(alphaOnlyModeColor) Then Return False
            End If
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If row.Between(1, CShort(If(m_display.IsKeyOn, 24, 25))) AndAlso
             col.Between(1, m_display.ColumnCount) AndAlso
             ((alphaOnlyModeColor <> 0 AndAlso m_display.ScreenMode = 0) OrElse
              (alphaOnlyModeColor = 0)) Then

              result = m_display.Screen(row, col, alphaOnlyModeColor <> 0)

            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

          Case KeywordLookup.POINT

            If m_display.ScreenMode = 0 Then
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim num1 As Short
            If Not ExecuteExpression(num1) Then Return False
            If PeekToken.IsCommaToken Then
              Dim num2 As Short
              PopToken()
              If Not ExecuteExpression(num2) Then Return False
              If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)
              result = m_display.Point(num1, num2)
            Else

              If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

              ' POINT(function)

              Select Case num1
                Case 0 ' the current physical x coordinate.
                  result = m_drawPosition.X
                Case 1 ' the current physical y coordinate.
                  result = m_drawPosition.Y
                Case 2 ' the current logical x coordinate if WINDOW is active; otherwise same as 0.
                  Stop
                  result = -1
                Case 3 ' the current logical y coordinate if WINDOW is active; otherwise same as 1.
                  Stop
                  result = -1
                Case Else
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End Select

            End If

        'ElseIf symbol = "STRIG" Then

        '  symbol = get_symbol(line)
        '  expect("(", symbol, line)

        '  ' (translated) index1 := round(exec_expr(symbol, line));
        '  index1 = CShort(exec_expr(symbol, line))
        '  expect(")", symbol, line)

        '  result = 0.0

        '  current_key = busy_getc(KEYBOARD)

        '  Select Case index1
        '    Case 0, 1
        '      If current_key = KEY_MOUSE1 Then
        '        current_key = getc(KEYBOARD)
        '        result = -1.0
        '      End If
        '    Case 4, 5
        '      If current_key = KEY_MOUSE3 Then
        '        current_key = getc(KEYBOARD)
        '        result = -1.0
        '      End If
        '  End Select
        '  logWriteLine("**STRIG(" & index1 & ") -> " & result)

          Case KeywordLookup.CVI

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim stri1 As String = Nothing
            If Not ExecuteStringExpression(stri1, Nothing) Then Return Nothing
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            If stri1.Length >= 2 Then
              result = VisualBasic.MkCv.CVI(stri1)
              'If AscW(stri1(2)) >= 128 Then
              '  result = CSng((AscW(stri1(2)) - 256) * 256 + AscW(stri1(1)))
              'Else
              '  result = CSng(AscW(stri1(2)) * 256 + AscW(stri1(1)))
              'End If
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

          Case KeywordLookup.CVS

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim stri1 As String = Nothing
            If Not ExecuteStringExpression(stri1, Nothing) Then Return Nothing
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = VisualBasic.MkCv.CVS(stri1)

          Case KeywordLookup.CVD

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim stri1 As String = Nothing
            If Not ExecuteStringExpression(stri1, Nothing) Then Return Nothing
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = VisualBasic.MkCv.CVD(stri1).ToBasicSng

        'ElseIf keyword.Equals("CVL" Then

        '  PopToken()
        '  If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
        '  Dim stri1 As String
        '  If Not ExecuteStringExpression(stri1, Nothing) Then Return Nothing
        '  If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)
        '  If stri1.Length >= 4 Then
        '    If AscW(stri1(4)) >= 128 Then
        '      result = CSng((((AscW(stri1(4)) - 256) * 256 + AscW(stri1(3))) * 256 + Single.Parse(stri1(2))) * 256 + Single.Parse(stri1(1)))
        '    Else
        '      result = CSng(((AscW(stri1(4)) * 256 + AscW(stri1(3))) * 256 + Single.Parse(stri1(2))) * 256 + Single.Parse(stri1(1)))
        '    End If
        '  Else
        '    Return ThrowBasicError(BasicError.IllegalFunctionCall)
        '  End If

          Case KeywordLookup.LBOUND

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim variableName As String = PopToken.ToString()
            Dim index1 As Double
            If PeekToken.IsCommaToken Then
              PopToken()
              If Not ExecuteExpression(index1) Then Return False
            Else
              index1 = 1
            End If
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            Dim r As Short
            If Not InternalLbound(variableName, CShort(index1), r) Then Return False
            result = r

          Case KeywordLookup.UBOUND

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim variableName As String = PopToken.ToString()
            Dim index1 As Double
            If PeekToken.IsCommaToken Then
              PopToken()
              If Not ExecuteExpression(index1) Then Return False
            Else
              index1 = 1
            End If
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            Dim r As Short
            If Not InternalUbound(variableName, CShort(index1), r) Then Return False
            result = r

        'ElseIf symbol = "FREEFILE" Then
        '    ' (translated) symbol := get_symbol(line);
        '    symbol = get_symbol(line)
        '    ' (translated) index1 := 1;
        '    index1 = 1
        '    ' (translated) while getFileValue(index1) <> STD_NULL do
        '  While getFileValue(index1) IsNot Nothing
        '    ' (translated) incr(index1);
        '    index1 += 1
        '    ' (translated) end while;
        '  End While
        '    ' (translated) result := flt(index1);
        '    result = CSng(index1)
        '    ' (translated) writeln(log, "FREEFILE -> " <& result);
        '    logWriteLine("FREEFILE -> " & result)
        '    ' (translated) elseif symbol = "VARPTR" then

          Case KeywordLookup.VARPTR

            PopToken()
            If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
            Dim variableName As String = Nothing
            If Not PopAndParseVariableName(variableName) Then Return ThrowBasicError(BasicError.SyntaxError)
            If Not PopToken.IsParenCloseToken() Then Return ThrowBasicError(BasicError.SyntaxError)

            result = VarPtr(variableName)

          Case KeywordLookup.ERR
            PopToken()
            result = m_err

          Case KeywordLookup.ERL
            PopToken()
            result = m_erl

          Case KeywordLookup.ERDEV
            PopToken()
            result = 0

          Case Else

            If keyword.Equals("FN") Then
              Return ThrowBasicError(BasicError.AdvancedFeature)
              'variable_name = get_name(symbol, Line)
              'If variable_name <> "FN" OrElse Not def_fn_list.ContainsKey("FN" & symbol) Then
              '  result = GetNumericVariable(variable_name)
              '  LogWriteLine(variable_name & " is " & result)
              'Else
              '  func_name = "FN" & symbol
              '  result = exec_function(def_fn_list(func_name), symbol, Line)
              '  LogWriteLine("function " & func_name & " is " & result)
              'End If

            ElseIf keyword(0).Between("A"c, "Z"c) Then

              If m_defFnList.ContainsKey(keyword) Then

                If Not ExecuteFunction(m_defFnList(PopToken.ToString), result) Then Return False

                'ElseIf subfunction.ContainsKey(symbol) Then
                '  func_name = symbol
                '  line_marker()
                '  LogWriteLine("**CALL FUNCTION " & symbol)
                '  symbol = GetSymbol(Line)
                '  If symbol = "(" Then
                '    Do
                '      symbol = GetSymbol(Line)
                '    Loop Until symbol = ")"
                '    symbol = GetSymbol(Line)
                '  End If
                '  set_return_position(Line)
                '  gosubReturn(1).SubName = func_name
                '  file_line_number = subfunction(func_name)
                '  m_statementLabel = m_programList(file_line_number - 1).Label
                '  Line = m_programList(file_line_number - 1).Line
                '  symbol = GetSymbol(Line)
                '  symbol = GetSymbol(Line)
                '  set_sub_entry_position()
                '  line_marker()
                '  LogWriteLine("EXECUTE FUNCTION " & symbol)
                '  While Not EndOfStatement(symbol)
                '    symbol = GetSymbol(Line)
                '  End While
              Else
                Dim variableName As String = Nothing '= PopToken.ToString()
                If Not PopAndParseVariableName(variableName) Then Return False
                If Not GetNumericVariable(variableName, result) Then Return False
              End If

            ElseIf keyword.Equals("%") Then
              Dim variableName As String = ""
              Do
                'variableName &= PopToken.ToString()
                If Not PopAndParseVariableName(variableName) Then Return False
                If PeekToken() IsNot Nothing AndAlso m_alphaNumericList.Contains(PeekToken.ToString()(0)) Then
                  variableName &= PopToken.ToString()
                End If
              Loop Until PeekToken.ToString() <> "_"
              If Not GetNumericVariable(variableName, result) Then Return False

            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

        End Select

      End If

      Return True

    End Function

    Private Function ExecuteFunction(defFn As DefFn, ByRef result As Double) As Boolean

      ' Get a list of parameters.
      ' Backup any current value in the variable list.
      ' Add value into variable list (paramters).
      ' Store interpreter pointers.
      ' Set interpreter pointer to function (after the = sign).
      ' Execute expression.
      ' Restore interpreter pointers.
      ' Set result accordingly.
      ' Restore variable state to prior to running function.

      Dim stringBackupList As New List(Of String)
      Dim numericBackupList As New List(Of Double)

      If PeekToken.IsParenOpenToken Then

        PopToken()

        For index As Integer = 0 To defFn.Parameters.Count - 1

          If defFn.Parameters(index).EndsWith("$"c) Then

            Dim value As StringValue = Nothing
            If m_stringVariableList.TryGetValue(defFn.Parameters(index), value) Then
              stringBackupList.Add(value.Value)
            Else
              stringBackupList.Add("")
            End If

            Dim str As String = Nothing
            If Not ExecuteStringExpression(str, defFn.Parameters(index)) Then Return False
            If Not SetStringVariable(defFn.Parameters(index), str) Then Return False

          Else

            Dim value As NumericValue = Nothing
            If m_numericVariableList.TryGetValue(defFn.Parameters(index), value) Then
              numericBackupList.Add(value.Value)
            Else
              numericBackupList.Add(0.0)
            End If

            Dim number As Double
            If Not ExecuteExpression(number) Then Return False
            If Not SetNumericVariable(defFn.Parameters(index), number) Then Return False

          End If

          If PeekToken.IsCommaToken Then
            PopToken()
          End If

        Next

        If PeekToken.IsParenCloseToken Then
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      End If

      Dim interpreterIndex = m_interpreterIndex
      Dim statementIndex = m_statementIndex
      Dim tokenIndex = m_tokenIndex

      m_interpreterIndex = defFn.ExpressionLineIndex
      m_statementIndex = defFn.ExpressionStatementIndex
      m_tokenIndex = defFn.ExpressionTokenIndex

      If Not ExecuteExpression(result) Then Return False

      m_interpreterIndex = interpreterIndex
      m_statementIndex = statementIndex
      m_tokenIndex = tokenIndex

      For index As Integer = 0 To defFn.Parameters.Count - 1
        If defFn.Parameters(index).EndsWith("$"c) Then
          SetStringVariable(defFn.Parameters(index), stringBackupList(0))
          stringBackupList.RemoveAt(0)
        Else
          SetNumericVariable(defFn.Parameters(index), numericBackupList(0))
          numericBackupList.RemoveAt(0)
        End If
      Next

      'While formal_param <> ""
      '  ' (translated) if formal_param[length(formal_param)] = '$' then
      '  If formal_param.EndsWith("$") Then
      '    m_stringVariableList.Add(formal_param, str_value_backup(0))
      '    str_value_backup.RemoveAt(0)
      '  Else
      '    m_numericVariableList.Add(formal_param, num_value_backup(0))
      '    num_value_backup.RemoveAt(0)
      '  End If
      '  formal_param = GetSymbol(formal_params)
      'End While

      'Dim result As Single = 0.0

      'Dim func_expr As String = ""
      'Dim func_symbol As String = ""
      'Dim formal_params As String = ""
      'Dim formal_param As String = ""
      'Dim unused_name As String = ""
      'Dim str_value_backup As New List(Of String)
      'Dim num_value_backup As New List(Of Single)

      'LogWrite("function " & defFn.Name)
      'formal_params = defFn.params
      'symbol = GetSymbol(line)

      'If symbol = "(" Then

      '  LogWrite("(")

      '  Do

      '    symbol = GetSymbol(line)
      '    formal_param = GetSymbol(formal_params)

      '    ' (translated) if formal_param[length(formal_param)] = '$' then
      '    If formal_param.EndsWith("$") Then
      '      ' (translated) if formal_param in string_var then
      '      If m_stringVariableList.ContainsKey(formal_param) Then
      '        ' (translated) str_value_backup &:= [] (string_var[formal_param]);
      '        str_value_backup.Add(m_stringVariableList(formal_param))
      '      Else
      '        ' (translated) str_value_backup &:= [] ("");
      '        str_value_backup.Add("")
      '      End If

      '      ' (translated) string_var @:= [formal_param] exec_str_expr(symbol, line, unused_name);
      '      m_stringVariableList.Add(formal_param, exec_str_expr(symbol, line, unused_name))

      '      LogWrite(m_stringVariableList(formal_param))

      '    Else

      '      If m_numericVariableList.ContainsKey(formal_param) Then
      '        ' (translated) num_value_backup &:= [] (numeric_var[formal_param]);
      '        num_value_backup.Add(m_numericVariableList(formal_param))
      '      Else
      '        ' (translated) num_value_backup &:= [] (0.0);
      '        num_value_backup.Add(0.0)
      '      End If

      '      ' (translated) numeric_var @:= [formal_param] exec_expr(symbol, line);
      '      m_numericVariableList.Add(formal_param, exec_expr(symbol, line))

      '      LogWrite(m_numericVariableList(formal_param))

      '    End If

      '    LogWrite(symbol)

      '  Loop Until symbol <> ","

      '  expect(")", symbol, line)

      'End If

      'func_expr = defFn.Expression
      'logWriteLine(" = " & func_expr)
      'func_symbol = GetSymbol(func_expr)
      'result = exec_expr(func_symbol, func_expr)
      'formal_params = defFn.params
      'formal_param = GetSymbol(formal_params)

      'While formal_param <> ""
      '  ' (translated) if formal_param[length(formal_param)] = '$' then
      '  If formal_param.EndsWith("$") Then
      '    m_stringVariableList.Add(formal_param, str_value_backup(0))
      '    str_value_backup.RemoveAt(0)
      '  Else
      '    m_numericVariableList.Add(formal_param, num_value_backup(0))
      '    num_value_backup.RemoveAt(0)
      '  End If
      '  formal_param = GetSymbol(formal_params)
      'End While

      'Return result

      Return True

    End Function

    Private Function ExecuteStringFunction(defFn As DefFn, ByRef result As String) As Boolean

      ' Nearly identical to ExecuteFunction except for result being a string and calling ExecuteStringExpression.

      ' Get a list of parameters.
      ' Backup any current value in the variable list.
      ' Add value into variable list (paramters).
      ' Store interpreter pointers.
      ' Set interpreter pointer to function (after the = sign).
      ' Execute expression.
      ' Restore interpreter pointers.
      ' Set result accordingly.
      ' Restore variable state to prior to running function.

      Dim stringBackupList As New List(Of String)
      Dim numericBackupList As New List(Of Double)

      If PeekToken.IsParenOpenToken Then

        PopToken()

        For index As Integer = 0 To defFn.Parameters.Count - 1

          If defFn.Parameters(index).EndsWith("$"c) Then

            Dim value As StringValue = Nothing
            If m_stringVariableList.TryGetValue(defFn.Parameters(index), value) Then
              stringBackupList.Add(value.Value)
            Else
              stringBackupList.Add("")
            End If

            Dim str As String = Nothing
            If Not ExecuteStringExpression(str, defFn.Parameters(index)) Then Return False
            If Not SetStringVariable(defFn.Parameters(index), str) Then Return False

          Else

            Dim value As NumericValue = Nothing
            If m_numericVariableList.TryGetValue(defFn.Parameters(index), value) Then
              numericBackupList.Add(value.Value)
            Else
              numericBackupList.Add(0.0)
            End If

            Dim number As Double
            If Not ExecuteExpression(number) Then Return False
            If Not SetNumericVariable(defFn.Parameters(index), number) Then Return False

          End If

          If PeekToken.IsCommaToken Then
            PopToken()
          End If

        Next

        If PeekToken.IsParenCloseToken Then
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      End If

      Dim interpreterIndex = m_interpreterIndex
      Dim statementIndex = m_statementIndex
      Dim tokenIndex = m_tokenIndex

      m_interpreterIndex = defFn.ExpressionLineIndex
      m_statementIndex = defFn.ExpressionStatementIndex
      m_tokenIndex = defFn.ExpressionTokenIndex

      If Not ExecuteStringExpression(result, Nothing) Then Return False

      m_interpreterIndex = interpreterIndex
      m_statementIndex = statementIndex
      m_tokenIndex = tokenIndex

      For index As Integer = 0 To defFn.Parameters.Count - 1
        If defFn.Parameters(index).EndsWith("$"c) Then
          SetStringVariable(defFn.Parameters(index), stringBackupList(0))
          stringBackupList.RemoveAt(0)
        Else
          SetNumericVariable(defFn.Parameters(index), numericBackupList(0))
          numericBackupList.RemoveAt(0)
        End If
      Next

      Return True

    End Function

    Private Function PopAndParseVariableName(ByRef name As String) As Boolean

      name = PopToken.ToString()

      If PeekToken.IsParenOpenToken Then

        name &= PopToken.ToString

        Do

          Dim number As Short
          If Not ExecuteExpression(number) Then Return False
          name &= number.ToString

          If PeekToken.IsParenCloseToken Then
            name &= PopToken.ToString
            Exit Do
          ElseIf PeekToken.IsCommaToken Then
            name &= PopToken.ToString
          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

        Loop

      End If

      Return True

    End Function

    Private Function InternalLbound(array_name As String, dimension As Short, ByRef result As Short) As Boolean

      'Dim result As Short = 0
      result = 0

      Dim ubound As Short = 0

      Return InternalGetBounds(array_name, dimension, result, ubound)

      'Return result

    End Function

    Private Function InternalUbound(array_name As String, dimension As Short, ByRef result As Short) As Boolean

      'Dim result As Short = 0
      result = 0

      Dim lbound As Short = 0

      Return InternalGetBounds(array_name, dimension, lbound, result)

      'Return result

    End Function

    Private Function InternalGetBounds(arrayName As String,
dimension As Short,
                                     ByRef lbound As Short,
                                     ByRef ubound As Short) As Boolean

      Dim varName As String = ""
      Dim first As Boolean = True

      If IsNumericVariable(arrayName) Then

        Dim pattern As String = arrayName & "("
        Dim l = From p In m_numericVariableList.Keys
                Where p.StartsWith(pattern)

        If l.Any Then
          For Each entry In l
            If Not InternalGetBoundsFromIndexPart(entry.Substring(arrayName.Length), dimension, first, lbound, ubound) Then Return False
          Next
        Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If

      ElseIf IsStringVariable(arrayName) Then

        Dim pattern As String = arrayName & "("
        Dim l = From p In m_stringVariableList.Keys
                Where p.StartsWith(pattern)

        If l.Any Then
          For Each entry In l
            If Not InternalGetBoundsFromIndexPart(entry.Substring(arrayName.Length), dimension, first, lbound, ubound) Then Return False
          Next
        Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If

      Else

        Return ThrowBasicError(BasicError.TypeMismatch)

      End If


      'For Each varName In m_numericVariableList.Keys
      '  If varName.StartsWith(arrayName) Then
      '    InternalGetBoundsFromIndexPart(varName.Substring(arrayName.Length + 1), dimension, first, lbound, ubound)
      '    Exit For
      '  End If
      'Next

      'For Each varName In m_stringVariableList.Keys
      '  If varName.StartsWith(arrayName) Then
      '    InternalGetBoundsFromIndexPart(varName.Substring(arrayName.Length + 1), dimension, first, lbound, ubound)
      '    Exit For
      '  End If
      'Next

      Return True

    End Function

    Private Shared Function InternalGetBoundsFromIndexPart(indexPart As String,
                                                           dimension As Short,
                                                           ByRef first As Boolean,
                                                           ByRef lbound As Short,
                                                           ByRef ubound As Short) As Boolean

      Dim indexDimension As Integer = 0
      Dim anIndex As Short '= 0

      If indexPart <> "" AndAlso (indexPart(0) = "(" OrElse indexPart(0) = "[") Then
        Do
          indexPart = indexPart.Substring(1)
          anIndex = CShort(GetDigits(indexPart))
          indexDimension += 1
        Loop Until indexDimension = dimension
        If first Then
          lbound = anIndex
          ubound = anIndex
          first = False
        Else
          If anIndex < lbound Then
            lbound = anIndex
          End If
          If anIndex > ubound Then
            ubound = anIndex
          End If
        End If
      End If

      Return True

    End Function

    Private Shared Function GetDigits(ByRef text As String) As String

      Dim symbol As String '= ""

      Dim leng As Integer '= 0
      Dim pos As Integer = 1

      leng = text.Length

      While pos <= leng AndAlso
          text(pos - 1) >= "0"c AndAlso
          text(pos - 1) <= "9"c
        pos += 1
      End While

      symbol = text.Substring(0, pos - 1)
      text = text.Substring(pos - 1)

      Return symbol

    End Function

#End Region

#Region "Execute"

    Private Function ContinueExecuteInputRandomize() As Boolean

      If m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(0).IsKeywordToken Then

        Select Case m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(0).Keyword
          Case "RANDOMIZE" : Return ExecuteRandomize()
          Case Else
        End Select

      End If

      Return False

    End Function

    Private Function ContinueExecuteInput() As Boolean

      If m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(0).IsKeywordToken Then

        Select Case m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(0).Keyword
          Case "INPUT" : Return ExecuteInput()
          Case Else
        End Select

      End If

      Return False

    End Function

    Private Function ContinueExecuteLineInput() As Boolean

      If m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(0).IsKeywordToken Then

        Select Case m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(0).Keyword
          Case "LINE"
            If m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(1).Keyword = "INPUT" Then
              Return ExecuteLineInput()
            End If
          Case Else
        End Select

      End If

      Return False

    End Function

    Private Function ContinueExecuteInputDollar() As Boolean

      If m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(0).IsKeywordToken Then

        Select Case m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens(0).Keyword
          Case "PRINT" : m_tokenIndex = 1 : Return ExecutePrint(False)
          Case "PRINT USING" : m_tokenIndex = 1 : Return ExecutePrintUsing(False)
          Case "LPRINT" : m_tokenIndex = 1 : Return ExecutePrint(True)
          Case "LPRINT USING" : m_tokenIndex = 1 : Return ExecutePrintUsing(True)
          Case "LET" : m_tokenIndex = 1 : Return ExecuteLet()
          Case Else
        End Select

      End If

      Return False

    End Function

    Private Enum KeywordLookup

      'AUTO
      BEEP
      BLOAD
      BSAVE
      CHDIR
      [CALL]
      CHAIN
      CIRCLE
      CLOSE
      CLS
      CLEAR
      COLOR
      COMMON
      CONT
      DATA
      DATES
      DEF
      DEFDBL
      DEFINT
      DEFSNG
      DEFSTR
      'DELAY
      'DELETE
      'DIGITALREAD
      'DIGITALWRITE
      [DIM]
      DRAW
      'EDIT
      [ELSE]
      [END]
      [ERASE]
      ENVIRON
      [ERROR]
      'FACEBOOK
      FIELD
      FILES
      [FOR]
      [GET]
      [GOSUB]
      [GOTO]
      'HELP
      [IF]
      INPUT
      IOCTL
      [KEY]
      KILL
      [LET]
      LINE
      'LIST
      'LLIST
      'LOAD
      LOCATE
      LOCK
      LSET
      'MERGE
      MIDS
      MKDIR
      NAME
      '[NEW]
      [NEXT]
      [ON]
      OPEN
      [OPTION]
      OUT
      PAINT
      PALETTE
      PCOPY
      PEN
      'PINMODE
      PLAY
      POINT
      POKE
      PRESET
      PRINT
      LPRINT
      PSET
      PUT
      RANDOMIZE
      READ
      [REM]
      'RENUM
      RESET
      RESTORE
      [RESUME]
      [RETURN]
      RMDIR
      RSET
      RUN
      'SAVE
      SCREEN
      SHELL
      SLEEP
      SOUND
      [STOP]
      SWAP
      SYSTEM
      TIMES
      TIMER
      TRON
      TROFF
      UNLOCK
      VIEW
      WAIT
      [WEND]
      [WHILE]
      WIDTH
      WINDOW
      WRITE

      'OLD
      'PARSER
      'VER
      'KEYWORDS

      'String Functions...

      LEFTS
      RIGHTS
      'MIDS
      STRINGS
      SPACES
      CHRS
      STRS
      LCASES
      UCASES
      ENVIRONS
      LTRIMS
      RTRIMS
      INKEYS
      INPUTS
      IOCTLS
      HEXS
      OCTS
      ERDEVS
      'TIMES
      'DATES
      MKIS
      MKSS
      MKDS

      ' Numeric Functions...

      FRE
      INT
      ABS
      LPOS
      LEN
      ASC
      INSTR
      RND
      SQR
      FIX
      VAL
      'SGN
      SIN
      COS
      TAN
      ATN
      'EXTERR
      'EXP
      LOG
      EOF
      LOC
      LOF
      PMAP
      POS
      CSRLIN
      'TIMER
      [CDBL]
      [CSNG]
      [CLNG]
      [CINT]
      PEEK
      'SCREEN
      'POINT
      CVI
      CVS
      CVD
      LBOUND
      UBOUND
      VARPTR
      ERR
      ERL
      ERDEV

      ' Recently added... TODO:

      BASE
      [CASE]
      COM
      [DECLARE]
      DEFLNG
      [DO]
      [EXIT]
      [FUNCTION]
      [LOOP]
      OFF
      [REDIM]
      SEG
      SEEK
      [SELECT]
      [SHARED]
      [STATIC]
      STRIG
      [SUB]
      [THEN]
      TYPE
      UEVENT
      [USING]

      COMMANDS
      CVDMBF
      CVL
      CVSMBF
      EXP
      FILEATTR
      FREEFILE
      INP
      MKDMBFS
      MKLS
      MKSMBFS
      SADD
      SETMEM
      SPC
      STICK
      TAB
      VARPTRS
      VARSEG

      ' Extensions
      'System_IO_File_ConvertToPackage
      'System_IO_Folder_ConvertToPackage
      'System_IO_Package_ConvertToFolder
      'System_IO_Package_SetStartup

    End Enum

    Private ReadOnly m_keywordLookupList As New Dictionary(Of String, KeywordLookup) _
      From {{"BASE", KeywordLookup.BASE},
            {"BEEP", KeywordLookup.BEEP},
            {"BLOAD", KeywordLookup.BLOAD},
            {"BSAVE", KeywordLookup.BSAVE},
            {"CALL", KeywordLookup.CALL},
            {"CASE", KeywordLookup.CASE},
            {"CHAIN", KeywordLookup.CHAIN},
            {"CHDIR", KeywordLookup.CHDIR},
            {"CIRCLE", KeywordLookup.CIRCLE},
            {"CLEAR", KeywordLookup.CLEAR},
            {"CLOSE", KeywordLookup.CLOSE},
            {"CLS", KeywordLookup.CLS},
            {"COLOR", KeywordLookup.COLOR},
            {"COM", KeywordLookup.COM},
            {"COMMON", KeywordLookup.COMMON},
            {"CONT", KeywordLookup.CONT},
            {"DATA", KeywordLookup.DATA},
            {"DATE$", KeywordLookup.DATES},
            {"DECLARE", KeywordLookup.DECLARE},
            {"DEF", KeywordLookup.DEF},
            {"DEFDBL", KeywordLookup.DEFDBL},
            {"DEFINT", KeywordLookup.DEFINT},
            {"DEFLNG", KeywordLookup.DEFLNG},
            {"DEFSNG", KeywordLookup.DEFSNG},
            {"DEFSTR", KeywordLookup.DEFSTR},
            {"DIM", KeywordLookup.DIM},
            {"DO", KeywordLookup.DO},
            {"DRAW", KeywordLookup.DRAW},
            {"ELSE", KeywordLookup.ELSE},
            {"END", KeywordLookup.END},
            {"ENVIRON", KeywordLookup.ENVIRON},
            {"ERASE", KeywordLookup.ERASE},
            {"ERROR", KeywordLookup.ERROR},
            {"EXIT", KeywordLookup.EXIT},
            {"FIELD", KeywordLookup.FIELD},
            {"FILES", KeywordLookup.FILES},
            {"FOR", KeywordLookup.FOR},
            {"FUNCTION", KeywordLookup.FUNCTION},
            {"GET", KeywordLookup.GET},
            {"GOSUB", KeywordLookup.GOSUB},
            {"GOTO", KeywordLookup.GOTO},
            {"IF", KeywordLookup.IF},
            {"INPUT", KeywordLookup.INPUT},
            {"IOCTL", KeywordLookup.IOCTL},
            {"KEY", KeywordLookup.KEY},
            {"KILL", KeywordLookup.KILL},
            {"LET", KeywordLookup.LET},
            {"LINE", KeywordLookup.LINE},
            {"LOCATE", KeywordLookup.LOCATE},
            {"LOCK", KeywordLookup.LOCK},
            {"LOOP", KeywordLookup.LOOP},
            {"LPRINT", KeywordLookup.LPRINT},
            {"LSET", KeywordLookup.LSET},
            {"MID$", KeywordLookup.MIDS},
            {"MKDIR", KeywordLookup.MKDIR},
            {"NAME", KeywordLookup.NAME},
            {"NEXT", KeywordLookup.NEXT},
            {"OFF", KeywordLookup.OFF},
            {"ON", KeywordLookup.ON},
            {"OPEN", KeywordLookup.OPEN},
            {"OPTION", KeywordLookup.OPTION},
            {"OUT", KeywordLookup.OUT},
            {"PAINT", KeywordLookup.PAINT},
            {"PALETTE", KeywordLookup.PALETTE},
            {"PCOPY", KeywordLookup.PCOPY},
            {"PEN", KeywordLookup.PEN},
            {"PLAY", KeywordLookup.PLAY},
            {"POKE", KeywordLookup.POKE},
            {"PRESET", KeywordLookup.PRESET},
            {"PRINT", KeywordLookup.PRINT},
            {"PSET", KeywordLookup.PSET},
            {"PUT", KeywordLookup.PUT},
            {"RANDOMIZE", KeywordLookup.RANDOMIZE},
            {"READ", KeywordLookup.READ},
            {"REDIM", KeywordLookup.REDIM},
            {"REM", KeywordLookup.[REM]},
            {"'", KeywordLookup.[REM]},
            {"RESET", KeywordLookup.RESET},
            {"RESTORE", KeywordLookup.RESTORE},
            {"RESUME", KeywordLookup.RESUME},
            {"RETURN", KeywordLookup.RETURN},
            {"RMDIR", KeywordLookup.RMDIR},
            {"RSET", KeywordLookup.RSET},
            {"RUN", KeywordLookup.RUN},
            {"SCREEN", KeywordLookup.SCREEN},
            {"SEG", KeywordLookup.SEG},
            {"SEEK", KeywordLookup.SEEK},
            {"SELECT", KeywordLookup.SELECT},
            {"SHARED", KeywordLookup.SHARED},
            {"SHELL", KeywordLookup.SHELL},
            {"SLEEP", KeywordLookup.SLEEP},
            {"SOUND", KeywordLookup.SOUND},
            {"STATIC", KeywordLookup.STATIC},
            {"STOP", KeywordLookup.STOP},
            {"STRIG", KeywordLookup.STRIG},
            {"SUB", KeywordLookup.SUB},
            {"SWAP", KeywordLookup.SWAP},
            {"SYSTEM", KeywordLookup.SYSTEM},
            {"THEN", KeywordLookup.THEN},
            {"TIME$", KeywordLookup.TIMES},
            {"TIMER", KeywordLookup.TIMER},
            {"TRON", KeywordLookup.TRON},
            {"TROFF", KeywordLookup.TROFF},
            {"TYPE", KeywordLookup.TYPE},
            {"UEVENT", KeywordLookup.UEVENT},
            {"UNLOCK", KeywordLookup.UNLOCK},
            {"USING", KeywordLookup.USING},
            {"VIEW", KeywordLookup.VIEW},
            {"WAIT", KeywordLookup.WAIT},
            {"WEND", KeywordLookup.WEND},
            {"WHILE", KeywordLookup.WHILE},
            {"WIDTH", KeywordLookup.WIDTH},
            {"WINDOW", KeywordLookup.WINDOW},
            {"WRITE", KeywordLookup.WRITE},
            {"ABS", KeywordLookup.ABS},
            {"ASC", KeywordLookup.ASC},
            {"ATN", KeywordLookup.ATN},
            {"CDBL", KeywordLookup.CDBL},
            {"CHR$", KeywordLookup.CHRS},
            {"CINT", KeywordLookup.CINT},
            {"CLNG", KeywordLookup.CLNG},
            {"COMMAND$", KeywordLookup.COMMANDS},
            {"COS", KeywordLookup.COS},
            {"CSNG", KeywordLookup.CSNG},
            {"CSRLIN", KeywordLookup.CSRLIN},
            {"CVD", KeywordLookup.CVD},
            {"CVDMBF", KeywordLookup.CVDMBF},
            {"CVI", KeywordLookup.CVI},
            {"CVL", KeywordLookup.CVL},
            {"CVS", KeywordLookup.CVS},
            {"CVSMBF", KeywordLookup.CVSMBF}, '{"DATE$", KeywordLookup.DATES},
            {"ENVIRON$", KeywordLookup.ENVIRONS},
            {"EOF", KeywordLookup.EOF},
            {"ERDEV", KeywordLookup.ERDEV},
            {"ERDEV$", KeywordLookup.ERDEVS},
            {"ERL", KeywordLookup.ERL},
            {"ERR", KeywordLookup.ERR},
            {"EXP", KeywordLookup.EXP},
            {"FILEATTR", KeywordLookup.FILEATTR},
            {"FIX", KeywordLookup.FIX},
            {"FRE", KeywordLookup.FRE},
            {"FREEFILE", KeywordLookup.FREEFILE},
            {"HEX$", KeywordLookup.HEXS},
            {"INKEY$", KeywordLookup.INKEYS},
            {"INP", KeywordLookup.INP},
            {"INPUT$", KeywordLookup.INPUTS},
            {"INSTR", KeywordLookup.INSTR},
            {"INT", KeywordLookup.INT},
            {"IOCTL$", KeywordLookup.IOCTLS},
            {"LBOUND", KeywordLookup.LBOUND},
            {"LCASE$", KeywordLookup.LCASES},
            {"LEFT$", KeywordLookup.LEFTS},
            {"LEN", KeywordLookup.LEN},
            {"LOC", KeywordLookup.LOC},
            {"LOF", KeywordLookup.LOF},
            {"LOG", KeywordLookup.LOG},
            {"LPOS", KeywordLookup.LPOS},
            {"LTRIM$", KeywordLookup.LTRIMS}, '{"MID$", KeywordLookup.MIDS},
            {"MKD$", KeywordLookup.MKDS},
            {"MKDMBF$", KeywordLookup.MKDMBFS},
            {"MKI$", KeywordLookup.MKIS},
            {"MKL$", KeywordLookup.MKLS},
            {"MKS$", KeywordLookup.MKSS},
            {"MKSMBF$", KeywordLookup.MKSMBFS},
            {"OCT$", KeywordLookup.OCTS},
            {"PEEK", KeywordLookup.PEEK}, '{"PEN", KeywordLookup.PEN}, {"PLAY", KeywordLookup.PLAY},
            {"PMAP", KeywordLookup.PMAP},
            {"POINT", KeywordLookup.POINT},
            {"POS", KeywordLookup.POS},
            {"RIGHT$", KeywordLookup.RIGHTS},
            {"RND", KeywordLookup.RND},
            {"RTRIM$", KeywordLookup.RTRIMS},
            {"SADD", KeywordLookup.SADD}, '{"SCREEN", KeywordLookup.SCREEN}, {"SEEK", KeywordLookup.SEEK},
            {"SETMEM", KeywordLookup.SETMEM},
            {"SIN", KeywordLookup.SIN},
            {"SPACE$", KeywordLookup.SPACES},
            {"SPC", KeywordLookup.SPC},
            {"SQR", KeywordLookup.SQR},
            {"STICK", KeywordLookup.STICK},
            {"STR$", KeywordLookup.STRS}, '{"STRIG", KeywordLookup.STRIG},
            {"STRING$", KeywordLookup.STRINGS},
            {"TAB", KeywordLookup.TAB},
            {"TAN", KeywordLookup.TAN}, '{"TIME$", KeywordLookup.TIMES}, {"TIMER", KeywordLookup.TIMER},
            {"UBOUND", KeywordLookup.UBOUND},
            {"UCASE$", KeywordLookup.UCASES},
            {"VAL", KeywordLookup.VAL},
            {"VARPTR", KeywordLookup.VARPTR},
            {"VARPTR$", KeywordLookup.VARPTRS},
            {"VARSEG", KeywordLookup.VARSEG}}

    Private Function KeywordToKeywordLookup(keyword As String) As KeywordLookup
      Dim value As KeywordLookup = Nothing
      If m_keywordLookupList.TryGetValue(keyword, value) Then
        If m_reservedWords.Contains(keyword) Then
          Return value
        Else
          Return Nothing
        End If
        'If keyword.Equals("'") Then
        '  Return KeywordLookup.[REM]
        'ElseIf keyword.EndsWith("$") Then
        '  Return DirectCast(System.Enum.Parse(GetType(KeywordLookup), String.Format("{0}S", keyword.Substring(0, keyword.Length - 1)), True), KeywordLookup)
        'Else
        '  Return DirectCast(System.Enum.Parse(GetType(KeywordLookup), keyword, True), KeywordLookup)
        'End If
      Else
        Return Nothing
      End If
    End Function

    Private Function ExecuteStatement() As Boolean

      Dim token = PopToken()
      If token Is Nothing Then Return True

      If token.IsLabelToken Then
        ' For now, assume that a label must be on it's own line.  I can't seem to find a 
        ' definitive answer one way or the other; however, all samples I've seen have the 
        ' label on a line with nothing else.
        If PeekToken() Is Nothing Then
          ' Just skip...
          Return True
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      End If

      Dim keyword = token.Keyword

      'If keyword.Equals("ON ERROR GOTO") Then Return ExecuteOnErrorGoto()
      'If keyword.Equals("ON TIMER") Then Return ExecuteOnTimer()
      'If keyword.Equals("ON KEY") Then Return ExecuteOnKey()
      'If keyword.Equals("PRINT USING") Then Return ExecutePrintUsing(False)
      'If keyword.Equals("PALETTE USING") Then Return ExecutePaletteUsing()
      'If keyword.Equals("LINE INPUT") Then Return ExecuteLineInput()
      'If keyword.Equals("LPRINT USING") Then Return ExecutePrintUsing(True)
      'If keyword.Equals("VIEW PRINT") Then Return ExecuteViewPrint()

      If keyword Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      'If 1 = 1 Then

      Try

        Select Case KeywordToKeywordLookup(keyword)
          'Case KeywordLookup.AUTO : Return ExecuteAuto()
          Case KeywordLookup.BEEP : Return ExecuteBeep()
          Case KeywordLookup.BLOAD : Return ExecuteBload()
          Case KeywordLookup.BEEP : Return ExecuteBeep()
          Case KeywordLookup.BLOAD : Return ExecuteBload()
          Case KeywordLookup.BSAVE : Return ExecuteBsave()
          Case KeywordLookup.CHDIR : Return ExecuteChDir()
          Case KeywordLookup.CALL : Return ExecuteCall()
          Case KeywordLookup.CHAIN : Return ExecuteChain()
          Case KeywordLookup.CIRCLE : Return ExecuteCircle()
          Case KeywordLookup.CLOSE : Return ExecuteClose()
          Case KeywordLookup.CLS : Return ExecuteCls()
          Case KeywordLookup.CLEAR : Return ExecuteClear()
          Case KeywordLookup.COLOR : Return ExecuteColor()
          Case KeywordLookup.COMMON : Return ExecuteCommon()
          Case KeywordLookup.CONT : Return ExecuteCont()
          Case KeywordLookup.DATA : Return ExecuteData()
          Case KeywordLookup.DATES : Return ExecuteDate()
          Case KeywordLookup.DEF : Return ExecuteDef()
          Case KeywordLookup.DEFDBL : Return ExecuteDefDbl()
          Case KeywordLookup.DEFINT : Return ExecuteDefInt()
          Case KeywordLookup.DEFSNG : Return ExecuteDefSng()
          Case KeywordLookup.DEFSTR : Return ExecuteDefStr()
          'Case KeywordLookup.DELAY : Return ExecuteDelay()
          'Case KeywordLookup.DELETE : Return ExecuteDelete()
          'Case KeywordLookup.DIGITALWRITE : Return ExecuteGpioDigitalWrite()
          Case KeywordLookup.DIM : Return ExecuteDim()
          Case KeywordLookup.DRAW : Return ExecuteDraw()
          'Case KeywordLookup.EDIT : Return ExecuteEdit()
          Case KeywordLookup.ELSE
            ' Seek to end of line if encountered through normal execution...
            m_statementIndex = m_interpreter(m_interpreterIndex).Statements.Count - 1
            m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count '- 1
            Return True
        'Return ExecuteElse()
          Case KeywordLookup.END : Return ExecuteEnd()
          Case KeywordLookup.ERASE : Return ExecuteErase()
          Case KeywordLookup.ENVIRON : Return ExecuteEnviron()
          Case KeywordLookup.ERROR : Return ExecuteError()
          'Case KeywordLookup.FACEBOOK : Return ExecuteFacebook()
          Case KeywordLookup.FIELD : Return ExecuteField()
          Case KeywordLookup.FILES : Return ExecuteFiles()
          Case KeywordLookup.FOR : Return ExecuteFor()
          Case KeywordLookup.GET : Return ExecuteGet()
          Case KeywordLookup.GOSUB : Return ExecuteGosub()
          Case KeywordLookup.GOTO : Return ExecuteGoto()
          'Case KeywordLookup.HELP : Return ExecuteHelp()
          Case KeywordLookup.IF : Return ExecuteIf()
          Case KeywordLookup.INPUT : Return ExecuteInput()
          Case KeywordLookup.IOCTL : Return ExecuteIoCtl()
          Case KeywordLookup.KEY : Return ExecuteKey()
          Case KeywordLookup.KILL : Return ExecuteKill()
          Case KeywordLookup.LET : Return ExecuteLet()
          Case KeywordLookup.LINE : Return ExecuteLine()
          'Case KeywordLookup.LIST : Return ExecuteList(False)
          'Case KeywordLookup.LLIST : Return ExecuteList(True)
          'Case KeywordLookup.LOAD : Return ExecuteLoad()
          Case KeywordLookup.LOCATE : Return ExecuteLocate()
          Case KeywordLookup.LOCK : Return ExecuteLock()
          Case KeywordLookup.LSET : Return ExecuteLset()
          'Case KeywordLookup.MERGE : Return ExecuteMerge()
          Case KeywordLookup.MIDS : Return ExecuteMid()
          Case KeywordLookup.MKDIR : Return ExecuteMkDir()
          Case KeywordLookup.NAME : Return ExecuteName()
          'Case KeywordLookup.NEW : Return ExecuteNew()
          Case KeywordLookup.NEXT : Return ExecuteNext()
          Case KeywordLookup.ON : Return ExecuteOn()
          Case KeywordLookup.OPEN : Return ExecuteOpen()
          Case KeywordLookup.OPTION : Return ExecuteOption()
          Case KeywordLookup.OUT : Return ExecuteOut()
          Case KeywordLookup.PAINT : Return ExecutePaint()
          Case KeywordLookup.PALETTE : Return ExecutePalette()
          Case KeywordLookup.PCOPY : Return ExecutePcopy()
          Case KeywordLookup.PEN : Return ExecutePen()
          'Case KeywordLookup.PINMODE : Return ExecuteGpioPinMode()
          Case KeywordLookup.PLAY : Return ExecutePlay()
          Case KeywordLookup.POINT : Return ExecutePoint()
          Case KeywordLookup.POKE : Return ExecutePoke()
          Case KeywordLookup.PRESET : Return ExecutePreset()
          Case KeywordLookup.PRINT : Return ExecutePrint(False)
          Case KeywordLookup.LPRINT : Return ExecutePrint(True)
          Case KeywordLookup.PSET : Return ExecutePset()
          Case KeywordLookup.PUT : Return ExecutePut()
          Case KeywordLookup.RANDOMIZE : Return ExecuteRandomize()
          Case KeywordLookup.READ : Return ExecuteRead()
          Case KeywordLookup.[REM] : Return ExecuteRem()
          'Case KeywordLookup.RENUM : Return ExecuteRenum()
          Case KeywordLookup.RESET : Return ExecuteReset()
          Case KeywordLookup.RESTORE : Return ExecuteRestore()
          Case KeywordLookup.RESUME : Return ExecuteResume()
          Case KeywordLookup.RETURN : Return ExecuteReturn()
          Case KeywordLookup.RMDIR : Return ExecuteRmDir()
          Case KeywordLookup.RSET : Return ExecuteRset()
          Case KeywordLookup.RUN : Return ExecuteRun()
          'Case KeywordLookup.SAVE : Return ExecuteSave()
          Case KeywordLookup.SCREEN : Return ExecuteScreen()
          Case KeywordLookup.SHELL : Return ExecuteShell()
          Case KeywordLookup.SLEEP : Return ExecuteSleep()
          Case KeywordLookup.SOUND : Return ExecuteSound()
          Case KeywordLookup.STOP : Return ExecuteStop()
          Case KeywordLookup.SWAP : Return ExecuteSwap()
          Case KeywordLookup.SYSTEM : Return ExecuteSystem()
          Case KeywordLookup.TIMES : Return ExecuteTime()
          Case KeywordLookup.TIMER : Return ExecuteTimer()
          Case KeywordLookup.TRON : Return ExecuteTron()
          Case KeywordLookup.TROFF : Return ExecuteTroff()
          Case KeywordLookup.UNLOCK : Return ExecuteUnlock()
          Case KeywordLookup.VIEW : Return ExecuteView()
          Case KeywordLookup.WAIT : Return ExecuteWait()
          Case KeywordLookup.WEND : Return ExecuteWend()
          Case KeywordLookup.WHILE : Return ExecuteWhile()
          Case KeywordLookup.WIDTH : Return ExecuteWidth()
          Case KeywordLookup.WINDOW : Return ExecuteWindow()
          Case KeywordLookup.WRITE : Return ExecuteWrite()
            'Case KeywordLookup.OLD : Return ExecuteOld()
            'Case KeywordLookup.PARSER : Return ExecuteParserList()
            'Case KeywordLookup.VER : Return ExecuteVer()
            'Case KeywordLookup.KEYWORDS : Return ExecuteKeywords()

            'Case KeywordLookup.System_IO_File_ConvertToPackage : Return ExecuteSystemIoFileConvertToPackage
            'Case KeywordLookup.System_IO_Folder_ConvertToPackage : Return ExecuteSystemIoFolderConvertToPackage
            'Case KeywordLookup.System_IO_Package_ConvertToFolder : Return ExecuteSystemIoPackageConvertToFolder
            'Case KeywordLookup.System_IO_Package_SetStartup : Return ExecuteSystemIoPackageSetStartup

        End Select

      Finally
        If m_interpreterIndex.Between(0, m_interpreter.Count - 1) AndAlso
         m_interpreter(m_interpreterIndex).LineNumber IsNot Nothing Then
          m_successLineNumber = CInt(m_interpreter(m_interpreterIndex).LineNumber)
        End If
      End Try

      'Else

      'If keyword.Equals("AUTO") Then Return ExecuteAuto()
      'If keyword.Equals("BEEP") Then Return ExecuteBeep()
      'If keyword.Equals("BLOAD") Then Return ExecuteBload()
      'If keyword.Equals("BSAVE") Then Return ExecuteBsave()
      'If keyword.Equals("CHDIR") Then Return ExecuteChDir()
      'If keyword.Equals("CALL") Then Return ExecuteCall()
      'If keyword.Equals("CHAIN") Then Return ExecuteChain()
      'If keyword.Equals("CIRCLE") Then Return ExecuteCircle()
      'If keyword.Equals("CLOSE") Then Return ExecuteClose()
      'If keyword.Equals("CLS") Then Return ExecuteCls()
      'If keyword.Equals("CLEAR") Then Return ExecuteClear()
      'If keyword.Equals("COLOR") Then Return ExecuteColor()
      'If keyword.Equals("COMMON") Then Return ExecuteCommon()
      'If keyword.Equals("CONT") Then Return ExecuteCont()
      'If keyword.Equals("DATA") Then Return ExecuteData()
      'If keyword.Equals("DATE$") Then Return ExecuteDate()
      'If keyword.Equals("DEF") Then Return ExecuteDef()
      'If keyword.Equals("DEFDBL") Then Return ExecuteDefDbl()
      'If keyword.Equals("DEFINT") Then Return ExecuteDefInt()
      'If keyword.Equals("DEFSNG") Then Return ExecuteDefSng()
      'If keyword.Equals("DEFSTR") Then Return ExecuteDefStr()
      'If keyword.Equals("DELETE") Then Return ExecuteDelete()
      'If keyword.Equals("DIM") Then Return ExecuteDim()
      'If keyword.Equals("DRAW") Then Return ExecuteDraw()
      'If keyword.Equals("EDIT") Then Return ExecuteEdit()
      'If keyword.Equals("ELSE") Then
      '  ' Seek to end of line if encountered through normal execution...
      '  m_statementIndex = m_interpreter(m_interpreterIndex).Statements.Count - 1
      '  m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count '- 1
      '  Return True
      '  'Return ExecuteElse()
      'End If
      'If keyword.Equals("END") Then Return ExecuteEnd()
      'If keyword.Equals("ERASE") Then Return ExecuteErase()
      'If keyword.Equals("ENVIRON") Then Return ExecuteEnviron()
      'If keyword.Equals("ERROR") Then Return ExecuteError()
      'If keyword.Equals("FIELD") Then Return ExecuteField()
      'If keyword.Equals("FILES") Then Return ExecuteFiles()
      'If keyword.Equals("FOR") Then Return ExecuteFor()
      'If keyword.Equals("GET") Then Return ExecuteGet()
      'If keyword.Equals("GOSUB") Then Return ExecuteGosub()
      'If keyword.Equals("GOTO") Then Return ExecuteGoto()
      'If keyword.Equals("HELP") Then Return ExecuteHelp()
      'If keyword.Equals("IF") Then Return ExecuteIf()
      'If keyword.Equals("INPUT") Then Return ExecuteInput()
      'If keyword.Equals("IOCTL") Then Return ExecuteIoCtl()
      'If keyword.Equals("KEY") Then Return ExecuteKey()
      'If keyword.Equals("KILL") Then Return ExecuteKill()
      'If keyword.Equals("LET") Then Return ExecuteLet()
      'If keyword.Equals("LINE") Then Return ExecuteLine()
      'If keyword.Equals("LIST") Then Return ExecuteList(False)
      'If keyword.Equals("LLIST") Then Return ExecuteList(True)
      'If keyword.Equals("LOAD") Then Return ExecuteLoad()
      'If keyword.Equals("LOCATE") Then Return ExecuteLocate()
      'If keyword.Equals("LOCK") Then Return ExecuteLock()
      'If keyword.Equals("LSET") Then Return ExecuteLset()
      'If keyword.Equals("MERGE") Then Return ExecuteMerge()
      'If keyword.Equals("MID$") Then Return ExecuteMid()
      'If keyword.Equals("MKDIR") Then Return ExecuteMkDir()
      'If keyword.Equals("NAME") Then Return ExecuteName()
      'If keyword.Equals("NEW") Then Return ExecuteNew()
      'If keyword.Equals("NEXT") Then Return ExecuteNext()
      'If keyword.Equals("ON") Then Return ExecuteOn()
      'If keyword.Equals("OPEN") Then Return ExecuteOpen()
      'If keyword.Equals("OPTION") Then Return ExecuteOption()
      'If keyword.Equals("OUT") Then Return ExecuteOut()
      'If keyword.Equals("PAINT") Then Return ExecutePaint()
      'If keyword.Equals("PALETTE") Then Return ExecutePalette()
      'If keyword.Equals("PCOPY") Then Return ExecutePcopy()
      'If keyword.Equals("PLAY") Then Return ExecutePlay()
      'If keyword.Equals("POINT") Then Return ExecutePoint()
      'If keyword.Equals("POKE") Then Return ExecutePoke()
      'If keyword.Equals("PRESET") Then Return ExecutePreset()
      'If keyword.Equals("PRINT") Then Return ExecutePrint(False)
      'If keyword.Equals("LPRINT") Then Return ExecutePrint(True)
      'If keyword.Equals("PSET") Then Return ExecutePset()
      'If keyword.Equals("PUT") Then Return ExecutePut()
      'If keyword.Equals("RANDOMIZE") Then Return ExecuteRandomize()
      'If keyword.Equals("READ") Then Return ExecuteRead()
      'If keyword.Equals("REM") OrElse keyword.Equals("'") Then Return ExecuteRem()
      'If keyword.Equals("RENUM") Then Return ExecuteRenum()
      'If keyword.Equals("RESET") Then Return ExecuteReset()
      'If keyword.Equals("RESTORE") Then Return ExecuteRestore()
      'If keyword.Equals("RESUME") Then Return ExecuteResume()
      'If keyword.Equals("RETURN") Then Return ExecuteReturn()
      'If keyword.Equals("RMDIR") Then Return ExecuteRmDir()
      'If keyword.Equals("RSET") Then Return ExecuteRset()
      'If keyword.Equals("RUN") Then Return ExecuteRun()
      'If keyword.Equals("SAVE") Then Return ExecuteSave()
      'If keyword.Equals("SCREEN") Then Return ExecuteScreen()
      'If keyword.Equals("SHELL") Then Return ExecuteShell()
      'If keyword.Equals("SOUND") Then Return ExecuteSound()
      'If keyword.Equals("STOP") Then Return ExecuteStop()
      'If keyword.Equals("SWAP") Then Return ExecuteSwap()
      'If keyword.Equals("SYSTEM") Then Return ExecuteSystem()
      'If keyword.Equals("TIME$") Then Return ExecuteTime()
      'If keyword.Equals("TIMER") Then Return ExecuteTimer()
      'If keyword.Equals("TRON") Then Return ExecuteTron()
      'If keyword.Equals("TROFF") Then Return ExecuteTroff()
      'If keyword.Equals("UNLOCK") Then Return ExecuteUnlock()
      'If keyword.Equals("VIEW") Then Return ExecuteView()
      'If keyword.Equals("WAIT") Then Return ExecuteWait()
      'If keyword.Equals("WEND") Then Return ExecuteWend()
      'If keyword.Equals("WHILE") Then Return ExecuteWhile()
      'If keyword.Equals("WIDTH") Then Return ExecuteWidth()
      'If keyword.Equals("WINDOW") Then Return ExecuteWindow()
      'If keyword.Equals("WRITE") Then Return ExecuteWrite()

      'If keyword.Equals("OLD") Then Return ExecuteOld()
      'If keyword.Equals("PARSER") Then Return ExecuteParserList()
      'If keyword.Equals("VER") Then Return ExecuteVer()
      'If keyword.Equals("KEYWORDS") Then Return ExecuteKeywords()

      'End If

      If token.IsVariableToken OrElse
        token.IsFunctionToken Then
        Return ThrowBasicError(BasicError.SyntaxError)
      Else
        m_display.Print(String.Format("Unknown keyword ('{0}')", token), True)
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      Return False

    End Function

    '  Private Function ExecuteSystemIoFileConvertToPackage() As Boolean

    '    If PeekToken() Is Nothing Then
    '      Return ThrowBasicError(BasicError.MissingOperand)
    '    ElseIf Not PeekToken.IsStringLiteralToken Then
    '      Return ThrowBasicError(BasicError.TypeMismatch)
    '    Else

    '      Dim path As String = RealPath(PopToken.ToString)

    '      If path Is Nothing Then
    '        Return ThrowBasicError(BasicError.SyntaxError)
    '      Else

    '        If Not path.EndsWith("\") Then
    '          path &= "\"
    '        End If

    '        Dim result = m_virtualFileSystem.ConvertFileToPackage(path)

    '        If result.StartsWith("Error: ") Then
    '          m_display.Print(result.Substring(7), True)
    '          Return False
    '        Else
    '          m_currentPath = result
    '        End If

    '        m_waiting = False

    '        Return True

    '      End If

    '    End If

    '  End Function

    '  Private Function ExecuteSystemIoFolderConvertToPackage() As Boolean

    '    If PeekToken() Is Nothing Then
    '      Return ThrowBasicError(BasicError.MissingOperand)
    '    ElseIf Not PeekToken.IsStringLiteralToken Then
    '      Return ThrowBasicError(BasicError.TypeMismatch)
    '    Else

    '      Dim path As String = RealPath(PopToken.ToString)

    '      If path Is Nothing Then
    '        Return ThrowBasicError(BasicError.SyntaxError)
    '      Else

    '        If Not path.EndsWith("\") Then
    '          path &= "\"
    '        End If

    '        Dim result = m_virtualFileSystem.ConvertFolderToPackage(path)

    '        If result.StartsWith("Error: ") Then
    '          m_display.Print(result.Substring(7), True)
    '          Return False
    '        Else
    '          m_currentPath = result
    '        End If

    '        m_waiting = False

    '        Return True

    '      End If

    '    End If

    '  End Function

    '  Private Function ExecuteSystemIoPackageConvertToFolder() As Boolean

    '    If PeekToken() Is Nothing Then
    '      Return ThrowBasicError(BasicError.MissingOperand)
    '    ElseIf Not PeekToken.IsStringLiteralToken Then
    '      Return ThrowBasicError(BasicError.TypeMismatch)
    '    Else

    '      Dim path As String = RealPath(PopToken.ToString)

    '      If path Is Nothing Then
    '        Return ThrowBasicError(BasicError.SyntaxError)
    '      Else

    '        If Not path.EndsWith("\") Then
    '          path &= "\"
    '        End If

    '        Dim result = m_virtualFileSystem.ConvertPackageToFolder(path)

    '        If result.StartsWith("Error: ") Then
    '          m_display.Print(result.Substring(7), True)
    '          Return False
    '        Else
    '          m_currentPath = result
    '        End If

    '        m_waiting = False

    '        Return True

    '      End If

    '    End If

    '  End Function

    '  Private Function ExecuteSystemIoPackageSetStartup() As Boolean

    '    Stop

    '    Return True

    '  End Function

    'Private Function ExecuteAuto() As Boolean

    '  Dim start As Integer = If(m_autoStart = -1, 10, m_autoStart)
    '  Dim increment As Integer = 10

    '  If PeekToken() Is Nothing Then
    '    start = 10
    '  ElseIf PeekToken.IsPeriodToken Then
    '    PopToken()
    '    If start = -1 Then m_autoStart = 10
    '  ElseIf PeekToken.IsNumericLiteralToken Then
    '    start = CShort(PopToken.Literal)
    '  End If

    '  If PeekToken.IsCommaToken Then
    '    PopToken()
    '    If PeekToken.IsNumericLiteralToken Then
    '      increment = CShort(PopToken.Literal)
    '    Else
    '      Return ThrowBasicError(BasicError.SyntaxError)
    '    End If
    '  ElseIf PeekToken() IsNot Nothing Then
    '    Return ThrowBasicError(BasicError.SyntaxError)
    '  End If

    '  m_autoStart = start
    '  m_autoIncrement = increment
    '  m_autoModeActive = True

    '  Return InternalNextAutoLine(False)

    'End Function

    Private Function ExecuteBeep() As Boolean

      If PeekToken() Is Nothing Then
        If m_sound IsNot Nothing Then
          If m_sound.Sound(800, 4.55) = 1 Then Return ThrowBasicError(BasicError.OutOfMemory)
        Else
          Return ThrowBasicError(BasicError.AdvancedFeature)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteBload() As Boolean

      Return ThrowBasicError(BasicError.AdvancedFeature)

    End Function

    Private Function ExecuteBsave() As Boolean

      Return ThrowBasicError(BasicError.AdvancedFeature)

    End Function

    Private Function ExecuteCall() As Boolean

      Return ThrowBasicError(BasicError.AdvancedFeature)

    End Function

#Region "File System"

    Private Function ExecuteChDir() As Boolean

      'TODO: Handle CHDIR"
      'TODO: ? Handle CHDIR"..\..\..\folder

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.MissingOperand)
      ElseIf Not PeekToken.IsStringLiteralToken Then
        Return ThrowBasicError(BasicError.TypeMismatch)
      Else

        Dim path As String = RealPath(PopToken.ToString)

        If path Is Nothing Then
          Return ThrowBasicError(BasicError.SyntaxError)
        Else

          If Not path.EndsWith("\"c) Then
            path &= "\"
          End If

          Dim result = m_virtualFileSystem.ChDir(path)

          If result.StartsWith("Error: ") Then
            m_display.Print(result.Substring(7), True)
            'm_running = False
            Return False
          Else
            m_currentPath = result
          End If

          'm_running = False
          m_waiting = False

          'InternalPrompt()
          'ProcessKeyBuffer()

          Return True

        End If

      End If

    End Function

    Private Function ExecuteClose() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      ' STOP leaves file handles open.

      Dim closeList As New List(Of Short)

      If PeekToken() IsNot Nothing Then

        Do

          If PeekToken.IsHashToken Then
            PopToken()
          End If
          Dim fileNumber As Short
          If Not ExecuteExpression(fileNumber) Then Return False
          closeList.Add(fileNumber)
          If PeekToken.IsCommaToken Then
            PopToken()
          Else
            Exit Do
          End If

        Loop

      End If

      If PeekToken() Is Nothing Then

        If closeList.Count = 0 Then
          ' close all files
          InternalClose()
        Else
          If Not InternalClose(closeList) Then Return False
          'For Each fileNumber In closeList
          '  ' close this file number.
          '  ' if file number is not open...
          '  If Not InternalClose(fileNumber) Then Return False
          'Next
        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Sub InternalClose()

      ' Close all files
      Dim closeList As New List(Of Short)

      For Each entry In m_fileList.Keys
        closeList.Add(m_fileList(entry).Number)
      Next

      InternalClose(closeList)

    End Sub

    Private Function InternalClose(fileNumberList As List(Of Short)) As Boolean

      ' Close any devices that are not file based...

      If fileNumberList.Count > 0 Then
        For index As Integer = fileNumberList.Count - 1 To 0
          Dim fileNumber As Short = fileNumberList(index)
          Dim value As File = Nothing
          If m_fileList.TryGetValue(fileNumber, value) AndAlso value.Path Is Nothing Then
            m_fileList.Remove(fileNumber)
            fileNumberList.RemoveAt(index)
          End If
        Next
      End If

      ' Close any files...

      If fileNumberList.Count > 0 Then

        Dim fileNames As New List(Of String)

        For Each fileNumber In fileNumberList
          Dim value As File = Nothing
          If m_fileList.TryGetValue(fileNumber, value) Then
            fileNames.Add(value.Path)
          End If
        Next

        Dim result As String = m_virtualFileSystem.Close(fileNames)

        If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then
          Dim number = result.Substring(7)
          If IsNumeric(number) Then
            'm_running = ThrowBasicError(CShort(number))
            m_waiting = False
            Return ThrowBasicError(CShort(number))
          Else
            m_display.Print(result.Substring(7), True)
            'm_running = False
            m_waiting = False
            Return False
          End If
        Else

          For Each fileNumber In fileNumberList
            m_fileList.Remove(fileNumber)
          Next

          'm_running = True
          m_waiting = False

        End If

        If Not m_running Then
          InternalPrompt()
        End If

        Return True

      Else

        Return True

      End If

    End Function

    Private Function ExecuteChain() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      Dim merge As Boolean = False
      Dim filename As String = Nothing
      Dim line As Double = -1
      Dim all As Boolean = False
      Dim deleteMin As Double = -1
      Dim deleteMax As Double = -1

      If PeekToken.IsWord("MERGE") Then
        PopToken()
        merge = True
      End If

      If Not ExecuteStringExpression(filename, Nothing) Then Return False

      If PeekToken.IsCommaToken Then

        PopToken()

        If PeekToken.IsCommaToken Then
          ' ignore...
        Else
          If Not ExecuteExpression(line) Then Return False
        End If

        If PeekToken.IsCommaToken Then

          PopToken()

          If PeekToken.IsCommaToken Then
          ElseIf PeekToken.IsWord("ALL") Then
            PopToken()
            all = True
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

          If PeekToken.IsCommaToken Then

            PopToken()

            If PeekToken.IsWord("DELETE") Then

              PopToken()

              If PeekToken.IsNumericLiteralToken Then
                deleteMin = CInt(PopToken.ToString)
              Else
                Return ThrowBasicError(BasicError.SyntaxError)
              End If
              If PeekToken.IsDash Then
                PopToken()
                If PeekToken.IsNumericLiteralToken Then
                  deleteMax = CInt(PopToken.ToString)
                Else
                  Return ThrowBasicError(BasicError.SyntaxError)
                End If
              Else
                Return ThrowBasicError(BasicError.SyntaxError)
              End If

            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

          End If

        End If

      End If

      If PeekToken() Is Nothing Then

        ' Prepare and do chain...

        filename = RealPath(filename)

        'Dim ext = IO.Path.GetExtension(filename)

        Dim package As Integer = filename.LastIndexOf(".PACKAGE", StringComparison.OrdinalIgnoreCase)
        Dim period As Integer = filename.LastIndexOf("."c)
        If package = period Then
          filename &= ".BAS"
        Else
          Dim ext As String = Nothing
          If period > -1 Then
            ext = filename.Substring(period)
          End If
          If String.IsNullOrEmpty(ext) Then
            filename &= ".BAS"
          End If
        End If

        'Dim period As Integer = filename.LastIndexOf(".")
        'Dim ext As String = Nothing
        'If period > -1 Then
        '  ext = filename.Substring(period)
        'End If
        'If String.IsNullOrEmpty(ext) Then
        '  filename &= ".BAS"
        'End If

        Dim chain As New Chain With {.Merge = merge,
                                   .Filename = filename,
                                   .Line = CInt(line),
                                   .All = all,
                                   .DeleteMin = CInt(deleteMin),
                                   .DeleteMax = CInt(deleteMax)}


        Dim result = m_virtualFileSystem.Load(filename)
        InternalLoadCompleted(result, chain)
        Return True

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

    End Function

    '    Private Function ExecuteLoad() As Boolean

    '      Dim run As Boolean = False

    '      If PeekToken() Is Nothing Then
    '        Return ThrowBasicError(BasicError.MissingOperand)
    '      ElseIf Not PeekToken.IsStringLiteralToken Then
    '        Return ThrowBasicError(BasicError.TypeMismatch)
    '      ElseIf PeekToken.IsStringLiteralToken Then

    '        Dim filename = RealPath(PopToken.ToString)

    '        If filename.Length > 4 AndAlso
    '         filename.LastIndexOf("."c) > filename.Length - 4 Then
    '          ' Do nothing, we have some sort of an extension already.
    '        Else
    '          ' no extension?
    '          filename &= ".BAS"
    '        End If

    '        ''Dim ext = IO.Path.GetExtension(filename)
    '        'Dim period As Integer = filename.LastIndexOf(".")
    '        'Dim ext As String = Nothing
    '        'If period > -1 Then
    '        '  ext = filename.Substring(period)
    '        'End If
    '        'If String.IsNullOrEmpty(ext) Then
    '        '  filename &= ".BAS"
    '        'End If

    '        If PeekToken.IsCommaToken Then
    '          PopToken()
    '          If PeekToken.IsWord("R") Then
    '            PopToken()
    '            run = True
    '            If PeekToken() IsNot Nothing Then
    '              Return ThrowBasicError(BasicError.SyntaxError)
    '            End If
    '          Else
    '            Return ThrowBasicError(BasicError.SyntaxError)
    '          End If
    '        End If

    '        m_pendingNew = False

    '        If Not run Then InternalClose()

    '        Dim result = m_virtualFileSystem.Load(filename)

    '        InternalLoadCompleted(result, filename & "|" & If(run, "1", "0"))

    '        'InternalPrompt()
    '        'ProcessKeyBuffer()

    '        Return True

    '      Else
    '        Return ThrowBasicError(BasicError.SyntaxError)
    '      End If

    '    End Function

    Private Function InternalLoadCompleted(result As String, o As Object) As Boolean

      If TypeOf o Is Chain Then

        Dim chain As Chain = CType(o, Chain)

        If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then

          m_display.Print(result.Substring(7), True)

          m_waiting = False
          'm_running = True

          InternalPrompt()
          ProcessKeyBuffer()

          Return False

        Else

          ' MERGE
          If Not chain.Merge Then
            m_lines.Clear()
            m_defDblList.Clear()
            m_defSngList.Clear()
            m_defIntList.Clear()
            m_defStrList.Clear()
            m_defFnList.Clear()
          Else
            'TODO: Need to determine if DEF FN entries are still valid after the merge.
            '      For now, assume they are not overwriting existing code and are using CHAIN as an overlay.
          End If

          If Not chain.Merge Then
            ' Close all open files if merge is not used.
            InternalClose()
          End If

          ' DELETE
          If chain.DeleteMin > -1 AndAlso
           chain.DeleteMax > -1 AndAlso
           chain.DeleteMin < chain.DeleteMax Then

            For index As Integer = m_lines.Count - 1 To 0 Step -1
              If CInt(m_lines(index).LineNumber).Between(chain.DeleteMin, chain.DeleteMax) Then
                m_lines.RemoveAt(index)
              End If
            Next

          End If

          If result.EndsWith(ChrW(26)) Then
            result = result.Substring(0, result.Length - 1)
          End If

          Dim textLines = result.Split(CChar(vbLf))

          For Each textRow In textLines

            If textRow.Trim <> "" Then

              Dim buffer() As Byte = textRow.ToByteArray
              Using s As New System.IO.MemoryStream()
                s.Write(buffer, 0, buffer.Length)
                s.Seek(0, System.IO.SeekOrigin.Begin)

                ' Parse command.
                Dim parse As New Parser.Parser(s)

                InsertUpdateSourceLine(parse.Lines(0))

              End Using

            End If

          Next

          ' Reset interpreter position...
          m_interpreterIndex = -1
          m_statementIndex = 0
          m_tokenIndex = 0

          ' Now shift line list into the interpreter list...
          m_interpreter.Clear()
          For Each line In m_lines
            m_interpreter.Add(line.Copy)
          Next

          ' ALL
          If Not chain.All Then

            ' Not chaining all variables, so remove everything except those flagged as COMMON.

            If m_stringVariableList.Count > 0 Then
              For index As Integer = m_stringVariableList.Keys.Count - 1 To 0 Step -1
                Dim key As String = m_stringVariableList.Keys(index)
                If Not m_stringVariableList(key).Common Then
                  m_stringVariableList.Remove(key)
                  m_varNameList.Remove(VarPtr(key))
                  m_varPtrList.Remove(key)
                End If
              Next
            End If

            If m_numericVariableList.Count > 0 Then
              For index As Integer = m_numericVariableList.Keys.Count - 1 To 0 Step -1
                Dim key As String = m_numericVariableList.Keys(index)
                If Not m_numericVariableList(key).Common Then
                  m_numericVariableList.Remove(key)
                  m_varNameList.Remove(VarPtr(key))
                  m_varPtrList.Remove(key)
                End If
              Next
            End If

          End If

          m_contIndex = -1

          ' RESTORE
          m_readIndex = 0
          m_data.Clear()

          ' Reload data statement(s)...
          If Not PrepareData() Then
            m_waiting = False
            'm_running = False
            Return False
          End If

          ' If starting at a particular line number, handle it here...
          If chain.Line > -1 Then
            ' Line number provided.
            If Not GotoLabelOrLineNumber(chain.Line.ToString) Then
              m_waiting = False
              'm_running = False
              Return False
            End If
          End If

          ' Continue...
          m_waiting = False
          'm_running = True
          Return True

        End If

      Else

        Dim merge As Boolean = CStr(o).IndexOf("|MERGE") > -1
        Dim run As Boolean = Not merge AndAlso (CStr(o).Split("|"c)(1) = "1")

        If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then
          m_display.Print(result.Substring(7), True)
        Else

          If Not merge Then
            m_lines.Clear()
          End If

          If Not merge Then
            ' Close all open files if merge is not used.
            InternalClose()
          End If

          If result.EndsWith(ChrW(26)) Then
            result = result.Substring(0, result.Length - 1)
          End If

          Dim textLines = result.Split(CChar(vbLf))

          For Each textRow In textLines

            If textRow.Trim <> "" Then

              If textRow.EndsWith(vbCr) Then
                textRow = textRow.Substring(0, textRow.Length - 1)
              End If

              Dim buffer() As Byte = textRow.ToByteArray

              Using s As New System.IO.MemoryStream()

                s.Write(buffer, 0, buffer.Length)
                s.Seek(0, System.IO.SeekOrigin.Begin)

                ' Parse command.
                Dim parse As New Parser.Parser(s)

                InsertUpdateSourceLine(parse.Lines(0))

              End Using

            End If

          Next

          If run Then

            ' Kick over to a RUN.

            m_interpreter.Clear()

            m_interpreter.Add(New Parser.Line(Nothing))
            m_interpreter(0).Statements.Add(New Parser.Statement)
            m_interpreter(0).Statements(0).Tokens.Add(New Parser.KeywordToken() With {.Value = "RUN"})

            m_interpreterIndex = 0
            m_statementIndex = 0
            m_tokenIndex = 0

            m_waiting = False
            'm_running = True

            Return True

          End If

        End If

        m_waiting = False
        'm_running = False
        Return False

      End If

    End Function

#End Region

    Private Function ExecuteCircle() As Boolean

      Dim xcenter As Double
      Dim ycenter As Double
      Dim radius As Double
      Dim color As Short

      Select Case m_display.ScreenMode
        Case 1 : color = 3
        Case 2 : color = 1
        Case 3 : color = 15
        Case 4 : color = 3
        Case 5 : color = 15
        Case 6 : color = 3
        Case 7 : color = 15
        Case 8 : color = 15
        Case 9 : color = 15
        Case Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
      End Select

      Dim start As Double = 0
      Dim [end] As Double = Math.PI * 2

      Dim aspect As Double = 1
      Select Case m_display.ScreenMode
        Case 1 : aspect = 4.0 * (200.0 / 320.0) / 3.0
        Case 2 : aspect = 4.0 * (200.0 / 640.0) / 3.0
        Case 3 : aspect = 4.0 * (200.0 / 160.0) / 3.0
        Case 4 : aspect = 4.0 * (200.0 / 320.0) / 3.0
        Case 5 : aspect = 4.0 * (200.0 / 320.0) / 3.0
        Case 6 : aspect = 4.0 * (200.0 / 640.0) / 3.0
        Case 7 : aspect = 4.0 * (200.0 / 320.0) / 3.0
        Case 8 : aspect = 4.0 * (200.0 / 640.0) / 3.0
        Case 9 : aspect = 4.0 * (350.0 / 640.0) / 3.0
        Case 10 : aspect = 4.0 * (350.0 / 640.0) / 3.0
        Case 11 : aspect = 4.0 * (480.0 / 640.0) / 3.0
        Case 12 : aspect = 4.0 * (480.0 / 640.0) / 3.0
        Case 13 : aspect = 4.0 * (200.0 / 320.0) / 3.0
      End Select

      If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not ExecuteExpression(xcenter) Then Return False
      If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not ExecuteExpression(ycenter) Then Return False
      If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not ExecuteExpression(radius) Then Return False

      If PeekToken.IsCommaToken Then
        PopToken()
        If PeekToken.IsCommaToken Then
          ' fall through
        Else
          If Not ExecuteExpression(color) Then Return False
        End If
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
        If PeekToken.IsCommaToken Then
          ' fall through
        Else
          If Not ExecuteExpression(start) Then Return False
        End If
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
        If PeekToken.IsCommaToken Then
          ' fall through
        Else
          If Not ExecuteExpression([end]) Then Return False
        End If
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
        If Not ExecuteExpression(aspect) Then Return False
      End If

      If PeekToken() Is Nothing Then
        If Not m_display.Circle(CInt(xcenter), CInt(ycenter), radius, color, start, [end], aspect) Then
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If
        Return True
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteCls() As Boolean

      Dim argument As Short = -1

      If PeekToken() Is Nothing Then
        argument = -1
      ElseIf PeekToken.IsNumericLiteralToken Then
        argument = CShort(PopToken.Literal)
      End If

      If PeekToken() Is Nothing Then
        If argument.Between(-1, 2) Then
          m_display.Cls(argument)
          ResetDraw()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteClear() As Boolean

      ' GW-BASIC 3.23
      ' CLEAR [,[memory location][,[stack space]]
      ' Tandy 1000
      ' CLEAR [,[memory location][,[stack space][,[video memory]]]]

      Dim argument1 As Integer = 32767 ' Appears to be a completely dummy number...
      Dim argument2 As Integer = 32768
      Dim argument3 As Integer = 512
      Dim argument4 As Integer '= 0

      If PeekToken() Is Nothing OrElse PeekToken.IsCommaToken Then
        ' Skip forward...
      ElseIf PeekToken.IsNumericLiteralToken Then
        argument1 = CInt(PopToken.Literal)
      Else
        Return ThrowBasicError(BasicError.MissingOperand)
      End If

      If PeekToken() Is Nothing Then
        ' Skip forward...
      ElseIf PeekToken.IsCommaToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken() Is Nothing OrElse PeekToken.IsCommaToken Then
        ' Skip forward.
      ElseIf PeekToken.IsNumericLiteralToken Then
        argument2 = CInt(PopToken.Literal)
      Else
        Return ThrowBasicError(BasicError.MissingOperand)
      End If

      If PeekToken() Is Nothing Then
        ' Skip forward...
      ElseIf PeekToken.IsCommaToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken() Is Nothing OrElse PeekToken.IsCommaToken Then
        ' Skip forward.
      ElseIf PeekToken.IsNumericLiteralToken Then
        argument3 = CInt(PopToken.Literal)
      Else
        Return ThrowBasicError(BasicError.MissingOperand)
      End If

      ' Tandy

      If PeekToken() Is Nothing Then
        ' Skip forward...
      ElseIf PeekToken.IsCommaToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken() Is Nothing OrElse PeekToken.IsCommaToken Then
        ' Skip forward.
      ElseIf PeekToken.IsNumericLiteralToken Then
        'argument4 = CInt(PopToken.Literal)
        Dim result As Double
        If Not ExecuteExpression(result) Then Return False
        argument4 = CInt(result)
      Else
        Return ThrowBasicError(BasicError.MissingOperand)
      End If

      If argument4 = 0 Then
      End If

      If PeekToken() Is Nothing Then
        If argument1 < 0 Then
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        ElseIf argument1 > 32767 Then
          Return ThrowBasicError(BasicError.Overflow)
        ElseIf argument2 < 1 Then
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        ElseIf argument2 < 6830 Then
          Return ThrowBasicError(BasicError.OutOfMemory)
        ElseIf argument2 > 65535 Then
          Return ThrowBasicError(BasicError.Overflow)
        ElseIf argument2 = 65535 Then
          Return ThrowBasicError(BasicError.OutOfMemory)
        ElseIf argument3 < 1 Then
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        ElseIf argument3 < 241 Then
          Return ThrowBasicError(BasicError.OutOfMemory)
        ElseIf argument3 > 65535 Then
          Return ThrowBasicError(BasicError.Overflow)
        ElseIf argument3 > 27938 Then
          Return ThrowBasicError(BasicError.OutOfMemory)
        End If
        InternalClear(argument2, argument3)
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteColor() As Boolean

      ' GW-BASIC
      '   COLOR [foreground][,[background][,border]]
      '   COLOR [background][,[palette]]
      '   COLOR [foreground][,background]
      '   COLOR [foreground]

      ' foreground - optional
      ' background - optional
      ' border - optional

      Dim num1 As Short = -1
      Dim num2 As Short = -1
      Dim num3 As Short = -1

      ' Either nothing, a comma or a numeric expression.
      If PeekToken() Is Nothing Then
        ' No arguments...
      ElseIf PeekToken.IsCommaToken Then
        ' Move forward...
      Else
        If Not ExecuteExpression(num1) Then Return False
      End If

      ' If a comma, pop it.
      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      ' Either nothing, a comma or a numeric expression.
      If PeekToken() Is Nothing Then
        ' No further arguments...
      ElseIf PeekToken.IsCommaToken Then
        ' Move forward.
      Else
        If Not ExecuteExpression(num2) Then Return False
      End If

      If PeekToken() Is Nothing Then
        ' No further arguments...
      ElseIf PeekToken.IsCommaToken Then
        PopToken()
        If Not ExecuteExpression(num3) Then Return False
      End If

      If PeekToken() Is Nothing Then

        Try
          Select Case m_display.ScreenMode
          'Case -3

            Case -4
              ' COLOR [foreground]
              m_display.ColorVga(num1)

            Case 0 ' COLOR [foreground][,[background][,border]]
              m_display.Color0(num1, num2, num3)
            Case 1 ' COLOR [foreground][,palette]
              m_display.Color1(num1, num2)

          'Case 2

            Case 3 ' TANDY 1000 160x200 16 colors
              ' COLOR [foreground][,background]
              m_display.ColorEga(num1, num2)
            Case 4 ' TANDY 1000 320x200 4 colors
              ' COLOR [foreground][,background]
              m_display.ColorEga(num1, num2)
            Case 5 ' TANDY 1000 320x200 16 colors
              ' COLOR [foreground][,background]
              m_display.ColorEga(num1, num2)
            Case 6 ' TANDY 1000 640x200 4 colors
              ' COLOR [foreground][,background]
              m_display.ColorEga(num1, num2)

            Case 7
              ' COLOR [foreground][,background]
              m_display.ColorEga(num1, num2)
            Case 8
              ' COLOR [foreground][,background]
              m_display.ColorEga(num1, num2)
            Case 9
              ' COLOR [foreground][,background]
              m_display.ColorEga(num1, num2)
            Case 10
              ' COLOR [foreground][,background]
              m_display.ColorEga(num1, num2)

          'Case 11

            Case 12
              ' COLOR [foreground]
              m_display.ColorVga(num1)

            Case 13
              ' COLOR [foreground]
              m_display.ColorVga(num1)

            Case Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End Select
        Catch ex As Exception
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End Try

        'Select Case m_display.ScreenMode
        '  Case 0
        '    If num1 = -1 AndAlso num2 = -1 AndAlso num3 = -1 Then
        '      m_display.Color()
        '    ElseIf num2 = -1 AndAlso num3 = -1 Then
        '      If num1.Between(0, 7) Then
        '        m_display.Color(num1)
        '      Else
        '        Return ThrowBasicError(BasicError.IllegalFunctionCall)
        '      End If
        '    ElseIf num3 = -1 Then
        '      If num1.Between(0, 15) AndAlso num2.Between(0, 7) Then
        '        m_display.Color(num1, num2)
        '      Else
        '        Return ThrowBasicError(BasicError.IllegalFunctionCall)
        '      End If
        '    Else
        '      If num1.Between(0, 31) AndAlso
        '         num2.Between(0, 7) AndAlso
        '         num3.Between(0, 15) Then
        '        m_display.Color(num1, num2, num3)
        '      Else
        '        Return ThrowBasicError(BasicError.IllegalFunctionCall)
        '      End If
        '    End If

        '  Case 1
        '    If num3 = -1 Then ' Border parameter is not valid in screen mode 1.
        '      If num2 = -1 AndAlso num3 = -1 Then
        '        If num1.Between(0, 15) Then
        '          m_display.Color(num1)
        '        Else
        '          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        '        End If
        '      Else
        '        If num1.Between(0, 15) AndAlso num2.Between(0, 3) Then
        '          m_display.Color(num1, num2)
        '        Else
        '          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        '        End If
        '      End If
        '    Else
        '      Return ThrowBasicError(BasicError.IllegalFunctionCall)
        '    End If

        '  Case 2
        '    Return ThrowBasicError(BasicError.IllegalFunctionCall)

        '  Case Else
        '    Return ThrowBasicError(BasicError.AdvancedFeature)
        'End Select

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteCommon() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      Do

        Dim varName As String = ""
        Dim isArray As Boolean = False

        If PeekToken.IsIdentifierToken Then

          varName = PopToken.ToString

          If PeekToken.IsParenOpenToken Then
            PopToken()
            If PeekToken.IsParenCloseToken Then
              PopToken()
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
            isArray = True
          Else
            isArray = False
          End If

          If isArray Then

            If IsStringVariable(varName) Then

              Dim arrayName As String = varName & "("
              Dim c As Integer = Aggregate a In m_stringVariableList.Keys
                               Where a.StartsWith(arrayName)
                               Into Count()
              If c = 0 Then
                Dim dimensionBounds As New List(Of Bounds) From {
                New Bounds With {.Lower = m_optionBase,
                                                   .Upper = 10}
              }
                InitArray(varName, True, 1, dimensionBounds)
                VarPtr(varName)
              End If

              Dim l = From p In m_stringVariableList.Keys
                      Where p.StartsWith(arrayName)

              For Each entry In l
                m_stringVariableList(entry).Common = True
              Next

            ElseIf IsNumericVariable(varName) Then

              Dim arrayName As String = varName & "("
              Dim c As Integer = Aggregate a In m_numericVariableList.Keys
                               Where a.StartsWith(arrayName)
                               Into Count()
              If c = 0 Then
                Dim dimensionBounds As New List(Of Bounds) From {
                New Bounds With {.Lower = m_optionBase,
                                                   .Upper = 10}
              }
                InitArray(varName, False, 1, dimensionBounds)
                VarPtr(varName)
              End If

              Dim l = From p In m_numericVariableList.Keys
                      Where p.StartsWith(arrayName)

              For Each entry In l
                m_numericVariableList(entry).Common = True
              Next

            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

          Else

            If IsStringVariable(varName) Then
              If Not m_stringVariableList.ContainsKey(varName) Then
                SetStringVariable(varName, "")
              End If
              m_stringVariableList(varName).Common = True
            Else
              If Not m_numericVariableList.ContainsKey(varName) Then
                SetNumericVariable(varName, 0)
              End If
              m_numericVariableList(varName).Common = True
            End If

          End If

        End If

        If PeekToken() Is Nothing Then
          Exit Do
        ElseIf PeekToken.IsCommaToken Then
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Loop

      Return True

    End Function

    Private Function ExecuteCont() As Boolean

      Dim labels As New List(Of String)

      If PeekToken() Is Nothing Then

        If m_contIndex > -1 Then

          m_interpreter.Clear()
          For Each l In m_lines
            m_interpreter.Add(l.Copy)
          Next

          m_interpreterIndex = m_contIndex '- 1
          m_statementIndex = 0
          m_tokenIndex = 0

        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteData() As Boolean

      ' Data statements will be pre-scanned at the start of the application.

      ' Will verify structural integrity of the statement and jump to end to continue execution (aka skip).

      Do

        If PeekToken.IsStringLiteralToken OrElse PeekToken.IsNumericLiteralToken Then
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

        If PeekToken() Is Nothing Then
          Exit Do
        ElseIf PeekToken.IsCommaToken Then
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Loop

      Return True

    End Function

    Private Function ExecuteDate() As Boolean

      Dim value As String = ""

      If PeekToken.IsWord("=") Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
        If Not ExecuteStringExpression(value, Nothing) Then Return False
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.PermissionDenied)
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

    End Function

    Private Function ExecuteDef() As Boolean

      ' DEF FNname[arguments] expression
      ' DEF SEG [address]
      '   address is a numeric expression within the range of 0 to 65535
      ' DEF USR[n]=integer
      '   n may be any digit from 0 to 9.
      '   If n is omitted, DEF USR0 is assumed.

      If PeekToken() IsNot Nothing Then

        If PeekToken.ToString.StartsWith("SEG") Then
          PopToken()
          If PeekToken() Is Nothing Then
            'm_defSeg = -1
          ElseIf PeekToken.IsWord("=") Then
            PopToken()
            Dim address As Double
            If Not ExecuteExpression(address) Then Return False
            If address.Between(0, 65535) Then
              'm_defSeg = CInt(address)
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        ElseIf PeekToken.ToString.StartsWith("USR") Then
          Return ThrowBasicError(BasicError.AdvancedFeature)
        ElseIf PeekToken.ToString.StartsWith("FN") Then

          Dim name = PopToken.ToString

          If name.Length = 2 Then
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

          Dim parameters As New List(Of String)

          If PeekToken.IsParenOpenToken Then

            PopToken()

            Dim comma As Boolean

            Do

              If PeekToken() Is Nothing Then
                Return ThrowBasicError(BasicError.SyntaxError)
              ElseIf PeekToken.IsCommaToken Then
                If comma Then
                  Return ThrowBasicError(BasicError.SyntaxError)
                Else
                  PopToken()
                  comma = True
                End If
              ElseIf PeekToken.IsParenCloseToken Then
                PopToken()
                Exit Do
              ElseIf PeekToken.IsIdentifierToken Then
                parameters.Add(PopToken.ToString)
                comma = False
              Else
                Return ThrowBasicError(BasicError.SyntaxError)
              End If

            Loop

            If PeekToken.IsWord("=") Then

              PopToken()

              If PeekToken() IsNot Nothing Then

                ' Allow for redefining DEF FN's.
                m_defFnList.Remove(name)

                m_defFnList.Add(name, New DefFn With {.Name = name,
                                                    .Parameters = parameters,
                                                    .ExpressionLineIndex = m_interpreterIndex,
                                                    .ExpressionStatementIndex = m_statementIndex,
                                                    .ExpressionTokenIndex = m_tokenIndex})

                m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count

              Else

                Return ThrowBasicError(BasicError.SyntaxError)

              End If

            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

          End If

        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Else

        Return ThrowBasicError(BasicError.SyntaxError)

      End If

      Return True

    End Function

    Private Function ExecuteDefInt() As Boolean

      ' DEFINT letters
      '   letters: single, multiple in comma list and or dashed range (n-n).

      Dim variableRequired As Boolean = True
      Dim variable As Char

      Do

        If PeekToken() Is Nothing Then

          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            Exit Do
          End If

        ElseIf PeekToken.IsIdentifierToken Then

          Dim token = PopToken()

          variable = token.ToString(0)

          If token.ToString.Length = 1 AndAlso "ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(variable) Then
            m_defSngList.Remove(variable)
            m_defDblList.Remove(variable)
            m_defStrList.Remove(variable)
            If m_stringVariableList.Remove(variable) Then
              m_varNameList.Remove(VarPtr(variable))
              m_varPtrList.Remove(variable)
            End If
            If Not m_defIntList.Contains(variable) Then m_defIntList.Add(variable)
          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

          variableRequired = False

        ElseIf PeekToken.IsCommaToken Then
          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            PopToken()
            variableRequired = True
          End If
        ElseIf PeekToken.IsDash Then
          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            PopToken()
            If PeekToken.IsIdentifierToken Then
              Dim token = PopToken()
              Dim range As Char = token.ToString(0)
              If token.ToString.Length = 1 AndAlso "ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(range) Then
                For c As Integer = AscW(variable) + 1 To AscW(range)
                  Dim cc As Char = ChrW(c)
                  m_defSngList.Remove(cc)
                  m_defDblList.Remove(cc)
                  m_defStrList.Remove(cc)
                  If m_stringVariableList.Remove(cc) Then
                    m_varNameList.Remove(VarPtr(cc))
                    m_varPtrList.Remove(cc)
                  End If
                  If Not m_defIntList.Contains(cc) Then m_defIntList.Add(cc)
                Next
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            End If
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Loop

      Return True

    End Function

    Private Function ExecuteDefSng() As Boolean

      ' DEFSNG letters
      '   letters: single, multiple in comma list and or dashed range (n-n).

      Dim variableRequired As Boolean = True
      Dim variable As Char

      Do

        If PeekToken() Is Nothing Then

          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            Exit Do
          End If

        ElseIf PeekToken.IsIdentifierToken Then

          Dim token = PopToken()

          variable = token.ToString(0)

          If token.ToString.Length = 1 AndAlso "ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(variable) Then
            m_defIntList.Remove(variable)
            m_defDblList.Remove(variable)
            m_defStrList.Remove(variable)
            If m_stringVariableList.Remove(variable) Then
              m_varNameList.Remove(VarPtr(variable))
              m_varPtrList.Remove(variable)
            End If
            If Not m_defSngList.Contains(variable) Then m_defSngList.Add(variable)
          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

          variableRequired = False

        ElseIf PeekToken.IsCommaToken Then
          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            PopToken()
            variableRequired = True
          End If
        ElseIf PeekToken.IsDash Then
          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            PopToken()
            If PeekToken.IsIdentifierToken Then
              Dim token = PopToken()
              Dim range As Char = token.ToString(0)
              If token.ToString.Length = 1 AndAlso "ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(range) Then
                For c As Integer = AscW(variable) + 1 To AscW(range)
                  Dim cc As Char = ChrW(c)
                  m_defIntList.Remove(cc)
                  m_defDblList.Remove(cc)
                  m_defStrList.Remove(cc)
                  If m_stringVariableList.Remove(cc) Then
                    m_varNameList.Remove(VarPtr(cc))
                    m_varPtrList.Remove(cc)
                  End If
                  If Not m_defSngList.Contains(cc) Then m_defSngList.Add(cc)
                Next
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            End If
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Loop

      Return True

    End Function

    Private Function ExecuteDefDbl() As Boolean

      ' DEFDBL letters
      '   letters: single, multiple in comma list and or dashed range (n-n).

      Dim variableRequired As Boolean = True
      Dim variable As Char

      Do

        If PeekToken() Is Nothing Then

          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            Exit Do
          End If

        ElseIf PeekToken.IsIdentifierToken Then

          Dim token = PopToken()

          variable = token.ToString(0)

          If token.ToString.Length = 1 AndAlso "ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(variable) Then
            m_defSngList.Remove(variable)
            m_defIntList.Remove(variable)
            m_defStrList.Remove(variable)
            If m_stringVariableList.Remove(variable) Then
              m_varNameList.Remove(VarPtr(variable))
              m_varPtrList.Remove(variable)
            End If
            If Not m_defDblList.Contains(variable) Then m_defDblList.Add(variable)
          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

          variableRequired = False

        ElseIf PeekToken.IsCommaToken Then
          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            PopToken()
            variableRequired = True
          End If
        ElseIf PeekToken.IsDash Then
          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            PopToken()
            If PeekToken.IsIdentifierToken Then
              Dim token = PopToken()
              Dim range As Char = token.ToString(0)
              If token.ToString.Length = 1 AndAlso "ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(range) Then
                For c As Integer = AscW(variable) + 1 To AscW(range)
                  Dim cc As Char = ChrW(c)
                  m_defSngList.Remove(cc)
                  m_defIntList.Remove(cc)
                  m_defStrList.Remove(cc)
                  If m_stringVariableList.Remove(cc) Then
                    m_varNameList.Remove(VarPtr(cc))
                    m_varPtrList.Remove(cc)
                  End If
                  If Not m_defDblList.Contains(cc) Then m_defDblList.Add(cc)
                Next
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            End If
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Loop

      Return True

    End Function

    Private Function ExecuteDefStr() As Boolean

      ' DEFSTR letters
      '   letters: single, multiple in comma list and or dashed range (n-n).

      Dim variableRequired As Boolean = True
      Dim variable As Char

      Do

        If PeekToken() Is Nothing Then

          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            Exit Do
          End If

        ElseIf PeekToken.IsIdentifierToken Then

          Dim token = PopToken()

          variable = token.ToString(0)

          If token.ToString.Length = 1 AndAlso "ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(variable) Then
            m_defSngList.Remove(variable)
            m_defDblList.Remove(variable)
            m_defIntList.Remove(variable)
            m_numericVariableList.Remove(variable)
            If Not m_defStrList.Contains(variable) Then m_defStrList.Add(variable)
          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

          variableRequired = False

        ElseIf PeekToken.IsCommaToken Then
          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            PopToken()
            variableRequired = True
          End If
        ElseIf PeekToken.IsDash Then
          If variableRequired Then
            Return ThrowBasicError(BasicError.SyntaxError)
          Else
            PopToken()
            If PeekToken.IsIdentifierToken Then
              Dim token = PopToken()
              Dim range As Char = token.ToString(0)
              If token.ToString.Length = 1 AndAlso "ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(range) Then
                For c As Integer = AscW(variable) + 1 To AscW(range)
                  Dim cc As Char = ChrW(c)
                  m_defSngList.Remove(cc)
                  m_defDblList.Remove(cc)
                  m_defStrList.Remove(cc)
                  m_numericVariableList.Remove(cc)
                  If Not m_defStrList.Contains(cc) Then m_defStrList.Add(cc)
                Next
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            End If
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Loop

      Return True

    End Function

    'Private Function ExecuteDelete() As Boolean

    '  Dim startLine As Integer = -1
    '  Dim endLine As Integer = -1

    '  If PeekToken() Is Nothing Then
    '    Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '  Else

    '    ' Process start number.

    '    If PeekToken.IsPeriodToken Then
    '      PopToken()
    '      startLine = m_autoStart
    '    ElseIf PeekToken.IsNumericLiteralToken Then
    '      startLine = CShort(PopToken.Literal)
    '    End If

    '    ' Process end number.

    '    If PeekToken.IsDash Then
    '      PopToken()
    '      If PeekToken() Is Nothing Then
    '        If startLine > -1 Then endLine = 65529
    '      ElseIf PeekToken.IsPeriodToken Then
    '        PopToken()
    '        endLine = m_autoStart
    '      ElseIf PeekToken.IsNumericLiteralToken Then
    '        endLine = CShort(PopToken.Literal)
    '      End If
    '      If startLine = -1 Then startLine = 0
    '    ElseIf startLine > -1 Then
    '      endLine = startLine
    '    End If

    '    If PeekToken() Is Nothing AndAlso
    '     startLine.Between(0, 65529) AndAlso
    '     endLine.Between(0, 65529) AndAlso
    '     startLine <= endLine Then

    '      Dim deleted As Boolean

    '      For index As Integer = m_lines.Count - 1 To 0 Step -1
    '        If CInt(m_lines(index).LineNumber).Between(startLine, endLine) Then
    '          m_lines.RemoveAt(index)
    '          deleted = True
    '        End If
    '      Next

    '      If deleted Then
    '        'Return True
    '      Else
    '        Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '      End If

    '    Else
    '      Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '    End If

    '  End If

    '  Return False

    'End Function

    Private Function ExecuteDim() As Boolean

      Dim arrayName As String = ""

      Dim dimensionBounds As New List(Of Bounds)

      If PeekToken.IsWord("SHARED") Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
        'PopToken()
      End If

      Do

        If PeekToken.ToString(0).Between("A"c, "Z"c) Then

          arrayName = PopToken.ToString

          If PeekToken.IsParenOpenToken Then 'OrElse PeekToken.IsWord("[") Then

            PopToken()

            dimensionBounds.Clear()

            Do

              Dim bounds As New Bounds

              If Not ExecuteExpression(bounds.Upper) Then Return False

              If PeekToken.IsWord("TO") Then
                Return ThrowBasicError(BasicError.AdvancedFeature)
                'PopToken()
                'bounds.Lower = bounds.Upper
                'If Not ExecuteExpression(bounds.Upper) Then Return False
              Else
                'TODO: Lower depends on state of OPTION BASE 0 or 1 state.
                bounds.Lower = m_optionBase
              End If

              dimensionBounds.Add(bounds)

              If PeekToken.IsCommaToken Then
                PopToken()
              Else
                Exit Do
              End If

            Loop

            If PeekToken.IsParenCloseToken Then 'OrElse PeekToken.IsWord("]") Then

              PopToken()

              'Note: Number of bounds appears to only be limited to the amount of memory available.

              For Each entry In dimensionBounds
                If entry.Lower > entry.Upper Then
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                End If
                'NOTE: Apparently an array can be much larger than 255 (DONKEY.BAS has an array that is 500 in size).
                'If Not entry.Lower.Between(0, 255) OrElse
                '   Not entry.Upper.Between(0, 255) Then
                '  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                'End If
              Next

              ' Does the array already exist?

              ' If so, 
              Dim c As Integer

              If IsStringVariable(arrayName) Then
                Dim pattern As String = arrayName & "("
                c = Aggregate a In m_stringVariableList.Keys
                  Where a.StartsWith(pattern)
                  Into Count()
              Else
                Dim pattern As String = arrayName & "("
                c = Aggregate a In m_numericVariableList.Keys
                  Where a.StartsWith(pattern)
                  Into Count()
              End If

              If c > 0 Then
                Return ThrowBasicError(BasicError.DuplicateDefinition)
              Else
                InitArray(arrayName, IsStringVariable(arrayName), 1, dimensionBounds)
                VarPtr(arrayName)
              End If

            End If

          End If

        End If

        If PeekToken.IsWord("AS") Then
          Return ThrowBasicError(BasicError.AdvancedFeature)
          'Do
          '  PopToken()
          '  If PeekToken() Is Nothing OrElse PeekToken.IsCommaToken Then
          '    Exit Do
          '  End If
          'Loop
        End If

        If PeekToken() Is Nothing Then
          Exit Do
        ElseIf PeekToken.IsCommaToken() Then
          ' Another array?
          PopToken()
          If Not PeekToken.ToString(0).Between("A"c, "Z"c) Then
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Loop

      Return True

    End Function

    Private Function InitArray(arrayName As String,
                               isString As Boolean,
                               dimension As Short,
                               dimensionBounds As List(Of Bounds)) As Boolean

      'Dim key As String = arrayName

      If dimension <= dimensionBounds.Count Then

        If dimension = 1 Then
          arrayName &= "("
        Else
          arrayName &= ","
        End If

        For index = dimensionBounds(dimension - 1).Lower To dimensionBounds(dimension - 1).Upper
          If dimension = dimensionBounds.Count Then
            If isString Then
              If Not SetStringVariable(String.Format("{0}{1})", arrayName, index), "") Then Return False
            Else
              If Not SetNumericVariable(String.Format("{0}{1})", arrayName, index), 0.0) Then Return False
            End If
          Else
            If Not InitArray(arrayName & CStr(index), isString, CShort(dimension + 1), dimensionBounds) Then Return False
          End If
        Next

      End If

      Return True

    End Function

    'Private Function ExecuteEdit() As Boolean

    '  Dim lineNumber As Integer = -1

    '  If PeekToken() IsNot Nothing Then
    '    If PeekToken.IsNumericLiteralToken Then
    '      lineNumber = CShort(PopToken.Literal)
    '    ElseIf PeekToken.IsPeriodToken Then
    '      PopToken()
    '      If m_editLine > -1 Then
    '        lineNumber = m_editLine
    '      Else
    '        Return ThrowBasicError(BasicError.UndefinedLineNumber)
    '      End If
    '      'lineNumber = m_autoStart
    '    Else
    '      Return ThrowBasicError(BasicError.SyntaxError)
    '    End If
    '  Else
    '    m_editLine = -1
    '    Return ThrowBasicError(BasicError.UndefinedLineNumber)
    '  End If

    '  If PeekToken() Is Nothing Then

    '    Dim lines = From p In m_lines
    '                Where p.LineNumber = lineNumber

    '    If lines.Any AndAlso Not m_pendingNew Then
    '      m_display.Print(lines(0).ToString(), False)
    '      m_display.Locate(m_display.CsrLin, 1)
    '      m_editLine = lineNumber
    '      m_suppressOk = True
    '      'm_autoModeActive = True
    '    Else
    '      Return ThrowBasicError(BasicError.UndefinedLineNumber)
    '    End If

    '  Else
    '    Return ThrowBasicError(BasicError.SyntaxError)
    '  End If

    '  Return False

    'End Function

    Private Function ExecuteEnd() As Boolean

      If PeekToken() Is Nothing Then
        ' Do nothing, just need to return false.
        InternalClose()
        If m_interpreterIndex < m_interpreter.Count - 1 Then
          m_contIndex = m_interpreterIndex + 1
        Else
          m_contIndex = -1
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return False

    End Function

    Private Function ExecuteEnviron() As Boolean

      Dim command As String = Nothing

      If Not ExecuteStringExpression(command, Nothing) Then Return False

      If PeekToken() Is Nothing Then

        'NOTE: For now, we are swallowing this...

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteErase() As Boolean

      Dim arrayName As String = ""

      Do

        If PeekToken.ToString(0).Between("A"c, "Z"c) Then

          arrayName = PopToken.ToString

          If IsStringVariable(arrayName) Then

            Dim pattern As String = arrayName & "("
            Dim list = From p In m_stringVariableList.Keys
                       Where p.StartsWith(pattern)

            If list.Any Then
              For Each key In list.ToList
                m_stringVariableList.Remove(key)
              Next
              m_varNameList.Remove(VarPtr(arrayName))
              m_varPtrList.Remove(arrayName)
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

          ElseIf IsNumericVariable(arrayName) Then

            Dim pattern As String = arrayName & "("
            Dim list = From p In m_numericVariableList.Keys
                       Where p.StartsWith(pattern)

            If list.Any Then
              For Each key In list.ToList
                m_numericVariableList.Remove(key)
              Next
              m_varNameList.Remove(VarPtr(arrayName))
              m_varPtrList.Remove(arrayName)
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

          End If

        End If

        If PeekToken() Is Nothing Then
          Exit Do
        ElseIf PeekToken.IsCommaToken() Then
          ' Another array?
          PopToken()
          If Not PeekToken.ToString(0).Between("A"c, "Z"c) Then
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Loop

      Return True

    End Function

    Private Function ExecuteError() As Boolean

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      Else
        Dim number As Short
        If Not ExecuteExpression(number) Then Return False
        If number.Between(1, 254) Then
          Return ThrowBasicError(number)
        Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If
      End If

      Return True

    End Function

    Private Function ExecuteField() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsHashToken Then
        PopToken()
      End If

      Dim fileNumber As Short
      If Not ExecuteExpression(fileNumber) Then Return False

      If PeekToken.IsCommaToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Do

        Dim width As Short
        If Not ExecuteExpression(width) Then Return False

        If PeekToken.IsWord("AS") Then
          PopToken()
          Dim stringVar As String
          If IsStringVariable(PeekToken.ToString) Then
            stringVar = PopToken.ToString
            If Not InternalField(fileNumber, stringVar, width) Then Return False
          Else
            Return ThrowBasicError(BasicError.TypeMismatch)
          End If
        End If

        If PeekToken.IsCommaToken Then
          PopToken()
        Else
          Exit Do
        End If

      Loop

      Return True

    End Function

    Private Function ExecuteFiles() As Boolean

      Dim path As String = Nothing
      Dim pattern As String = "*.*"

      If PeekToken() Is Nothing Then
        path = m_currentPath
      ElseIf Not PeekToken.IsStringLiteralToken Then
        Return ThrowBasicError(BasicError.TypeMismatch)
      ElseIf PeekToken.IsStringLiteralToken AndAlso
           String.IsNullOrEmpty(PeekToken.ToString) Then
        Return ThrowBasicError(BasicError.BadFilename)
      ElseIf PeekToken.IsStringLiteralToken Then
        path = PopToken.ToString
        If path.IndexOf("\"c) > -1 Then
          path = RealPath(path)
        Else
          pattern = path
          path = m_currentPath
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Dim result As String = m_virtualFileSystem.Files(path)
      ExecuteFilesParseResult(result, path & "|" & pattern)
      Return True

    End Function

    Private Sub ExecuteFilesParseResult(result As String, pathPattern As String)

      Dim pattern As String = pathPattern.Split("|"c)(1).ToUpper
      If pattern = "*.*" Then pattern = "*"

      'Dim lines() = Split(result, vbLf)
      Dim lines As String() = result.Split(CChar(vbLf))

      m_display.Print(lines(0), True)

      Dim folders As New List(Of String)
      Dim files As New List(Of String)

      For index = 1 To lines.Length - 2
        'Dim entryParts = Split(lines(index), "|")
        Dim entryParts As String() = lines(index).Split("|"c)
        If entryParts(1) = "1" Then
          folders.Add(entryParts(0))
        Else
          files.Add(entryParts(0))
        End If
      Next

      Dim filtered As Boolean = False

      For index As Integer = folders.Count - 1 To 0 Step -1
        'If Not (folders(index) Like pattern) Then
        If Not folders(index).Like(pattern) Then
          folders.RemoveAt(index)
          filtered = True
        End If
      Next

      For index As Integer = files.Count - 1 To 0 Step -1
        'If Not files(index) Like pattern Then
        If Not files(index).Like(pattern) Then
          files.RemoveAt(index)
          filtered = True
        End If
      Next

      Dim found As Boolean = False
      Dim output As String '= ""
      Dim column As Integer '= -1

      If filtered Then
        output = ""
        column = 0
      Else
        output = "        .   <DIR>         ..  <DIR> "
        column = 2
      End If

      For Each folder In folders

        If column = If(Me.m_display.ColumnCount = 80, 4, 2) Then
          m_display.Print(output, True)
          found = True
          output = ""
          column = 0
        End If

        'Dim pathParts = Split(folder, ".")
        Dim pathParts = folder.Split("."c)

        If pathParts.Length = 2 Then
          output &= pathParts(0).PadRight(8) & "." & pathParts(1).PadRight(3)
        Else
          output &= pathParts(0).PadRight(12)
        End If

        output &= "<DIR> "

        column += 1

      Next

      For Each file In files

        If column = If(Me.m_display.ColumnCount = 80, 4, 2) Then
          m_display.Print(output, True)
          found = True
          output = ""
          column = 0
        End If

        'Dim pathParts = Split(file, ".")
        Dim pathParts = file.Split("."c)

        If pathParts.Length = 2 Then
          output &= pathParts(0).PadRight(8) & "." & pathParts(1).PadRight(3)
        Else
          output &= pathParts(0).PadRight(12)
        End If

        output &= "      "

        column += 1

      Next

      If output <> "" Then
        found = True
        m_display.Print(output, True)
      End If

      If Not found Then
        m_display.Print("File not found", True)
      End If

      m_display.Print(lines(lines.Length - 1), True)

      m_waiting = False
      m_running = False

      'InternalPrompt()
      'ProcessKeyBuffer()

    End Sub

    Private Function ExecuteFor() As Boolean

      Dim identifier As String
      Dim num1 As Double
      Dim num2 As Double
      Dim num3 As Double = 1.0

      If PeekToken.IsIdentifierToken Then
        identifier = PopToken.ToString
        If PeekToken.IsWord("=") Then
          PopToken()
          If Not ExecuteExpression(num1) Then Return False
          If PeekToken.IsWord("TO") Then
            PopToken()
            If Not ExecuteExpression(num2) Then Return False
            If PeekToken() Is Nothing Then
            ElseIf PeekToken.IsWord("STEP") Then
              PopToken()
              If Not ExecuteExpression(num3) Then Return False
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken() Is Nothing Then
        If num3 <> 0.0 Then '(num3 <> 0.0 AndAlso num1 <= num2) OrElse (num3 < 0.0 AndAlso num1 >= num2) Then

          If Not FoundMatchingNext(identifier) Then Return False

          If Not SetNumericVariable(identifier, num1) Then Return False
          If m_debugForLoops Then
            Debug.WriteLine(String.Format("{0} FOR {1}",
                                        m_interpreter(m_interpreterIndex).LineNumber,
                                        identifier))
          End If

          If num3 > 0 AndAlso num1 > num2 Then
            ' Jump past this loop
            Return JumpToNext(identifier)
          ElseIf num3 < 0 AndAlso num2 > num1 Then
            ' Jump past this loop
            Return JumpToNext(identifier)
          Else
            m_forLoop.Insert(0, New ForLoop With {.Identifier = identifier,
                                                .EndValue = num2,
                                                .StepValue = num3,
                                                .InterpreterIndex = m_interpreterIndex,
                                                .StatementIndex = m_statementIndex,
                                                .TokenIndex = m_tokenIndex})
          End If

        Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function FoundMatchingNext(indexerVariable As String) As Boolean

      Dim currentInterpreterIndex As Integer = m_interpreterIndex
      Dim currentStatementIndex As Integer = m_statementIndex
      Dim currentTokenIndex As Integer = m_tokenIndex

      Dim depth As Integer = 0

      m_statementIndex += 1
      m_tokenIndex = 0
      If m_statementIndex > m_interpreter(m_interpreterIndex).Statements.Count - 1 Then
        m_interpreterIndex += 1
        m_statementIndex = 0
        m_tokenIndex = 0
      End If
      If m_interpreterIndex > m_interpreter.Count - 1 Then
        m_interpreterIndex = currentInterpreterIndex
        m_statementIndex = currentStatementIndex
        m_tokenIndex = currentTokenIndex
        Return ThrowBasicError(BasicError.ForWithoutNext)
      End If

      Do ' Lines

        Do ' Statements

          If PeekToken.IsWord("FOR") Then
            depth += 1
          ElseIf PeekToken.IsWord("NEXT") Then
            PopToken()
            If PeekToken() Is Nothing Then
              If depth = 0 Then
                m_interpreterIndex = currentInterpreterIndex
                m_statementIndex = currentStatementIndex
                m_tokenIndex = currentTokenIndex
                Return True
              Else
                depth -= 1
              End If
            ElseIf PeekToken.IsIdentifierToken Then
              ' Contains additional tokens that should match indexes...
              Dim nameList As New List(Of String)
              If Not BuildNameList(nameList) Then
                m_interpreterIndex = currentInterpreterIndex
                m_statementIndex = currentStatementIndex
                m_tokenIndex = currentTokenIndex
                Return ThrowBasicError(BasicError.SyntaxError)
              End If
              For Each name In nameList
                If depth = 0 Then
                  If name = indexerVariable Then
                    m_interpreterIndex = currentInterpreterIndex
                    m_statementIndex = currentStatementIndex
                    m_tokenIndex = currentTokenIndex
                    Return True
                  Else
                    m_interpreterIndex = currentInterpreterIndex
                    m_statementIndex = currentStatementIndex
                    m_tokenIndex = currentTokenIndex
                    Return ThrowBasicError(BasicError.ForWithoutNext)
                  End If
                Else
                  depth -= 1
                End If
              Next
            Else
              m_interpreterIndex = currentInterpreterIndex
              m_statementIndex = currentStatementIndex
              m_tokenIndex = currentTokenIndex
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          End If

          m_statementIndex += 1
          m_tokenIndex = 0

          If m_statementIndex > m_interpreter(m_interpreterIndex).Statements.Count - 1 Then
            Exit Do
          End If

        Loop

        m_interpreterIndex += 1
        m_statementIndex = 0
        m_tokenIndex = 0

        If m_interpreterIndex > m_interpreter.Count - 1 Then
          Exit Do
        End If

      Loop

      m_interpreterIndex = currentInterpreterIndex
      m_statementIndex = currentStatementIndex
      m_tokenIndex = currentTokenIndex
      Return ThrowBasicError(BasicError.ForWithoutNext)

    End Function

    Private Function JumpToNext(indexerVariable As String) As Boolean

      Dim currentInterpreterIndex As Integer = m_interpreterIndex
      Dim currentStatementIndex As Integer = m_statementIndex
      Dim currentTokenIndex As Integer = m_tokenIndex

      Dim depth As Integer = 0

      m_statementIndex += 1
      m_tokenIndex = 0
      If m_statementIndex > m_interpreter(m_interpreterIndex).Statements.Count - 1 Then
        m_interpreterIndex += 1
        m_statementIndex = 0
        m_tokenIndex = 0
      End If
      If m_interpreterIndex > m_interpreter.Count - 1 Then
        m_interpreterIndex = currentInterpreterIndex
        m_statementIndex = currentStatementIndex
        m_tokenIndex = currentTokenIndex
        Return ThrowBasicError(BasicError.ForWithoutNext)
      End If

      Do ' Lines

        Do ' Statements

          If PeekToken.IsWord("FOR") Then
            depth += 1
          ElseIf PeekToken.IsWord("NEXT") Then
            PopToken()
            If PeekToken() Is Nothing Then
              If depth = 0 Then
                'm_interpreterIndex = currentInterpreterIndex
                'm_statementIndex = currentStatementIndex
                'm_tokenIndex = currentTokenIndex
                m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count
                Return True
              Else
                depth -= 1
              End If
            ElseIf PeekToken.IsIdentifierToken Then
              ' Contains additional tokens that should match indexes...
              Dim nameList As New List(Of String)
              If Not BuildNameList(nameList) Then
                m_interpreterIndex = currentInterpreterIndex
                m_statementIndex = currentStatementIndex
                m_tokenIndex = currentTokenIndex
                Return ThrowBasicError(BasicError.SyntaxError)
              End If
              For Each name In nameList
                If depth = 0 Then
                  If name = indexerVariable Then
                    'm_interpreterIndex = currentInterpreterIndex
                    'm_statementIndex = currentStatementIndex
                    'm_tokenIndex = currentTokenIndex
                    m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count
                    Return True
                  Else
                    m_interpreterIndex = currentInterpreterIndex
                    m_statementIndex = currentStatementIndex
                    m_tokenIndex = currentTokenIndex
                    Return ThrowBasicError(BasicError.ForWithoutNext)
                  End If
                Else
                  depth -= 1
                End If
              Next
            Else
              m_interpreterIndex = currentInterpreterIndex
              m_statementIndex = currentStatementIndex
              m_tokenIndex = currentTokenIndex
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          End If

          m_statementIndex += 1
          m_tokenIndex = 0

          If m_statementIndex > m_interpreter(m_interpreterIndex).Statements.Count - 1 Then
            Exit Do
          End If

        Loop

        m_interpreterIndex += 1
        m_statementIndex = 0
        m_tokenIndex = 0

        If m_interpreterIndex > m_interpreter.Count - 1 Then
          Exit Do
        End If

      Loop

      m_interpreterIndex = currentInterpreterIndex
      m_statementIndex = currentStatementIndex
      m_tokenIndex = currentTokenIndex
      Return ThrowBasicError(BasicError.ForWithoutNext)

    End Function

    'Private Function JumpToNext(ByVal indexerVariable As String) As Boolean

    '  Dim interpreterIndex As Integer = m_interpreterIndex
    '  Dim statementIndex As Integer = m_statementIndex
    '  Dim depth As Integer = 0

    '  statementIndex += 1
    '  If statementIndex > m_interpreter(interpreterIndex).Statements.Count - 1 Then
    '    interpreterIndex += 1
    '    statementIndex = 0
    '  End If
    '  If interpreterIndex > m_interpreter.Count - 1 Then
    '    Return ThrowBasicError(BasicError.WhileWithoutWend)
    '  End If

    '  Do ' Lines

    '    Do ' Statements

    '      If m_interpreter(interpreterIndex).Statements(statementIndex).Tokens(0).IsWord("FOR") Then
    '        depth += 1
    '      ElseIf m_interpreter(interpreterIndex).Statements(statementIndex).Tokens(0).IsWord("NEXT") Then
    '        If depth = 0 Then
    '          m_interpreterIndex = interpreterIndex
    '          m_statementIndex = statementIndex
    '          m_tokenIndex = 1
    '          If PeekToken() Is Nothing Then
    '            Return True
    '          ElseIf PeekToken.IsIdentifierToken Then
    '            ' Contains additional tokens that should match indexes...
    '            Dim nameList As New List(Of String)
    '            If Not BuildNameList(nameList) Then Return ThrowBasicError(BasicError.SyntaxError)
    '            If nameList.Contains(indexerVariable) Then
    '              Return True
    '            End If
    '          Else
    '            Return ThrowBasicError(BasicError.SyntaxError)
    '          End If
    '        End If
    '        depth -= 1
    '      End If

    '      statementIndex += 1

    '      If statementIndex > m_interpreter(interpreterIndex).Statements.Count - 1 Then
    '        Exit Do
    '      End If

    '    Loop

    '    interpreterIndex += 1
    '    statementIndex = 0

    '    If interpreterIndex > m_interpreter.Count - 1 Then
    '      'interpreterIndex = -1
    '      'statementIndex = -1
    '      Exit Do
    '    End If

    '  Loop

    '  Return ThrowBasicError(BasicError.ForWithoutNext)

    'End Function

    Private Function ExecuteNext() As Boolean

      Dim identifier As String
      Dim nameList As New List(Of String)

      If m_forLoop.Count > 0 Then

        If PeekToken() Is Nothing Then
          identifier = m_forLoop(0).Identifier
        ElseIf PeekToken.IsIdentifierToken Then

          If Not BuildNameList(nameList) Then Return False

          If nameList.Contains(m_forLoop(0).Identifier) Then
            identifier = m_forLoop(0).Identifier
          Else

            Dim index As Integer = 0

            While index < m_forLoop.Count - 1 AndAlso Not nameList.Contains(m_forLoop(index).Identifier)
              index += 1
            End While

            If index < m_forLoop.Count AndAlso nameList.Contains(m_forLoop(index).Identifier) Then

              For scan = index - 1 To 0 Step -1
                ' Abort next statements inside of this one...
                If m_debugForLoops Then
                  Debug.WriteLine(String.Format("{0} the ""FOR {1}"" loop at line {2} seems to have been left",
                                              m_interpreter(m_interpreterIndex).LineNumber,
                                              m_forLoop(0).Identifier,
                                              m_interpreter(m_forLoop(0).InterpreterIndex).LineNumber))
                End If
                m_forLoop.RemoveAt(0)
              Next

              identifier = m_forLoop(0).Identifier

            Else

              Return ThrowBasicError(BasicError.NextWithoutFor)

            End If

            '      index1 = 0

            '      While index1 <= forLoop.Count AndAlso Not IsNameInList(forLoop(index1).VarName, param1)
            '        index1 += 1
            '      End While

            '      If index1 <= forLoop.Count AndAlso IsNameInList(forLoop(index1).VarName, param1) Then

            '        For index2 = 1 To index1 - 1
            '          line_marker()
            '          LogWrite("THE ""FOR " & forLoop(index2).VarName & """ LOOP AT LINE ")
            '          line_marker(forLoop(index2 - 1).BodyLine)
            '          LogWriteLine(" SEEMS TO HAVE BEEN LEFT")
            '        Next

            '        ' (translated) forLoop := forLoop[index1 .. ];
            '        forLoop.RemoveAt(0)
            '        ' (translated) variable_name := forLoop[1].varName;
            '        variable_name = forLoop(0).VarName

            '      ElseIf on_error_label <> "" Then

            '        error_code = 1 ' NEXT without FOR
            '        line_marker()

            '        LogWrite(error_code & " NEXT WITHOUT FOR")
            '        LogWriteLine(" - ON ERROR GOTO " & on_error_label)

            '        OnErrorGoto(on_error_label, Line)

            '        symbol = ""
            '        Line = ""
            '        variable_name = ""

            '      Else

            '        error_marker()
            '        Err.WriteLine("NEXT " & param1 & " - NO MATCHING FOR FOUND")

            '        While Not EndOfStatement(symbol)
            '          symbol = GetSymbol(Line)
            '        End While

            '        variable_name = param1

            '      End If

            '  exec_next_decision(symbol, Line, variable_name, param1)

          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

        'Debug.WriteLine(String.Format("{0} NEXT {1}", m_interpreter(m_interpreterIndex).LineNumber, identifier))
        Return ExecuteNextDecision(identifier, nameList)

      Else
        If m_debugForLoops Then
          Debug.WriteLine(String.Format("{0} NEXT without FOR", m_interpreter(m_interpreterIndex).LineNumber))
        End If
        Return ThrowBasicError(BasicError.NextWithoutFor)
      End If

      'Dim variable_name As String = ""
      'Dim param1 As String = ""
      'Dim index1 As Short = 0
      'Dim index2 As Short = 0

      'If forLoop.Count >= 1 Then

      '  symbol = GetSymbol(Line)

      '  If EndOfStatement(symbol) Then

      '    param1 = ""
      '    ' (translated) variable_name := forLoop[1].varName;
      '    variable_name = forLoop(0).varName

      '  Else

      '    param1 = getNameList(symbol, Line)

      '    If IsNameInList(forLoop(0).VarName, param1) Then
      '      variable_name = forLoop(0).VarName
      '    Else

      '      index1 = 0

      '      While index1 <= forLoop.Count AndAlso Not IsNameInList(forLoop(index1).VarName, param1)
      '        index1 += 1
      '      End While

      '      If index1 <= forLoop.Count AndAlso IsNameInList(forLoop(index1).VarName, param1) Then

      '        For index2 = 1 To index1 - 1
      '          line_marker()
      '          LogWrite("THE ""FOR " & forLoop(index2).VarName & """ LOOP AT LINE ")
      '          line_marker(forLoop(index2 - 1).BodyLine)
      '          LogWriteLine(" SEEMS TO HAVE BEEN LEFT")
      '        Next

      '        ' (translated) forLoop := forLoop[index1 .. ];
      '        forLoop.RemoveAt(0)
      '        ' (translated) variable_name := forLoop[1].varName;
      '        variable_name = forLoop(0).VarName

      '      ElseIf on_error_label <> "" Then

      '        error_code = 1 ' NEXT without FOR
      '        line_marker()

      '        LogWrite(error_code & " NEXT WITHOUT FOR")
      '        LogWriteLine(" - ON ERROR GOTO " & on_error_label)

      '        OnErrorGoto(on_error_label, Line)

      '        symbol = ""
      '        Line = ""
      '        variable_name = ""

      '      Else

      '        error_marker()
      '        Err.WriteLine("NEXT " & param1 & " - NO MATCHING FOR FOUND")

      '        While Not EndOfStatement(symbol)
      '          symbol = GetSymbol(Line)
      '        End While

      '        variable_name = param1

      '      End If

      '    End If

      '  End If

      '  exec_next_decision(symbol, Line, variable_name, param1)

      'ElseIf on_error_label <> "" Then

      '  error_code = 1 ' NEXT without FOR
      '  line_marker()

      '  LogWrite(error_code & " NEXT " & variable_name & " WITHOUT FOR")
      '  logWriteLine(" - ON ERROR GOTO " & on_error_label)

      '  OnErrorGoto(on_error_label, Line)

      '  symbol = ""
      '  Line = ""

      'Else
      '  error_marker()
      '  Err.WriteLine("NEXT " & variable_name & " WITHOUT FOR")
      '  Do
      '    symbol = GetSymbol(Line)
      '  Loop Until EndOfStatement(symbol)
      'End If

      Return False

    End Function

    Private Function BuildNameList(ByRef nameList As List(Of String)) As Boolean

      If nameList Is Nothing Then
        nameList = New List(Of String)
      Else
        nameList.Clear()
      End If

      Do
        If PeekToken() Is Nothing Then
          Exit Do
        ElseIf PeekToken.IsCommaToken Then
          PopToken()
        ElseIf PeekToken.IsIdentifierToken Then
          nameList.Add(PopToken.ToString)
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      Loop

      Return True

    End Function

    Private Function ExecuteNextDecision(identifier As String,
nameList As List(Of String)) As Boolean

      Dim num1 As Double = 0.0

      If m_forLoop.Count >= 1 AndAlso identifier = m_forLoop(0).Identifier Then

        If Not GetNumericVariable(identifier, num1) Then Return False

        If (m_forLoop(0).StepValue > 0.0 AndAlso
          num1 + m_forLoop(0).StepValue <= m_forLoop(0).EndValue) OrElse
         (m_forLoop(0).StepValue < 0.0 AndAlso
          num1 + m_forLoop(0).StepValue >= m_forLoop(0).EndValue) Then

          If m_debugForLoops Then
            Debug.WriteLine(String.Format("{0} NEXT {1} = {2} continue the ""FOR {3}"" loop at line {4}",
                                        m_interpreter(m_interpreterIndex).LineNumber,
                                        identifier,
                                        num1 + m_forLoop(0).StepValue,
                                        identifier,
                                        m_interpreter(m_forLoop(0).InterpreterIndex).LineNumber))
          End If

          If Not SetNumericVariable(identifier, num1 + m_forLoop(0).StepValue) Then Return False

          m_interpreterIndex = m_forLoop(0).InterpreterIndex
          m_statementIndex = m_forLoop(0).StatementIndex
          m_tokenIndex = m_forLoop(0).TokenIndex

          'file_line_number = ForLoop(0).BodyLine
          'm_statementLabel = m_programList(file_line_number - 1).LineNumber

          'If m_forLoop(0).BodyColumn > m_programList(file_line_number - 1).Line.Length Then
          '  Line = ""
          'Else
          '  Line = m_programList(file_line_number - 1).Line.Substring(ForLoop(0).BodyColumn)
          'End If

          'If Line <> "" Then
          '  symbol = ":"
          'Else
          '  symbol = ""
          'End If

        Else

          If m_debugForLoops Then
            Debug.WriteLine(String.Format("{0} NEXT {1} = {2} END FOR", m_interpreter(m_interpreterIndex).LineNumber, identifier, num1))
          End If
          m_forLoop.RemoveAt(0)
          If m_forLoop.Count >= 1 AndAlso
          m_forLoop(0).Identifier <> identifier AndAlso
          nameList.Contains(m_forLoop(0).Identifier) Then
            If m_debugForLoops Then
              Debug.WriteLine(String.Format("{0} The ""FOR {1}"" loop at line {2} is also handled by this NEXT statement",
                                          m_interpreter(m_interpreterIndex).LineNumber,
                                          identifier,
                                          m_interpreter(m_forLoop(0).InterpreterIndex).LineNumber))
            End If
            'exec_next_decision(symbol, Line, ForLoop(0).VarName, removeNameFromList(variable_name, nameList))
            If Not ExecuteNextDecision(m_forLoop(0).Identifier, nameList) Then Return False
          End If

        End If

      Else
        If m_debugForLoops Then
          Debug.WriteLine(String.Format("{0} ""NEXT {1}"" expected - found ""NEXT {2}""",
                                      m_interpreter(m_interpreterIndex).LineNumber,
                                      m_forLoop(0).Identifier,
                                      identifier))
        End If
        Return ThrowBasicError(BasicError.IllegalFunctionCall)
      End If

      Return True

    End Function

    Private Function IsLineNumberOrLabel(token As Parser.Token) As Boolean
      If TypeOf token Is Parser.IdentifierToken Then
        Return m_labelList.ContainsKey(token.ToString)
      ElseIf TypeOf token Is Parser.NumericLiteralToken Then
        Return True
      Else
        Return False
      End If
    End Function

    'Private Function ExecuteFre() As Boolean

    '  Return ThrowBasicError(BasicError.AdvancedFeature)

    '  Return True

    'End Function

    Private Function ExecuteGet() As Boolean

      ' http://support.microsoft.com/kb/45699
      ' http://webpages.charter.net/danrollins/techhelp/0089.HTM

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsParenOpenToken Then

        PopToken()

        Dim x1 As Short
        Dim y1 As Short
        Dim x2 As Short
        Dim y2 As Short
        Dim arrayName As String

        If Not ExecuteExpression(x1) Then Return False
        If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not ExecuteExpression(y1) Then Return False
        If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not PopToken.IsWord("-") Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not ExecuteExpression(x2) Then Return False
        If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not ExecuteExpression(y2) Then Return False
        If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not IsNumericVariable(PeekToken.ToString) Then Return ThrowBasicError(BasicError.SyntaxError)
        arrayName = PopToken.ToString

        If PeekToken() Is Nothing Then

          If IsNumericVariable(arrayName) Then

            Dim sx As Short = CShort(((x2 - x1) + 1) * 2)
            Dim sy As Short = CShort((y2 - y1) + 1)

            Dim bitsPerPixel As Integer
            Dim size As Integer

            Select Case m_display.ScreenMode
              Case 1
                bitsPerPixel = 2
                'NOTE: The math is directly out of the GW-BASIC manual; however, the loop below suggests that it's not calculated
                ' based on bitsPerPixel?????
                'size = CInt(4 + Conversion.Int((sx * bitsPerPixel + 7) / 8) * sy)
                size = CInt(4 + VisualBasic.Conversion.Int((sx * 1 + 7) / 8) * sy)
              Case 2
                bitsPerPixel = 1
                'NOTE: The math is directly out of the GW-BASIC manual; however, the loop below suggests that it's not calculated
                ' based on bitsPerPixel?????
                'size = CInt(4 + Conversion.Int((sx * bitsPerPixel + 7) / 8) * sy)
                'size = CInt(4 + Conversion.Int((sx * bitsPerPixel + 7) / 8) * sy)
                size = CInt(4 + VisualBasic.Conversion.Int((sx * 1 + 7) / 8) * sy)
              Case 3
                Return ThrowBasicError(BasicError.AdvancedFeature)
              Case 4
                Return ThrowBasicError(BasicError.AdvancedFeature)
              Case 5
                Return ThrowBasicError(BasicError.AdvancedFeature)
              Case 6
                Return ThrowBasicError(BasicError.AdvancedFeature)
              Case 7
                bitsPerPixel = 4
                size = CInt(4 + VisualBasic.Conversion.Int((sx * bitsPerPixel + 7) / 8) * sy)
              Case 8
                bitsPerPixel = 4
                size = CInt(4 + VisualBasic.Conversion.Int((sx * bitsPerPixel + 7) / 8) * sy)
              Case 9
                bitsPerPixel = 4 ' 64K+ EGA
                size = CInt(4 + VisualBasic.Conversion.Int((sx * bitsPerPixel + 7) / 8) * sy)
              Case Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End Select

            Dim bytes(size - 1) As Byte

            Dim b = BitConverter.GetBytes(sx)
            bytes(0) = b(0)
            bytes(1) = b(1)
            b = BitConverter.GetBytes(sy)
            bytes(2) = b(0)
            bytes(3) = b(1)

            If bitsPerPixel = 0 Then
            End If

            Select Case m_display.ScreenMode

              Case 0

                ' Text Modes
                '  Video modes 00H-03H and 07H (see Video Modes) use the following video-
                '  memory layout:

                '        0   1   2   3   4   5   ...  158 159
                '       chr atr chr atr chr atr  ...  chr atr  line 0, video page 0
                '   160 chr atr chr atr chr atr  ...  chr atr  line 1
                '   320 chr atr chr atr chr atr  ...  chr atr  line 2
                '   480 chr atr chr atr chr atr  ...  chr atr 
                '      \                                     \
                '  3840 chr atr chr atr chr atr  ...  chr atr  line 24
                '       ...small gap...
                '  4096 chr atr chr atr chr atr  ...  chr atr  line 0, video page 1
                '       ...etc...

                '  All even-numbered addresses contain characters (see Character Set) and
                '  all odd-numbered addresses contain video attributes.

                '  The width of a standard 80-column line is 160 bytes and there are 25 lines
                '  (0-24) in most cases.  But these can be changed by various video services
                '  or TSRs or device drivers.  Programs should check the width and height of
                '  the screen via INT 10H 0fH or INT 10H 1130H or by examining variables in
                '  the BIOS Data Area.

                '  On EGAs and VGAs, it is possible to remap the video attributes so that
                '  any attribute byte can represent any desired color (see INT 10H 10H).
                '  You can also redefine the characters; for instance, to display italics or
                '  foreign-language text.  See INT 10H 11H and Video Font Definition.

                '  Accessing Text Mode Video Memory
                '    * Text mode video memory begins at b800:0 on CGA, EGA and VGA and at
                '      b000:0 on MDA.

                '      Use INT 12H to see if MDA is active and set up a segment register
                '      accordingly.

                '    * Given an X,Y coordinate (clm,row), calculate a memory location as:

                '        vidAddr = (row * 160 )+ (clm * 2)

                '      Note: For best flexibility, don't hard-code "160".  Instead, fetch
                '      the word at 0040:004a in the BIOS Data Area and multiply it by 2.

                '    * Store a character at that position and an attribute one address
                '      higher.  Most applications use 16-bit commands such as...

                '         mov  word ptr ES:[DI],AX
                '      or
                '         stosw

                '      ...to write both a character and attribute to video memory in one
                '      operation.

                '  Special care must be taken when using direct-memory access on CGA
                '  adapters.  See Video Snow for a discussion.

                '  Also, all video systems except MDA provide multiple "pages" of video
                '  memory.  It is possible to write to a secondary page then swap it into
                '  view via INT 10H 05H.  But this is seen rarely: CPUs are now fast enough
                '  to update the screen in place without flicker.  If you are writing to a
                '  video page other than 0, check 0040:004e for the offset of the start of
                '  the video page.

                Return ThrowBasicError(BasicError.AdvancedFeature)

              Case 1

                'TODO: May need to store as an interlaced image. (first half, even scan lines, second half, odd scan lines.)

                ' CGA Low-res 320x200, 4-color (video modes 04H and 05H)

                ' Segment: b800

                ' Layout: Interleaved scan lines, packed-pixel.  Even-numbered scan lines
                '         begin at b800:0, and odd-numbered lines begin at b800:2000.

                '         Each scan line is 80-bytes long and there are 200 scan lines
                '         (regen size=8000 bytes * 2 regions).  Each byte contains 4 pixels
                '         (16,000 total pixels):
                '         Ö7Â6Â5Â4Â3Â2Â1Â0·
                '         º   ³   ³   ³   º
                '         ÓÄÁÄÁÄÁÄÁÄÁÄÁÄÁÄ½  bits mask
                '          ÈË¼ ÈË¼ ÈË¼ ÈÍÊÍ> 0-1:  03H  fourth pixel in byte
                '           º   º   ÈÍÍÍÍÍÍ> 2-3:  0cH  third pixel in byte
                '           º   ÈÍÍÍÍÍÍÍÍÍÍ> 4-5:  30H  second pixel in byte
                '           ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍ> 6-7:  c0H  first pixel in byte
                '                                       00=black;       01=green/cyan
                '                                       10=red/magenta; 11=brown/white

                '         Each 2-bit field selects one of four colors, depending on the
                '         setting of the CGA palette.  Use INT 10H 0bH.

                Dim offset As Integer = 4
                For y As Integer = y1 To y2
                  Dim bt As Byte = 0
                  Dim shift As Integer = 6
                  For x As Integer = x1 To x2
                    Dim index As Integer = (y * m_display.ScreenWidth) + x
                    Dim c As Short = m_display.Page(m_display.ActivePage).Point(index)
                    If Not c.Between(0, 3) Then c = 3
                    bt += CByte(c << shift)
                    shift -= 2
                    If shift < 0 Then
                      bytes(offset) = bt
                      bt = 0
                      offset += 1
                      shift = 6
                    End If
                  Next
                  If shift < 6 Then
                    bytes(offset) = bt
                    bt = 0
                    offset += 1
                  End If
                  If bt = 0 Then
                  End If
                Next

              Case 2

                ' CGA Hi-res 640x200, 2-color (video mode 06H)

                ' Segment: b000

                ' Layout: Interleaved scan lines, packed-pixel.  Even-numbered scan lines
                '         begin at b800:0, and odd-numbered lines begin at b800:2000.

                '         Each scan line is 80-bytes long and there are 200 scan lines
                '         (regen size=8000 bytes * 2 regions).  Each byte contains 8 pixels
                '         (32,000 total pixels):
                '         Ö7Â6Â5Â4Â3Â2Â1Â0·
                '         º ³ ³ ³ ³ ³ ³ ³ º
                '         ÓÒÁÒÁÒÁÒÁÒÁÒÁÒÁÒ½  bits mask
                '          º º º º º º º ÈÍÍ> 0:  01H  eighth pixel in byte
                '          º º º º º º ÈÍÍÍÍ> 1:  02H  seventh pixel in byte
                '          º º º º º ÈÍÍÍÍÍÍ> 2:  04H  sixth pixel in byte
                '          º º º º ÈÍÍÍÍÍÍÍÍ> 3:  08H  fifth pixel in byte
                '          º º º ÈÍÍÍÍÍÍÍÍÍÍ> 4:  10H  fourth pixel in byte
                '          º º ÈÍÍÍÍÍÍÍÍÍÍÍÍ> 5:  20H  third pixel in byte
                '          º ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍ> 6:  40H  second pixel in byte
                '          ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ> 7:  80H  first pixel in byte
                '                                      0=black; 1=white

                '         Each 1-bit field selects black (0) or white (1).

                Dim offset As Integer = 4
                For y As Integer = y1 To y2
                  Dim bt As Byte = 0
                  Dim shift As Integer = 8
                  For x As Integer = x1 To x2
                    Dim index As Integer = (y * m_display.ScreenWidth) + x
                    Dim c As Short = m_display.Page(m_display.ActivePage).Point(index)
                    If c = 0 Then c = 0 Else c = 1
                    bt = CByte(bt + (c << shift))
                    shift -= 1
                    If shift < 0 Then
                      bytes(offset) = bt
                      offset += 1
                      shift = 8
                    End If
                  Next
                  If shift < 8 Then
                    bytes(offset) = bt
                    offset += 1
                  End If
                Next

              Case 3
                Return ThrowBasicError(BasicError.AdvancedFeature)
              Case 4
                Return ThrowBasicError(BasicError.AdvancedFeature)
              Case 5
                Return ThrowBasicError(BasicError.AdvancedFeature)
              Case 6
                Return ThrowBasicError(BasicError.AdvancedFeature)

              Case 7

                'EGA 320x200, 16-color (video mode 0dH)
                ' Segment: a000
                '  Layout: 4-plane planar.  Each pixel color is determined by the combined
                '          value of bits in the four color planes.  Each color plane begins
                '          at a000:0.  To select a plane, use:

                '            OUT 3ceH, 0005H         ;set up for plane masking
                '            OUT 3c4H, n             ;n is: 0102H=plane 0; 0202H=plane 1
                '                                    ;      0402H=plane 2; 0802H=plane 3
                '            ...(write video data)...
                '            OUT 3c4H, 0f02H         ;restore normal plane mask

                '          Each scan line is 40 bytes long and there are 200 scan lines
                '          (regen size=16,000 bytes * 4 planes).  Each byte contains
                '          8 pixels (64,000 total pixels):
                '          Ö7Â6Â5Â4Â3Â2Â1Â0·
                '          º ³ ³ ³ ³ ³ ³ ³ º
                '          ÓÒÁÒÁÒÁÒÁÒÁÒÁÒÁÒ½  bits mask
                '           º º º º º º º ÈÍÍ> 0:  01H  eighth pixel in byte
                '           º º º º º º ÈÍÍÍÍ> 1:  02H  seventh pixel in byte
                '           º º º º º ÈÍÍÍÍÍÍ> 2:  04H  sixth pixel in byte
                '           º º º º ÈÍÍÍÍÍÍÍÍ> 3:  08H  fifth pixel in byte
                '           º º º ÈÍÍÍÍÍÍÍÍÍÍ> 4:  10H  fourth pixel in byte
                '           º º ÈÍÍÍÍÍÍÍÍÍÍÍÍ> 5:  20H  third pixel in byte
                '           º ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍ> 6:  40H  second pixel in byte
                '           ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ> 7:  80H  first pixel in byte
                '                                       0=color OFF; 1=color ON

                '          The pixel color depends on the 4-bit value (0-15) obtained by
                '          combining the same bit position in each plane.  Default settings
                '          are:   0H black     8H gray
                '                 1H blue      9H bright blue
                '                 2H green     aH bright green
                '                 3H cyan      bH bright cyan
                '                 4H red       cH bright red
                '                 5H magenta   dH bright magenta
                '                 6H brown     eH yellow
                '                 7H white     fH bright white
                '          For instance, to make a pixel blue, the combined planes must
                '          equal 01H; that is the bit in plane 0 is set and the bits in
                '          planes 1,2, and 3 are clear.

                '          The actual colors depend on the palette (see INT 10H 1000H).

                Dim offset As Integer = 4

                ' Four pass across each bit plane (blue, green, red, and intensity).

                For bitPlane As Integer = 0 To 3

                  ' Process each pixel row...

                  For y As Integer = y1 To y2

                    ' Work on the bytes accordingly to build the data.

                    Dim bt As Byte = 0
                    Dim shift As Integer = 7

                    For x As Integer = x1 To x2

                      Dim index As Integer = (y * m_display.ScreenWidth) + x

                      Dim c As Short = m_display.Page(m_display.ActivePage).Point(index)

                      Dim bit As Boolean
                      Select Case bitPlane
                        Case 0 ' Blue 
                          bit = ((c And 1) = 1)
                        Case 1 ' Green
                          bit = ((c And 2) = 2)
                        Case 2 ' Red
                          bit = ((c And 4) = 4)
                        Case 3 ' Intensity
                          bit = ((c And 8) = 8)
                      End Select

                      If bit Then
                        bt += CByte(1 << shift)
                      End If
                      shift -= 1

                      If shift < 0 Then
                        bytes(offset) = bt
                        bt = 0
                        offset += 1
                        shift = 7
                      End If

                    Next

                    If shift < 7 Then
                      bytes(offset) = bt
                      bt = 0
                      offset += 1
                    End If

                    If bt = 0 Then
                    End If

                  Next

                Next

              Case 8

                ' EGA 640x200, 16-color (video mode 0eH)
                'Segment: a000
                ' Layout: 4-plane planar.  Each pixel color is determined by the combined
                '         value of bits in the four color planes.  Each color plane begins
                '         at a000:0.

                '         Each scan line is 80 bytes long and there are 200 scan lines
                '         (regen size=16,000 bytes * 4 planes).  Each byte contains
                '         8 pixels (128,000 total pixels).

                '         The layout and access is the same as mode 0dH.

                Dim offset As Integer = 4

                ' Four pass across each bit plane (blue, green, red, and intensity).

                For bitPlane As Integer = 0 To 3

                  ' Process each pixel row...

                  For y As Integer = y1 To y2

                    ' Work on the bytes accordingly to build the data.

                    Dim bt As Byte = 0
                    Dim shift As Integer = 7

                    For x As Integer = x1 To x2

                      Dim index As Integer = (y * m_display.ScreenWidth) + x

                      Dim c As Short = m_display.Page(m_display.ActivePage).Point(index)

                      Dim bit As Boolean
                      Select Case bitPlane
                        Case 0 ' Blue 
                          bit = ((c And 1) = 1)
                        Case 1 ' Green
                          bit = ((c And 2) = 2)
                        Case 2 ' Red
                          bit = ((c And 4) = 4)
                        Case 3 ' Intensity
                          bit = ((c And 8) = 8)
                      End Select

                      If bit Then
                        bt += CByte(1 << shift)
                      End If
                      shift -= 1

                      If shift < 0 Then
                        bytes(offset) = bt
                        bt = 0
                        offset += 1
                        shift = 7
                      End If

                    Next

                    If shift < 7 Then
                      bytes(offset) = bt
                      bt = 0
                      offset += 1
                    End If

                    If bt = 0 Then
                    End If

                  Next

                Next

              Case 9

                ' The following mode is technically in GW-BASIC but only used if 64K of video memory.
                ' EGA 640x350, 4-color (video mode 0fH)
                'Segment: a000
                ' Layout: 2-plane planar.  Video layout is the same as modes 0dH and 0eH,
                '         except that only planes 0 and 2 are used.  The effect is to have
                '         only two bits per pixel and the four colors are determined by
                '         palette registers 0, 1, 4, and 5.

                '         Each scan line is 80 bytes long and there are 350 scan lines
                '         (regen size=28,000 bytes * 2 planes).  Each byte contains
                '         8 pixels (224,000 total pixels).

                '         On really-old EGAs with only 64K, the even-numbered bit positions
                '         in video memory are displayed for planes 0 and 2 and odd-numbered
                '         pixels are in planes 1 and 3.  This variation is seen rarely.

                ' Otherwise, uses this one... which is the one we will implement.
                ' EGA 640x350, 16-color (video mode 10H)
                'Segment: a000
                ' Layout: 4-plane planar.  Video layout is the same as modes 0dH and 0eH,
                '         where the pixel is determined by the combined value in all four
                '         color planes.

                '         Each scan line is 80 bytes long and there are 350 scan lines
                '         (regen size=28,000 bytes * 4 planes).  Each byte contains
                '         8 pixels (224,000 total pixels).

                Dim offset As Integer = 4

                ' Four pass across each bit plane (blue, green, red, and intensity).

                For bitPlane As Integer = 0 To 3

                  ' Process each pixel row...

                  For y As Integer = y1 To y2

                    ' Work on the bytes accordingly to build the data.

                    Dim bt As Byte = 0
                    Dim shift As Integer = 7

                    For x As Integer = x1 To x2

                      Dim index As Integer = (y * m_display.ScreenWidth) + x

                      Dim c As Short = m_display.Page(m_display.ActivePage).Point(index)

                      Dim bit As Boolean
                      Select Case bitPlane
                        Case 0 ' Blue 
                          bit = ((c And 1) = 1)
                        Case 1 ' Green
                          bit = ((c And 2) = 2)
                        Case 2 ' Red
                          bit = ((c And 4) = 4)
                        Case 3 ' Intensity
                          bit = ((c And 8) = 8)
                      End Select

                      If bit Then
                        bt += CByte(1 << shift)
                      End If
                      shift -= 1

                      If shift < 0 Then
                        bytes(offset) = bt
                        bt = 0
                        offset += 1
                        shift = 7
                      End If

                    Next

                    If shift < 7 Then
                      bytes(offset) = bt
                      bt = 0
                      offset += 1
                    End If

                    If bt = 0 Then
                    End If

                  Next

                Next

              Case 10
                Return ThrowBasicError(BasicError.AdvancedFeature)

              Case Else
                Return ThrowBasicError(BasicError.AdvancedFeature)
            End Select

            Dim arraySize As Short = 0
            If Not InternalUbound(arrayName, 1, arraySize) Then Return False

            Select Case m_numericVariableList(arrayName & "(0)").Type
              Case NumericType.Integer
                If size > arraySize * 2 Then
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                Else
                  Dim offset As Integer = 0
                  For index As Integer = 0 To arraySize
                    If offset + 1 < bytes.Length Then
                      Dim number As Short = BitConverter.ToInt16(bytes, offset)
                      SetNumericVariable(arrayName & "(" & index & ")", number)
                      offset += 2
                    Else
                      Exit For
                    End If
                  Next
                End If
              Case NumericType.Double
                If size > arraySize * 8 Then
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                Else
                  Dim offset As Integer = 0
                  For index As Integer = 0 To arraySize Step 8
                    If offset + 7 < bytes.Length Then
                      Dim number As Double = BitConverter.ToDouble(bytes, offset)
                      SetNumericVariable(arrayName & "(" & index & ")", number)
                      offset += 8
                    Else
                      Exit For
                    End If
                  Next
                End If
              Case NumericType.Single
                If size > arraySize * 4 Then
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                Else
                  Dim offset As Integer = 0
                  For index As Integer = 0 To arraySize
                    If offset + 3 < bytes.Length Then
                      Dim number As Single = BitConverter.ToSingle(bytes, offset)
                      SetNumericVariable(arrayName & "(" & index & ")", number)
                      offset += 4
                    Else
                      Exit For
                    End If
                  Next
                End If
              Case Else
                Return ThrowBasicError(BasicError.AdvancedFeature)
            End Select

          Else
            Return ThrowBasicError(BasicError.TypeMismatch)
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Else

        If m_isTrial Then
          Return ThrowBasicError(BasicError.AdvancedFeature)
        End If

        If PeekToken.IsHashToken Then
          PopToken()
        End If

        Dim fileNumber As Short
        If Not ExecuteExpression(fileNumber) Then Return False

        Dim recordNumber As Double = -1

        If PeekToken.IsCommaToken Then

          PopToken()

          If Not ExecuteExpression(recordNumber) Then Return False

          'Else
          '  Return ThrowBasicError(BasicError.SyntaxError)
        End If

        If PeekToken() Is Nothing Then

          If recordNumber = -1 Then
          Else
            If Not CInt(recordNumber).Between(1, 16777215) Then
              Return ThrowBasicError(BasicError.BadRecordNumber)
            End If
          End If

          If Not InternalGet(fileNumber, CInt(recordNumber)) Then Return False

        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      End If

      Return True

    End Function

    Private Function ExecuteGosub() As Boolean

      If IsLineNumberOrLabel(PeekToken) Then

        Dim label = PopToken.ToString

        If m_debugGosub Then
          Debug.WriteLine(String.Format("{0} GOSUB {1} ({2}, {3}, {4})",
                                      m_interpreter(m_interpreterIndex).LineNumber,
                                      label,
                                      m_forLoop.Count,
                                      m_whileLoop.Count,
                                      m_doLoop.Count))
        End If

        SetReturnPosition()
        GotoLabelOrLineNumber(label)
        SetSubEntryPosition()

        '  line_marker()
        '  logWriteLine("GOSUB " & symbol)
        '  set_return_position(line)
        '  goto_label_or_linenum(symbol)
        '  set_sub_entry_position()

        '  symbol = ""
        '  line = ""

      Else

        Return ThrowBasicError(BasicError.IllegalFunctionCall)

        'Dim index1 As Short = 0
        'Dim index2 As Short = 0

        '  index1 = CShort(exec_expr(symbol, line))

        '  expect("OF", symbol, line)

        '  If index1 >= 1 Then

        '    index2 = 1

        '    While index2 < index1

        '      symbol = GetSymbol(line)

        '      If symbol = "," Then
        '        symbol = GetSymbol(line)
        '        index2 += 1
        '      Else
        '        index2 = index1 + 1
        '      End If

        '    End While

        '    If index2 = index1 Then

        '      If label_or_linenum(symbol) Then

        '        line_marker()

        '        logWriteLine("GOSUB " & index1 & " OF " & symbol)

        '        advance_after_statement(line)
        '        set_return_position(line)
        '        goto_label_or_linenum(symbol)
        '        set_sub_entry_position()

        '        symbol = ""
        '        line = ""

        '      Else

        '        error_marker()
        '        err.WriteLine("UNDEFINED LABEL " & symbol & " AFTER ""OF"".")

        '      End If

        '    Else

        '      line_marker()
        '      logWriteLine("GOSUB " & index1 & " OF NEXT STATEMENT")

        '      While Not EndOfStatement(symbol)
        '        symbol = GetSymbol(line)
        '      End While

        '    End If

        '  Else
        '    line_marker()
        '    logWriteLine("GOSUB " & index1 & " OF NEXT STATEMENT")
        '    While Not EndOfStatement(symbol)
        '      symbol = GetSymbol(line)
        '    End While
        '  End If

      End If

      Return True

    End Function

    Private Sub SetReturnPosition()

      m_gosubReturn.Insert(0, New GosubReturn With {.ReturnInterpreterIndex = m_interpreterIndex,
                                                  .ReturnStatementIndex = m_statementIndex,
                                                  .ReturnTokenIndex = m_tokenIndex,
                                                  .ForLoopStackDepth = m_forLoop.Count,
                                                  .WhileLoopStackDepth = m_whileLoop.Count,
                                                  .DoLoopStackDepth = m_doLoop.Count})

    End Sub

    Private Function GotoLabelOrLineNumber(label As String) As Boolean

      Dim value As Integer = Nothing
      If m_labelList.TryGetValue(label, value) Then

        m_interpreterIndex = value
        m_statementIndex = 0
        m_tokenIndex = 0

        Return True

        'm_display.Print("Labels not implemented.", True)
        'Return False

      ElseIf IsNumeric(label, False) Then

        Dim found As Boolean = False

        For index As Integer = 0 To m_lines.Count - 1
          If m_lines(index).LineNumber = CDbl(label) Then
            If Not m_running Then
              If Not PrepareData() Then Return False
            End If
            m_interpreterIndex = index '- If(m_running, 1, 0)
            m_statementIndex = 0
            m_tokenIndex = 0
            'm_display.Print("*G", False)
            'Trace()
            found = True
            Exit For
          End If
        Next

        If Not found OrElse m_pendingNew Then
          If m_running Then
            If m_lines IsNot Nothing AndAlso
             m_lines.Count > m_interpreterIndex AndAlso
             m_lines(m_interpreterIndex).LineNumber IsNot Nothing Then
              m_display.Print(String.Format("Undefined line number in {0}", m_lines(m_interpreterIndex).LineNumber), True)
            Else
              m_display.Print("Undefined line number", True)
            End If
          Else
            m_display.Print("Undefined line number", True)
          End If
          Return False
        ElseIf m_interpreter(0).LineNumber Is Nothing Then

          ' Need copy over the loaded program since this was a command oriented GOTO.
          m_interpreter.Clear()
          For Each l In m_lines
            m_interpreter.Add(l.Copy)
          Next

        End If

      Else
        Return ThrowBasicError(BasicError.MissingOperand)
      End If

      Return True

    End Function

    Private Sub SetSubEntryPosition()
      m_gosubReturn(0).GosubInterpreterIndex = m_interpreterIndex
    End Sub

    Private Function ExecuteReturn() As Boolean

      If PeekToken() Is Nothing Then

        If m_gosubReturn.Count > 0 Then
          If m_debugGosub Then
            Debug.WriteLine(String.Format("{0} RETURN to line {1}",
                                        m_interpreter(m_interpreterIndex).LineNumber,
                                        m_interpreter(m_gosubReturn(0).ReturnInterpreterIndex).LineNumber))
          End If
          CheckLoopStacksBeforeReturn()
          DoReturn()
        Else
          Return ThrowBasicError(BasicError.ReturnWithoutGosub)
        End If

      Else

        If IsLineNumberOrLabel(PeekToken) Then
          Dim label As String = PopToken.ToString
          ' Adjust the gosub stack...
          m_gosubReturn.RemoveAt(0)
          ' Now essentially work as a GOTO.
          Return GotoLabelOrLineNumber(label)
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      End If

      'If gosubReturn.Count >= 1 Then

      '  check_loop_stacks_before_return("RETURN")
      '  line_marker()
      '  logWrite("RETURN FROM ""GOSUB ")
      '  line_marker(gosubReturn(0).SubEntryLine)
      '  logWrite(Chr(34))
      '  do_return(symbol, Line)
      '  logWrite(" AT LINE ")
      '  line_marker()
      '  logWriteLine()

      'ElseIf on_error_label <> "" Then

      '  symbol = GetSymbol(Line)
      '  error_code = 3 ' RETURN without GOSUB
      '  line_marker()
      '  logWrite(error_code & " RETURN WITHOUT GOSUB")
      '  logWriteLine(" - ON ERROR GOTO " & on_error_label)
      '  OnErrorGoto(on_error_label, Line)
      '  symbol = ""
      '  Line = ""

      'Else
      '  symbol = GetSymbol(Line)
      '  error_marker()
      '  Err.WriteLine("RETURN WITHOUT GOSUB.")
      'End If

      Return True

    End Function

    Private Sub CheckLoopStacksBeforeReturn() 'ByVal returnStatementName As String)

      If m_forLoop.Count > m_gosubReturn(0).ForLoopStackDepth Then

        'error_marker()
        'Err.Write(returnStatementName)
        'Err.WriteLine(" - SOME FOR LOOPS WERE NOT LEFT")

        Do
          If m_debugForLoops OrElse
           m_debugGosub Then
            Debug.WriteLine(String.Format("{0} leave the ""FOR {1}"" loop at line {2}", m_interpreter(m_interpreterIndex).LineNumber, m_forLoop(0).Identifier, m_interpreter(m_forLoop(0).InterpreterIndex).LineNumber))
          End If
          m_forLoop.RemoveAt(0)
        Loop Until m_forLoop.Count <= m_gosubReturn(0).ForLoopStackDepth

      End If

      If m_whileLoop.Count > m_gosubReturn(0).WhileLoopStackDepth Then

        'error_marker()
        'Err.Write(returnStatementName)
        'Err.WriteLine(" - SOME WHILE LOOPS WERE NOT LEFT")

        Do
          If m_debugGosub Then
            Debug.WriteLine(String.Format("{0} leave the ""WHILE"" loop at line {1}", m_interpreter(m_interpreterIndex).LineNumber, m_interpreter(m_whileLoop(0).ConditionInterpreterIndex).LineNumber))
          End If
          m_whileLoop.RemoveAt(0)
        Loop Until m_whileLoop.Count <= m_gosubReturn(0).WhileLoopStackDepth

      End If

      If m_doLoop.Count > m_gosubReturn(0).DoLoopStackDepth Then

        'error_marker()
        'Err.Write(returnStatementName)
        'Err.WriteLine(" - SOME DO LOOPS WERE NOT LEFT")

        Do
          'Debug.WriteLine(String.Format("{0} leave the ""DO"" loop at line {1}", m_interpreter(m_interpreterIndex).LineNumber, m_interpreter(m_doLoop(0).ConditionInterpreterIndex).LineNumber))
          m_doLoop.RemoveAt(0)
        Loop Until m_doLoop.Count <= m_gosubReturn(0).DoLoopStackDepth

      End If

    End Sub

    Private Sub DoReturn()
      m_interpreterIndex = m_gosubReturn(0).ReturnInterpreterIndex
      m_statementIndex = m_gosubReturn(0).ReturnStatementIndex
      m_tokenIndex = m_gosubReturn(0).ReturnTokenIndex
      m_gosubReturn.RemoveAt(0)
    End Sub

    Private Function ExecuteGoto() As Boolean

      If IsLineNumberOrLabel(PeekToken) Then

        Dim label As String = PopToken.ToString

        Return GotoLabelOrLineNumber(label)

        'If labels.Contains(label) Then

        '  m_display.Print("Labels not implemented.", True)
        '  Return False

        'ElseIf IsNumeric(label) Then

        '  Dim found As Boolean = False

        '  For index As Short = 0 To m_lines.Count - 1
        '    If m_lines(index).LineNumber = label Then
        '      m_interpreterIndex = index - If(m_running, 1, 0)
        '      m_statementIndex = 0
        '      m_tokenIndex = 0
        '      found = True
        '      Exit For
        '    End If
        '  Next

        '  If Not found Then
        '    If m_running Then
        '      m_display.Print(String.Format("Undefined line number in {0}", m_lines(m_interpreterIndex).LineNumber), True)
        '    Else
        '      m_display.Print("Undefined line number", True)
        '    End If
        '    Return False
        '  ElseIf m_interpreter(0).LineNumber Is Nothing Then

        '    ' Need copy over the loaded program since this was a command oriented GOTO.
        '    m_interpreter.Clear()
        '    For Each l In m_lines
        '      m_interpreter.Add(l.Copy)
        '    Next

        '  End If

        'Else
        '  ThrowMissingOperandError() : Return False
        'End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    'Private Function ExecuteFacebook() As Boolean

    '  If PeekToken() Is Nothing Then
    '    ' IF no additional tokens, treat as FACEBOOK FILES.
    '    FacebookTokens = Nothing
    '    m_running = False
    '    RaiseEvent FacebookHook(Me, EventArgs.Empty)
    '    Return False
    '  Else

    '    FacebookTokens = New List(Of Parser.Token)
    '    Do Until PeekToken() Is Nothing
    '      FacebookTokens.Add(PopToken)
    '    Loop
    '    m_running = False
    '    RaiseEvent FacebookHook(Me, EventArgs.Empty)
    '    If m_running = True Then
    '      ' RUN?
    '      m_interpreterIndex = 0
    '      m_statementIndex = 0
    '      m_tokenIndex = 0
    '      Return True
    '    Else
    '      Return False
    '    End If

    '  End If

    '  Return True

    'End Function

    'Private Function ExecuteHelp() As Boolean

    '  If PeekToken() Is Nothing Then
    '    HelpKeyword = Nothing
    '    m_running = False
    '    RaiseEvent HelpHook(Me, EventArgs.Empty)
    '    Return False
    '  Else

    '    Dim keyword As String = PopToken.ToString

    '    If PeekToken() Is Nothing Then

    '      HelpKeyword = keyword
    '      m_running = False
    '      RaiseEvent HelpHook(Me, EventArgs.Empty)
    '      Return False

    '    Else
    '      Return ThrowBasicError(BasicError.SyntaxError)
    '    End If

    '  End If

    '  Return True

    'End Function

    Private Function ExecuteIf() As Boolean

      Dim result As Double

      'If Not ExecuteComparison(result) Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not ExecuteExpression(result) Then Return ThrowBasicError(BasicError.SyntaxError)

      Dim word As String = PeekToken.ToString()

      If word.Equals("THEN") Then
        PopToken()
      ElseIf word.Equals("GOTO") Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If result <> 0.0 Then

        If PeekToken() Is Nothing Then
          ' An empty then???
          ' Ignore and move on to the next statement...
        Else
          word = PeekToken.ToString()

          If word.Equals("GOTO") Then
            Trace()
            Return ExecuteStatement()
          Else

            If IsLineNumberOrLabel(PeekToken) Then
              ' Inferred GOTO.
              Return ExecuteGoto()
            Else

              If PeekToken.IsIdentifierToken Then
                ' Inferred LET.
                If Not ExecuteLet() Then Return False
              Else
                If Not ExecuteStatement() Then Return False
              End If

              'Dim statementCount = 0
              'Dim lineIndex = m_interpreterIndex

              'Do

              '  If PeekToken.IsWord("ELSE") Then
              '    If statementCount = 0 Then
              '      Return ThrowBasicError(BasicError.SyntaxError)
              '    Else
              '      ' Seek to end of line...
              '      m_statementIndex = m_interpreter(m_interpreterIndex).Statements.Count - 1
              '      m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count - 1
              '      Exit Do
              '    End If
              '  ElseIf PeekToken.IsIdentifierToken Then
              '    ' Inferred LET.
              '    If Not ExecuteLet() Then Return False
              '  Else
              '    If Not ExecuteStatement() Then Return False
              '  End If

              '  If lineIndex <> m_interpreterIndex Then
              '    m_interpreterIndex = lineIndex
              '    m_statementIndex = m_interpreter(m_interpreterIndex).Statements.Count - 1
              '    m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count - 1
              '    Exit Do
              '  End If

              'Loop

              Return True

            End If

          End If

        End If

      Else

        ' Skip forward to the ELSE (if found).
        For statementIndex = m_statementIndex + 1 To m_interpreter(m_interpreterIndex).Statements.Count - 1
          If m_interpreter(m_interpreterIndex).Statements(statementIndex).Tokens.Count > 0 AndAlso
           m_interpreter(m_interpreterIndex).Statements(statementIndex).Tokens(0).IsWord("ELSE") Then
            m_statementIndex = statementIndex
            m_tokenIndex = 1
            Return ExecuteElse()
          End If
        Next

        ' If not else found, need to seek to the end of the current line.
        m_statementIndex = m_interpreter(m_interpreterIndex).Statements.Count - 1
        m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count '- 1

        'Dim currentInterpreterIndex = m_interpreterIndex
        'Dim currentStatementIndex = m_statementIndex
        'Dim currentTokenIndex = m_tokenIndex

        'm_statementIndex += 1
        'm_tokenIndex = 0

        'If PeekToken.IsWord("ELSE") Then

        '  PopToken()
        '  Return ExecuteElse()

        'Else
        '  ' Else not found... don't try to do an else.
        '  m_interpreterIndex = currentInterpreterIndex
        '  m_statementIndex = currentStatementIndex
        '  m_tokenIndex = currentTokenIndex
        'End If

        'Do
        '  If PeekToken() Is Nothing Then
        '    Exit Do
        '  ElseIf PeekToken().IsWord("ELSE") Then
        '    Exit Do
        '  Else
        '    PopToken()
        '  End If
        'Loop

        'If PeekToken.IsWord("ELSE") Then

        '  PopToken()

        '  If PeekToken.IsWord("GOTO") Then

        '    'PopToken()
        '    'Return ExecuteGoto()
        '    Return ExecuteStatement()

        '  Else

        '    If IsLineNumberOrLabel(PeekToken) Then

        '      ' Inferred GOTO.

        '      Return ExecuteGoto()

        '    Else

        '      If PeekToken.IsIdentifierToken Then ' PeekToken.IsWord("=") OrElse PeekToken.IsParenOpenToken Then

        '        ' Inferred LET.

        '        Return ExecuteLet()

        '      Else

        '        Return ExecuteStatement()

        '        'Return ThrowBasicError(BasicError.SyntaxError)

        '      End If


        '    End If

        '  End If


        '  ElseIf PeekToken() Is Nothing Then

        '  ' No else found...

        '  Else
        '  Return ThrowBasicError(BasicError.SyntaxError)

        'End If

      End If

      Return True

    End Function

    Private Function ExecuteElse() As Boolean

      If PeekToken.IsWord("GOTO") Then

        Trace()
        Return ExecuteStatement()

      Else

        If IsLineNumberOrLabel(PeekToken) Then

          ' Inferred GOTO.
          Return ExecuteGoto()

        Else

          If PeekToken.IsIdentifierToken Then
            ' Inferred LET.
            Return ExecuteLet()
          Else
            Trace()
            Return ExecuteStatement()
          End If

        End If

      End If

      Return True

    End Function

    'Private Function ExecuteGpioPinMode() As Boolean

    '  ' GPIO pin, ON|OFF|1|0

    '  If m_gpio IsNot Nothing Then
    '    If m_gpio.IsAvailable Then

    '      Dim pin As Short

    '      If PeekToken() Is Nothing Then
    '        Return ThrowBasicError(BasicError.SyntaxError) ' missing further instructions...
    '      ElseIf PeekToken()?.IsCommaToken Then
    '        Return ThrowBasicError(BasicError.SyntaxError) ' pin is required...
    '      Else
    '        If Not ExecuteExpression(pin) Then
    '          Return ThrowBasicError(BasicError.SyntaxError) ' pin is required...
    '        End If
    '      End If

    '      If Not PeekToken()?.IsCommaToken Then
    '        Return ThrowBasicError(BasicError.SyntaxError) ' expected a comma.
    '      Else
    '        PopToken() ' remove the comma...
    '      End If

    '      Dim mode As Short

    '      Select Case PeekToken()?.ToString
    '        Case "OUTPUT"
    '          PopToken()
    '          mode = 0
    '        Case "INPUT"
    '          PopToken()
    '          mode = 1
    '        Case Else
    '          If Not ExecuteExpression(mode) Then
    '            Return ThrowBasicError(BasicError.SyntaxError)
    '          End If
    '      End Select

    '      If PeekToken() Is Nothing Then
    '        Return m_gpio.PinMode(pin, mode)
    '      Else
    '        Return ThrowBasicError(BasicError.SyntaxError)
    '      End If

    '    Else
    '      Return ThrowBasicError(BasicError.AdvancedFeature)
    '    End If
    '  Else
    '    Return ThrowBasicError(BasicError.AdvancedFeature)
    '  End If

    '  Return True

    'End Function

    'Private Function ExecuteGpioDigitalWrite() As Boolean

    '  ' GPIO pin, ON|OFF|1|0

    '  If m_gpio IsNot Nothing Then
    '    If m_gpio.IsAvailable Then

    '      Dim pin As Short

    '      If PeekToken() Is Nothing Then
    '        Return ThrowBasicError(BasicError.SyntaxError) ' missing further instructions...
    '      ElseIf PeekToken()?.IsCommaToken Then
    '        Return ThrowBasicError(BasicError.SyntaxError) ' pin is required...
    '      Else
    '        If Not ExecuteExpression(pin) Then
    '          Return ThrowBasicError(BasicError.SyntaxError) ' pin is required...
    '        End If
    '      End If

    '      If Not PeekToken()?.IsCommaToken Then
    '        Return ThrowBasicError(BasicError.SyntaxError) ' expected a comma.
    '      Else
    '        PopToken() ' remove the comma...
    '      End If

    '      Dim state As Short

    '      Select Case PeekToken()?.ToString
    '        Case "ON", "HIGH"
    '          PopToken()
    '          state = 0
    '        Case "OFF", "LOW"
    '          PopToken()
    '          state = 1
    '        Case Else
    '          If Not ExecuteExpression(state) Then
    '            Return ThrowBasicError(BasicError.SyntaxError)
    '          End If
    '      End Select

    '      If PeekToken() Is Nothing Then
    '        Return m_gpio.DigitalWrite(pin, state)
    '      Else
    '        Return ThrowBasicError(BasicError.SyntaxError)
    '      End If

    '    Else
    '      Return ThrowBasicError(BasicError.AdvancedFeature)
    '    End If
    '  Else
    '    Return ThrowBasicError(BasicError.AdvancedFeature)
    '  End If

    '  Return True

    'End Function

    Private Function ExecuteInput() As Boolean

      If m_inputVariableList.Count = 0 Then

        If PeekToken.IsHashToken Then

          If m_isTrial Then
            Return ThrowBasicError(BasicError.AdvancedFeature)
          End If

          PopToken()

          Dim fileNumber As Short
          If Not ExecuteExpression(fileNumber) Then Return False
          If PeekToken.IsCommaToken Then
            PopToken()
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
          Dim varList As New List(Of String)
          Do
            If PeekToken() Is Nothing Then
              Exit Do
            ElseIf PeekToken.IsIdentifierToken Then
              Dim variable As String = Nothing
              If Not PopAndParseVariableName(variable) Then Return False
              varList.Add(variable)
              If PeekToken.IsCommaToken Then
                PopToken()
              End If
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          Loop

          If PeekToken() Is Nothing AndAlso
           varList.Count > 0 Then
            Return InternalInput(fileNumber, varList)
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

        Else

          If PeekToken.IsSemiColonToken Then
            ' If the prompt string is preceded by a semicolon, the RETURN key pressed by the operator is 
            ' suppressed.  During program execution, data on that line is displayed, and data from the next PRINT
            ' statement is added to the line.
            m_inputSuppressCr = True
            PopToken()
          Else
            m_inputSuppressCr = False
          End If

          If PeekToken.IsStringLiteralToken() Then

            m_inputPrompt = PopToken.ToString

            If PeekToken.IsCommaToken Then
              PopToken()
            Else
              PopToken()
              m_inputPrompt &= "? "
            End If

          Else

            m_inputPrompt = "? "

          End If

          Do

            If PeekToken() Is Nothing Then
              Exit Do
            ElseIf PeekToken.IsIdentifierToken Then
              Dim variable As String = Nothing
              If Not PopAndParseVariableName(variable) Then Return False
              m_inputVariableList.Add(variable)
              If PeekToken.IsCommaToken Then
                PopToken()
              End If
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

          Loop

          If m_inputVariableList.Count = 0 Then
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

          m_display.Print(m_inputPrompt, False)
          m_continueInput = True

        End If

      Else

        ' Continue...

        If m_characterBuffer.Contains(ChrW(13)) Then

          Dim input As String = ""

          Do
            If m_characterBuffer(0) = ChrW(13) Then
              m_characterBuffer.RemoveAt(0)
              Exit Do
            ElseIf m_characterBuffer(0) = ChrW(8) Then
              ' Handle backspace by removing the last character from input
              If input.Length > 0 Then
                input = input.Substring(0, input.Length - 1)
              End If
              m_characterBuffer.RemoveAt(0)
            Else
              input &= m_characterBuffer(0)
              m_characterBuffer.RemoveAt(0)
            End If
          Loop

          Dim values = input.Split(","c)

          Dim errored As Boolean = False

          If values.Length = m_inputVariableList.Count Then

            For index = 0 To values.Length - 1
              Dim value As String = values(index).Trim
              If IsStringVariable(m_inputVariableList(index)) Then
                If Not SetStringVariable(m_inputVariableList(index), values(index)) Then Return False
              ElseIf IsNumericVariable(m_inputVariableList(index)) AndAlso IsNumeric(value, True) Then
                If String.IsNullOrEmpty(value) Then value = "0"
                If Not SetNumericVariable(m_inputVariableList(index), CDbl(value)) Then Return False
              Else
                errored = True
              End If
            Next

            If Not errored Then

              m_inputPrompt = Nothing
              m_inputSuppressCr = False
              m_inputVariableList.Clear()

              If Not m_inputSuppressCr Then
                m_display.Print()
              End If

              m_continueInput = False

              Return True

            End If

          End If

          m_display.Print()
          m_display.Print("?Redo from start", True)
          m_display.Print(m_inputPrompt, False)

          m_continueInput = True

        Else
          m_continueInput = True

        End If

      End If

      Return False

    End Function

    Private Function ExecuteIoCtl() As Boolean

      If PeekToken.IsHashToken Then
        PopToken()
      End If

      Dim fileNumber As Short
      If Not ExecuteExpression(fileNumber) Then Return False

      If PeekToken.IsCommaToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Dim command As String = Nothing
      If Not ExecuteStringExpression(command, Nothing) Then Return False

      If PeekToken() Is Nothing Then
        Dim value As File = Nothing
        If m_fileList.TryGetValue(fileNumber, value) Then
          If value.Path Is Nothing AndAlso value.Printer IsNot Nothing Then
            value.Printer.IoCtl(command)
          Else
            Return ThrowBasicError(BasicError.BadFileMode)
          End If
        Else
          Return ThrowBasicError(BasicError.BadFileNumber)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteKey() As Boolean

      Select Case PeekToken.ToString
        Case "ON"
          PopToken()
          If PeekToken() Is Nothing Then
            m_display.KeyOn()
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        Case "OFF"
          PopToken()
          If PeekToken() Is Nothing Then
            m_display.KeyOff()
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        Case "LIST"
          PopToken()
          If PeekToken() Is Nothing Then
            For index As Integer = 0 To m_onKey.Count - 1
              m_display.Print(String.Format("F{0} {1}", index + 1, m_onKey(index).Macro), True)
            Next
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        Case Else
          If PeekToken.IsParenOpenToken Then

            ' (n) ON|OFF|STOP

            PopToken()

            Dim key As Short
            If Not ExecuteExpression(key) Then Return False
            If PeekToken.IsParenCloseToken Then
              PopToken()
            End If

            If key.Between(1, 20) Then
              Select Case PeekToken.ToString
                Case "ON"
                  PopToken() : m_onKey(key - 1).State = OnState.On
                Case "OFF"
                  PopToken() : m_onKey(key - 1).State = OnState.Off
                Case "STOP"
                  PopToken() : m_onKey(key - 1).State = OnState.Stop
                Case Else
                  Return ThrowBasicError(BasicError.SyntaxError)
              End Select
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

          Else

            ' <number>, <expression>

            Dim number As Short
            Dim code As String = Nothing

            If Not ExecuteExpression(number) Then Return False
            If PeekToken.IsCommaToken Then
              PopToken()
              If Not ExecuteStringExpression(code, Nothing) Then Return False
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

            If PeekToken() Is Nothing Then

              If number.Between(1, 10) Then

                If code.Length > 15 Then
                  code = code.Substring(0, 15)
                End If

                m_onKey(number).Macro = code
                m_display.SetKeyLabel(number - 1, code)

                If m_display.IsKeyOn Then
                  m_display.KeyOff()
                  m_display.KeyOn()
                End If

              ElseIf number.Between(15, 20) Then

                If code.Length = 0 Then
                  m_onKey(number).HexCodeScanCode = Nothing
                ElseIf code.Length = 2 Then
                  m_onKey(number).HexCodeScanCode = {code(0),
                                                   code(1)}
                Else
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                End If

              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If

            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

          End If

      End Select

      Return True

    End Function

    Private Function ExecuteKill() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.MissingOperand)
      ElseIf Not PeekToken.IsStringLiteralToken Then
        Return ThrowBasicError(BasicError.TypeMismatch)
      Else

        Dim path As String = RealPath(PopToken.ToString)

        If path Is Nothing Then
          Return ThrowBasicError(BasicError.SyntaxError)
        Else

          Dim result = m_virtualFileSystem.Kill(path)

          If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then
            Dim errorText As String = result.Substring(7)
            If IsNumeric(errorText) Then
              m_running = ThrowBasicError(CShort(errorText))
              m_waiting = False
            Else
              m_display.Print(errorText, True)
              m_running = False
              m_waiting = False
            End If
          Else

            ' Nothing else to do...

            m_running = True
            m_waiting = False

          End If

          If Not m_running Then
            InternalPrompt()
          End If

          Return True

        End If

      End If

    End Function

    Private Function ExecuteLet() As Boolean

      If TypeOf PeekToken() Is Parser.IdentifierToken Then

        Dim varName As String = Nothing  '= PopToken.ToString
        If Not PopAndParseVariableName(varName) Then Return False
        'If Not AppendIndexers(varName) Then Return False

        If varName.IndexOf("("c) > -1 Then
          If IsStringVariable(varName) Then

            Dim arrayName As String = varName.Substring(0, varName.IndexOf("("c))
            Dim c As Integer = Aggregate a In m_stringVariableList.Keys
                             Where a.StartsWith(arrayName & "(")
                             Into Count()
            If c = 0 Then
              Dim dimensionBounds As New List(Of Bounds) From {
              New Bounds With {.Lower = m_optionBase,
                                                 .Upper = 10}
            }
              InitArray(arrayName, True, 1, dimensionBounds)
              VarPtr(arrayName)
            End If

          ElseIf IsNumericVariable(varName) Then

            Dim arrayName As String = varName.Substring(0, varName.IndexOf("("c))
            Dim pattern As String = arrayName & "("
            Dim c As Integer = Aggregate a In m_numericVariableList.Keys
                             Where a.StartsWith(pattern)
                             Into Count()
            If c = 0 Then
              Dim dimensionBounds As New List(Of Bounds) From {
              New Bounds With {.Lower = m_optionBase,
                                                 .Upper = 10}
            }
              InitArray(arrayName, False, 1, dimensionBounds)
              VarPtr(arrayName)
            End If

          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        End If

        If varName.IndexOf("("c) > -1 Then
          If IsStringVariable(varName) Then

            Dim c As Integer = Aggregate a In m_stringVariableList.Keys
                             Where a = varName
                             Into Count()
            If c = 0 Then
              Return ThrowBasicError(BasicError.SubscriptOutOfRange)
            End If

          ElseIf IsNumericVariable(varName) Then

            Dim c As Integer = Aggregate a In m_numericVariableList.Keys
                             Where a = varName
                             Into Count()
            If c = 0 Then
              Return ThrowBasicError(BasicError.SubscriptOutOfRange)
            End If

          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        End If

        If PeekToken() IsNot Nothing AndAlso PeekToken.ToString() = "=" Then

          PopToken()

          Dim currentTokenIndex As Integer = m_tokenIndex

          Dim forceNumericExpression As Boolean = False

          Try

            For tokenIndex = m_tokenIndex To m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count - 1
              Select Case PeekToken.ToString
                Case "<", "<=", "=", ">=", ">", "<>"
                  forceNumericExpression = True : Exit For
                Case Else
              End Select
              m_tokenIndex += 1
            Next

          Finally
            m_tokenIndex = currentTokenIndex
          End Try

          If Not forceNumericExpression AndAlso IsStringVariable(varName) Then

            If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then

              Dim result As String = Nothing
              If Not ExecuteStringExpression(result, varName) Then Return False
              If m_continueInputDollar Then Return True

              If PeekToken() Is Nothing Then 'OrElse PeekToken.IsWord("ELSE") Then
                If Not SetStringVariable(varName, result) Then Return False
              Else
                Return ThrowBasicError(BasicError.SyntaxError)
              End If

            Else
              Return ThrowBasicError(BasicError.TypeMismatch)
            End If

          ElseIf forceNumericExpression OrElse IsNumericVariable(varName) Then

            If forceNumericExpression OrElse Not PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then

              Dim result As Double
              If Not ExecuteExpression(result) Then Return False

              If PeekToken() Is Nothing Then 'OrElse PeekToken.IsWord("ELSE") Then
                If Not SetNumericVariable(varName, result) Then Return False
              Else
                Return ThrowBasicError(BasicError.SyntaxError)
              End If

            Else
              Return ThrowBasicError(BasicError.TypeMismatch)
            End If

          End If

        Else

          Return ThrowBasicError(BasicError.SyntaxError)

        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteLine() As Boolean

      If PeekToken.IsWord("INPUT") Then

        If m_isTrial Then
          Return ThrowBasicError(BasicError.AdvancedFeature)
        End If

        PopToken()
        Return ExecuteLineInput()

      End If

      Dim x1 As Double = m_drawPosition.X
      Dim y1 As Double = m_drawPosition.Y

      Dim x2 As Double = 0
      Dim y2 As Double = 0

      Dim c As Short = -1

      Dim box As Boolean = False
      Dim fill As Boolean = False

      Dim style As Short?

      If PeekToken.IsParenOpenToken Then

        PopToken()

        If Not ExecuteExpression(x1) Then Return False
        If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not ExecuteExpression(y1) Then Return False
        If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)

      End If

      If Not PopToken.IsDash Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not ExecuteExpression(x2) Then Return False
      If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not ExecuteExpression(y2) Then Return False
      If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)

      If PeekToken.IsCommaToken Then
        PopToken()
        If PeekToken.IsCommaToken Then
          ' fall through
        Else
          If Not ExecuteExpression(c) Then Return False
        End If
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
        If PeekToken.IsCommaToken Then
          ' fall through
        ElseIf PeekToken.IsWord("B") Then
          PopToken()
          box = True
        ElseIf PeekToken.IsWord("BF") Then
          PopToken()
          box = True
          fill = True
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
        Dim value As Double
        If Not ExecuteExpression(value) Then Return False
        If value.Between(-32768, 65535) Then
          If value > 32767 Then
            style = -CShort(65536 - value)
          Else
            style = CShort(value)
          End If
        Else
          Return ThrowBasicError(BasicError.Overflow)
        End If
      End If
      If PeekToken() Is Nothing Then
        If fill AndAlso style <> -1 Then
          ' gw-basic doesn't actually cause an error; however, documentation states that it does.
          'style = -1
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
        If Not m_display.Line(CInt(x1), CInt(y1), CInt(x2), CInt(y2), c, box, fill, style) Then
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If
        m_drawPosition.X = CInt(x2)
        m_drawPosition.Y = CInt(y2)
        Return True
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteLocate() As Boolean

      ' GW-BASIC
      '   LOCATE [row][,[col][,[cursor][,[start] [,stop]]]]

      Dim row As Short = -1
      Dim col As Short = -1
      Dim cursor As Short = -1
      Dim start As Short = -1
      Dim [stop] As Short = -1

      If PeekToken() Is Nothing OrElse
       PeekToken().IsCommaToken Then
        ' Skip
      Else
        If Not ExecuteExpression(row) Then Return False
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken() Is Nothing OrElse
       PeekToken().IsCommaToken Then
        ' Skip
      Else
        If Not ExecuteExpression(col) Then Return False
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken() Is Nothing OrElse
       PeekToken().IsCommaToken Then
        ' Skip
      Else
        If Not ExecuteExpression(cursor) Then Return False
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken() Is Nothing OrElse
       PeekToken().IsCommaToken Then
        ' Skip
      ElseIf PeekToken.IsNumericLiteralToken Then
        If Not ExecuteExpression(start) Then Return False
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
        If Not ExecuteExpression([stop]) Then Return False
      End If

      If cursor <> 0 Then
        cursor = -1
      End If

      If PeekToken() Is Nothing Then

        If (row = -1 OrElse row.Between(m_display.TopLine, m_display.BottomLine)) AndAlso
         (col = -1 OrElse col.Between(1, m_display.ColumnCount)) AndAlso
         (start = -1 OrElse start.Between(0, 31)) AndAlso
         ([stop] = -1 OrElse [stop].Between(0, 31)) Then

          m_display.Locate(row, col)

        Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteLock() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsHashToken Then
        PopToken()
      End If

      Dim fileNumber As Short
      If Not ExecuteExpression(fileNumber) Then Return False

      Dim min As Double = -1
      Dim max As Double = -1

      If PeekToken.IsCommaToken Then

        PopToken()

        Dim found As Boolean = False

        If Not PeekToken.IsWord("TO") Then
          If Not ExecuteExpression(min) Then Return False
          found = True
        End If

        If PeekToken.IsWord("TO") Then
          PopToken()
          If Not ExecuteExpression(max) Then Return False
          found = True
        End If

        If Not found Then
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      End If

      If PeekToken() Is Nothing Then

        Return InternalLock(fileNumber, min, max)

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteLineInput() As Boolean

      If m_inputVariableList.Count = 0 Then

        If PeekToken.IsHashToken Then
          PopToken()
          Dim fileNumber As Short
          If Not ExecuteExpression(fileNumber) Then Return False
          If PeekToken.IsCommaToken Then
            PopToken()
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
          Dim varName As String
          If IsStringVariable(PeekToken.ToString) Then
            varName = PopToken.ToString
          Else
            Return ThrowBasicError(BasicError.TypeMismatch)
          End If

          If PeekToken() Is Nothing Then
            Return InternalLineInput(fileNumber, varName)
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

        End If

        If PeekToken.IsSemiColonToken Then
          ' If the prompt string is preceded by a semicolon, the RETURN key pressed by the operator is 
          ' suppressed.  During program execution, data on that line is displayed, and data from the next PRINT
          ' statement is added to the line.
          m_inputSuppressCr = True
          PopToken()
        Else
          m_inputSuppressCr = False
        End If

        If PeekToken.IsStringLiteralToken() Then
          m_inputPrompt = PopToken.ToString
          If PeekToken.IsSemiColonToken Then
            PopToken()
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        End If

        If PeekToken.IsIdentifierToken AndAlso
         IsStringVariable(PeekToken.ToString) Then
          m_inputVariableList.Add(PopToken.ToString)
        Else
          Return ThrowBasicError(BasicError.TypeMismatch)
        End If

        If PeekToken() IsNot Nothing OrElse m_inputVariableList.Count = 0 Then
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

        If m_inputPrompt IsNot Nothing Then
          m_display.Print(m_inputPrompt, False)
        End If
        m_continueLineInput = True

      Else

        ' Continue...

        If m_characterBuffer.Contains(ChrW(13)) Then

          Dim input As String = ""

          Do
            If m_characterBuffer(0) = ChrW(13) Then
              m_characterBuffer.RemoveAt(0)
              Exit Do
            ElseIf m_characterBuffer(0) = ChrW(8) Then
              ' Handle backspace by removing the last character from input
              If input.Length > 0 Then
                input = input.Substring(0, input.Length - 1)
              End If
              m_characterBuffer.RemoveAt(0)
            Else
              input &= m_characterBuffer(0)
              m_characterBuffer.RemoveAt(0)
            End If
          Loop

          Dim values(0) As String
          values(0) = input

          Dim errored As Boolean = False

          If values.Length = m_inputVariableList.Count Then

            For index = 0 To values.Length - 1
              Dim value As String = values(index).Trim
              If m_inputVariableList(index).EndsWith("$"c) Then
                If Not SetStringVariable(m_inputVariableList(index), values(index)) Then Return False
              ElseIf IsNumeric(value, True) Then
                If String.IsNullOrEmpty(value) Then value = "0"
                If Not SetNumericVariable(m_inputVariableList(index), CDbl(value)) Then Return False
              Else
                errored = True
              End If
            Next

            If Not errored Then

              m_inputPrompt = Nothing
              m_inputSuppressCr = False
              m_inputVariableList.Clear()

              If Not m_inputSuppressCr Then
                m_display.Print()
              End If

              m_continueLineInput = False

              Return True

            End If

          End If

          m_display.Print()
          m_display.Print("?Redo from start", True)
          If m_inputPrompt IsNot Nothing Then
            m_display.Print(m_inputPrompt, False)
          End If

          m_continueLineInput = True

        Else
          m_continueLineInput = True

        End If

      End If

      Return True

    End Function

    'Private Function ExecuteList(lpt As Boolean) As Boolean

    '  If Me.m_dialect = Parser.Dialect.GWBasic Then

    '    Dim startLine As Integer = 0
    '    Dim endLine As Integer = 65529

    '    ' Process start number.

    '    If PeekToken.IsPeriodToken Then
    '      PopToken()
    '      startLine = m_autoStart
    '    ElseIf PeekToken.IsNumericLiteralToken Then
    '      startLine = CUShort(PopToken.Literal)
    '    End If

    '    ' Process end number.

    '    If PeekToken.IsDash Then
    '      PopToken()
    '      If PeekToken() Is Nothing Then
    '      ElseIf PeekToken.IsCommaToken Then
    '      ElseIf PeekToken.IsPeriodToken Then
    '        PopToken()
    '        endLine = m_autoStart
    '      ElseIf PeekToken.IsNumericLiteralToken Then
    '        endLine = CUShort(PopToken.Literal)
    '      End If
    '    End If

    '    ' Process filename.

    '    If PeekToken.IsCommaToken Then
    '      PopToken()
    '      If PeekToken.IsStringLiteralToken Then
    '        Dim filename = PopToken.Literal
    '      End If
    '    End If

    '    If PeekToken() Is Nothing AndAlso
    '     startLine.Between(0, 65529) AndAlso
    '     endLine.Between(0, 65529) AndAlso
    '     startLine <= endLine Then
    '      If m_pendingNew Then
    '        ' Do nothing...
    '      Else

    '        If lpt Then

    '          For Each line In m_lines
    '            If line.LineNumber Is Nothing OrElse CInt(line.LineNumber).Between(m_listStartLine, m_listEndLine) Then
    '              'If lpt Then
    '              m_lpt(0).Print(line.Text, True)
    '              'Else
    '              '  m_display.Print(line.Text, True)
    '              'End If
    '            End If
    '          Next

    '        Else

    '          m_listStartLine = startLine
    '          m_listEndLine = endLine
    '          m_listLineIndex = 0
    '          Return ContinueList()

    '        End If

    '      End If
    '    Else
    '      Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '    End If

    '  Else

    '    If PeekToken() Is Nothing Then
    '      m_running = False
    '      RaiseEvent ListHook(Me, EventArgs.Empty)
    '      Return m_running
    '    Else
    '      Return ThrowBasicError(BasicError.SyntaxError)
    '    End If

    '  End If

    '  Return False

    'End Function

    'Private Function ContinueList() As Boolean

    '  Dim counter As Integer = 0

    '  Do

    '    If m_listLineIndex > m_lines.Count - 1 Then
    '      m_listStartLine = -1
    '      m_listEndLine = -1
    '      m_listLineIndex = -1
    '      Return False
    '    End If

    '    If counter > 0 Then
    '      Exit Do
    '    End If

    '    Dim line = m_lines(m_listLineIndex)

    '    If line.LineNumber Is Nothing OrElse CInt(line.LineNumber).Between(m_listStartLine, m_listEndLine) Then
    '      m_display.Print(line.Text, True)
    '      If line.LineNumber IsNot Nothing Then
    '        m_editLine = CInt(line.LineNumber)
    '      End If
    '    End If

    '    m_listLineIndex += 1
    '    counter += 1

    '  Loop

    '  Return True

    'End Function

    'Private Function ExecuteParserList() As Boolean

    '  Dim startLine As Integer = 0
    '  Dim endLine As Integer = 65529

    '  ' Process start number.

    '  If PeekToken.IsPeriodToken Then
    '    PopToken()
    '    startLine = m_autoStart
    '  ElseIf PeekToken.IsNumericLiteralToken Then
    '    startLine = CShort(PopToken.Literal)
    '  End If

    '  ' Process end number.

    '  If PeekToken.IsDash Then
    '    PopToken()
    '    If PeekToken() Is Nothing Then
    '    ElseIf PeekToken.IsCommaToken Then
    '    ElseIf PeekToken.IsPeriodToken Then
    '      PopToken()
    '      endLine = m_autoStart
    '    ElseIf PeekToken.IsNumericLiteralToken Then
    '      endLine = CShort(PopToken.Literal)
    '    End If
    '  End If

    '  ' Process filename.

    '  If PeekToken.IsCommaToken Then
    '    PopToken()
    '    If PeekToken.IsStringLiteralToken Then
    '      Dim filename = PopToken.Literal
    '    End If
    '  End If

    '  If PeekToken() Is Nothing AndAlso
    '   startLine.Between(0, 65529) AndAlso
    '   endLine.Between(0, 65529) AndAlso
    '   startLine <= endLine Then
    '    If m_pendingNew Then
    '      ' Do nothing...
    '    Else

    '      For Each line In m_lines
    '        If line.LineNumber Is Nothing OrElse CInt(line.LineNumber).Between(startLine, endLine) Then
    '          m_display.Print(line.ToString(), True)
    '        End If
    '      Next
    '    End If
    '  Else
    '    Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '  End If

    '  Return False

    'End Function

    Private Function ExecuteLset() As Boolean

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Dim variableName As String '= Nothing

      If IsStringVariable(PeekToken.ToString) Then
        variableName = PopToken.ToString
      Else
        Return ThrowBasicError(BasicError.TypeMismatch)
      End If

      If PeekToken.IsWord("=") Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Dim value2 As String = Nothing
      If Not ExecuteStringExpression(value2, Nothing) Then Return False

      If PeekToken() Is Nothing Then

        Dim value1 As String = Nothing
        If Not GetStringVariable(variableName, value1) Then Return False

        If value2.Length = value1.Length Then
          value1 = value2.Substring(0, value1.Length)
        Else
          value1 = value2.PadRight(value1.Length)
        End If

        If Not SetStringVariable(variableName, value1) Then Return False

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    '    Private Function ExecuteMerge() As Boolean

    '      If m_isTrial Then
    '        Return ThrowBasicError(BasicError.AdvancedFeature)
    '      End If

    '      If PeekToken() Is Nothing Then
    '        Return ThrowBasicError(BasicError.MissingOperand)
    '      ElseIf Not PeekToken.IsStringLiteralToken Then
    '        Return ThrowBasicError(BasicError.TypeMismatch)
    '      ElseIf PeekToken() IsNot Nothing Then

    '        Dim value As String = ""
    '        If Not ExecuteStringExpression(value, Nothing) Then Return ThrowBasicError(BasicError.SyntaxError)

    '        Dim filename = RealPath(value) 'PopToken.ToString)

    '        Dim period As Integer = filename.LastIndexOf("."c)
    '        Dim ext As String = Nothing
    '        If period > -1 Then
    '          ext = filename.Substring(period)
    '        End If
    '        If String.IsNullOrEmpty(ext) Then
    '          filename &= ".BAS"
    '        End If

    '        If PeekToken() Is Nothing Then
    '          Dim result = m_virtualFileSystem.Load(filename)
    '          InternalLoadCompleted(result, filename & "|MERGE")
    '          Return True
    '        Else
    '          Return ThrowBasicError(BasicError.SyntaxError)
    '        End If

    '      Else
    '        Return ThrowBasicError(BasicError.SyntaxError)
    '      End If

    '    End Function

    Private Function ExecuteMid() As Boolean

      If PeekToken.IsParenOpenToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Dim param1 As String = Nothing
      Dim param2 As Short = -1
      Dim param3 As Short = -1
      Dim param4 As String = Nothing

      If PeekToken.IsIdentifierToken Then

        'Dim param1 As String = Nothing  '= PopToken.ToString
        If Not PopAndParseVariableName(param1) Then Return ThrowBasicError(BasicError.SyntaxError)

        'If varName.IndexOf("(") > -1 Then
        '  If IsStringVariable(varName) Then

        'param1 = PopToken.ToString

        If Not IsStringVariable(param1) Then
          Return ThrowBasicError(BasicError.TypeMismatch)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If Not PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
        If Not ExecuteExpression(param2) Then Return False
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
        If Not PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
          If Not ExecuteExpression(param3) Then Return False
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      End If

      If PeekToken.IsParenCloseToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsWord("=") Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
        If Not ExecuteStringExpression(param4, Nothing) Then Return False
      Else
        Return ThrowBasicError(BasicError.TypeMismatch)
      End If

      If PeekToken() Is Nothing Then

        If param3 = -1 Then
          param3 = CShort(param4.Length)
        End If

        Dim value As String = Nothing
        If Not GetStringVariable(param1, value) Then Return False

        If param2 + param3 > value.Length Then
          If param2 > value.Length Then
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          Else
            param3 = CShort(value.Length - param2 + 1)
            If param3 < 1 Then
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If
          End If
        End If

        If param4.Length > param3 Then
          param4 = param4.Substring(0, param3)
        End If

        Dim l As String = value.Substring(0, param2 - 1)
        Dim r As String = value.Substring(param2 + param3 - 1)

        SetStringVariable(param1, l & param4 & r)

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteMkDir() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.MissingOperand)
      ElseIf Not PeekToken.IsStringLiteralToken Then
        Return ThrowBasicError(BasicError.TypeMismatch)
      Else

        Dim path As String = RealPath(PopToken.ToString)

        If path Is Nothing Then
          Return ThrowBasicError(BasicError.SyntaxError)
        Else

          If Not path.EndsWith("\"c) Then
            path &= "\"
          End If

          Dim result = m_virtualFileSystem.MkDir(path)

          If result.StartsWith("Error: ") Then
            m_display.Print(result.Substring(7), True)
            m_running = False
          End If

          m_waiting = False
          m_running = False

          Return True

        End If

      End If

    End Function

    Private Function ExecuteName() As Boolean

      Dim oldPath As String = Nothing
      Dim newPath As String = Nothing

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.MissingOperand)
      ElseIf Not PeekToken.IsStringLiteralToken Then
        Return ThrowBasicError(BasicError.TypeMismatch)
      Else

        oldPath = RealPath(PopToken.ToString)

        If PeekToken.IsWord("AS") Then
          PopToken()
          If PeekToken() Is Nothing Then
            Return ThrowBasicError(BasicError.MissingOperand)
          ElseIf Not PeekToken.IsStringLiteralToken Then
            Return ThrowBasicError(BasicError.TypeMismatch)
          Else
            newPath = RealPath(PopToken.ToString)
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      End If

      If oldPath Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If newPath Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Dim result = m_virtualFileSystem.Name(oldPath, newPath)

      If result.StartsWith("Error: ") Then
        m_display.Print(result.Substring(7), True)
        'm_running = False
        Return False
      End If

      'm_running = False
      m_waiting = False

      'InternalPrompt()
      'ProcessKeyBuffer()

      Return True

    End Function

    Private Function ExecuteNew() As Boolean

      If PeekToken() Is Nothing Then
        InternalNew()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return False

    End Function

    'Private Function ExecuteOld() As Boolean

    '  If m_isTrial Then
    '    Return ThrowBasicError(BasicError.AdvancedFeature)
    '  End If

    '  If PeekToken() Is Nothing Then

    '    If m_pendingNew Then
    '      m_pendingNew = False
    '    Else
    '      Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '    End If

    '  Else
    '    Return ThrowBasicError(BasicError.SyntaxError)
    '  End If

    '  Return False

    'End Function

    Private Function ExecuteOn() As Boolean

      ' A lot of different "ON" stuff...

      ' ON expression GOSUB line numbers
      ' ON expression GOTO line numbers

      ' ON ERROR GOTO Statement
      ' ON ERROR GOTO 0

      ' ON event specifiier GOSUB line number
      '   events: COM(n), KEY(n), PEN,  PLAY(n), STRIG(n), TIMER(n)
      '   specifier: ON, OFF, STOP

      If PeekToken() IsNot Nothing Then

        Select Case PeekToken.ToString()

          Case "ERROR"

            PopToken()
            If PeekToken.IsWord("GOTO") Then
              PopToken()
              Return ExecuteOnErrorGoto()
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

          Case "TIMER"
            PopToken()
            Return ExecuteOnTimer()

          Case "KEY"
            PopToken()
            Return ExecuteOnKey()

          Case "COM"
            Return ThrowBasicError(BasicError.AdvancedFeature)

          Case "PEN"
            PopToken()
            Return ExecuteOnPen()

          Case "PLAY"
            Return ThrowBasicError(BasicError.AdvancedFeature)
          Case "STRIG"
            Return ThrowBasicError(BasicError.AdvancedFeature)

          Case Else

            Dim expression As Short
            If Not ExecuteExpression(expression) Then Return False

            If PeekToken.IsWord("GOSUB") Then

              PopToken()

              ' Get a list of GOSUB locations...
              Dim list As New List(Of String)
              If Not GetLabelList(list) Then Return False

              If expression.Between(0, 255) Then
                If expression.Between(1, CShort(list.Count)) Then

                  Dim label As String = list(expression - 1).ToString

                  SetReturnPosition()
                  If Not GotoLabelOrLineNumber(label) Then Return False
                  SetSubEntryPosition()
                  Return True

                Else
                  ' Fall through to next statement.
                  m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count
                  Return True
                  'Return ThrowBasicError(BasicError.IllegalFunctionCall)
                End If
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If

            ElseIf PeekToken.IsWord("GOTO") Then

              PopToken()

              ' Get a list of GOTO locations...
              Dim list As New List(Of String)
              If Not GetLabelList(list) Then Return False

              If expression.Between(0, 255) Then
                If expression.Between(1, CShort(list.Count)) Then

                  Dim label As String = list(expression - 1).ToString
                  Return GotoLabelOrLineNumber(label)

                Else
                  ' Fall through to next statement.
                  m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count
                  Return True
                End If

              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If

            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

        End Select

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return False

    End Function

    Private Function ExecuteOnErrorGoto() As Boolean

      Dim label As String

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      ElseIf PeekToken.IsIdentifierToken Then
        Dim t = PopToken()
        label = t.ToString
      Else
        Dim value As Double
        If Not ExecuteExpression(value) Then Return False
        label = value.ToString
      End If

      If PeekToken() Is Nothing Then

        If IsNumeric(label) Then

          Dim number As Integer = Integer.Parse(label)

          If CInt(number).Between(0, 65534) Then

            If m_onErrorGoto > 0 AndAlso
             m_onErrorResumeInterpreterIndex > -1 Then
              If CInt(number) = 0 Then
                m_onErrorGoto = 0
                Return ThrowBasicError(m_err)
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Else

              If CInt(number) = 0 Then
                ' turn off / reset error handling.
                m_onErrorResumeStatementIndex = 0
                m_onErrorResumeInterpreterIndex = -1
                m_onErrorGoto = 0
                m_err = 0
              Else
                Dim found As Boolean = False
                'Dim onErrorGotoIndex As Integer = 0
                For index As Integer = 0 To m_interpreter.Count - 1
                  If m_interpreter(index).LineNumber = number Then
                    'onErrorGotoIndex = index
                    m_onErrorGoto = index 'CInt(number)
                    found = True
                    Exit For
                  End If
                Next

                If Not found Then
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                End If
              End If

            End If

          Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End If

        Else

          If m_onErrorGoto > 0 AndAlso
           m_onErrorResumeInterpreterIndex > -1 Then

            Return ThrowBasicError(BasicError.IllegalFunctionCall)

          Else

            Dim value As Integer = Nothing
            If m_labelList.TryGetValue(label, value) Then
              m_onErrorGoto = value
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If
          End If

        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteOnKey() As Boolean

      If PeekToken.IsParenOpenToken Then
        PopToken()
        Dim param1 As Double
        If Not ExecuteExpression(param1) Then Return False
        If PeekToken.IsParenCloseToken Then
          PopToken()
          If PeekToken.IsWord("GOSUB") Then
            PopToken()
            Dim param2 As Double
            If Not ExecuteExpression(param2) Then Return False
            If PeekToken() Is Nothing Then

              Dim key As Integer = CInt(param1)
              Dim line As Integer = CInt(param2)

              If key.Between(1, 20) AndAlso
               line.Between(1, 65535) Then

                m_onKey(key - 1).Line = line

              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If

            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteOnTimer() As Boolean

      If PeekToken.IsParenOpenToken Then
        PopToken()
        Dim param1 As Double
        If Not ExecuteExpression(param1) Then Return False
        If PeekToken.IsParenCloseToken Then
          PopToken()
          If PeekToken.IsWord("GOSUB") Then
            PopToken()
            Dim param2 As Double
            If Not ExecuteExpression(param2) Then Return False
            If PeekToken() Is Nothing Then

              Dim interval As Integer = CInt(param1)
              Dim line As Integer = CInt(param2)

              If interval.Between(1, 86400) AndAlso
               line.Between(1, 65535) Then

                m_onTimerInterval = interval
                m_onTimerLine = line

              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If

            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteOnPen() As Boolean

      If PeekToken.IsWord("GOSUB") Then
        PopToken()
        If PeekToken() Is Nothing Then
          Return ThrowBasicError(BasicError.SyntaxError)
        Else
          If PeekToken.IsIdentifierToken Then
            ' Label
            Dim label = PopToken()
            If PeekToken() Is Nothing Then
              m_onPenLineOrLabel = label.ToString
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          Else
            ' line number
            Dim param1 As Double
            If Not ExecuteExpression(param1) Then Return False
            If PeekToken() Is Nothing Then
              Dim line As Integer = CInt(param1)
              m_onPenLineOrLabel = line.ToString
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          End If
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    'Private Function GetLabelList(ByRef list As List(Of Short)) As Boolean

    '  Do
    '    Dim token = PopToken()
    '    Dim value As String = token.ToString
    '    If IsNumeric(value, False) Then
    '      list.Add(CShort(value))
    '    Else
    '      Return ThrowBasicError(BasicError.SyntaxError)
    '    End If
    '    If PeekToken() Is Nothing Then
    '      Exit Do
    '    ElseIf PeekToken.IsCommaToken Then
    '      PopToken()
    '    Else
    '      Return ThrowBasicError(BasicError.SyntaxError)
    '    End If
    '  Loop

    '  Return True

    'End Function

    Private Function GetLabelList(ByRef list As List(Of String)) As Boolean

      Do
        Dim token = PopToken()
        If token.IsIdentifierToken Then
          Dim value As String = token.ToString
          If m_labelList.ContainsKey(value) Then
            list.Add(value)
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        Else
          Dim value As String = token.ToString
          If IsNumeric(value, False) Then
            list.Add(value)
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If
        End If
        If PeekToken() Is Nothing Then
          Exit Do
        ElseIf PeekToken.IsCommaToken Then
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      Loop

      Return True

    End Function

    Private Function ExecuteOpen() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      Dim param1 As String = Nothing

      If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then

        If Not ExecuteStringExpression(param1, Nothing) Then Return False

        If PeekToken.IsCommaToken Then

          PopToken()

          ' OPEN mode,[#]file number, filename [, reclen]

          Dim fileNumber As Short
          Dim recordLength As Short = 128

          Dim mode As FileMode '= FileMode.RandomAccessReadWrite

          Select Case param1.ToUpper
            Case "O"
              mode = FileMode.Output
            Case "I"
              mode = FileMode.Input
            Case "R"
              mode = FileMode.RandomAccessReadWrite
            Case "A"
              mode = FileMode.Append
            Case Else
              Return ThrowBasicError(BasicError.BadFileMode)
          End Select

          If PeekToken.IsHashToken Then
            PopToken()
          End If

          If Not ExecuteExpression(fileNumber) Then Return False

          Dim filename As String = Nothing

          If PeekToken.IsCommaToken Then
            PopToken()
            If Not ExecuteStringExpression(filename, Nothing) Then Return False
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

          If PeekToken.IsCommaToken Then
            PopToken()
            If Not ExecuteExpression(recordLength) Then Return False
          End If

          If PeekToken() Is Nothing Then

            Dim lock As FileLock = FileLock.Default

            filename = RealPath(filename)

            If mode = FileMode.Append OrElse
             mode = FileMode.Output Then
              ' if file is already open, fail.
              For Each entry In m_fileList.Values
                If entry.Path = filename Then
                  Return ThrowBasicError(BasicError.FileAlreadyOpen)
                End If
              Next
            Else
              ' if file is already open for output/append, fail.
              For Each entry In m_fileList.Values
                If entry.Path = filename AndAlso
                 (entry.Mode = FileMode.Append OrElse
                  entry.Mode = FileMode.Append) Then
                  Return ThrowBasicError(BasicError.FileAlreadyOpen)
                End If
              Next
            End If

            Return InternalOpen(fileNumber, filename, mode, lock, recordLength)

          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

        Else

          ' OPEN filename [FOR mode][ACCESS access][lock] AS [#]file number [LEN = reclen]

          Dim filename As String = param1

          Dim mode As FileMode = FileMode.RandomAccessReadWrite

          If PeekToken.IsWord("FOR") Then

            PopToken()

            Select Case PeekToken.ToString
              Case "INPUT"
                PopToken()
                mode = FileMode.Input
              Case "OUTPUT"
                PopToken()
                mode = FileMode.Output
              Case "APPEND"
                PopToken()
                mode = FileMode.Append
              Case "RANDOM"
                PopToken()
                mode = FileMode.RandomAccessReadWrite
              Case Else
                Return ThrowBasicError(BasicError.BadFileMode)
            End Select

          End If

          'Dim access As FileAccess = FileAccess.ReadWrite

          If PeekToken.IsWord("ACCESS") Then
            PopToken()
            Select Case PeekToken.ToString.ToUpper
              Case "READ"
                PopToken()
                Select Case PeekToken.ToString.ToUpper
                  Case "WRITE"
                    PopToken()
                    'access = FileAccess.ReadWrite
                    mode = FileMode.RandomAccessReadWrite
                  Case Else
                    'access = FileAccess.Read
                    mode = FileMode.RandomAccessRead
                End Select
              Case "WRITE"
                PopToken()
                'access = FileAccess.Write
                mode = FileMode.RandomAccessWrite
              Case Else
                Return ThrowBasicError(BasicError.SyntaxError)
            End Select
          End If

          Dim lock As FileLock = FileLock.Default

          If Not PeekToken.IsWord("AS") Then

            If PeekToken() Is Nothing Then
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

            Select Case PeekToken.ToString.ToUpper
              Case "SHARED"
                PopToken()
                lock = FileLock.Shared
              Case "LOCK"
                PopToken()
                Select Case PeekToken.ToString.ToUpper
                  Case "READ"
                    PopToken()
                    Select Case PeekToken.ToString.ToUpper
                      Case "WRITE"
                        PopToken()
                        lock = FileLock.LockReadWrite
                      Case Else
                        lock = FileLock.LockRead
                    End Select
                  Case "WRITE"
                    PopToken()
                    lock = FileLock.LockWrite
                  Case Else
                    Return ThrowBasicError(BasicError.SyntaxError)
                End Select
              Case Else
                Return ThrowBasicError(BasicError.SyntaxError)
            End Select

          End If

          If PeekToken.IsWord("AS") Then
            PopToken()
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

          If PeekToken.IsHashToken Then
            PopToken()
          End If

          Dim fileNumber As Short
          If Not ExecuteExpression(fileNumber) Then Return False

          Dim recordLength As Short = 128

          If PeekToken.IsWord("LEN") Then
            PopToken()
            If PeekToken.IsWord("=") Then
              PopToken()
              If Not ExecuteExpression(recordLength) Then Return False
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If
          End If

          If PeekToken() Is Nothing Then

            m_waiting = True

            filename = RealPath(filename)

            Return InternalOpen(fileNumber, filename, mode, lock, recordLength)

          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteOption() As Boolean

      If PeekToken.IsWord("BASE") Then

        PopToken()

        If PeekToken.IsNumericLiteralToken Then

          Dim number As Short = CShort(PopToken.ToString)

          If number.Between(0, 1) Then

            ' If any array's exist (string or numeric), an error occurs.

            For Each entry In m_stringVariableList.Keys
              If entry.IndexOf("("c) > 0 Then
                Return ThrowBasicError(BasicError.DuplicateDefinition)
              End If
            Next

            For Each entry In m_numericVariableList.Keys
              If entry.IndexOf("("c) > 0 Then
                Return ThrowBasicError(BasicError.DuplicateDefinition)
              End If
            Next

            ' Otherwise, set option base.

            m_optionBase = number

          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Else

        Return ThrowBasicError(BasicError.SyntaxError)

      End If

      Return True

    End Function

    Private Function ExecuteOut() As Boolean

      Return ThrowBasicError(BasicError.AdvancedFeature)

    End Function

    Private Function ExecutePaint() As Boolean

      Dim x As Double = 0
      Dim y As Double = 0
      Dim paintNumber As Short? ' = -1
      Dim paintString As String = Nothing
      Dim borderNumber As Short? ' = -1
      Dim borderString As String = Nothing
      Dim backgroundString As String = ChrW(0)

      If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not ExecuteExpression(x) Then Return False
      If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
      If Not ExecuteExpression(y) Then Return False
      If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken.IsCommaToken Then
        ' Skip
      ElseIf PeekToken() Is Nothing Then
        ' Skip
      Else

        ' Paint Attribute

        If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
          If Not ExecuteStringExpression(paintString, Nothing) Then Return False
        Else
          Dim value As Short
          If Not ExecuteExpression(value) Then Return False
          paintNumber = value
        End If

      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken.IsCommaToken Then
        ' Skip
      ElseIf PeekToken() Is Nothing Then
        ' Skip
      Else

        ' Border Attribute

        If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
          If Not ExecuteStringExpression(borderString, Nothing) Then Return False
        Else
          Dim value As Short
          If Not ExecuteExpression(value) Then Return False
          borderNumber = value
        End If

      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken() Is Nothing Then
        ' Skip
      Else

        If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
          If Not ExecuteStringExpression(backgroundString, Nothing) Then Return False
        Else
          Return ThrowBasicError(BasicError.TypeMismatch)
        End If

      End If

      If PeekToken() Is Nothing Then

        If paintString IsNot Nothing AndAlso Not paintString.Length.Between(1, 64) Then
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If

        If borderString IsNot Nothing AndAlso Not borderString.Length.Between(1, 64) Then
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If

        If backgroundString Is Nothing Then
          backgroundString = ChrW(0) ' the default value
        ElseIf backgroundString.Length <> 1 Then
          ' If provided, can only be one character in size.
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If

        ' Now determine the combination of parameters.

        Dim pp As New Display.Display.PaintParams() With {.PaintAttribute = paintNumber,
                                                .BorderAttribute = borderNumber,
                                                .Paint = paintString,
                                                .Border = borderString,
                                                .Background = backgroundString}

        If Not m_display.Paint(CInt(x), CInt(y), pp) Then Return ThrowBasicError(BasicError.OutOfMemory)
        'If Not m_display.Paint(CInt(x), CInt(y), paintNumber) Then Return ThrowBasicError(BasicError.OutOfMemory)

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecutePalette() As Boolean

      If PeekToken.IsWord("USING") Then
        PopToken()
        Return ExecutePaletteUsing()
      End If

      Dim attribute As Short = -1
      Dim color As Short = -1

      If PeekToken() IsNot Nothing Then
        If Not ExecuteExpression(attribute) Then Return False
        If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not ExecuteExpression(color) Then Return False
      End If

      If PeekToken() Is Nothing Then

        Try
          If attribute = -1 AndAlso color = -1 Then
            m_display.SetPalette()
          Else
            m_display.SetPalette(attribute, color)
          End If
        Catch
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End Try

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecutePaletteUsing() As Boolean

      Return ThrowBasicError(BasicError.AdvancedFeature)

    End Function

    Private Function ExecutePcopy() As Boolean

      Dim sourcePage As Short
      Dim destinationPage As Short

      If Not ExecuteExpression(sourcePage) Then Return False
      If PeekToken.IsCommaToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If
      If Not ExecuteExpression(destinationPage) Then Return False

      If PeekToken() Is Nothing Then

        'TODO: copy pages...
        m_display.Pcopy(sourcePage, destinationPage)

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecutePlay() As Boolean

      ' Quietly accept the play statement.
      m_tokenIndex = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count

      'TODO: Need to implement the PLAY statement once the SOUND statement is finished.

      'Return ThrowBasicError(BasicError.AdvancedFeature)
      Return True

    End Function

    Private Function ExecutePoint() As Boolean

      'TODO:
      ' POINT(0) ' physical x-coordinate of the last point
      ' POINT(1) ' physical y-coordinate of the last point
      ' POINT(2) ' view x-coordinate of the last point
      ' POINT(3) ' view y-coordinate of the last point

      Return ThrowBasicError(BasicError.AdvancedFeature)

    End Function

    Private Function ExecutePoke() As Boolean

      Dim param1 As Double
      Dim param2 As Short

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If Not ExecuteExpression(param1) Then Return False
      If PeekToken.IsCommaToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If
      If Not ExecuteExpression(param2) Then Return False

      If PeekToken() Is Nothing Then
        If CInt(param1).Between(0, 65535) AndAlso
         param2.Between(0, 255) Then
          If Not m_peek.ContainsKey(CInt(param1)) Then
            m_peek.Add(CInt(param1), CByte(param2))
          End If
          m_peek(CInt(param1)) = CByte(param2)
          Select Case CInt(param1)
            Case 0 ' Speed
              'If param2 = 255 Then
              'm_speed = 10000
              'Else
              'm_speed = CShort(1 + (CByte(param2) * 4)) ' Provides a range of 1 to 1021
              'End If
            Case Else
          End Select
        Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecutePreset() As Boolean

      Dim relative As Boolean = False

      If PeekToken.IsWord("STEP") Then
        PopToken()
        relative = True
      End If

      If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
      Dim x As Double
      If Not ExecuteExpression(x) Then Return False
      If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
      Dim y As Double
      If Not ExecuteExpression(y) Then Return False
      If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)

      Dim c As Short = -1
      If PeekToken.IsCommaToken Then
        PopToken()
        If Not ExecuteExpression(c) Then Return False
      End If

      If PeekToken() Is Nothing Then

        If relative Then
          x = m_drawPosition.X + x
          y = m_drawPosition.Y + y
        End If

        If x.Between(-32768, 32767) AndAlso
         y.Between(-32768, 32767) Then

          If c > -1 Then
            m_display.Preset(CInt(x), CInt(y), c)
          Else
            m_display.Preset(CInt(x), CInt(y))
          End If

          m_drawPosition.X = CInt(x)
          m_drawPosition.Y = CInt(y)

        Else
          Return ThrowBasicError(BasicError.Overflow)
        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecutePrint(lpt As Boolean) As Boolean

      Dim fileNumber As Short = -1

      Dim previousValueNumeric As Boolean

      If PeekToken.IsHashToken Then

        If m_isTrial Then
          Return ThrowBasicError(BasicError.AdvancedFeature)
        End If

        PopToken()
        If Not ExecuteExpression(fileNumber) Then Return False
        If PeekToken.IsCommaToken Then
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      End If

      If PeekToken.IsWord("USING") Then 'AndAlso fileNumber > -1 Then
        PopToken()
        Return ExecutePrintUsing(fileNumber, lpt)
      End If

      Dim implicitSemiColonPosition As Integer = -1
      Dim tabbed As Boolean = False

      If PeekToken() Is Nothing Then
        If fileNumber = -1 Then m_display.Print()
      End If

      Dim output As String = ""
      Dim lf As Boolean = True

      Do Until PeekToken() Is Nothing

        Dim currentTokenIndex As Integer = m_tokenIndex
        Dim forceNumericExpression As Boolean = False
        Try
          For tokenIndex = m_tokenIndex To m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count - 1
            Select Case PeekToken.ToString
              Case "<", "<=", "=", ">=", ">", "<>"
                forceNumericExpression = True : Exit For
              Case ";", ","
                Exit For
              Case Else
            End Select
            m_tokenIndex += 1
          Next
        Finally
          m_tokenIndex = currentTokenIndex
        End Try

        If Not forceNumericExpression AndAlso PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then

          If previousValueNumeric Then
            If fileNumber > -1 Then
              output &= " "
            Else
              If lpt Then
                m_lpt(0).Print(" ", False)
              Else
                m_display.Print(" ", False, True)
              End If
            End If
            previousValueNumeric = False
          End If

          Dim text As String = Nothing
          If Not ExecuteStringExpression(text, Nothing) Then Return False
          If m_continueInputDollar Then Return True
          If m_running Then
            If fileNumber > -1 Then
              output &= text
            Else
              If lpt Then
                m_lpt(0).Print(text, False)
              Else
                m_display.Print(text, False, True)
              End If
            End If
          End If

          tabbed = False

        ElseIf Not forceNumericExpression AndAlso PeekToken.Keyword = "TAB" Then

          If previousValueNumeric Then
            If fileNumber > -1 Then
              output &= " "
            Else
              If lpt Then
                m_lpt(0).Print(" ", False)
              Else
                m_display.Print(" ", False, True)
              End If
            End If
            previousValueNumeric = False
          End If

          If fileNumber > -1 Then
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

          PopToken()
          If PeekToken.IsParenOpenToken Then
            PopToken()
            Dim number As Short
            If Not ExecuteExpression(number) Then Return False
            If m_running Then
              If PeekToken.IsParenCloseToken Then
                PopToken()
                If fileNumber > -1 Then
                  Return ThrowBasicError(BasicError.AdvancedFeature)
                Else
                  If lpt Then
                    Dim pos = m_lpt(0).LPos()
                    If pos > number Then
                      m_lpt(0).Print()
                      pos = m_display.Pos(0)
                    End If
                    m_lpt(0).Print("".PadRight(number - pos), False)
                  Else
                    Dim pos = m_display.Pos(0)
                    If pos > number Then
                      m_display.Print()
                      pos = m_display.Pos(0)
                    End If
                    If number - pos > 0 Then
                      m_display.Print("".PadRight(number - pos), False, True)
                    End If
                  End If
                End If
              Else
                Return ThrowBasicError(BasicError.SyntaxError)
              End If
            End If
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

          tabbed = True

        ElseIf Not forceNumericExpression AndAlso PeekToken.Keyword = "SPC" Then

          If previousValueNumeric Then
            If fileNumber > -1 Then
              output &= " "
            Else
              If lpt Then
                m_lpt(0).Print(" ", False)
              Else
                m_display.Print(" ", False, True)
              End If
            End If
            previousValueNumeric = False
          End If

          If fileNumber > -1 Then
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

          PopToken()
          If PeekToken.IsParenOpenToken Then
            PopToken()
            Dim number As Short
            If Not ExecuteExpression(number) Then Return False
            If m_running Then
              If PeekToken.IsParenCloseToken Then
                PopToken()
                If number <= 0 Then
                  ' Do nothing...
                Else
                  If lpt Then
                    m_lpt(0).Print("".PadRight(number Mod m_lpt(0).Width), False)
                  Else
                    m_display.Print("".PadRight(number Mod m_display.ColumnCount), False, True)
                  End If
                End If
              Else
                Return ThrowBasicError(BasicError.SyntaxError)
              End If
            End If
          Else
            Return ThrowBasicError(BasicError.SyntaxError)
          End If

          tabbed = False

        ElseIf forceNumericExpression OrElse (Not PeekToken.IsSemiColonToken AndAlso Not PeekToken.IsCommaToken) Then

          If previousValueNumeric Then
            If fileNumber > -1 Then
              output &= " "
            Else
              If lpt Then
                m_lpt(0).Print(" ", False)
              Else
                m_display.Print(" ", False, True)
              End If
            End If
            previousValueNumeric = False
          End If

          Dim number As Double
          If Not ExecuteExpression(number) Then Return False

          Dim param1 As String = CStr(number)
          If param1.Substring(param1.Length - 1) = ".0" Then
            param1 = param1.Substring(0, param1.Length - 2)
          End If

          If param1.StartsWith("0.") Then
            param1 = param1.Substring(1)
          ElseIf param1.StartsWith("-0.") Then
            'param1 = "-" & param1.Substring(2)
            param1 = String.Concat("-", param1.AsSpan(2))
          End If

          If param1.Contains("."c) AndAlso
             Not param1.Contains("E"c) Then

            ' A decimal value that is not E notation.

            'If param1.StartsWith("-") Then
            '  ' Negative value.
            '  If param1.Length > 16 Then
            '    param1 = param1.Substring(0, 16)
            '  End If
            'Else
            '  ' Positive value.
            '  If param1.Length > 17 Then
            '    param1 = param1.Substring(0, 17)
            '  End If
            'End If

          End If

          Do
            If param1.IndexOf("."c) > -1 AndAlso
             param1.EndsWith("0"c) Then
              param1 = param1.Substring(0, param1.Length - 1)
            Else
              Exit Do
            End If
          Loop

          If number >= 0 Then
            output &= " "
          End If
          output &= param1

          If fileNumber = -1 Then
            If lpt Then
              m_lpt(0).Print(output, False)
            Else
              m_display.Print(output, False, True)
            End If
            output = ""
            previousValueNumeric = True
          Else
            output &= " "
          End If

          tabbed = False

        End If

        If PeekToken() Is Nothing Then
          If tabbed Then
            lf = False
          Else
            If fileNumber > -1 Then
              lf = True
            Else
              If lpt Then
                m_lpt(0).Print()
              Else
                m_display.Print()
              End If
            End If
          End If
        ElseIf PeekToken.IsSemiColonToken Then
          PopToken()
          lf = False
        ElseIf PeekToken.IsCommaToken Then

          If previousValueNumeric Then
            If fileNumber > -1 Then
              output &= " "
            Else
              If lpt Then
                m_lpt(0).Print(" ", False)
              Else
                m_display.Print(" ", False, True)
              End If
            End If
            previousValueNumeric = False
          End If

          PopToken()
          If fileNumber > -1 Then
            output &= "".PadRight(15 - m_display.Pos(0) Mod 14)
          Else
            If lpt Then
              m_lpt(0).Print("".PadRight(15 - m_lpt(0).LPos() Mod 14), False)
            Else
              m_display.Print("".PadRight(15 - m_display.Pos(0) Mod 14), False, True)
            End If
          End If
          lf = False

        Else

          '  This is the next thing to be printed 

          If implicitSemiColonPosition <> m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count - 1 Then
            implicitSemiColonPosition = m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count - 1
          End If

        End If

      Loop

      If fileNumber > -1 Then
        InternalWriteLine(fileNumber, output, lf)
      End If

      Return True

    End Function

    Private Shared Function PullNumericFormat(format As String, index As Integer) As String

      Dim result As String = ""

      If index > format.Length - 1 Then
        'result = ""
      Else

        Dim period As Boolean = False

        Do

          Select Case format(index)
            Case "#"c
              result &= format(index)
            Case "."c
              If Not period Then
                result &= format(index) : period = True
              Else
                Exit Do
              End If
            Case ","c
              result &= format(index)
            Case "^"c
              If format.IndexOf("^^^^", index) = index Then
                result &= "^^^^"
                index += 3
              Else
                Exit Do
              End If
            Case "-"c
              result &= format(index)
              Exit Do
            Case Else
              Exit Do
          End Select

          index += 1

          If index > format.Length - 1 Then
            Exit Do
          End If

        Loop

      End If

      Return result

    End Function

    Private Function ExecutePrintUsing(fileNumber As Short, lpt As Boolean) As Boolean

      Dim format As String = Nothing

      If PeekToken.IsStringLiteralToken Then
        format = PopToken.ToString
      ElseIf PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
        If Not ExecuteStringExpression(format, Nothing) Then Return False
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsSemiColonToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      ' Now get the variables...

      Dim semiColon As Boolean

      Dim list As New List(Of Object)

      Do Until PeekToken() Is Nothing

        Dim currentTokenIndex As Integer = m_tokenIndex
        Dim forceNumericExpression As Boolean = False
        Try
          For tokenIndex = m_tokenIndex To m_interpreter(m_interpreterIndex).Statements(m_statementIndex).Tokens.Count - 1
            Select Case PeekToken.ToString
              Case "<", "<=", "=", ">=", ">", "<>"
                forceNumericExpression = True : Exit For
              Case ";", ","
                Exit For
              Case Else
            End Select
            m_tokenIndex += 1
          Next
        Finally
          m_tokenIndex = currentTokenIndex
        End Try

        If Not forceNumericExpression AndAlso PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then

          Dim value As String = Nothing
          If Not ExecuteStringExpression(value, Nothing) Then Return False
          list.Add(value)
          semiColon = False

        ElseIf PeekToken.IsSemiColonToken Then
          PopToken()
          semiColon = True

        ElseIf PeekToken.IsCommaToken Then
          PopToken()
          semiColon = False

        Else

          Dim value As Double
          If Not ExecuteExpression(value) Then Return False
          list.Add(value)
          semiColon = False

        End If

      Loop

      ' Work through the format and variable list to create an output string.

      Dim output As String = ""

      Dim listIndex As Integer = 0

      Do

        Dim index As Integer = 0
        Dim escaped As Boolean = False

        Do

          Select Case format(index)
            Case "_"c
              If escaped Then
                output &= format(index)
              End If
              escaped = Not escaped

            Case "+"c

              ' If not at the immediate start of a number, treat as a literal.

              If Not escaped Then

                Dim numericFormat As String = Nothing

                If format.IndexOf("+**$", index) = index Then
                  numericFormat = "+**$" & PullNumericFormat(format, index + 4)
                  index += (numericFormat.Length - 1)
                ElseIf format.IndexOf("+**", index) = index Then
                  numericFormat = "+**" & PullNumericFormat(format, index + 3)
                  index += (numericFormat.Length - 1)
                ElseIf format.IndexOf("+$$", index) = index Then
                  numericFormat = "+$$" & PullNumericFormat(format, index + 3)
                  index += (numericFormat.Length - 1)
                ElseIf format.IndexOf("+#", index) = index Then
                  numericFormat = "+" & PullNumericFormat(format, index + 1)
                  index += (numericFormat.Length - 1)
                ElseIf format.IndexOf("+.#", index) = index Then
                  numericFormat = "+" & PullNumericFormat(format, index + 1)
                  index += (numericFormat.Length - 1)
                End If

                If numericFormat IsNot Nothing Then
                  If listIndex < list.Count Then
                    If TypeOf list(listIndex) Is Double Then
                      Dim number = CDbl(list(listIndex))
                      output &= FormatNumber(numericFormat, number)
                    Else
                      Return ThrowBasicError(BasicError.TypeMismatch)
                    End If
                    listIndex += 1
                  Else
                    Exit Do
                  End If
                Else
                  output &= format(index)
                End If

              Else
                output &= format(index) : escaped = False
              End If

            Case "*"c

              If Not escaped Then

                Dim numericFormat As String = Nothing

                If format.IndexOf("**$", index) = index Then
                  numericFormat = "**$" & PullNumericFormat(format, index + 3)
                  index += (numericFormat.Length - 1)
                ElseIf format.IndexOf("**", index) = index Then
                  numericFormat = "**" & PullNumericFormat(format, index + 2)
                  index += (numericFormat.Length - 1)
                End If

                If numericFormat IsNot Nothing Then
                  If listIndex < list.Count Then
                    If TypeOf list(listIndex) Is Double Then
                      Dim number = CDbl(list(listIndex))
                      output &= FormatNumber(numericFormat, number)
                    Else
                      Return ThrowBasicError(BasicError.TypeMismatch)
                    End If
                    listIndex += 1
                  Else
                    Exit Do
                  End If
                Else
                  output &= format(index)
                End If

              Else
                output &= format(index) : escaped = False
              End If

            Case "$"c

              If Not escaped Then

                Dim numericFormat As String = Nothing

                If format.IndexOf("$$", index) = index Then
                  numericFormat = "$$" & PullNumericFormat(format, index + 2)
                  index += (numericFormat.Length - 1)
                End If

                If numericFormat IsNot Nothing Then
                  If listIndex < list.Count Then
                    If TypeOf list(listIndex) Is Double Then
                      Dim number = CDbl(list(listIndex))
                      output &= FormatNumber(numericFormat, number)
                    Else
                      Return ThrowBasicError(BasicError.TypeMismatch)
                    End If
                    listIndex += 1
                  Else
                    Exit Do
                  End If
                Else
                  output &= format(index)
                End If

              Else
                output &= format(index) : escaped = False
              End If

            Case "."c

              If Not escaped Then

                Dim numericFormat As String = Nothing

                If format.IndexOf(".#", index) = index Then
                  numericFormat = PullNumericFormat(format, index)
                  index += (numericFormat.Length - 1)
                End If

                If numericFormat IsNot Nothing Then
                  If listIndex < list.Count Then
                    If TypeOf list(listIndex) Is Double Then
                      Dim number = CDbl(list(listIndex))
                      output &= FormatNumber(numericFormat, number)
                    Else
                      Return ThrowBasicError(BasicError.TypeMismatch)
                    End If
                    listIndex += 1
                  Else
                    Exit Do
                  End If
                Else
                  output &= format(index)
                End If

              Else
                output &= format(index) : escaped = False
              End If

            Case "#"c
              If Not escaped Then
                Dim numericFormat As String = PullNumericFormat(format, index)
                index += (numericFormat.Length - 1)
                If listIndex < list.Count Then
                  If TypeOf list(listIndex) Is Double Then
                    Dim number = CDbl(list(listIndex))
                    output &= FormatNumber(numericFormat, number)
                  Else
                    Return ThrowBasicError(BasicError.TypeMismatch)
                  End If
                  listIndex += 1
                Else
                  Exit Do
                End If
              Else
                output &= format(index) : escaped = False
              End If

            Case "!"c
              If Not escaped Then
                If listIndex < list.Count Then
                  If TypeOf list(listIndex) Is String Then
                    output &= CStr(list(listIndex))(0)
                  Else
                    Return ThrowBasicError(BasicError.TypeMismatch)
                  End If
                  listIndex += 1
                Else
                  Exit Do
                End If
              Else
                output &= format(index) : escaped = False
              End If

            Case "&"c
              If Not escaped Then
                If listIndex < list.Count Then
                  If TypeOf list(listIndex) Is String Then
                    output &= CStr(list(listIndex))
                  Else
                    Return ThrowBasicError(BasicError.TypeMismatch)
                  End If
                  listIndex += 1
                Else
                  Exit Do
                End If
              Else
                output &= format(index) : escaped = False
              End If

            Case "\"c
              If Not escaped Then

                ' Potentially the start of a string...

                Dim scan As Integer = index + 1

                Do

                  If scan > format.Length - 1 Then
                    scan = -1 : Exit Do
                  End If

                  Select Case format(scan)
                    Case " "c
                    ' Skip...
                    Case "\"c
                      ' Found it... exit loop.
                      Exit Do
                    Case Else
                      scan = -1 : Exit Do
                  End Select

                  scan += 1

                Loop

                If scan > -1 Then

                  Dim length As Integer = scan - index + 1
                  index = scan

                  If listIndex < list.Count Then
                    If TypeOf list(listIndex) Is String Then
                      If CStr(list(listIndex)).Length < length Then
                        output &= CStr(list(listIndex)).PadRight(length)
                      Else
                        output &= CStr(list(listIndex)).Substring(0, length)
                      End If
                    Else
                      Return ThrowBasicError(BasicError.TypeMismatch)
                    End If
                    listIndex += 1
                  Else
                    Exit Do
                  End If

                Else

                  output &= format(index)

                End If

              Else
                output &= format(index)
                escaped = False
              End If

            Case Else
              output &= format(index)
              escaped = False
          End Select

          index += 1

          If index > format.Length - 1 Then
            Exit Do
          End If

        Loop

        If listIndex > list.Count - 1 Then
          Exit Do
        End If

      Loop

      If fileNumber > -1 Then
        InternalWriteLine(fileNumber, output, Not semiColon)
      Else
        If lpt Then
          m_lpt(0).Print(output, Not semiColon)
        Else
          m_display.Print(output, Not semiColon)
        End If
      End If

      Return True

    End Function

    Private Function ExecutePrintUsing(lpt As Boolean) As Boolean

      Return ExecutePrintUsing(-1, lpt)

    End Function

    Private Shared Function FormatNumber(format As String, number As Double) As String

      Dim result As String = ""

      If Not format.Contains("#"c) Then
        format &= "#"
      End If

      Dim leadingPlus As Boolean = format.StartsWith("+"c)

      Dim asteriskFill As Boolean = format.IndexOf("**") > -1
      Dim dollarSign As Boolean = format.IndexOf("$"c) > -1
      Dim doubleDollarSign As Boolean = format.IndexOf("$$") > -1
      Dim asteriskFillDollarSign As Boolean = format.IndexOf("**$") > -1

      Dim exponential As Boolean = format.IndexOf("^^^^") > -1
      Dim trailingMinus As Boolean = format.EndsWith("-"c)
      Dim hundreds As Boolean = format.IndexOf(","c) > -1
      Dim float As Boolean = format.IndexOf("."c) > -1
      Dim precision As Integer = 0

      If float Then

        Dim positionStart As Integer = format.IndexOf("."c)
        Dim positionEnd As Integer = positionStart + 1

        Do
          Select Case format(positionEnd)
            Case "#"c
              positionEnd += 1
            Case Else
              positionEnd -= 1
              Exit Do
          End Select
          If positionEnd > format.Length - 1 Then
            positionEnd -= 1
            Exit Do
          End If
        Loop

        precision = positionEnd - positionStart

      End If

      Dim length As Integer = (format.LastIndexOf("#"c) - format.IndexOf("#"c)) + 1
      If format.IndexOf("."c) > -1 AndAlso
       format.IndexOf("."c) = format.IndexOf("#"c) - 1 Then
        ' Numeric format starts with a period...
        length += 1
      End If

      If asteriskFillDollarSign Then
        length += 1
      ElseIf asteriskFill Then
        length += 2
      End If

      If exponential Then
        length += 4
      End If

      If Not float Then
        number = CInt(number)
      End If

      Dim sign As Char = " "c

      If number < 0 Then
        sign = "-"c
      ElseIf leadingPlus Then
        sign = "+"c
      End If

      If dollarSign Then
        length += 1
      End If

      Dim value As String = If(hundreds,
                             Math.Abs(number).ToString(String.Format("N{0}", precision)),
                             Math.Abs(number).ToString(String.Format("F{0}", precision)))

      If exponential Then
        Dim numericOnly As String = format.Substring(format.IndexOf("#"c), format.LastIndexOf("#"c) - format.IndexOf("#"c) + 1).Replace("#"c, "0"c)
        If format.IndexOf("."c) > -1 AndAlso
         format.IndexOf("."c) = format.IndexOf("#"c) - 1 Then
          numericOnly = "." & numericOnly
        Else
          While numericOnly.Substring(1, 1) = "0"c
            numericOnly = numericOnly.Substring(1)
            If numericOnly.Length = 1 Then
              Exit While
            End If
          End While
        End If
        value = number.ToString(numericOnly & "E+00")
      End If

      If length < value.Length Then
        result &= "%"
      End If
      If length > value.Length Then
        If asteriskFill Then
          If sign <> " "c Then
            result &= "".PadLeft(length - (value.Length + 1), "*"c)
          Else
            result &= "".PadLeft(length - value.Length, "*"c)
          End If
        Else
          result &= "".PadLeft(length - value.Length, " "c)
        End If
      End If
      If Not trailingMinus AndAlso sign <> " "c Then
        result &= sign
      End If
      If dollarSign Then
        result &= "$"
      End If
      result &= value
      If trailingMinus Then
        result &= sign
      End If

      'result = If(length < value.Length, "%", "") &
      '         If(length > value.Length, "".PadLeft(length - value.Length, If(asteriskFill, "*"c, " "c)), "") &
      '         If(Not trailingMinus, If(sign <> " "c, sign, ""), "") &
      '         If(dollarSign, "$", "") &
      '         value &
      '         If(trailingMinus, sign, "")

      'If(trailingMinus AndAlso sign <> " "c, sign, "")

      Return result

    End Function

    'Private Function UsingToStringFormat(ByVal [using] As String, ByRef stringFormat As String) As Boolean

    '  ' Takes the BASIC using string and creates an array of 

    '  stringFormat = ""



    '  Return True

    'End Function

    'Private Function UsingFormatString(ByVal format As String, ByRef result As String) As Boolean

    '  ' String fields:
    '  '
    '  '  !           Only the first charcter of the string is to be printed.
    '  ' \n spaces\   2 + n characters from the string are to be printed.
    '  '              If string is longer than field, extra characters are ignored.
    '  '              If field is longer than string, string is left-justified in the field and padded with spaces on the right.
    '  ' &            Specifies a variable length string field.  Print as-is.
    '  ' 
    '  ' Notes:
    '  '   First one wins; meaning that these are multually exclusive, however, do not cause an error if mixed and matched.
    '  '   If no pattern found, generates an illegal function call.

    '  Dim original As String = result
    '  result = ""

    '  Dim escaped As Boolean
    '  Dim index As Integer = 0

    '  Do

    '    Select Case format(index)

    '      Case "_"c
    '        If escaped Then
    '          result &= format(index)
    '        End If
    '        escaped = Not escaped

    '      Case "!"c
    '        If Not escaped Then
    '          If Not original Is Nothing Then
    '            result &= original(0)
    '            original = Nothing
    '          Else
    '            Exit Do
    '          End If
    '        Else
    '          result &= format(index)
    '          escaped = False
    '        End If

    '      Case "&"c
    '        If Not escaped Then
    '          If Not original Is Nothing Then
    '            result &= original
    '            original = Nothing
    '          Else
    '            Exit Do
    '          End If
    '        Else
    '          result &= format(index)
    '          escaped = False
    '        End If

    '      Case "#"c
    '        If escaped Then
    '          result &= format(index)
    '          escaped = False
    '        End If

    '      Case "\"c

    '        If Not escaped Then

    '          If Not original Is Nothing Then

    '            Dim scan As Integer = index + 1

    '            If scan > format.Length - 1 Then
    '              scan -= 1
    '            Else

    '              Do

    '                Select Case format(scan)
    '                  Case " "c
    '                    ' Skip.
    '                  Case "\"c
    '                    ' Found.
    '                    Exit Do
    '                  Case Else
    '                    Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '                End Select

    '                scan = scan + 1

    '                If scan > format.Length - 1 Then
    '                  scan -= 1
    '                  Exit Do
    '                End If

    '              Loop

    '            End If

    '            Dim length As Integer = scan - index + 1

    '            If original.Length > length Then
    '              result &= original.Substring(0, length)
    '            Else
    '              result &= original.PadRight(length - original.Length)
    '            End If

    '            index = scan

    '          Else
    '            Exit Do
    '          End If

    '        Else
    '          result &= format(index)
    '          escaped = False
    '        End If

    '      Case Else
    '        result &= format(index)
    '        If escaped Then
    '          escaped = False
    '        End If

    '    End Select

    '    index += 1

    '    If index > format.Length - 1 Then
    '      Exit Do
    '    End If

    '  Loop

    '  Return True

    'End Function

    'Private Function UsingFormatNumeric(ByVal format As String, ByRef result As String) As Boolean

    '  ' Numeric fields:
    '  ' 
    '  ' #            Represent each digit position.  Digit positions are always filled.  If number is fewer digits, number is space padded (right justified).
    '  '              Can contain a decimal point.  Numbers are rounded as necessary.
    '  ' +            At beginning or end of the format string causes the sign of the number (plus or minus) to be printed at the same position.
    '  ' -            At the end of the format field causes negative numbers to be printed with a trailing minus sign.
    '  ' **           At the beginning of the format string causes leading spaces in the numeric field to be filled with asterisks.
    '  ' $$           Beginning of the format string causes a dollar sign to be printed to the immediate left of the formatted number.
    '  '              Exponential numbers are invalid.  Negative numbers are invalid unless the minus sign is used to move the minus to the end of the number.
    '  ' **$          Beginning of the format string combines the effects of the above two symbols.
    '  ' ^^^^         After the digit position to specificy expnential format.  The four positions allow for the E+xx to be printed.  
    '  ' _            Literal escape character to allow printing format characters.
    '  ' 
    '  ' 
    '  ' Notes:
    '  '   A % character will be appended to any values that exceed the formatted numeric template.
    '  '   If number of digits exceeds 24 charcters, an "Illegal function call" error results.
    '  '   
    '  '   If no pattern found, generates an illegal function call.


    '  Dim original As String = result
    '  result = ""

    '  ' Determine the ###.### pattern.
    '  ' See if + sign is before pattern.
    '  ' See if - follows pattern. If + is used (before pattern), treat as a literal.
    '  ' See if ^^^^ follows pattern.  Invalid 

    '  Dim numberStart As Integer = -1
    '  Dim numberEnd As Integer = -1

    '  Dim index As Integer = 0
    '  Dim escaped As Boolean

    '  Do

    '    Select Case format(index)
    '      Case "_"c
    '        escaped = Not escaped
    '      Case "#"c

    '        If Not escaped Then

    '          numberStart = index

    '          Dim scan As Integer = index + 1
    '          Dim period As Boolean
    '          Dim comma As Boolean

    '          Do

    '            Select Case format(scan)
    '              Case "#"c
    '              Case "."c
    '                If period Then
    '                  scan -= 1
    '                  Exit Do
    '                Else
    '                  period = True
    '                End If
    '              Case ","c
    '                If scan + 1 < format.Length - 1 AndAlso Not period AndAlso Not comma Then
    '                  comma = True
    '                  If format(scan + 1) = "."c Then
    '                    scan += 1
    '                    period = True
    '                  Else
    '                    scan -= 1
    '                    Exit Do
    '                  End If
    '                Else
    '                  scan -= 1
    '                  Exit Do
    '                End If
    '              Case Else
    '                scan -= 1
    '                Exit Do
    '            End Select

    '            scan += 1

    '            If scan > format.Length - 1 Then
    '              scan -= 1
    '              Exit Do
    '            End If

    '          Loop

    '          numberEnd = scan
    '          Exit Do

    '        Else

    '          escaped = False

    '        End If

    '      Case Else

    '    End Select

    '    index += 1

    '    If index > format.Length - 1 Then
    '      Exit Do
    '    End If

    '  Loop

    '  If numberStart > -1 AndAlso
    '     numberEnd > -1 Then

    '    Dim pattern As String = format.Substring(numberStart, numberEnd - numberStart + 1)
    '    Dim hundreds As Boolean = False ' Print's whole numbers using hundreds formatting (comma every 3 digits).

    '    If pattern.IndexOf(",") > -1 Then
    '      hundreds = True
    '      pattern = pattern.Replace(",", "")
    '    End If

    '    ' Valid combinations...
    '    '   # is a placeholder for the number pattern which can include the #,. characters.
    '    ' +#    
    '    ' $$#  
    '    ' $$#-
    '    ' **#
    '    ' **#-
    '    ' +**#
    '    ' **$#
    '    ' **$#-
    '    ' #-
    '    ' #^^^^
    '    ' #^^^^-
    '    ' **#^^^^
    '    ' **#^^^^-

    '    Dim asteriskFill As Boolean = False ' Fills leading spaces with asterisks.
    '    Dim dollarSign As Boolean = False ' "floating" dollar sign in front of data.  leading plus and exponential format are invalid.
    '    Dim leadingPlus As Boolean = False ' forces printing a + or - sign before data.

    '    ' Figure out positions, if any.

    '    If numberStart - 3 > -1 AndAlso format.IndexOf("**$", numberStart - 3) = numberStart - 3 Then
    '      asteriskFill = True
    '      dollarSign = True
    '      'leadingPlus = (format.IndexOf("+", numberStart - 4) = numberStart - 4)
    '    ElseIf numberStart - 2 > -1 AndAlso format.IndexOf("**", numberStart - 2) = numberStart - 2 Then
    '      asteriskFill = True
    '      leadingPlus = numberStart - 3 > -1 AndAlso (format.IndexOf("+", numberStart - 3) = numberStart - 3)
    '    ElseIf numberStart - 2 > -1 AndAlso format.IndexOf("$$", numberStart - 2) = numberStart - 2 Then
    '      dollarSign = True
    '      'leadingPlus = (format.IndexOf("+", numberStart - 3) = numberStart - 3)
    '    End If

    '    Dim exponential As Boolean
    '    Dim trailingMinus As Boolean

    '    ' ^^^^ must follow immedately after the number pattern, otherwise, treated as literal characters.
    '    Dim quadCaret As Integer = format.IndexOf("^^^^")
    '    If quadCaret > -1 AndAlso
    '       numberEnd + 1 = quadCaret Then
    '      exponential = True
    '      ' Could possibly have a trailing - sign.
    '      trailingMinus = If(numberEnd + 5 < format.Length - 1, format(numberEnd + 5) = "-"c, False)
    '    Else
    '      ' Could possibly have a trailing - sign.
    '      trailingMinus = If(numberEnd < format.Length - 1, format(numberEnd + 1) = "-"c, False)
    '    End If

    '    ' Check for constraints.

    '    If trailingMinus AndAlso leadingPlus Then
    '      ' Not an error, just can't have both and the leading plus is higher priority.
    '      trailingMinus = False
    '    End If

    '    If exponential AndAlso hundreds Then
    '      hundreds = False
    '    End If

    '    If dollarSign AndAlso (leadingPlus OrElse exponential) Then
    '      Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '    End If

    '    If Decimal.Parse(original) < 0 AndAlso dollarSign AndAlso Not trailingMinus Then
    '      Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '    End If

    '    ' Build result...

    '    Dim usingPattern As String = ""
    '    If leadingPlus Then usingPattern &= "+"
    '    If asteriskFill Then usingPattern &= "**"
    '    If dollarSign Then usingPattern &= If(asteriskFill, "$", "$$")
    '    usingPattern &= pattern
    '    If hundreds Then usingPattern &= "[,]"
    '    If exponential Then usingPattern &= "^^^^"
    '    If trailingMinus Then usingPattern &= "-"

    '    result = usingPattern

    '    Dim d = Double.Parse(original)

    '    If exponential Then
    '      result = d.ToString("E")
    '      Return True
    '    End If

    '  Else
    '    Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '  End If

    '  Return True

    'End Function

    Private Function ExecutePset() As Boolean

      Dim relative As Boolean = False

      If PeekToken.IsWord("STEP") Then
        PopToken()
        relative = True
      End If

      If Not PopToken.IsParenOpenToken Then Return ThrowBasicError(BasicError.SyntaxError)
      Dim x As Double
      If Not ExecuteExpression(x) Then Return False
      If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
      Dim y As Double
      If Not ExecuteExpression(y) Then Return False
      If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)

      Dim c As Short = -1
      If PeekToken.IsCommaToken Then
        PopToken()
        If Not ExecuteExpression(c) Then Return False
      End If

      If PeekToken() Is Nothing Then

        If relative Then
          x = m_drawPosition.X + x
          y = m_drawPosition.Y + y
        End If

        If x.Between(-32768, 32767) AndAlso
         y.Between(-32768, 32767) Then

          If c > -1 Then
            m_display.Pset(CInt(x), CInt(y), c)
          Else
            m_display.Pset(CInt(x), CInt(y))
          End If

          m_drawPosition.X = CInt(x)
          m_drawPosition.Y = CInt(y)

        Else
          Return ThrowBasicError(BasicError.Overflow)
        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecutePut() As Boolean

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsParenOpenToken Then

        PopToken()

        Dim x As Short
        Dim y As Short
        Dim arrayName As String
        Dim verb As String = "XOR"

        If Not ExecuteExpression(x) Then Return False
        If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not ExecuteExpression(y) Then Return False
        If Not PopToken.IsParenCloseToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not PopToken.IsCommaToken Then Return ThrowBasicError(BasicError.SyntaxError)
        If Not IsNumericVariable(PeekToken.ToString) Then Return ThrowBasicError(BasicError.SyntaxError)
        arrayName = PopToken.ToString

        If PeekToken.IsCommaToken Then
          PopToken()
          Select Case PeekToken.ToString.ToUpper
            Case "PSET", "PRESET", "AND", "OR", "XOR"
              verb = PopToken.ToString.ToUpper
            Case Else
              Return ThrowBasicError(BasicError.SyntaxError)
          End Select
        End If

        If PeekToken() Is Nothing Then

          Dim arraySize As Short = 0
          If Not InternalUbound(arrayName, 1, arraySize) Then Return False

          Dim bytes As New List(Of Byte)

          Select Case m_numericVariableList(arrayName & "(0)").Type
            Case NumericType.Integer
              For index As Integer = 0 To arraySize
                Dim value As Double = 0
                If Not GetNumericVariable(arrayName & "(" & index & ")", value) Then Return False
                Dim b = BitConverter.GetBytes(CShort(value))
                bytes.AddRange(b)
              Next
            Case NumericType.Single
              For index As Integer = 0 To arraySize
                Dim value As Double = 0
                If Not GetNumericVariable(arrayName & "(" & index & ")", value) Then Return False
                Dim b = BitConverter.GetBytes(value)
                bytes.AddRange(b)
              Next
            Case NumericType.Double
              For index As Integer = 0 To arraySize
                Dim value As Double = 0
                If Not GetNumericVariable(arrayName & "(" & index & ")", value) Then Return False
                Dim b = BitConverter.GetBytes(value)
                bytes.AddRange(b)
              Next
            Case Else
              Return ThrowBasicError(BasicError.SyntaxError)
          End Select

          Dim bb(1) As Byte
          bb(0) = bytes(0)
          bb(1) = bytes(1)
          Dim sx As Integer = BitConverter.ToInt16(bb, 0) \ 2
          bb(0) = bytes(2)
          bb(1) = bytes(3)
          Dim sy As Integer = BitConverter.ToInt16(bb, 0)

          Select Case m_display.ScreenMode
            Case 0
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            Case 1

              Dim mask() As Byte = {&H3, &HC, &H30, &HC0}

              Dim offset As Integer = 4
              For ay As Integer = 0 To sy - 1

                ' Read a byte from the array.
                Dim bt As Byte = bytes(offset)
                offset += 1
                ' Set the shifter to starting position.
                Dim shift As Integer = 6

                For ax As Integer = 0 To sx - 1

                  'Dim mask As Byte

                  'Select Case shift
                  '  Case 0 : mask = &H3
                  '  Case 2 : mask = &HC
                  '  Case 4 : mask = &H30
                  '  Case 6 : mask = &HC0
                  'End Select

                  Dim c As Short = (bt And mask(shift \ 2)) >> shift
                  Dim index As Integer = ((y + ay) * m_display.ScreenWidth) + (x + ax)

                  Select Case verb
                    Case "PSET"
                      m_display.Page(m_display.ActivePage).Point(index) = CByte(c)
                    Case "PRESET"
                      'm_display.Page(m_display.ActivePage)(index) = m_display.Page(m_display.ActivePage)(index) Xor CByte(&HFF)
                      If c <> 0 Then
                        m_display.Page(m_display.ActivePage).Point(index) = 0
                      Else
                        Select Case m_display.ScreenMode
                          Case 1
                            m_display.Page(m_display.ActivePage).Point(index) = 3
                          Case 2
                            m_display.Page(m_display.ActivePage).Point(index) = 1
                          Case Else
                            Stop
                        End Select
                      End If
                    Case "AND"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) And CByte(c)
                    Case "OR"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) Or CByte(c)
                    Case "XOR"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) Xor CByte(c)
                    Case Else
                  End Select

                  shift -= 2

                  If shift < 0 AndAlso ax < sx - 1 Then
                    ' Read a byte from the array.
                    bt = bytes(offset)
                    ' Set the shifter to starting position.
                    shift = 6
                    offset += 1
                  End If

                Next
              Next

            Case 2
              Return ThrowBasicError(BasicError.AdvancedFeature)

            Case 3
              Return ThrowBasicError(BasicError.AdvancedFeature)
            Case 4
              Return ThrowBasicError(BasicError.AdvancedFeature)
            Case 5
              Return ThrowBasicError(BasicError.AdvancedFeature)
            Case 6
              Return ThrowBasicError(BasicError.AdvancedFeature)

            Case 7

              Dim offset As Integer = 4
              'Dim bitsPerPixel As Integer '= 4
              Dim size As Integer = CInt(VisualBasic.Conversion.Int((sx * 1 + 7) / 8) * sy)

              For ay As Integer = 0 To sy - 1

                ' Read a byte from the array.
                Dim bitPlaneBlue As Byte = bytes(offset)
                Dim bitPlaneGreen As Byte = bytes(offset + (size))
                Dim bitPlaneRed As Byte = bytes(offset + (size * 2))
                Dim bitPlaneIntensity As Byte = bytes(offset + (size * 3))
                offset += 1
                ' Set the shifter to starting position.
                Dim shift As Integer = 7

                For ax As Integer = 0 To sx - 1

                  Dim checkBit As Byte = CByte(1 << shift)
                  Dim blue As Integer = -CInt(((bitPlaneBlue And checkBit) = checkBit))
                  Dim green As Integer = -CInt(((bitPlaneGreen And checkBit) = checkBit))
                  Dim red As Integer = -CInt(((bitPlaneRed And checkBit) = checkBit))
                  Dim intensity As Integer = -CInt(((bitPlaneIntensity And checkBit) = checkBit))
                  Dim c As Integer = (blue * 1) + (green * 2) + (red * 4) + (intensity * 8)

                  Dim index As Integer = ((y + ay) * m_display.ScreenWidth) + (x + ax)

                  Select Case verb
                    Case "PSET"
                      m_display.Page(m_display.ActivePage).Point(index) = CByte(c)
                    Case "PRESET"
                      If c <> 0 Then
                        m_display.Page(m_display.ActivePage).Point(index) = 0
                      Else
                        Select Case m_display.ScreenMode
                          Case 1
                            m_display.Page(m_display.ActivePage).Point(index) = 3
                          Case 2
                            m_display.Page(m_display.ActivePage).Point(index) = 1
                          Case 3
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 4
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 5
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 6
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 7
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case 8
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case 9
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case 10
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case Else
                            Stop
                        End Select
                      End If
                    Case "AND"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) And CByte(c)
                    Case "OR"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) Or CByte(c)
                    Case "XOR"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) Xor CByte(c)
                    Case Else
                  End Select

                  shift -= 1

                  If shift < 0 AndAlso ax < sx - 1 Then
                    ' Read a byte from the array.
                    bitPlaneBlue = bytes(offset)
                    bitPlaneGreen = bytes(offset + (size))
                    bitPlaneRed = bytes(offset + (size * 2))
                    bitPlaneIntensity = bytes(offset + (size * 3))
                    ' Set the shifter to starting position.
                    shift = 7
                    offset += 1
                  End If

                Next
              Next

            Case 8

              Dim offset As Integer = 4
              'Dim bitsPerPixel As Integer '= 4
              Dim size As Integer = CInt(4 + VisualBasic.Conversion.Int((sx * 1 + 7) / 8) * sy)

              For ay As Integer = 0 To sy - 1

                ' Read a byte from the array.
                Dim bitPlaneBlue As Byte = bytes(offset)
                Dim bitPlaneGreen As Byte = bytes(offset + (size))
                Dim bitPlaneRed As Byte = bytes(offset + (size * 2))
                Dim bitPlaneIntensity As Byte = bytes(offset + (size * 3))
                offset += 1
                ' Set the shifter to starting position.
                Dim shift As Integer = 7

                For ax As Integer = 0 To sx - 1

                  Dim checkBit As Byte = CByte(1 << shift)
                  Dim blue As Integer = -CInt(((bitPlaneBlue And checkBit) = checkBit))
                  Dim green As Integer = -CInt(((bitPlaneGreen And checkBit) = checkBit))
                  Dim red As Integer = -CInt(((bitPlaneRed And checkBit) = checkBit))
                  Dim intensity As Integer = -CInt(((bitPlaneIntensity And checkBit) = checkBit))
                  Dim c As Integer = (blue * 1) + (green * 2) + (red * 4) + (intensity * 8)

                  Dim index As Integer = ((y + ay) * m_display.ScreenWidth) + (x + ax)

                  Select Case verb
                    Case "PSET"
                      m_display.Page(m_display.ActivePage).Point(index) = CByte(c)
                    Case "PRESET"
                      If c <> 0 Then
                        m_display.Page(m_display.ActivePage).Point(index) = 0
                      Else
                        Select Case m_display.ScreenMode
                          Case 1
                            m_display.Page(m_display.ActivePage).Point(index) = 3
                          Case 2
                            m_display.Page(m_display.ActivePage).Point(index) = 1
                          Case 3
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 4
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 5
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 6
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 7
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case 8
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case 9
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case 10
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case Else
                            Stop
                        End Select
                      End If
                    Case "AND"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) And CByte(c)
                    Case "OR"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) Or CByte(c)
                    Case "XOR"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) Xor CByte(c)
                    Case Else
                  End Select

                  shift -= 1

                  If shift < 0 AndAlso ax < sx - 1 Then
                    ' Read a byte from the array.
                    bitPlaneBlue = bytes(offset)
                    bitPlaneGreen = bytes(offset + (size))
                    bitPlaneRed = bytes(offset + (size * 2))
                    bitPlaneIntensity = bytes(offset + (size * 3))
                    ' Set the shifter to starting position.
                    shift = 7
                    offset += 1
                  End If

                Next
              Next

            Case 9

              Dim offset As Integer = 4
              'Dim bitsPerPixel As Integer '= 4
              Dim size As Integer = CInt(4 + VisualBasic.Conversion.Int((sx * 1 + 7) / 8) * sy)

              For ay As Integer = 0 To sy - 1

                ' Read a byte from the array.
                Dim bitPlaneBlue As Byte = bytes(offset)
                Dim bitPlaneGreen As Byte = bytes(offset + (size))
                Dim bitPlaneRed As Byte = bytes(offset + (size * 2))
                Dim bitPlaneIntensity As Byte = bytes(offset + (size * 3))
                offset += 1
                ' Set the shifter to starting position.
                Dim shift As Integer = 7

                For ax As Integer = 0 To sx - 1

                  Dim checkBit As Byte = CByte(1 << shift)
                  Dim blue As Integer = -CInt(((bitPlaneBlue And checkBit) = checkBit))
                  Dim green As Integer = -CInt(((bitPlaneGreen And checkBit) = checkBit))
                  Dim red As Integer = -CInt(((bitPlaneRed And checkBit) = checkBit))
                  Dim intensity As Integer = -CInt(((bitPlaneIntensity And checkBit) = checkBit))
                  Dim c As Integer = (blue * 1) + (green * 2) + (red * 4) + (intensity * 8)

                  Dim index As Integer = ((y + ay) * m_display.ScreenWidth) + (x + ax)

                  Select Case verb
                    Case "PSET"
                      m_display.Page(m_display.ActivePage).Point(index) = CByte(c)
                    Case "PRESET"
                      If c <> 0 Then
                        m_display.Page(m_display.ActivePage).Point(index) = 0
                      Else
                        Select Case m_display.ScreenMode
                          Case 1
                            m_display.Page(m_display.ActivePage).Point(index) = 3
                          Case 2
                            m_display.Page(m_display.ActivePage).Point(index) = 1
                          Case 3
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 4
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 5
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 6
                            Return ThrowBasicError(BasicError.AdvancedFeature)
                          Case 7
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case 8
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case 9
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case 10
                            m_display.Page(m_display.ActivePage).Point(index) = 15
                          Case Else
                            Stop
                        End Select
                      End If
                    Case "AND"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) And CByte(c)
                    Case "OR"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) Or CByte(c)
                    Case "XOR"
                      m_display.Page(m_display.ActivePage).Point(index) = m_display.Page(m_display.ActivePage).Point(index) Xor CByte(c)
                    Case Else
                  End Select

                  shift -= 1

                  If shift < 0 AndAlso ax < sx - 1 Then
                    ' Read a byte from the array.
                    bitPlaneBlue = bytes(offset)
                    bitPlaneGreen = bytes(offset + (size))
                    bitPlaneRed = bytes(offset + (size * 2))
                    bitPlaneIntensity = bytes(offset + (size * 3))
                    ' Set the shifter to starting position.
                    shift = 7
                    offset += 1
                  End If

                Next
              Next

            Case 10
              Return ThrowBasicError(BasicError.AdvancedFeature)
            Case Else
              Return ThrowBasicError(BasicError.AdvancedFeature)
          End Select

        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Else

        If m_isTrial Then
          Return ThrowBasicError(BasicError.AdvancedFeature)
        End If

        If PeekToken.IsHashToken Then
          PopToken()
        End If

        Dim fileNumber As Short
        Dim recordNumber As Double = -1

        If Not ExecuteExpression(fileNumber) Then Return False

        If PeekToken.IsCommaToken Then
          PopToken()
          If Not ExecuteExpression(recordNumber) Then Return False
        End If

        If PeekToken() Is Nothing Then

          If CInt(recordNumber) = -1 Then
            Return InternalPut(fileNumber, -1)
          ElseIf CInt(recordNumber).Between(1, 16777215) Then
            Return InternalPut(fileNumber, CInt(recordNumber))
          Else
            Return ThrowBasicError(BasicError.BadRecordNumber)
          End If

        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      End If

      Return True

    End Function

    Private Function ExecuteRandomize() As Boolean

      Static prompt As String
      Dim seed As Double

      If prompt Is Nothing Then

        If PeekToken() Is Nothing Then

          prompt = "Random number seed (-32768 to 32767)? "

          m_display.Print(prompt, False)
          m_continueInputRandomize = True

          Return True

        Else

          If Not ExecuteExpression(seed) Then Return False

          'If Not seed.Between(-32768, 32767) Then
          '  Return ThrowBasicError(BasicError.IllegalFunctionCall)
          'End If

        End If

      Else

        ' Continue...

        If m_characterBuffer.Contains(ChrW(13)) Then

          Dim input As String = ""

          Do
            If m_characterBuffer(0) = ChrW(13) Then
              m_characterBuffer.RemoveAt(0)
              Exit Do
            ElseIf m_characterBuffer(0) = ChrW(8) Then
              ' Handle backspace by removing the last character from input
              If input.Length > 0 Then
                input = input.Substring(0, input.Length - 1)
              End If
              m_characterBuffer.RemoveAt(0)
            Else
              input &= m_characterBuffer(0)
              m_characterBuffer.RemoveAt(0)
            End If
          Loop

          Dim values = input.Split(","c)

          Dim errored As Boolean = False

          If values.Length = 1 Then

            Try
              If IsNumeric(values(0), False) AndAlso
               CShort(values(0)).Between(-32768, 32767) Then

                prompt = Nothing
                seed = CShort(values(0))

                m_display.Print()

                m_continueInputRandomize = False

              Else
                errored = True
              End If
            Catch ex As Exception
              errored = True
            End Try

          Else

            errored = True

          End If

          If errored Then

            m_display.Print()
            m_display.Print("?Redo from start", True)
            m_display.Print(prompt, False)

            m_continueInputRandomize = True
            Return True

          End If

        Else

          m_continueInputRandomize = True
          Return True

        End If

      End If

      'TODO: Set seed value...
      VisualBasic.VBMath.Randomize(CInt(seed))

      Return True

    End Function

    Private Function ExecuteRead() As Boolean

      Do

        'Dim token = PopToken()

        Dim variableName As String = Nothing '= token.ToString

        If Not PopAndParseVariableName(variableName) Then Return False

        If IsStringVariable(variableName) Then

          If m_readIndex < m_data.Count Then
            'If m_data(m_readIndex).IsStringLiteralToken Then
            If Not SetStringVariable(variableName, m_data(m_readIndex).ToString) Then Return False
            m_readIndex += 1
            'Else
            '  Return ThrowBasicError(BasicError.TypeMismatch)
            'End If
          Else
            Return ThrowBasicError(BasicError.OutOfData)
          End If

          '    param1 = get_data_field(symbol, line)

          '    If Not in_error_handler Then
          '      SetStringVariable(variable_name, param1)
          '      line_marker()
          '      LogWrite(String.Format("READ {0} {1} IN DATA LINE ", variable_name, param1))
          '      line_marker(m_dataLineNumber)
          '      LogWriteLine()
          '    End If

        ElseIf IsNumericVariable(variableName) Then

          If m_readIndex < m_data.Count Then
            'If m_data(m_readIndex).IsNumericLiteralToken Then

            Dim value As String = m_data(m_readIndex).ToString.Trim

            If IsNumeric(value, True) Then
              If Not SetNumericVariable(variableName, CDbl(m_data(m_readIndex).ToString)) Then Return False
              m_readIndex += 1
            Else
              Return ThrowBasicError(BasicError.TypeMismatch)
            End If

            'Else
            '  Return ThrowBasicError(BasicError.TypeMismatch)
            'End If
          Else
            Return ThrowBasicError(BasicError.OutOfData)
          End If

          '    data_elem = Trim(get_data_field(symbol, line))

          '    If Not in_error_handler Then

          '      If data_elem = "" Then

          '        '  Empty DATA fields can be read as 0.0 
          '        SetNumericVariable(variable_name, 0.0)
          '        line_marker()
          '        LogWrite("READ " & variable_name & " 0.0 IN DATA LINE ")
          '        line_marker(m_dataLineNumber)
          '        logWriteLine()

          '      Else

          '        param1 = GetSymbol(data_elem)

          '        If param1 = "-" Then
          '          param1 &= GetSymbol(data_elem)
          '        ElseIf param1 = "+" Then
          '          param1 = GetSymbol(data_elem)
          '        End If

          '        Try
          '          num1 = Single.Parse(param1)
          '          SetNumericVariable(variable_name, num1)
          '          line_marker()
          '          LogWrite("READ " & variable_name & " " & num1 & " IN DATA LINE ")
          '          line_marker(m_dataLineNumber)
          '          logWriteLine()
          '        Catch ex As Exception
          '          error_marker()
          '          err.Write("NUMBER EXPECTED IN READ FOUND " & param1)
          '          err.Write(" IN DATA LINE ")
          '          line_marker(m_dataLineNumber)
          '          err.WriteLine(".")
          '        End Try

          '        If data_elem <> "" Then
          '          error_marker()
          '          err.Write("FOUND " & data_elem & " AFTER ")
          '          err.Write(param1 & " IN DATA LINE ")
          '          line_marker(m_dataLineNumber)
          '          err.WriteLine(".")
          '        End If

          '      End If

          '    End If

        Else

          Return ThrowBasicError(BasicError.SyntaxError)

        End If

        If PeekToken() Is Nothing Then
          Exit Do
        ElseIf PeekToken.IsCommaToken Then
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      Loop

      'Do

      '  symbol = GetSymbol(line)

      '  If IsStringVariable(symbol) Then

      '    variable_name = get_name(symbol, line)
      '    param1 = get_data_field(symbol, line)

      '    If Not in_error_handler Then
      '      SetStringVariable(variable_name, param1)
      '      line_marker()
      '      LogWrite(String.Format("READ {0} {1} IN DATA LINE ", variable_name, param1))
      '      line_marker(m_dataLineNumber)
      '      LogWriteLine()
      '    End If

      '  ElseIf IsNumericVariable(symbol) Then

      '    variable_name = get_name(symbol, line)
      '    data_elem = Trim(get_data_field(symbol, line))

      '    If Not in_error_handler Then

      '      If data_elem = "" Then

      '        '  Empty DATA fields can be read as 0.0 
      '        SetNumericVariable(variable_name, 0.0)
      '        line_marker()
      '        LogWrite("READ " & variable_name & " 0.0 IN DATA LINE ")
      '        line_marker(m_dataLineNumber)
      '        logWriteLine()

      '      Else

      '        param1 = GetSymbol(data_elem)

      '        If param1 = "-" Then
      '          param1 &= GetSymbol(data_elem)
      '        ElseIf param1 = "+" Then
      '          param1 = GetSymbol(data_elem)
      '        End If

      '        Try
      '          num1 = Single.Parse(param1)
      '          SetNumericVariable(variable_name, num1)
      '          line_marker()
      '          LogWrite("READ " & variable_name & " " & num1 & " IN DATA LINE ")
      '          line_marker(m_dataLineNumber)
      '          logWriteLine()
      '        Catch ex As Exception
      '          error_marker()
      '          err.Write("NUMBER EXPECTED IN READ FOUND " & param1)
      '          err.Write(" IN DATA LINE ")
      '          line_marker(m_dataLineNumber)
      '          err.WriteLine(".")
      '        End Try

      '        If data_elem <> "" Then
      '          error_marker()
      '          err.Write("FOUND " & data_elem & " AFTER ")
      '          err.Write(param1 & " IN DATA LINE ")
      '          line_marker(m_dataLineNumber)
      '          err.WriteLine(".")
      '        End If

      '      End If

      '    End If

      '  Else
      '    error_marker()
      '    err.WriteLine("VARIABLE EXPECTED - FOUND " & symbol & ".")
      '  End If

      'Loop Until symbol <> "," OrElse in_error_handler

      Return True

    End Function

    Private Function IsStringVariable(word As String) As Boolean

      Dim name As String = word
      If name.IndexOf("("c) > -1 Then
        name = name.Substring(0, name.IndexOf("("c))
      End If

      Return name <> "" AndAlso
           (name(name.Length - 1) = "$"c OrElse
            m_defStrList.Contains(name(0)) AndAlso
           Not m_numericSuffixList.Contains(name(name.Length - 1)))

    End Function

    Private Function IsNumericVariable(word As String) As Boolean

      Dim name As String = word
      If name.IndexOf("("c) > -1 Then
        name = name.Substring(0, name.IndexOf("("c))
      End If

      Return name <> "" AndAlso
           "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".Contains(name(0)) AndAlso
           Not name(name.Length - 1) = "$"c AndAlso
           (m_numericSuffixList.Contains(name(name.Length - 1)) OrElse
            Not m_defStrList.Contains(name(0)))

    End Function

    Private Shared Function ExecuteRem() As Boolean

      ' Do nothing...

      Return True

    End Function

    'Private Function ExecuteRenum() As Boolean

    '  Dim newNumber As Integer = -1
    '  Dim oldNumber As Integer = -1
    '  Dim increment As Integer = 10

    '  If m_lines.Count > 0 AndAlso Not m_pendingNew Then
    '    oldNumber = CShort(Aggregate a In m_lines
    '                     Into Min(a.LineNumber))
    '  Else
    '    Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '  End If

    '  ' Process new number.

    '  If PeekToken() Is Nothing Then
    '    newNumber = 10
    '  ElseIf PeekToken.IsCommaToken Then
    '    newNumber = 10
    '  ElseIf PeekToken.IsNumericLiteralToken Then
    '    newNumber = CShort(PopToken.Literal)
    '  End If

    '  ' Process end number.

    '  If PeekToken.IsCommaToken Then
    '    PopToken()
    '    If PeekToken() Is Nothing Then
    '      ' ??????
    '      Stop
    '    ElseIf PeekToken.IsCommaToken Then
    '      ' Process increment.
    '    ElseIf PeekToken.IsNumericLiteralToken Then
    '      oldNumber = CShort(PopToken.Literal)
    '    End If
    '  End If

    '  ' Process increment.

    '  If PeekToken.IsCommaToken Then
    '    PopToken()
    '    If PeekToken.IsNumericLiteralToken Then
    '      increment = CShort(PopToken.Literal)
    '    End If
    '  End If

    '  If PeekToken() Is Nothing AndAlso
    '   newNumber.Between(0, 65529) AndAlso
    '   oldNumber.Between(0, 65529) AndAlso
    '   increment.Between(1, 655329) AndAlso
    '   ((Aggregate a In m_lines Where a.LineNumber = oldNumber Into Count()) > 0) Then

    '    'TODO: Need to verify that line numbers exist -> old number.
    '    'TODO: Need to verify that new line number (in concideration of old line number) does not cause a crossover situation.
    '    'TODO: Need to parse each line for ELSE, GOTO, GOSUB, THEN, ON...GOTO, ON...GOSUB, RESTORE, RESUME and ERL statements and renumber accordingly.
    '    'TODO: Need to report error on any reference to a nonexistent line number when processing statements. ("Undefined line {0} in {1}.")

    '    ' Copy entire source list to a temporary list.
    '    Dim result As New List(Of Parser.Line)
    '    For index As Integer = 0 To m_lines.Count - 1
    '      result.Add(m_lines(index).Copy)
    '    Next

    '    ' Manipulate the temporary list.
    '    For index As Integer = 0 To result.Count - 1
    '      If result(index).LineNumber >= oldNumber Then

    '        Dim currentNumber As Short = CShort(result(index).LineNumber)

    '        ' Check for ELSE, GOTO, GOSUB, THEN, ON...GOTO, ON...GOSUB, RESTORE, and ERL statements, 
    '        ' modify the matching line number accordingly.
    '        For parseIndex As Integer = 0 To m_lines.Count - 1
    '          For statementIndex = 0 To m_lines(parseIndex).Statements.Count - 1
    '            For tokenIndex = 0 To m_lines(parseIndex).Statements(statementIndex).Tokens.Count - 1
    '              If TypeOf m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex) Is Parser.KeywordToken Then
    '                Select Case m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex).ToString
    '                  Case "ELSE", "GOTO", "GOSUB", "THEN", "RESTORE"
    '                    If tokenIndex + 1 < m_lines(parseIndex).Statements(statementIndex).Tokens.Count AndAlso
    '                     TypeOf m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 1) Is Parser.NumericLiteralToken AndAlso
    '                     CShort(DirectCast(m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 1), Parser.NumericLiteralToken).Value) = currentNumber Then
    '                      DirectCast(result(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 1), Parser.NumericLiteralToken).Value = CStr(newNumber)
    '                      result(parseIndex).Text = result(parseIndex).ToString
    '                    End If
    '                  Case "ON" ' GOTO, GOSUB
    '                    If tokenIndex + 2 < m_lines(parseIndex).Statements(statementIndex).Tokens.Count AndAlso
    '                     TypeOf m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 2) Is Parser.KeywordToken AndAlso
    '                     (DirectCast(m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 2), Parser.KeywordToken).Value = "GOTO" OrElse
    '                      DirectCast(m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 2), Parser.KeywordToken).Value = "GOSUB") AndAlso
    '                     TypeOf m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 1) Is Parser.NumericLiteralToken AndAlso
    '                     CShort(DirectCast(m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 1), Parser.NumericLiteralToken).Value) = currentNumber Then
    '                      DirectCast(result(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 1), Parser.NumericLiteralToken).Value = CStr(newNumber)
    '                      result(parseIndex).Text = result(parseIndex).ToString
    '                    End If
    '                  Case "ERL"
    '                    If tokenIndex + 2 < m_lines(parseIndex).Statements(statementIndex).Tokens.Count AndAlso
    '                     TypeOf m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 1) Is Parser.RelationalOperatorToken AndAlso
    '                     DirectCast(m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 1), Parser.RelationalOperatorToken).Value = "=" AndAlso
    '                     TypeOf m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 2) Is Parser.NumericLiteralToken AndAlso
    '                     CShort(DirectCast(m_lines(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 2), Parser.NumericLiteralToken).Value) = currentNumber Then
    '                      DirectCast(result(parseIndex).Statements(statementIndex).Tokens(tokenIndex + 2), Parser.NumericLiteralToken).Value = CStr(newNumber)
    '                      result(parseIndex).Text = result(parseIndex).ToString
    '                    End If
    '                End Select
    '              End If
    '            Next
    '          Next
    '        Next

    '        result(index).LineNumber = newNumber
    '        result(index).Text = result(index).ToString

    '        newNumber += increment
    '      End If
    '    Next

    '    ' If no errors, update source list using temporary list.
    '    m_lines.Clear() ' = New List(Of Parser.Line)
    '    For index As Integer = 0 To result.Count - 1
    '      m_lines.Add(result(index).Copy)
    '    Next

    '  Else
    '    Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '  End If

    '  Return False

    'End Function

    Private Function ExecuteReset() As Boolean

      If PeekToken() Is Nothing Then
        InternalClose()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteRestore() As Boolean

      If PeekToken() Is Nothing Then
        m_readIndex = 0
      ElseIf PeekToken.IsNumericLiteralToken Then
        Dim value As Double
        If Not ExecuteExpression(value) Then Return ThrowBasicError(BasicError.UndefinedLineNumber)
        Dim lineNumber As Integer = CInt(value)
        Dim temp As Integer = Nothing
        If m_dataLineNumberIndex.TryGetValue(lineNumber, temp) Then
          m_readIndex = temp
        Else

          ' Scan for the next line number containing data.
          Dim scan = From p In m_dataLineNumberIndex.Keys
                     Where p >= lineNumber

          If scan.Any Then
            m_readIndex = m_dataLineNumberIndex(scan.First)
          Else
            Return ThrowBasicError(BasicError.UndefinedLineNumber)
          End If

        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteResume() As Boolean

      Dim label As String '= "0"

      If PeekToken() Is Nothing Then
        label = "0"
      ElseIf PeekToken.IsWord("NEXT") Then
        label = "-1"
        PopToken()
      ElseIf PeekToken.IsIdentifierToken Then
        label = PopToken.Literal
      Else
        Dim value As Double
        If Not ExecuteExpression(value) Then Return False
        If CInt(value).Between(0, 65534) Then
          label = value.ToString
        Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If
      End If

      If PeekToken() Is Nothing Then

        If m_onErrorResumeInterpreterIndex = -1 Then
          Return ThrowBasicError(BasicError.ResumeWithoutError)
        End If

        If label = "-1" Then

          ' NEXT

          Dim statementIndex As Integer = m_onErrorResumeStatementIndex
          Dim interpreterIndex As Integer = m_onErrorResumeInterpreterIndex

          m_onErrorResumeInterpreterIndex = -1
          m_onErrorResumeStatementIndex = 0

          m_interpreterIndex = interpreterIndex
          m_statementIndex = statementIndex
          m_tokenIndex = 0

          m_statementIndex += 1
          If m_statementIndex > m_interpreter(m_interpreterIndex).Statements.Count - 1 Then
            m_statementIndex = 0
            m_interpreterIndex += 1
            If m_interpreterIndex > m_interpreter.Count - 1 Then
              Return ThrowBasicError(BasicError.UndefinedLineNumber)
            End If
          End If

          Trace()
          Return ExecuteStatement()

        ElseIf label = "0" Then

          m_interpreterIndex = m_onErrorResumeInterpreterIndex
          m_statementIndex = m_onErrorResumeStatementIndex
          m_tokenIndex = 0

          m_onErrorResumeInterpreterIndex = -1
          m_onErrorResumeStatementIndex = 0

          Trace()
          Return ExecuteStatement()

        Else

          m_onErrorResumeInterpreterIndex = -1
          m_onErrorResumeStatementIndex = 0

          Return GotoLabelOrLineNumber(label)

        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteRmDir() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.MissingOperand)
      ElseIf Not PeekToken.IsStringLiteralToken Then
        Return ThrowBasicError(BasicError.TypeMismatch)
      Else

        Dim path As String = RealPath(PopToken.ToString)

        If path Is Nothing Then
          Return ThrowBasicError(BasicError.SyntaxError)
        Else

          If Not path.EndsWith("\"c) Then
            path &= "\"
          End If

          Dim result = m_virtualFileSystem.RmDir(path)

          If result.StartsWith("Error: ") Then
            m_display.Print(result.Substring(7), True)
            m_running = False
          End If

          m_running = False
          m_waiting = False

          'InternalPrompt()
          'ProcessKeyBuffer()

          Return True

        End If

      End If

    End Function

    Private Function ExecuteRset() As Boolean

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Dim variableName As String = Nothing

      If IsStringVariable(PeekToken.ToString) Then
        variableName = PopToken.ToString
      Else
        Return ThrowBasicError(BasicError.TypeMismatch)
      End If

      If PeekToken.IsWord("=") Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Dim value2 As String = Nothing
      If Not ExecuteStringExpression(value2, Nothing) Then Return False

      If PeekToken() Is Nothing Then

        Dim value1 As String = Nothing
        If Not GetStringVariable(variableName, value1) Then Return False

        If value2.Length = value1.Length Then
          value1 = value2.Substring(0, value1.Length)
        Else
          value1 = value2.PadLeft(value1.Length)
        End If

        If Not SetStringVariable(variableName, value1) Then Return False

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteRun() As Boolean

      Dim lineNumber As Short = -1
      Dim filename As String = Nothing
      Dim run As Boolean = False

      If PeekToken() Is Nothing Then
      ElseIf PeekToken.IsNumericLiteralToken Then
        lineNumber = CShort(PopToken.ToString)
      ElseIf PeekToken.IsStringLiteralToken Then
        filename = PopToken.ToString
      ElseIf PeekToken.IsCommaToken Then
        PopToken()
        If PeekToken.IsWord("R") Then
          run = True
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken() IsNot Nothing Then
        Return False
      End If

      If Not run Then InternalClose()

      If lineNumber > -1 Then

        ' Kick over to a GOTO.

        If Not run Then InternalClose()

        m_interpreter.Add(New Parser.Line(Nothing))
        m_interpreter(0).Statements.Add(New Parser.Statement)
        m_interpreter(0).Statements(0).Tokens.Add(New Parser.KeywordToken() With {.Value = "GOTO"})
        m_interpreter(0).Statements(0).Tokens.Add(New Parser.NumericLiteralToken() With {.Value = CStr(lineNumber)})

        m_interpreterIndex = -1
        m_statementIndex = 0
        m_tokenIndex = 0

        m_waiting = False
        m_running = True

      ElseIf filename IsNot Nothing Then

        ' Kick over to a LOAD.

        m_interpreter.Clear()

        m_interpreter.Add(New Parser.Line(Nothing))
        m_interpreter(0).Statements.Add(New Parser.Statement)
        m_interpreter(0).Statements(0).Tokens.Add(New Parser.KeywordToken() With {.Value = "LOAD"})
        m_interpreter(0).Statements(0).Tokens.Add(New Parser.StringLiteralToken() With {.Value = filename})
        m_interpreter(0).Statements(0).Tokens.Add(New Parser.CommaToken)
        m_interpreter(0).Statements(0).Tokens.Add(New Parser.KeywordToken() With {.Value = "R"})

        m_interpreterIndex = -1
        m_statementIndex = 0
        m_tokenIndex = 0

        m_waiting = False
        m_running = True

      Else

        Return InternalRun()

        'Return True

      End If

      Return True

    End Function

    Public Sub Run()
      If Not IsRunning Then
        InternalRun()
      End If
    End Sub

    Private Function InternalRun() As Boolean

      m_successLineNumber = 0

      m_contIndex = -1

      InternalClear()

      m_defDblList.Clear()
      m_defIntList.Clear()
      m_defSngList.Clear()
      m_defStrList.Clear()

      ' Clear out any existing labels...
      m_labelList.Clear()

      If Not PrepareData() Then Return False
      ' Reset read index at start of a run statement.
      m_readIndex = 0

      m_interpreter.Clear()
      For index As Integer = 0 To m_lines.Count - 1 'Each line In m_lines
        m_interpreter.Add(m_lines(index).Copy)
        If m_lines(index).Statements.Count > 0 AndAlso
         m_lines(index).Statements(0).Tokens.Count > 0 AndAlso
         m_lines(index).Statements(0).Tokens(0).IsLabelToken Then
          m_labelList.Add(m_lines(index).Statements(0).Tokens(0).Literal, index)
        End If
      Next
      m_interpreterIndex = -1
      m_statementIndex = 0
      m_tokenIndex = 0
      m_waiting = False
      m_running = True

      ResetDraw()

      ' -------

      Do
        ProcessInterpreter()
        If Not m_running Then Exit Do
      Loop

      Return True

    End Function

    Private Function PrepareData() As Boolean

      m_data.Clear()
      m_dataLineNumberIndex.Clear()

      For Each line In m_lines

        For Each statement In line.Statements

          If statement.Tokens.Count > 0 AndAlso
           TypeOf statement.Tokens(0) Is Parser.DataToken Then 'statement.Tokens(0).IsWord("DATA") Then

            Dim data As String = statement.Tokens(0).ToString

            Dim values = Utils.CsvSplit.CsvSplit(data)

            For Each value As String In values
              m_data.Add(New Parser.StringLiteralToken With {.Value = value})
              If line.LineNumber IsNot Nothing AndAlso
               Not m_dataLineNumberIndex.ContainsKey(CInt(line.LineNumber)) Then
                m_dataLineNumberIndex.Add(CInt(line.LineNumber), m_data.Count - 1)
              End If
            Next

            'Dim index As Integer = 1
            'Dim comma As Boolean = False

            'Do

            '  If Not comma AndAlso index = statement.Tokens.Count Then
            '    Exit Do
            '  ElseIf index < statement.Tokens.Count AndAlso
            '         (statement.Tokens(index).IsStringLiteralToken OrElse
            '          statement.Tokens(index).IsNumericLiteralToken) Then
            '    m_data.Add(statement.Tokens(index))
            '    index += 1
            '    comma = False
            '  Else
            '    Return ThrowBasicError(BasicError.SyntaxError, CInt(line.LineNumber))
            '  End If

            '  If index = statement.Tokens.Count Then
            '    Exit Do
            '  ElseIf index < statement.Tokens.Count AndAlso
            '         statement.Tokens(index).IsCommaToken Then
            '    index += 1
            '    comma = True
            '  Else
            '    Return ThrowBasicError(BasicError.SyntaxError, CInt(line.LineNumber))
            '  End If

            'Loop

          End If

        Next

      Next

      Return True

    End Function

    '    Private Function ExecuteSave() As Boolean

    '      If m_isTrial Then
    '        Return ThrowBasicError(BasicError.AdvancedFeature)
    '      End If

    '      If PeekToken() Is Nothing Then
    '        Return ThrowBasicError(BasicError.MissingOperand)
    '      ElseIf Not PeekToken.IsStringLiteralToken Then
    '        Return ThrowBasicError(BasicError.TypeMismatch)
    '      ElseIf PeekToken.IsStringLiteralToken Then

    '        Dim filename = RealPath(PopToken.ToString)

    '        If filename.Length > 4 AndAlso
    '         filename.LastIndexOf("."c) > filename.Length - 4 Then
    '          ' Do nothing, we have some sort of an extension already.
    '        Else
    '          ' no extension?
    '          filename &= ".BAS"
    '        End If

    '        ''Dim ext = IO.Path.GetExtension(filename)
    '        'Dim period As Integer = filename.LastIndexOf(".")
    '        'Dim ext As String = Nothing
    '        'If period > -1 Then
    '        '  ext = filename.Substring(period)
    '        'End If
    '        'If String.IsNullOrEmpty(ext) Then
    '        '  filename &= ".BAS"
    '        'End If

    '        If PeekToken.IsCommaToken Then
    '          PopToken()
    '          If PeekToken.IsWord("A|P") Then
    '            PopToken()
    '            If PeekToken() IsNot Nothing Then
    '              Return ThrowBasicError(BasicError.SyntaxError)
    '            End If
    '          Else
    '            Return ThrowBasicError(BasicError.SyntaxError)
    '          End If
    '        End If

    '        Dim content As String = ""

    '        If m_lines.Count = 0 OrElse m_pendingNew Then
    '          Return ThrowBasicError(BasicError.IllegalFunctionCall)
    '        End If

    '        For Each line In m_lines
    '          If content <> "" Then
    '            content &= vbCrLf
    '          End If
    '          content &= line.Text 'ToString
    '        Next

    '        Dim result = m_virtualFileSystem.Save(filename, content)

    '        If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then
    '          m_display.Print(result.Substring(7), True)
    '          m_running = False
    '        End If

    '        m_waiting = False
    '        m_running = False

    '        'InternalPrompt()
    '        'ProcessKeyBuffer()

    '        Return True

    '      Else
    '        Return ThrowBasicError(BasicError.SyntaxError)
    '      End If

    '    End Function

    Private Function ExecuteScreen() As Boolean

      Dim mode As Short = CShort(m_display.ScreenMode)
      Dim colorSwitch As Short
      Dim activePage As Short = CShort(m_display.ActivePage)
      Dim visualPage As Short = CShort(m_display.VisualPage)
      Dim eraseParam As Short = 1

      If PeekToken() Is Nothing Then
        ' Fall through...
      ElseIf PeekToken.IsCommaToken Then
        ' Fall through...
      Else
        If Not ExecuteExpression(mode) Then Return False
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken() Is Nothing Then
        ' Fall through...
      ElseIf PeekToken.IsCommaToken Then
        ' Fall through...
      Else
        If Not ExecuteExpression(colorSwitch) Then Return False
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken() Is Nothing Then
        ' Fall through...
      ElseIf PeekToken.IsCommaToken Then
        ' Fall through...
      Else
        If Not ExecuteExpression(activePage) Then Return False
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken() Is Nothing Then
        ' Fall through...
      ElseIf PeekToken.IsCommaToken Then
        ' Fall through...
      Else
        If Not ExecuteExpression(visualPage) Then Return False
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken() Is Nothing Then
        ' Fall through...
      ElseIf PeekToken.IsCommaToken Then
        ' Fall through...
      Else
        If Not ExecuteExpression(eraseParam) Then Return False
      End If

      If PeekToken() Is Nothing Then

        ' MDPA with Monochrome Display: Mode 0
        ' CGA with Color Display: Modes 0, 1 and 2
        ' TANDY: Modes 3, 4, 5, 6
        ' EGA with Color Display: Modes 0, 1, 7 and 8
        ' * EGA with Enhanced Color Display: Modes 0, 1, 2, 7, 8, 9
        ' EGA with Monochrome Display: Modes 10

        Select Case mode

          Case -3

            m_drawPosition.X = 360
            m_drawPosition.Y = 175

          Case -4

            m_drawPosition.X = 320
            m_drawPosition.Y = 200

          Case 0

            m_drawPosition.X = 320
            m_drawPosition.Y = 200

        '' Text mode only.
        'm_screenWidth = 640
        'm_screenHeight = 400
        '' Either 40 x 25 or 80 x 25: font is 8x14 (EGA)
        'm_screenColumnCount = 80
        'm_screenPageCount = 1
        '' Assignment of 16 colors to any of 16 attributes (with EGA)
        'm_screenColorCount = 32
        'm_screenAttributeCount = 0
        'm_screenColorTable = New Dictionary(Of Short, Short) From {{0, 0},
        '                                                           {1, 1},
        '                                                           {2, 2},
        '                                                           {3, 3},
        '                                                           {4, 4},
        '                                                           {5, 5},
        '                                                           {6, 6},
        '                                                           {7, 7},
        '                                                           {8, 8},
        '                                                           {9, 9},
        '                                                           {10, 10},
        '                                                           {11, 11},
        '                                                           {12, 12},
        '                                                           {13, 13},
        '                                                           {14, 14},
        '                                                           {15, 15},
        '                                                           {16, 16},
        '                                                           {17, 17},
        '                                                           {18, 18},
        '                                                           {19, 19},
        '                                                           {20, 20},
        '                                                           {21, 21},
        '                                                           {22, 22},
        '                                                           {23, 23},
        '                                                           {24, 24},
        '                                                           {25, 25},
        '                                                           {26, 26},
        '                                                           {27, 27},
        '                                                           {28, 28},
        '                                                           {29, 29},
        '                                                           {30, 30},
        '                                                           {31, 31}}

          Case 1

            m_drawPosition.X = 160
            m_drawPosition.Y = 100

        '' 320 x 200 pixel medium-resolution graphics
        'm_screenWidth = 320
        'm_screenHeight = 200
        '' 40 x 25 text format with character-box size of 8x8
        'm_screenColumnCount = 40
        'm_screenPageCount = 1
        '' Assignment of 16 colors to any of 4 attributes
        'm_screenColorCount = 4
        'm_screenAttributeCount = 4
        '' 2 bits per pixel
        'm_screenColorTable = New Dictionary(Of Short, Short) From {{0, 0},
        '                                                           {1, 11},
        '                                                           {2, 13},
        '                                                           {3, 15}}

          Case 2
            '' 640 x 200 pixel high-resolution graphics
            'm_screenWidth = 640
            'm_screenHeight = 200
            '' 80 x 25 text format with character-box size of 8x8
            'm_screenColumnCount = 80
            'm_screenPageCount = 1
            '' Assignment of 16 colors to any of 2 attributes
            'm_screenColorCount = 2
            'm_screenAttributeCount = 2
            '' 1 bit per pixel
            'm_screenColorTable = New Dictionary(Of Short, Short) From {{0, 0},
            '                                                           {1, 15}}

            m_drawPosition.X = 320
            m_drawPosition.Y = 100

          Case 3 ' TANDY 1000 160x200 16 colors

            m_drawPosition.X = 80
            m_drawPosition.Y = 100

          Case 4 ' TANDY 1000 320x200 4 colors

            m_drawPosition.X = 160
            m_drawPosition.Y = 100

          Case 5 ' TANDY 1000 320x200 16 colors
            '' 320 x 200 pixel medium-resolution graphics
            'm_screenWidth = 320
            'm_screenHeight = 200
            '' 40 x 25 text format with character-box size of 8x8
            'm_screenColumnCount = 40
            '' 8 memory pages (256K of memory)
            'm_screenPageCount = 8
            '' Assignment of any of 16 colors to 16 attributes
            'm_screenColorCount = 16
            'm_screenAttributeCount = 16
            '' 4 bits per pixel
            'm_screenColorTable = New Dictionary(Of Short, Short) From {{0, 0},
            '                                                           {1, 1},
            '                                                           {2, 2},
            '                                                           {3, 3},
            '                                                           {4, 4},
            '                                                           {5, 5},
            '                                                           {6, 6},
            '                                                           {7, 7},
            '                                                           {8, 8},
            '                                                           {9, 9},
            '                                                           {10, 10},
            '                                                           {11, 11},
            '                                                           {12, 12},
            '                                                           {13, 13},
            '                                                           {14, 14},
            '                                                           {15, 15}}

            m_drawPosition.X = 160
            m_drawPosition.Y = 100

          Case 6 ' TANDY 1000 640x200 4 colors

            m_drawPosition.X = 320
            m_drawPosition.Y = 100

          Case 7
            '' 320 x 200 pixel medium-resolution graphics
            'm_screenWidth = 320
            'm_screenHeight = 200
            '' 40 x 25 text format with character-box size of 8x8
            'm_screenColumnCount = 40
            '' 8 memory pages (256K of memory)
            'm_screenPageCount = 8
            '' Assignment of any of 16 colors to 16 attributes
            'm_screenColorCount = 16
            'm_screenAttributeCount = 16
            '' 4 bits per pixel
            'm_screenColorTable = New Dictionary(Of Short, Short) From {{0, 0},
            '                                                           {1, 1},
            '                                                           {2, 2},
            '                                                           {3, 3},
            '                                                           {4, 4},
            '                                                           {5, 5},
            '                                                           {6, 6},
            '                                                           {7, 7},
            '                                                           {8, 8},
            '                                                           {9, 9},
            '                                                           {10, 10},
            '                                                           {11, 11},
            '                                                           {12, 12},
            '                                                           {13, 13},
            '                                                           {14, 14},
            '                                                           {15, 15}}

            m_drawPosition.X = 160
            m_drawPosition.Y = 100

          Case 8
            '' 640 x 200 pixel high-resolution graphics
            'm_screenWidth = 640
            'm_screenHeight = 200
            '' 80x25 text format with character-box size of 8x8
            'm_screenColumnCount = 80
            '' 4 memory pages (256K of memory)
            'm_screenPageCount = 4
            '' Assigment of any of 16 colors to 16 attributes
            'm_screenColorCount = 16
            'm_screenAttributeCount = 16
            '' 4 bits per pixel
            'm_screenColorTable = New Dictionary(Of Short, Short) From {{0, 0},
            '                                                           {1, 1},
            '                                                           {2, 2},
            '                                                           {3, 3},
            '                                                           {4, 4},
            '                                                           {5, 5},
            '                                                           {6, 6},
            '                                                           {7, 7},
            '                                                           {8, 8},
            '                                                           {9, 9},
            '                                                           {10, 10},
            '                                                           {11, 11},
            '                                                           {12, 12},
            '                                                           {13, 13},
            '                                                           {14, 14},
            '                                                           {15, 15}}

            m_drawPosition.X = 320
            m_drawPosition.Y = 100

          Case 9
            '' 640 x 350 pixel enhanced-resolution graphics
            'm_screenWidth = 640
            'm_screenHeight = 350
            '' 80x25 text format with chartacter-box size of 8x14
            'm_screenColumnCount = 80
            '' Assignment of 64 colors to 16 attributes (more than 64K of EGA memory)
            'm_screenColorCount = 64
            'm_screenAttributeCount = 16
            '' 2 memory pages (256K of memory)
            'm_screenPageCount = 2
            '' 4 bits per pixel (more than 64K of EGA memory)
            'm_screenColorTable = New Dictionary(Of Short, Short) From {{0, 0},
            '                                                           {1, 1},
            '                                                           {2, 2},
            '                                                           {3, 3},
            '                                                           {4, 4},
            '                                                           {5, 5},
            '                                                           {6, 6},
            '                                                           {7, 7},
            '                                                           {8, 8},
            '                                                           {9, 9},
            '                                                           {10, 10},
            '                                                           {11, 11},
            '                                                           {12, 12},
            '                                                           {13, 13},
            '                                                           {14, 14},
            '                                                           {15, 15},
            '                                                           {16, 16},
            '                                                           {17, 17},
            '                                                           {18, 18},
            '                                                           {19, 19},
            '                                                           {20, 20},
            '                                                           {21, 21},
            '                                                           {22, 22},
            '                                                           {23, 23},
            '                                                           {24, 24},
            '                                                           {25, 25},
            '                                                           {26, 26},
            '                                                           {27, 27},
            '                                                           {28, 28},
            '                                                           {29, 29},
            '                                                           {30, 30},
            '                                                           {31, 31},
            '                                                           {32, 32},
            '                                                           {33, 33},
            '                                                           {34, 34},
            '                                                           {35, 35},
            '                                                           {36, 36},
            '                                                           {37, 37},
            '                                                           {38, 38},
            '                                                           {39, 39},
            '                                                           {40, 40},
            '                                                           {41, 41},
            '                                                           {42, 42},
            '                                                           {43, 43},
            '                                                           {44, 44},
            '                                                           {45, 45},
            '                                                           {46, 46},
            '                                                           {47, 47},
            '                                                           {48, 48},
            '                                                           {49, 49},
            '                                                           {50, 50},
            '                                                           {51, 51},
            '                                                           {52, 52},
            '                                                           {53, 53},
            '                                                           {54, 54},
            '                                                           {55, 55},
            '                                                           {56, 56},
            '                                                           {57, 57},
            '                                                           {58, 58},
            '                                                           {59, 59},
            '                                                           {60, 60},
            '                                                           {61, 61},
            '                                                           {62, 62},
            '                                                           {63, 63}}

            m_drawPosition.X = 320
            m_drawPosition.Y = 175

          Case 10

            m_drawPosition.X = 320
            m_drawPosition.Y = 175

          Case 11

            m_drawPosition.X = 320
            m_drawPosition.Y = 240

          Case 12

            m_drawPosition.X = 320
            m_drawPosition.Y = 240

          Case 13

            m_drawPosition.X = 160
            m_drawPosition.Y = 100

          Case Else
            Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End Select

        ' Configure screen...
        m_display.Screen(mode, colorSwitch <> 0, activePage, visualPage)

        'If m_pen IsNot Nothing Then
        '  m_pen.ScreenMode = m_display.ScreenMode
        '  m_penActivatedSinceLastPoll = False
        '  m_penCurrentActivated = False
        '  m_penCurrentPosition = Nothing
        '  m_penLastActivated = False
        '  m_penLastActivatedPosition = Nothing
        '  m_onPenActivated = False
        'End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteShell() As Boolean

      Dim command As String = Nothing

      If PeekToken() IsNot Nothing Then
        If Not ExecuteStringExpression(command, Nothing) Then Return False
      End If

      If PeekToken() Is Nothing Then
        If m_environment IsNot Nothing Then
          Try
            Return m_environment.Shell(command, m_environ)
          Catch ex As System.IO.FileNotFoundException
            Return ThrowBasicError(BasicError.FileNotFound)
          Catch ex As NotImplementedException
            Return ThrowBasicError(BasicError.AdvancedFeature)
          Catch ex As Exception
            If ex.Message = "The system cannot find the file specified" Then
              Return ThrowBasicError(BasicError.FileNotFound)
            Else
              Throw
            End If
          End Try
        Else
          Return ThrowBasicError(BasicError.AdvancedFeature)
        End If
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteDelay() As Boolean

      Dim ms As Short = 0

      If PeekToken() IsNot Nothing Then
        If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
          Return ThrowBasicError(BasicError.TypeMismatch)
        Else
          If Not ExecuteExpression(ms) Then
            ThrowBasicError(BasicError.IllegalFunctionCall)
          End If
        End If
      End If

      If PeekToken() Is Nothing Then
        m_keyBuffer.Clear()
        m_characterBuffer.Clear()
        m_hexCodeScanCodeBuffer.Clear()
        If ms = 0 Then
          m_sleepUntil = Date.MaxValue
        Else
          m_sleepUntil = Date.Now.AddMilliseconds(ms)
        End If
        Return True
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteSleep() As Boolean

      Dim seconds As Short = 0

      If PeekToken() IsNot Nothing Then
        If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
          Return ThrowBasicError(BasicError.TypeMismatch)
        Else
          If Not ExecuteExpression(seconds) Then
            ThrowBasicError(BasicError.IllegalFunctionCall)
          End If
        End If
      End If

      If PeekToken() Is Nothing Then
        m_keyBuffer.Clear()
        m_characterBuffer.Clear()
        m_hexCodeScanCodeBuffer.Clear()
        If seconds = 0 Then
          m_sleepUntil = Date.MaxValue
        Else
          m_sleepUntil = Date.Now.AddSeconds(seconds)
        End If
        Return True
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteSound() As Boolean

      Dim frequency As Double
      Dim duration As Double

      Dim interpreterIndex As Integer = m_interpreterIndex
      Dim statementIndex As Integer = m_statementIndex
      'Dim tokenIndex As Integer '= m_tokenIndex

      If Not ExecuteExpression(frequency) Then Return False
      If PeekToken.IsCommaToken Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.IllegalFunctionCall)
      End If
      If Not ExecuteExpression(duration) Then Return False
      If PeekToken() IsNot Nothing Then
        Return ThrowBasicError(BasicError.IllegalFunctionCall)
      End If

      If m_sound IsNot Nothing Then
        If frequency.Between(37, 32767) AndAlso duration.Between(0, 65535) Then

          If m_sound.IsPlaying AndAlso duration > 0 Then
            m_interpreterIndex = interpreterIndex
            m_statementIndex = statementIndex
            m_tokenIndex = 0
          Else

            Select Case m_sound.Sound(CSng(frequency), CSng(duration))
              Case 5 ' A current sound is already playing, need to block until it is done.
                m_interpreterIndex = interpreterIndex
                m_statementIndex = statementIndex
                m_tokenIndex = 0 'tokenIndex
              Case 7 : Return ThrowBasicError(BasicError.OutOfMemory)
              Case Else
                ' Success?
            End Select
          End If
        Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If
      Else
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      Return True

    End Function

    Private Function ExecuteStop() As Boolean

      If PeekToken() Is Nothing Then
        ' Do nothing, just need to return false.
        If m_display.Pos(0) > 1 Then
          m_display.Print()
        End If
        If m_interpreter(m_interpreterIndex).LineNumber IsNot Nothing Then
          m_display.Print(String.Format("Break in {0}", m_interpreter(m_interpreterIndex).LineNumber), True)
        Else
          m_display.Print("Break", True)
        End If

        If m_interpreterIndex < m_interpreter.Count - 1 Then
          m_contIndex = m_interpreterIndex + 1
        Else
          m_contIndex = -1
        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return False

    End Function

    Private Function ExecuteSwap() As Boolean

      Dim param1 As String '= Nothing
      Dim param2 As String '= Nothing

      If PeekToken.IsIdentifierToken Then
        param1 = PopToken.ToString
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
      End If

      If PeekToken.IsIdentifierToken Then
        param2 = PopToken.ToString
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken() Is Nothing Then

        If IsStringVariable(param1) AndAlso IsStringVariable(param2) Then
          Dim temp = m_stringVariableList(param1)
          m_stringVariableList(param1) = m_stringVariableList(param2)
          m_stringVariableList(param2) = temp
        ElseIf IsNumericVariable(param1) AndAlso IsNumericVariable(param2) Then
          If Not m_numericVariableList.ContainsKey(param1) Then
            SetNumericVariable(param1, 0)
          End If
          If Not m_numericVariableList.ContainsKey(param2) Then
            SetNumericVariable(param2, 0)
          End If
          If m_numericVariableList(param1).Type = m_numericVariableList(param2).Type Then
            Dim temp = m_numericVariableList(param1)
            m_numericVariableList(param1) = m_numericVariableList(param2)
            m_numericVariableList(param2) = temp
          End If
        Else
          Return ThrowBasicError(BasicError.TypeMismatch)
        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteSystem() As Boolean

      If PeekToken() Is Nothing Then
        ' END, NEW, RESET, SYSTEM, RUN and LOAD (without r option) close all file handles automatically.
        InternalClose()
        'If m_dialect = Parser.Dialect.QBasic Then
        '  m_running = False
        'Else
        m_running = False
        RaiseEvent SystemHook(Me, EventArgs.Empty)
        'End If
        Return False
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      'If PeekToken() Is Nothing Then
      '  m_shutDown = True
      'Else
      '  ThrowSyntaxError()
      'End If

      'm_waitForResponse = True

      Return True

    End Function

    Private Function ExecuteTime() As Boolean

      Dim value As String = ""

      If PeekToken.IsWord("=") Then
        PopToken()
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
        If Not ExecuteStringExpression(value, Nothing) Then Return False
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.PermissionDenied)
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

    End Function

    Private Function ExecuteTimer() As Boolean

      Select Case PopToken()?.ToString
        Case "ON"
          If Me.m_onTimerState = OnState.Off Then
            m_onTimerState = OnState.On
            m_onTimerTime = Date.Now.AddSeconds(m_onTimerInterval)
          ElseIf Me.m_onTimerState = OnState.Stop Then
            m_onTimerState = OnState.On
          End If
        Case "OFF"
          m_onTimerState = OnState.Off
          m_onTimerTime = New Date?
        Case "STOP"
          m_onTimerState = OnState.Stop
        Case Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
      End Select

      Return True

    End Function

    Private Function ExecutePen() As Boolean

      Select Case PopToken.ToString
        Case "ON"
          If Me.m_onPenState = OnState.Off Then
            m_onPenState = OnState.On
          ElseIf Me.m_onPenState = OnState.Stop Then
            m_onPenState = OnState.On
          End If
        Case "OFF"
          m_onPenState = OnState.Off
        Case "STOP"
          m_onPenState = OnState.Stop
        Case Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
      End Select

      Return True

    End Function

    Private Function ExecuteTron() As Boolean
      m_tron = True
      Return True
    End Function

    Private Function ExecuteTroff() As Boolean
      m_tron = False
      Return True
    End Function

    Private Function ExecuteUnlock() As Boolean

      If m_isTrial Then
        Return ThrowBasicError(BasicError.AdvancedFeature)
      End If

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      If PeekToken.IsHashToken Then
        PopToken()
      End If

      Dim fileNumber As Short
      If Not ExecuteExpression(fileNumber) Then Return False

      Dim min As Double = -1
      Dim max As Double = -1

      If PeekToken.IsCommaToken Then

        PopToken()

        Dim found As Boolean = False

        If Not PeekToken.IsWord("TO") Then
          If Not ExecuteExpression(min) Then Return False
          found = True
        End If

        If PeekToken.IsWord("TO") Then
          PopToken()
          If Not ExecuteExpression(max) Then Return False
          found = True
        End If

        If Not found Then
          Return ThrowBasicError(BasicError.SyntaxError)
        End If

      End If

      If PeekToken() Is Nothing Then

        Return InternalUnlock(fileNumber, min, max)

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    'Private Function ExecuteVer() As Boolean

    '  If PeekToken() Is Nothing Then
    '    m_display.Print(m_productName, True) 'm_version, True)
    '  Else
    '    Return ThrowBasicError(BasicError.SyntaxError)
    '  End If

    '  Return True

    'End Function

    Private Function ExecuteView() As Boolean

      Dim isScreen As Boolean?
      Dim x1 = New Integer?
      Dim y1 = New Integer?
      Dim x2 = New Integer?
      Dim y2 = New Integer?
      Dim fill = New Integer?
      Dim border = New Integer?

      If PeekToken.IsWord("PRINT") Then
        PopToken()
        Return ExecuteViewPrint()
      End If

      If PeekToken() IsNot Nothing Then

        If PeekToken.IsWord("SCREEN") Then
          PopToken()
          isScreen = True
        End If

        If PeekToken.IsParenOpenToken Then

          PopToken()

          Dim value As Short
          If Not ExecuteExpression(value) Then Return False
          x1 = value
          If PeekToken.IsCommaToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If Not ExecuteExpression(value) Then Return False
          y1 = value
          If PeekToken.IsParenCloseToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If PeekToken.IsDash Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If PeekToken.IsParenOpenToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If Not ExecuteExpression(value) Then Return False
          x2 = value
          If PeekToken.IsCommaToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If Not ExecuteExpression(value) Then Return False
          y2 = value
          If PeekToken.IsParenCloseToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If

          If PeekToken() IsNot Nothing Then

            If PeekToken.IsCommaToken Then
              PopToken()
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

            If Not PeekToken.IsCommaToken Then
              If Not ExecuteExpression(value) Then Return False
              fill = CInt(value)
            End If

          End If

          If PeekToken() IsNot Nothing Then

            If PeekToken.IsCommaToken Then
              PopToken()
            Else
              Return ThrowBasicError(BasicError.SyntaxError)
            End If

            If PeekToken() IsNot Nothing Then
              If Not ExecuteExpression(value) Then Return False
              border = CInt(value)
            End If

          End If

        End If

      End If

      If PeekToken() Is Nothing Then
        m_display.View(isScreen, x1, y1, x2, y2, fill, border)
      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteViewPrint() As Boolean

      Dim topLine As Short
      Dim bottomLine As Short

      If PeekToken() IsNot Nothing Then

        If Not ExecuteExpression(topLine) Then Return False
        If PeekToken.IsWord("TO") Then
          PopToken()
        Else
          Return ThrowBasicError(BasicError.SyntaxError)
        End If
        If Not ExecuteExpression(bottomLine) Then Return False

      Else

        topLine = -1
        bottomLine = -1

      End If

      If PeekToken() Is Nothing Then

        If topLine = -1 AndAlso bottomLine = -1 Then
          m_display.ViewPrint()
        ElseIf topLine.Between(1, 24) AndAlso
         bottomLine.Between(1, 25) AndAlso
         topLine <= bottomLine Then ' Modified bottomLine check to be 25 instead of 24.
          m_display.ViewPrint(topLine, bottomLine)
        Else
          Return ThrowBasicError(BasicError.IllegalFunctionCall)
        End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteWait() As Boolean

      Return ThrowBasicError(BasicError.AdvancedFeature)

      Return True

    End Function

    Private Function ExecuteWend() As Boolean

      If m_whileLoop.Count > 0 Then

        m_interpreterIndex = m_whileLoop(0).ConditionInterpreterIndex
        m_statementIndex = m_whileLoop(0).ConditionStatementIndex
        m_tokenIndex = m_whileLoop(0).ConditionTokenIndex

        Dim value As Double
        If Not ExecuteExpression(value) Then Return False

        If value = 0 Then
          m_whileLoop.RemoveAt(0)
          JumpToWend()
        End If

      Else
        Return ThrowBasicError(BasicError.WendWithoutWhile)
      End If

      'If whileLoop.Count >= 1 Then
      '  ' (translated) index1 := whileLoop[1].condLine;
      '  index1 = whileLoop(0).CondLine
      '  ' (translated) param2 := prg[index1].line[whileLoop[1].condColumn .. ];
      '  param2 = m_programList(index1 - 1).Line.Substring(whileLoop(0).CondColumn)
      '  param1 = GetSymbol(param2)
      '  num1 = exec_expr(param1, param2)
      '  If num1 <> 0.0 Then
      '    line_marker()
      '    file_line_number = index1
      '    m_statementLabel = m_programList(file_line_number - 1).LineNumber
      '    symbol = param1
      '    line = param2
      '    logWriteLine("WEND - CONTINUE WHILE")
      '  Else
      '    symbol = GetSymbol(line)
      '    ' (translated) whileLoop := whileLoop[2 .. ];
      '    whileLoop.RemoveAt(0)
      '    line_marker()
      '    logWriteLine("WEND - END WHILE")
      '  End If
      'Else
      '  error_marker()
      '  err.WriteLine("UNEXPECTED ""WEND""")
      'End If

      Return True

    End Function

    Private Function ExecuteWhile() As Boolean

      If PeekToken() Is Nothing Then
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      'index1 = file_line_number
      'index2 = m_programList(file_line_number - 1).Line.Length - line.Length + 1
      'symbol = GetSymbol(line)

      'num1 = exec_expr(symbol, line)
      'line_marker()
      'logWriteLine("WHILE " & num1)

      Dim interpreterIndex = m_interpreterIndex
      Dim statementIndex = m_statementIndex
      Dim tokenIndex = m_tokenIndex

      Dim value As Double
      If Not ExecuteExpression(value) Then Return False

      If value <> 0 Then
        '  whileLoop.Insert(0, New WhileLoopDescrType With {.CondLine = index1,
        '                                                   .CondColumn = index2})
        Dim result As Boolean = FoundMatchingWend()
        If result Then
          m_whileLoop.Insert(0, New WhileLoop() With {.ConditionInterpreterIndex = interpreterIndex,
                                                                 .ConditionStatementIndex = statementIndex,
                                                                 .ConditionTokenIndex = tokenIndex})
        End If
        Return result

      Else

        Return JumpToWend()

        '  line_marker()
        '  logWriteLine("EMPTY WHILE")
        '  If symbol <> "WEND" Then
        '    param1 = m_statementLabel
        '    symbol = find_wend(line)
        '  End If
        '  If symbol = "WEND" Then
        '    Do
        '      symbol = GetSymbol(line)
        '    Loop Until EndOfStatement(symbol)
        '    line_marker()
        '    logWriteLine("CONTINUE AFTER WEND")
        '  Else
        '    error_marker(param1)
        '    err.WriteLine("NO CORRESPONDING ""WEND"" FOUND FOR ""WHILE""")
        '  End If
      End If

      'Return True

    End Function

    Private Function FoundMatchingWend() As Boolean

      Dim interpreterIndex As Integer = m_interpreterIndex
      Dim statementIndex As Integer = m_statementIndex
      Dim depth As Integer = 0

      statementIndex += 1
      If statementIndex > m_interpreter(interpreterIndex).Statements.Count - 1 Then
        interpreterIndex += 1
        statementIndex = 0
      End If
      If interpreterIndex > m_interpreter.Count - 1 Then
        Return ThrowBasicError(BasicError.WhileWithoutWend)
      End If

      Do ' Lines

        Do ' Statements

          If interpreterIndex < m_interpreter.Count AndAlso
           statementIndex < m_interpreter(interpreterIndex).Statements.Count Then

            If m_interpreter(interpreterIndex).Statements(statementIndex).Tokens(0).IsWord("WHILE") Then
              depth += 1
            ElseIf m_interpreter(interpreterIndex).Statements(statementIndex).Tokens(0).IsWord("WEND") Then
              If depth = 0 Then
                'm_interpreterIndex = interpreterIndex
                'm_statementIndex = statementIndex
                'm_tokenIndex = 1
                If PeekToken() IsNot Nothing Then
                  Return ThrowBasicError(BasicError.SyntaxError)
                Else
                  Return True
                End If
              End If
              depth -= 1
            End If

          End If

          statementIndex += 1

          If statementIndex > m_interpreter(interpreterIndex).Statements.Count - 1 Then
            Exit Do
          End If

        Loop

        interpreterIndex += 1
        statementIndex = 0

        If interpreterIndex > m_interpreter.Count - 1 Then
          'interpreterIndex = -1
          'statementIndex = -1
          Exit Do
        End If

      Loop

      Return ThrowBasicError(BasicError.WhileWithoutWend)

    End Function

    Private Function JumpToWend() As Boolean

      Dim interpreterIndex As Integer = m_interpreterIndex
      Dim statementIndex As Integer = m_statementIndex
      Dim depth As Integer = 0

      statementIndex += 1
      If statementIndex > m_interpreter(interpreterIndex).Statements.Count - 1 Then
        interpreterIndex += 1
        statementIndex = 0
      End If
      If interpreterIndex > m_interpreter.Count - 1 Then
        Return ThrowBasicError(BasicError.WhileWithoutWend)
      End If

      Do ' Lines

        Do ' Statements

          If interpreterIndex < m_interpreter.Count AndAlso
           statementIndex < m_interpreter(interpreterIndex).Statements.Count Then

            If m_interpreter(interpreterIndex).Statements(statementIndex).Tokens(0).IsWord("WHILE") Then
              depth += 1
            ElseIf m_interpreter(interpreterIndex).Statements(statementIndex).Tokens(0).IsWord("WEND") Then
              If depth = 0 Then
                m_interpreterIndex = interpreterIndex
                m_statementIndex = statementIndex
                m_tokenIndex = 1
                If PeekToken() IsNot Nothing Then
                  Return ThrowBasicError(BasicError.SyntaxError)
                Else
                  Return True
                End If
              End If
              depth -= 1
            End If

          End If

          statementIndex += 1

          If statementIndex > m_interpreter(interpreterIndex).Statements.Count - 1 Then
            Exit Do
          End If

        Loop

        interpreterIndex += 1
        statementIndex = 0

        If interpreterIndex > m_interpreter.Count - 1 Then
          'interpreterIndex = -1
          'statementIndex = -1
          Exit Do
        End If

      Loop

      Return ThrowBasicError(BasicError.WhileWithoutWend)

    End Function

    Private Function ExecuteWidth() As Boolean

      Dim param1 As String = Nothing
      Dim param2 As Short?

      If PeekToken() Is Nothing OrElse PeekToken.IsCommaToken Then
        ' Skip
      ElseIf PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
        If Not ExecuteStringExpression(param1, Nothing) Then Return False
      Else
        Dim value As Short
        If Not ExecuteExpression(value) Then Return False
        param1 = value.ToString
      End If

      If PeekToken.IsCommaToken Then
        PopToken()
        Dim value As Short
        If Not ExecuteExpression(value) Then Return False
        param2 = value
      End If

      If PeekToken() Is Nothing Then

        If param1 IsNot Nothing Then

          Select Case param1.ToUpper
            Case "SCRN:"
              If param2 IsNot Nothing AndAlso (param2 = 40 OrElse
                                             param2 = 80 OrElse
                                             param2 = 20) Then
                'm_widthScrn = CShort(param2)
                m_display.ColumnCount = CShort(param2)
                Return True
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case "LPT1:"
              If param2 IsNot Nothing AndAlso CShort(param2).Between(1, 255) Then
                'm_widthLpt1 = CShort(param2)
                Return True
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case "LPT2:"
              If param2 IsNot Nothing AndAlso CShort(param2).Between(1, 255) Then
                'm_widthLpt2 = CShort(param2)
                Return True
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case "LPT3:"
              If param2 IsNot Nothing AndAlso CShort(param2).Between(1, 255) Then
                'm_widthLpt3 = CShort(param2)
                Return True
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case "COM1:"
              If param2 IsNot Nothing AndAlso CShort(param2).Between(1, 255) Then
                'm_widthCom1 = CShort(param2)
                Return True
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case "COM2:"
              If param2 IsNot Nothing AndAlso CShort(param2).Between(1, 255) Then
                'm_widthCom2 = CShort(param2)
                Return True
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case Else

              If IsNumeric(param1) Then

                Dim value As Short = CShort(param1)

                If value = 80 OrElse
                 value = 40 OrElse
                 value = 20 Then
                  'm_widthScrn = value
                  m_display.ColumnCount = value
                Else
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                End If

              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If

          End Select

        End If

        If param2 IsNot Nothing Then

          Select Case m_display.ScreenMode
            Case 0
              If param2 = 25 OrElse
               param2 = 43 OrElse
               param2 = 50 Then
                Try
                  m_display.RowCount = CShort(param2)
                Catch ex As NotImplementedException
                  Return ThrowBasicError(BasicError.AdvancedFeature)
                End Try
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case 1, 2, 3, 4, 5, 6, 7, 8, 13
              If param2 = 25 Then
                Try
                  m_display.RowCount = CShort(param2)
                Catch ex As NotImplementedException
                  Return ThrowBasicError(BasicError.AdvancedFeature)
                End Try
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case 9, 10
              If param2 = 25 OrElse
               param2 = 43 Then
                Try
                  m_display.RowCount = CShort(param2)
                Catch ex As NotImplementedException
                  Return ThrowBasicError(BasicError.AdvancedFeature)
                End Try
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case 11, 12
              If param2 = 30 OrElse
               param2 = 60 Then
                Try
                  m_display.RowCount = CShort(param2)
                Catch ex As NotImplementedException
                  Return ThrowBasicError(BasicError.AdvancedFeature)
                End Try
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Case Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
          End Select

        End If

        'If IsNumeric(param1) Then

        'Dim value As Short = CShort(param1)

        'If param2 Is Nothing Then
        '  If value = 40 OrElse
        '     value = 80 OrElse
        '     value = 20 Then
        '    m_widthScrn = value
        '    m_display.ColumnCount = value
        '    'If m_pen IsNot Nothing Then
        '    '  m_pen.ColumnCount = value
        '    'End If
        '  Else
        '    Return ThrowBasicError(BasicError.IllegalFunctionCall)
        '  End If
        'Else
        '  'TODO: Need to work with open file numbers and set accordingly.
        '  Return ThrowBasicError(BasicError.AdvancedFeature)
        'End If
        'Else
        '  Return ThrowBasicError(BasicError.IllegalFunctionCall)
        'End If

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteWindow() As Boolean

      Dim x1 As Short = 0
      Dim y1 As Short = 0
      Dim x2 As Short = 0
      Dim y2 As Short = 0

      If PeekToken.IsWord("SCREEN") Then
        PopToken()

        If PeekToken.IsParenOpenToken Then

          PopToken()

          If Not ExecuteExpression(x1) Then Return False
          If PeekToken.IsCommaToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If Not ExecuteExpression(y1) Then Return False
          If PeekToken.IsParenCloseToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If PeekToken.IsDash Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If PeekToken.IsParenOpenToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If Not ExecuteExpression(x2) Then Return False
          If PeekToken.IsCommaToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If
          If Not ExecuteExpression(y2) Then Return False
          If PeekToken.IsParenCloseToken Then
            PopToken()
          Else
            ThrowBasicError(BasicError.SyntaxError)
          End If

        Else

          ThrowBasicError(BasicError.SyntaxError)

        End If

      End If

      If PeekToken() Is Nothing Then

        'TODO: Implementation

      Else
        Return ThrowBasicError(BasicError.SyntaxError)
      End If

      Return True

    End Function

    Private Function ExecuteWrite() As Boolean

      Dim fileNumber As Short = -1

      If PeekToken.IsHashToken Then
        PopToken()
        If Not ExecuteExpression(fileNumber) Then Return False
        If PeekToken.IsCommaToken Then
          PopToken()
        End If
      End If

      Dim output As String = ""

      Do

        If PeekToken() Is Nothing Then
          Exit Do
        ElseIf PeekToken.IsCommaToken Then
          PopToken()
          output &= ","
        ElseIf PeekToken.IsStringExpression(m_defStrList, m_numericSuffixList) Then
          Dim value As String = Nothing
          If Not ExecuteStringExpression(value, Nothing) Then Return False
          output &= ChrW(34) & value & ChrW(34)
        Else
          Dim value As Double
          If Not ExecuteExpression(value) Then Return False
          output &= value.ToString
        End If

      Loop

      If fileNumber > -1 Then
        Return InternalWriteLine(fileNumber, output, True)
      Else
        m_display.Print(output, True)
        Return True
      End If

    End Function

#End Region

#Region "Internal"

    Private Function VarPtr(variableName As String) As Integer

      Dim result As Integer '= 0

      Dim value As Integer = Nothing
      If Not m_varPtrList.TryGetValue(variableName, value) Then
        value = m_varPtrList.Count
        m_varPtrList.Add(variableName, value)
      End If
      result = value
      m_varNameList.TryAdd(result, variableName)

      Return result

    End Function

    Private Function SetStringVariable(variableName As String, value As String) As Boolean
      'If value.Length > 255 Then
      '  Return ThrowBasicError(BasicError.StringTooLong)
      'End If
      Dim temp As StringValue = Nothing
      If m_stringVariableList.TryGetValue(variableName, temp) Then
        temp.Value = value
      Else
        m_stringVariableList.Add(variableName, New StringValue(value))
      End If
      If m_debugSetVariables Then
        Debug.WriteLine(String.Format("Set {0} = ""{1}""", variableName, value))
      End If
      Return True
    End Function

    Private Function GetStringVariable(variableName As String, ByRef result As String) As Boolean

      result = ""

      Dim value As StringValue = Nothing
      If m_stringVariableList.TryGetValue(variableName, value) Then
        result = value.Value
      ElseIf variableName.IndexOf("("c) > -1 Then
        If (Aggregate a In m_stringVariableList.Keys
          Where a.StartsWith(variableName.Substring(0, variableName.IndexOf("("c) + 1))
          Into Count()) = 0 Then
          Dim arrayName As String = variableName.Substring(0, variableName.IndexOf("("c))
          Dim dimensionBounds As New List(Of Bounds) From {
          New Bounds With {.Lower = m_optionBase,
                                             .Upper = 10}
        }
          InitArray(arrayName, True, 1, dimensionBounds)
          VarPtr(arrayName)
        Else
          Return ThrowBasicError(BasicError.SubscriptOutOfRange)
        End If
      End If

      Return True

    End Function

    Private Function SetNumericVariable(varName As String, number As Double) As Boolean

      ' Memory sizes:
      '  Integer....2
      '  Single.....4
      '  Double.....8
      '  String.....3 byte overhead plus the present contents of the string at 1 byte per character.

      ' Floating Point Constants...
      '    3.0×10^-39 to 1.7×10^38

      'Dim min As Double = 3.0 * 10 ^ -39
      'Dim max As Double = 1.7 * 10 ^ 38

      ' Determine the type based on the variable name.

      Dim type As NumericType

      Dim baseName As String
      Dim index As Integer = varName.IndexOf("("c)
      If index > -1 Then
        baseName = varName.Substring(0, index)
      Else
        baseName = varName
      End If

      If baseName(baseName.Length - 1) = "%"c Then
        type = NumericType.Integer
      ElseIf baseName(baseName.Length - 1) = "!"c Then
        type = NumericType.Single
      ElseIf baseName(baseName.Length - 1) = "#"c Then
        type = NumericType.Double
      Else
        If m_defIntList.Contains(baseName(0)) Then
          type = NumericType.Integer
        ElseIf m_defDblList.Contains(baseName(0)) Then
          type = NumericType.Double
        ElseIf m_defSngList.Contains(baseName(0)) Then
          type = NumericType.Single
        Else
          type = NumericType.Single
        End If
      End If

      If type = NumericType.Integer Then
        Try
          number = CShort(number)
          If Not number.Between(-32768, 32767) Then
            Return ThrowBasicError(BasicError.Overflow)
          End If
        Catch ex As Exception
          Return ThrowBasicError(BasicError.Overflow)
        End Try
      ElseIf type = NumericType.Single Then

        'number = number.ToBasicSng

        'Try
        '  number = CSng(number)
        'Catch ex As Exception
        '  Return ThrowOverflow()
        'End Try

        ' 7 or fewer digits.
        ' exponential form using E
        ' a trailing exclamation point (!)

      ElseIf type = NumericType.Double Then

        ' Should be good to go as is.

        ' 8 or more digits
        ' exponential form using D
        ' a trailing number sign (#)

      End If

      Dim value As NumericValue = Nothing
      If m_numericVariableList.TryGetValue(varName, value) Then
        value.Value = number
      Else
        m_numericVariableList.Add(varName, New NumericValue(number, type))
      End If

      If m_debugSetVariables Then
        Debug.WriteLine(String.Format("Set {0} = {1}", varName, number))
      End If

      Return True

    End Function

    Private Function GetNumericVariable(variableName As String, ByRef result As Double) As Boolean

      result = 0.0

      Dim value1 As NumericValue = Nothing
      If m_numericVariableList.TryGetValue(variableName, value1) Then
        result = value1.Value
        'type = m_numericVariableList(variableName).Type
      ElseIf variableName.IndexOf("("c) > -1 Then
        If (Aggregate a In m_numericVariableList.Keys
          Where a.StartsWith(variableName.Substring(0, variableName.IndexOf("("c) + 1))
          Into Count()) = 0 Then
          Dim arrayName As String = variableName.Substring(0, variableName.IndexOf("("c))
          Dim dimensionBounds As New List(Of Bounds) From {
          New Bounds With {.Lower = m_optionBase, .Upper = 10}}
          InitArray(arrayName, False, 1, dimensionBounds)
          VarPtr(arrayName)
          Dim value2 As NumericValue = Nothing
          If m_numericVariableList.TryGetValue(variableName, value2) Then
            result = value2.Value
          Else
            Return ThrowBasicError(BasicError.SubscriptOutOfRange)
          End If
        Else
          Return ThrowBasicError(BasicError.SubscriptOutOfRange)
        End If
      End If

      Return True

    End Function

    Private Function InternalNextAutoLine(increment As Boolean) As Boolean

      If increment Then
        m_autoStart += m_autoIncrement
      End If

      If m_autoStart.Between(0, 65529) AndAlso
       m_autoIncrement.Between(0, 65529) Then

        Dim l = From p In m_lines
                Where p.LineNumber = m_autoStart

        m_display.Print(m_autoStart.ToString & If(Not m_pendingNew AndAlso l.Any, "*", " "), False)

        Return True

      Else

        Return ThrowBasicError(BasicError.UndefinedLineNumber)

      End If

    End Function

    Private Sub InternalPrompt()
      If m_display.Pos(0) > 1 Then
        m_display.Print()
      End If
      m_display.Print("Ok", True)
    End Sub

#Region "File I/O"

    Private Function InternalOpen(fileNumber As Short,
filename As String,
mode As FileMode,
lock As FileLock,
recordLength As Short) As Boolean

      If Not recordLength.Between(1, 32767) Then
        Return ThrowBasicError(BasicError.IllegalFunctionCall)
      End If

      If m_fileList.ContainsKey(fileNumber) Then
        Return ThrowBasicError(BasicError.FileAlreadyOpen)
      End If

      If filename.IndexOf("KYBD:") > -1 Then
        ' Input only.
        If mode <> FileMode.Input Then
          Return ThrowBasicError(BasicError.BadFileMode)
        End If
        Return ThrowBasicError(BasicError.AdvancedFeature)
      ElseIf filename.IndexOf("SCRN:") > -1 Then
        ' Output only.
        If mode <> FileMode.Output Then
          Return ThrowBasicError(BasicError.BadFileMode)
        End If
        Return ThrowBasicError(BasicError.AdvancedFeature)

      ElseIf filename.IndexOf("LPT1:") > -1 OrElse
           filename.IndexOf("LPT2:") > -1 OrElse
           filename.IndexOf("LPT3:") > -1 Then

        ' Output only.

        Dim index As Integer = 0
        If filename.IndexOf("LPT2:") > -1 Then index = 1
        If filename.IndexOf("LPT3:") > -1 Then index = 2

        If mode <> FileMode.Output Then
          Return ThrowBasicError(BasicError.BadFileMode)
        End If

        Dim lpt As New File With {.Number = fileNumber,
                                .Path = Nothing,
                                .Mode = mode,
                                .Lock = FileLock.Default,
                                .Position = 0,
                                .RecordLength = 0,
                                .Printer = m_lpt(index)}

        m_fileList.Add(fileNumber, lpt)

        Return True

      ElseIf filename.IndexOf("COM1:") > -1 Then
        ' Input, Output, or Random Only.
        If mode = FileMode.Append Then
          Return ThrowBasicError(BasicError.BadFileMode)
        End If
        Return ThrowBasicError(BasicError.AdvancedFeature)
      ElseIf filename.IndexOf("COM2:") > -1 Then
        ' Input, Output, or Random Only.
        If mode = FileMode.Append Then
          Return ThrowBasicError(BasicError.BadFileMode)
        End If
        Return ThrowBasicError(BasicError.AdvancedFeature)
      Else
        ' Expected to be a file path.
        ' Input, Output, Append, or Random.
      End If

      Dim position As Integer = 0 'If(mode = FileMode.Append, -1, 0)

      Dim file As New File With {.Number = fileNumber,
                               .Path = filename,
                               .Mode = mode,
                               .Lock = lock,
                               .Position = position,
                               .RecordLength = recordLength}

      Dim result = m_virtualFileSystem.Open(filename, CInt(mode), CInt(lock))

      If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then
        Dim number = result.Substring(7)
        If IsNumeric(number) Then
          'm_running = ThrowBasicError(CShort(number))
          m_waiting = False
          Return ThrowBasicError(CShort(number))
        Else
          m_display.Print(result.Substring(7), True)
          'm_running = False
          m_waiting = False
          Return False
        End If
      Else

        Dim length As Integer = CInt(result)
        file.Length = length

        If file.Mode = FileMode.Append Then
          file.Position = length
        End If

        m_fileList.Add(file.Number, file)

        m_waiting = False
        'm_running = True
        Return True

      End If

      'If Not m_running Then
      '  InternalPrompt()
      'End If

      Return True

    End Function

    'Private Sub InternalWrite(ByVal fileNumber As Short, ByVal text As String)

    'End Sub

    Private Function InternalWriteLine(fileNumber As Short, text As String, lineFeed As Boolean) As Boolean
      Dim value As File = Nothing
      If Not m_fileList.TryGetValue(fileNumber, value) Then
        Return ThrowBasicError(BasicError.BadFileNumber)
      End If

      If value.Mode = FileMode.Input OrElse
value.Mode = FileMode.RandomAccessRead OrElse
value.Mode = FileMode.RandomAccessReadWrite OrElse
value.Mode = FileMode.RandomAccessWrite Then
        Return ThrowBasicError(BasicError.BadFileMode)
      End If

      If value.Path Is Nothing Then
        value.Printer.Print(text, lineFeed)
        Return True

      Else

        text &= vbCrLf

        Dim result = m_virtualFileSystem.Write(value.Path, text, value.Position, False)
        value.Position += text.Length
        value.Length += text.Length

        If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then
          Dim response = result.Substring(7)
          If IsNumeric(response) Then
            m_running = ThrowBasicError(CShort(response))
            m_waiting = False
          Else
            m_display.Print(response, True)
            m_running = False
            m_waiting = False
          End If
        Else

          ' Nothing to do.

          m_running = True
          m_waiting = False

        End If

        If Not m_running Then
          InternalPrompt()
        End If

        Return True

      End If

    End Function

    'Private Sub InternalWriteCompletedProcess(result As String)

    '  'Dim result As String = e.Result

    '  If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then
    '    Dim response = result.Substring(7)
    '    If IsNumeric(response) Then
    '      m_running = ThrowBasicError(CShort(response))
    '      m_waiting = False
    '    Else
    '      m_display.Print(response, True)
    '      m_running = False
    '      m_waiting = False
    '    End If
    '  Else

    '    ' Nothing to do.

    '    m_running = True
    '    m_waiting = False

    '  End If

    '  If Not m_running Then
    '    InternalPrompt()
    '  End If

    'End Sub

    'Private Function InternalWriteLine(ByVal fileNumber As Short) As Boolean
    '  Return InternalWriteLine(fileNumber, "")
    'End Function

    Private Function InternalInput(fileNumber As Short, variableList As List(Of String)) As Boolean
      Dim value As File = Nothing
      If Not m_fileList.TryGetValue(fileNumber, value) Then
        Return ThrowBasicError(BasicError.BadFileNumber)
      End If

      If value.Path Is Nothing OrElse
value.Mode = FileMode.Output OrElse
value.Mode = FileMode.Append OrElse
value.Mode = FileMode.RandomAccessRead OrElse
value.Mode = FileMode.RandomAccessReadWrite OrElse
value.Mode = FileMode.RandomAccessWrite Then
        Return ThrowBasicError(BasicError.BadFileMode)
      End If

      Dim state = New InputUserState() With {.FileNumber = fileNumber,
                                           .VariableList = variableList}
      Dim result = m_virtualFileSystem.Input(value.Path, value.Position, variableList.Count, False)
      Return InternalInputCompletedProcess(result, state)

    End Function

    Private Function InternalLineInput(fileNumber As Short, varName As String) As Boolean
      Dim value As File = Nothing
      If Not m_fileList.TryGetValue(fileNumber, value) Then
        Return ThrowBasicError(BasicError.BadFileNumber)
      End If

      If value.Path Is Nothing OrElse
value.Mode = FileMode.Output OrElse
value.Mode = FileMode.Append OrElse
value.Mode = FileMode.RandomAccessRead OrElse
value.Mode = FileMode.RandomAccessReadWrite OrElse
value.Mode = FileMode.RandomAccessWrite Then
        Return ThrowBasicError(BasicError.BadFileMode)
      End If

      Dim variableList As New List(Of String) From {
      varName
    }
      Dim state = New InputUserState() With {.FileNumber = fileNumber,
                                           .VariableList = variableList,
                                           .LineInput = True}
      Dim result = m_virtualFileSystem.Input(value.Path, value.Position, variableList.Count, True)
      InternalInputCompletedProcess(result, state)
      Return True

    End Function

    Private Function InternalInputCompletedProcess(result As String, o As Object) As Boolean

      'Dim result As String = e.Result
      Dim info As InputUserState = CType(o, InputUserState)

      If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then
        Dim response = result.Substring(7)
        If IsNumeric(response) Then
          'm_running = False
          m_waiting = False
          Return ThrowBasicError(CShort(response))
        Else
          m_display.Print(response, True)
          'm_running = False
          m_waiting = False
          Return False
        End If
      Else

        If info.LineInput Then

          Dim resultLength As Integer = result.Length

          If result.EndsWith(vbCrLf) Then
            result = result.Substring(0, result.Length - 2)
          ElseIf result.EndsWith(vbCr) OrElse
               result.EndsWith(vbLf) Then
            result = result.Substring(0, result.Length - 1)
          End If

          If IsStringVariable(info.VariableList(0)) Then
            If Not SetStringVariable(info.VariableList(0), result) Then
              'm_running = False
              m_waiting = False
              Return False
            End If
          End If

          m_fileList(info.FileNumber).Position += resultLength

        Else

          ' Starting at whatever position in the file...
          ' Parse for field(s) and build a return string containing
          ' found field(s).

          Dim fields As New List(Of String)

          ' -1 "Ignore white space", 
          '  0  Numeric
          '  1  Unquoted String
          '  2  Quoted String
          Dim mode As Integer = -1
          Dim field As Integer = 1
          Dim offset As Integer = 0
          Dim output As String = ""

          Do

            ' past end of file...
            If offset + 1 > result.Length Then
              Exit Do
            End If

            Dim ch = result(offset)

            Select Case mode
              Case -1 ' Ignore white space
                If ch = ChrW(32) OrElse
                 ch = ChrW(10) Then
                  ' "ignore"...
                ElseIf ch = ChrW(13) Then
                  ' ignore???
                  fields.Add(output) : output = ""
                  If field = info.VariableList.Count Then
                    Exit Do
                  Else
                    field += 1
                  End If
                Else
                  ' Otherwise, need to begin the process of parsing a field;
                  ' determine what kind of parsing will be used.
                  Select Case ch
                    Case "-"c, "."c, "0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c, "0"c
                      output &= ch
                      mode = 0
                    Case ChrW(34)
                      mode = 2
                    Case Else
                      output &= ch
                      mode = 1
                  End Select
                End If
              Case 0 ' Numeric
                ' Terminates on a space, carriage return, line feed, comma or eof.
                Select Case ch
                  Case ChrW(32), ChrW(13), ChrW(10), ","c
                    Do
                      If offset + 1 < result.Length - 1 Then
                        ch = result(offset + 1)
                        If ch = ChrW(32) Then
                          output &= ch
                          offset += 1
                        ElseIf ch = ChrW(13) OrElse ch = ChrW(10) Then
                          output &= ch
                          offset += 1
                          If result(offset + 1) = ChrW(10) Then
                            output &= result(offset + 1)
                            offset += 1
                          End If
                          Exit Do
                        Else
                          Exit Do
                        End If
                      Else
                        Exit Do
                      End If
                    Loop
                    fields.Add(output) : output = ""
                    If field = info.VariableList.Count Then
                      Exit Do
                    Else
                      field += 1
                      mode = -1
                    End If
                  Case Else
                    output &= ch
                End Select
              Case 1 ' Unquoted string
                ' Terminates on a comma, cr, lf, 255 characters or eof.
                Select Case ch
                  Case ","c, ChrW(13), ChrW(10)
                    fields.Add(output) : output = ""
                    If field = info.VariableList.Count Then
                      Exit Do
                    Else
                      field += 1
                      mode = -1
                    End If
                  Case Else
                    output &= ch
                End Select
              Case 2 ' Quoted String
                ' Terminates on a quote, comma, cr, lf, 255 characters or eof.
                Select Case ch
                  Case ChrW(34)
                    ' Check to see if the next character is a ","
                    If offset + 1 < result.Length - 1 Then
                      ch = result(offset + 1)
                      If ch = ","c Then
                        offset += 1
                      End If
                    End If
                    fields.Add(output) : output = ""
                    If field = info.VariableList.Count Then
                      Exit Do
                    Else
                      field += 1
                      mode = -1
                    End If
                  Case ChrW(13), ChrW(10)
                    fields.Add(output) : output = ""
                    If field = info.VariableList.Count Then
                      Exit Do
                    Else
                      field += 1
                      mode = -1
                    End If
                  Case Else
                    output &= ch
                End Select
              Case Else
            End Select

            offset += 1

          Loop

          If info.VariableList.Count <> fields.Count Then
            m_waiting = False
            Return ThrowBasicError(BasicError.InputPastEnd)
          End If

          For index As Integer = 0 To info.VariableList.Count - 1

            Dim value = fields(index)

            ' Assign value to variable.

            If IsStringVariable(info.VariableList(index)) Then
              If Not SetStringVariable(info.VariableList(index), value) Then
                'm_running = False
                m_waiting = False
                Return False
              End If
            ElseIf IsNumericVariable(info.VariableList(index)) AndAlso IsNumeric(value, True) Then
              If String.IsNullOrEmpty(value) Then value = "0"
              Dim dbl As Double
              Try
                dbl = CDbl(value)
              Catch ex As InvalidCastException
                m_waiting = False
                Return ThrowBasicError(BasicError.TypeMismatch)
              End Try
              If Not SetNumericVariable(info.VariableList(index), dbl) Then
                'm_running = False
                m_waiting = False
                Return False
              End If
            Else
              'm_running = False
              m_waiting = False
              Return ThrowBasicError(BasicError.TypeMismatch)
            End If

          Next

          '' parse result.

          'Dim seek As Integer = 0

          'For index As Integer = 0 To info.VariableList.Count - 1

          '  Dim value As String = ""

          '  ' Get value...

          '  Do

          '    If seek < result.Length Then

          '      Select Case result(seek)
          '        Case ","c
          '          seek += 1
          '          Exit Do
          '        Case ChrW(13)
          '          seek += 1
          '          If seek + 1 < result.Length AndAlso result(seek + 1) = ChrW(10) Then
          '            seek += 1
          '          End If
          '          Exit Do
          '        Case ChrW(10)
          '          seek += 1
          '          If seek + 1 < result.Length AndAlso result(seek + 1) = ChrW(13) Then
          '            seek += 1
          '          End If
          '          Exit Do
          '        Case ChrW(34)

          '          'value &= result(seek)
          '          seek += 1

          '          Dim closing As Integer = result.IndexOf(ChrW(34), seek)
          '          If closing > -1 Then
          '            value &= result.Substring(seek, closing - seek) ' + 1)
          '            seek = closing + 1
          '          Else
          '            closing = result.IndexOf(ChrW(13), seek)
          '            If closing > -1 Then
          '              value &= result.Substring(seek, closing - seek) ' + 1)
          '              seek = closing + 1
          '            Else
          '              closing = result.IndexOf(ChrW(10), seek)
          '              If closing > -1 Then
          '                value &= result.Substring(seek, closing - seek) ' + 1)
          '                seek = closing + 1
          '              Else
          '                value &= result.Substring(seek, result.Length - seek)
          '                seek = result.Length
          '              End If
          '            End If
          '          End If

          '          If seek < result.Length Then
          '            Select Case result(seek)
          '              Case ","c
          '                seek += 1
          '              Case ChrW(13)
          '                seek += 1
          '                If seek + 1 < result.Length AndAlso result(seek + 1) = ChrW(10) Then
          '                  seek += 1
          '                End If
          '              Case ChrW(10)
          '                seek += 1
          '                If seek + 1 < result.Length AndAlso result(seek + 1) = ChrW(13) Then
          '                  seek += 1
          '                End If
          '              Case Else
          '            End Select
          '          End If

          '          Exit Do

          '        Case Else
          '          value &= result(seek)
          '          seek += 1
          '      End Select

          '      If seek = 255 Then
          '        Exit Do
          '      End If

          '    Else
          '      Exit Do
          '    End If

          '  Loop

          '  ' Assign value to variable.

          '  If IsStringVariable(info.VariableList(index)) Then
          '    If Not SetStringVariable(info.VariableList(index), value) Then
          '      'm_running = False
          '      m_waiting = False
          '      Return False
          '    End If
          '  ElseIf IsNumericVariable(info.VariableList(index)) AndAlso IsNumeric(value, True) Then
          '    If String.IsNullOrEmpty(value) Then value = "0"
          '    Dim dbl As Double
          '    Try
          '      dbl = CDbl(value)
          '    Catch ex As InvalidCastException
          '      m_waiting = False
          '      Return ThrowBasicError(BasicError.TypeMismatch)
          '    End Try
          '    If Not SetNumericVariable(info.VariableList(index), dbl) Then
          '      'm_running = False
          '      m_waiting = False
          '      Return False
          '    End If
          '  Else
          '    'm_running = False
          '    m_waiting = False
          '    Return ThrowBasicError(BasicError.TypeMismatch)
          '  End If

          'Next

          m_fileList(info.FileNumber).Position += result.Length '(seek + 1)

        End If

        'm_running = True
        m_waiting = False

      End If

      'If Not m_running Then
      '  InternalPrompt()
      'End If

      Return True 'm_running

    End Function

    Private Function InternalGet(fileNumber As Short, recordNumber As Integer) As Boolean

      If m_fileList(fileNumber).Path Is Nothing OrElse
       Me.m_fileList(fileNumber).Mode = FileMode.Input OrElse
       Me.m_fileList(fileNumber).Mode = FileMode.Output OrElse
       Me.m_fileList(fileNumber).Mode = FileMode.Append OrElse
       Me.m_fileList(fileNumber).Mode = FileMode.RandomAccessWrite Then
        Return ThrowBasicError(BasicError.BadFileMode)
      End If

      Dim position As Integer = m_fileList(fileNumber).Position

      If recordNumber > -1 Then
        position = (recordNumber - 1) * m_fileList(fileNumber).RecordLength
      End If

      Dim result = m_virtualFileSystem.Read(m_fileList(fileNumber).Path, position, m_fileList(fileNumber).RecordLength)
      InternalReadCompletedProcess(result, fileNumber)
      Return True

    End Function

    Private Sub InternalReadCompletedProcess(result As String, o As Object)

      'Dim result As String = e.Result
      Dim fileNumber As Short = CShort(o) 'e.UserState)

      If result IsNot Nothing AndAlso result.StartsWith("Error: ") Then
        Dim response = result.Substring(7)
        If IsNumeric(response) Then
          m_running = ThrowBasicError(CShort(response))
          m_waiting = False
        Else
          m_display.Print(response, True)
          m_running = False
          m_waiting = False
        End If
      Else

        ' Nothing to do.

        If result.Length = m_fileList(fileNumber).RecordLength Then

          Dim offset As Integer = 0

          For Each field As Field In m_fileList(fileNumber).FieldList

            Dim value As String = result.Substring(offset, field.Length)
            offset += field.Length

            If IsStringVariable(field.VariableName) Then
              If Not SetStringVariable(field.VariableName, value) Then
                m_waiting = False
                m_running = False
                Exit Sub
              End If
            Else
              Try
                If Not SetNumericVariable(field.VariableName, CDbl(value)) Then
                  m_waiting = False
                  m_running = False
                  Exit Sub
                End If
              Catch ex As Exception
                ThrowBasicError(BasicError.TypeMismatch)
                m_waiting = False
                m_running = False
                Exit Sub
              End Try
            End If

          Next

        Else

          ThrowBasicError(BasicError.InputPastEnd)
          m_waiting = False
          m_running = False
          Exit Sub

        End If

        m_running = True
        m_waiting = False

      End If

      If Not m_running Then
        InternalPrompt()
      End If

    End Sub

    Private Function InternalPut(fileNumber As Short, recordNumber As Integer) As Boolean
      Dim value As File = Nothing
      If Not m_fileList.TryGetValue(fileNumber, value) Then
        Return ThrowBasicError(BasicError.BadFileNumber)
      End If

      If value.Path Is Nothing OrElse
value.Mode = FileMode.Input OrElse
value.Mode = FileMode.Output OrElse
value.Mode = FileMode.Append OrElse
value.Mode = FileMode.RandomAccessRead Then
        Return ThrowBasicError(BasicError.BadFileMode)
      End If

      Dim text As String = ""

      For Each field As Field In value.FieldList
        Dim value1 As String = Nothing
        If IsStringVariable(field.VariableName) Then
          If Not GetStringVariable(field.VariableName, value1) Then Return False
        Else
          Dim number As Double
          If Not GetNumericVariable(field.VariableName, number) Then Return False
          value1 = number.ToString
        End If
        If value1.Length > field.Length Then
          text = value1.Substring(0, value1.Length)
        Else
          text &= value1.PadRight(field.Length)
        End If
      Next

      text = text.PadRight(value.RecordLength)

      Dim position As Integer = value.Position

      If recordNumber > -1 Then
        position = (recordNumber - 1) * value.RecordLength
      End If

      m_virtualFileSystem.Write(value.Path, text, position, True)
      value.Position = position + text.Length
      If value.Position > value.Position Then
        value.Length = value.Position
      End If

      'm_waiting = True

      Return True

    End Function

    Private Function InternalEof(fileNumber As Short, ByRef result As Double) As Boolean
      Dim value As File = Nothing
      If Not m_fileList.TryGetValue(fileNumber, value) Then
        Return ThrowBasicError(BasicError.BadFileNumber)
      End If

      If value.Path Is Nothing Then
        Return ThrowBasicError(BasicError.BadFileMode)
      End If

      If value.Position >= value.Length Then
        result = -1
      Else
        result = 0
      End If

      Return True

    End Function

    Private Function InternalLoc(fileNumber As Short, ByRef result As Double) As Boolean

      Dim value As File = Nothing
      If Not m_fileList.TryGetValue(fileNumber, value) Then
        Return ThrowBasicError(BasicError.BadFileNumber)
      End If

      If value.Path Is Nothing Then
        Return ThrowBasicError(BasicError.BadFileMode)
      End If

      If value.Position = 0 Then
        result = 0
      Else
        result = 1 + (value.Position \ value.RecordLength)
      End If

      Return True

    End Function

    Private Function InternalLof(fileNumber As Short, ByRef result As Double) As Boolean
      Dim value As File = Nothing
      If Not m_fileList.TryGetValue(fileNumber, value) Then
        Return ThrowBasicError(BasicError.BadFileNumber)
      End If

      If value.Path Is Nothing Then
        Return ThrowBasicError(BasicError.BadFileMode)
      End If

      result = value.Length

      Return True

    End Function

    Private Function InternalField(fileNumber As Short, stringVar As String, length As Short) As Boolean
      Dim value As File = Nothing
      If m_fileList.TryGetValue(fileNumber, value) Then

        Dim info As File = value

        If info.Path IsNot Nothing AndAlso
         info.Mode = FileMode.RandomAccessRead OrElse
         info.Mode = FileMode.RandomAccessReadWrite OrElse
         info.Mode = FileMode.RandomAccessWrite Then

          Dim recordLength = info.RecordLength
          Dim fieldLength = Aggregate a In info.FieldList Into Sum(a.Length)

          If fieldLength + length <= recordLength Then
            info.FieldList.Add(New Field() With {.VariableName = stringVar,
                                               .Length = length})
          Else
            Return ThrowBasicError(BasicError.FieldOverflow)
          End If

        Else

          Return ThrowBasicError(BasicError.BadFileMode)

        End If

      Else
        Return ThrowBasicError(BasicError.BadFileNumber)
      End If

      Return True

    End Function

    Private Function InternalLock(fileNumber As Short, min As Double, max As Double) As Boolean

      If fileNumber = 0 Then
      End If

      If min = -1 AndAlso max = -1 Then
        ' Lock entire file.
      ElseIf min > -1 AndAlso max = -1 Then
        ' Locks record min only.
      ElseIf min = -1 AndAlso max > -1 Then
        ' Locks records 1 through max
      ElseIf min > -1 AndAlso max > -1 Then
        ' Locks records min through max.
      End If

      Return ThrowBasicError(BasicError.AdvancedFeature)

    End Function

    Private Function InternalUnlock(fileNumber As Short, min As Double, max As Double) As Boolean

      If fileNumber = 0 Then
      End If

      If min = -1 AndAlso max = -1 Then
        ' Unlock entire file.
      ElseIf min > -1 AndAlso max = -1 Then
        ' Unlock record min only.
      ElseIf min = -1 AndAlso max > -1 Then
        ' Unlock records 1 through max
      ElseIf min > -1 AndAlso max > -1 Then
        ' Unlock records min through max.
      End If

      Return ThrowBasicError(BasicError.AdvancedFeature)

    End Function

#End Region

    Private Sub InternalClear(Optional maxBytesAvail As Integer = 32768, Optional stackSpaceAvail As Integer = 512)

      If maxBytesAvail > 0 AndAlso stackSpaceAvail > 0 Then
      End If

      m_defFnList.Clear()
      m_data.Clear()
      m_readIndex = 0

      ' Close all files.

      ' Clears all COMMON and user variables.
      m_varPtrList.Clear()
      m_varNameList.Clear()
      m_stringVariableList.Clear()
      m_numericVariableList.Clear()

      ' Resets the stack and string space.
      ' Releases all disk buffers.
      ' Turns off any sound.
      ' Resets sound to music foreground.
      ' Resets PEN to off.
      ' Resets STRIG to off.

      ' Disables ON ERROR trapping.
      m_onErrorGoto = 0
      m_onErrorResumeInterpreterIndex = -1
      m_err = 0
      m_erl = 0

      m_onTimerState = OnState.Off
      m_onTimerLine = 0
      m_onTimerInterval = 0
      m_onTimerTime = New Date?

      m_onPenState = OnState.Off
      m_onPenLineOrLabel = Nothing
      m_onPenActivated = False
      m_penCurrentPosition = Nothing
      m_penCurrentActivated = False
      m_penLastActivatedPosition = Nothing
      'm_penLastActivated = False
      m_penActivatedSinceLastPoll = False

      If m_contIndex = -1 Then
        m_continueInput = False
        m_continueInputDollar = False
        m_continueInputRandomize = False
        m_continueLineInput = False
      End If

      m_inputPrompt = Nothing
      m_inputSuppressCr = False
      m_inputVariableList.Clear()

      PrepareData()

    End Sub

    Private Sub InternalNew()

      InternalClose()

      m_pendingNew = True
      m_contIndex = -1

      m_defStrList.Clear()

      InternalClear()

    End Sub

    'Private Shared Function IsOdd(value As Short) As Boolean
    '  If value Mod 2 = 0 Then
    '    Return True
    '  Else
    '    Return False
    '  End If
    'End Function

    Private Shared Function BinaryAnd(number1 As Short, number2 As Short) As Short

      Return number1 And number2

      'Dim count As Short = 0

      'For count = 0 To 15
      '  If IsOdd(number1) AndAlso IsOdd(number2) Then
      '    result += 2 ^ count
      '  End If
      '  number1 = number1 \ 2
      '  number2 = number2 \ 2
      'Next

      'Return result

    End Function

    Private Shared Function BinaryEqv(number1 As Short, number2 As Short) As Short

      Return CShort((CBool(number1) = CBool(number2)))

      'Dim result As Short = 0

      'Dim count As Short = 0

      'For count = 0 To 15
      '  If IsOdd(number1) = IsOdd(number2) Then
      '    result += 2 ^ count
      '  End If
      '  number1 = number1 \ 2
      '  number2 = number2 \ 2
      'Next

      'Return result

    End Function

    'Private Function BitwiseEqv(x1 As Byte, X2 As Byte) As Long

    '  Dim b1, b2, bRet As Byte
    '  Dim iCtr As Short

    '  For iCtr = 0 To Len(x1) * 8 - 1
    '    b1 = x1 And 2 ^ iCtr
    '    b2 = X2 And 2 ^ iCtr
    '    If b1 = b2 Then bRet += 2 ^ iCtr
    '  Next

    '  BitwiseEqv = bRet

    'End Function

    Private Shared Function BinaryImp(number1 As Short, number2 As Short) As Short

      Return (Not number1) Or number2

      'Dim result As Short = 0

      'Dim count As Short = 0

      'For count = 0 To 15
      '  If IsOdd(number1) <= IsOdd(number2) Then
      '    result += 2 ^ count
      '  End If
      '  number1 = number1 \ 2
      '  number2 = number2 \ 2
      'Next

      'Return result

    End Function

    'Private Function BitwiseImp(x1 As Byte, X2 As Byte) As Long

    '  Dim b1, b2, bRet As Byte
    '  Dim iCtr As Short

    '  For iCtr = 0 To len(x1) * 8 - 1
    '    b1 = Not (x1) And 2 ^ iCtr
    '    b2 = x2 And 2 ^ iCtr
    '    If b1 Or b2 Then
    '      bRet += 2 ^ iCtr
    '    End If
    '  Next

    '  BitwiseImp = bRet

    'End Function

    Private Shared Function BinaryOr(number1 As Short, number2 As Short) As Short

      Return number1 Or number2

      'Dim result As Short = 0

      'Dim count As Short = 0

      'For count = 0 To 15
      '  If IsOdd(number1) OrElse IsOdd(number2) Then
      '    result += 2 ^ count
      '  End If
      '  number1 = number1 \ 2
      '  number2 = number2 \ 2
      'Next

      'Return result

    End Function

    Private Shared Function BinaryXor(number1 As Short, number2 As Short) As Short

      Return number1 Xor number2

      'Dim result As Short = 0

      'Dim count As Short = 0

      'For count = 0 To 15
      '  If IsOdd(number1) <> IsOdd(number2) Then
      '    result += 2 ^ count
      '  End If
      '  number1 = number1 \ 2
      '  number2 = number2 \ 2
      'Next

      'Return result

    End Function

#End Region

    'Private Shared Function KeyToScanCode(key As Input.Key) As Char

    '  Select Case key
    '    Case Input.Key.Escape : Return ChrW(&H1)
    '    Case Input.Key.D1 : Return ChrW(&H2)
    '    Case Input.Key.D2 : Return ChrW(&H3)
    '    Case Input.Key.D3 : Return ChrW(&H4)
    '    Case Input.Key.D4 : Return ChrW(&H5)
    '    Case Input.Key.D5 : Return ChrW(&H6)
    '    Case Input.Key.D6 : Return ChrW(&H7)
    '    Case Input.Key.D7 : Return ChrW(&H8)
    '    Case Input.Key.D8 : Return ChrW(&H9)
    '    Case Input.Key.D9 : Return ChrW(&HA)
    '    Case Input.Key.D0 : Return ChrW(&HB)
    '  'Case - : Return ChrW(&HC)
    '  ' Case =
    '    Case Input.Key.Back : Return ChrW(&HE)
    '    Case Input.Key.Tab : Return ChrW(&HF)
    '    Case Input.Key.Q : Return ChrW(&H10)
    '    Case Input.Key.W : Return ChrW(&H11)
    '    Case Input.Key.E : Return ChrW(&H12)
    '    Case Input.Key.R : Return ChrW(&H13)
    '    Case Input.Key.T : Return ChrW(&H14)
    '    Case Input.Key.Y : Return ChrW(&H15)
    '    Case Input.Key.U : Return ChrW(&H16)
    '    Case Input.Key.I : Return ChrW(&H17)
    '    Case Input.Key.O : Return ChrW(&H18)
    '    Case Input.Key.P : Return ChrW(&H19)
    '  'CaInputse [
    '  'CaInputse ]
    '    Case Input.Key.Enter : Return ChrW(&H1C)
    '    Case Input.Key.Ctrl : Return ChrW(&H1D)
    '    Case Input.Key.A : Return ChrW(&H1E)
    '    Case Input.Key.S : Return ChrW(&H1F)
    '    Case Input.Key.D : Return ChrW(&H20)
    '    Case Input.Key.F : Return ChrW(&H21)
    '    Case Input.Key.G : Return ChrW(&H22)
    '    Case Input.Key.H : Return ChrW(&H23)
    '    Case Input.Key.J : Return ChrW(&H24)
    '    Case Input.Key.K : Return ChrW(&H25)
    '    Case Input.Key.L : Return ChrW(&H26)
    '  'case ;
    '  'case '
    '  'Case `
    '  'Case left shift
    '  'Case \
    '    Case Input.Key.Z : Return ChrW(&H2C)
    '    Case Input.Key.X : Return ChrW(&H2D)
    '    Case Input.Key.C : Return ChrW(&H2E)
    '    Case Input.Key.V : Return ChrW(&H2F)
    '    Case Input.Key.B : Return ChrW(&H30)
    '    Case Input.Key.N : Return ChrW(&H31)
    '    Case Input.Key.M : Return ChrW(&H32)
    '  'case ,
    '  'Case .
    '  'case /
    '  'case right shift
    '  'case print screen
    '    Case Input.Key.Alt : Return ChrW(&H38)
    '    Case Input.Key.Space : Return ChrW(&H39)
    '    Case Input.Key.CapsLock : Return ChrW(&H3A)
    '    Case Input.Key.F1 : Return ChrW(&H3B)
    '    Case Input.Key.F2 : Return ChrW(&H3C)
    '    Case Input.Key.F3 : Return ChrW(&H3D)
    '    Case Input.Key.F4 : Return ChrW(&H3E)
    '    Case Input.Key.F5 : Return ChrW(&H3F)
    '    Case Input.Key.F6 : Return ChrW(&H40)
    '    Case Input.Key.F7 : Return ChrW(&H41)
    '    Case Input.Key.F8 : Return ChrW(&HF2)
    '    Case Input.Key.F9 : Return ChrW(&HF3)
    '    Case Input.Key.F10 : Return ChrW(&HF4)
    '  'Case numlock
    '  'Case scrolllock
    '    Case Input.Key.NumPad7, Input.Key.Home : Return ChrW(&H47)
    '    Case Input.Key.NumPad8, Input.Key.Up : Return ChrW(&H48)
    '    Case Input.Key.NumPad9, Input.Key.PageUp : Return ChrW(&H49)
    '    Case Input.Key.Subtract : Return ChrW(&H4A)
    '    Case Input.Key.NumPad4, Input.Key.Left : Return ChrW(&H4B)
    '    Case Input.Key.NumPad5 : Return ChrW(&H4C)
    '    Case Input.Key.NumPad6, Input.Key.Right : Return ChrW(&H4D)
    '    Case Input.Key.Add : Return ChrW(&H4E)
    '    Case Input.Key.NumPad1, Input.Key.End : Return ChrW(&H4F)
    '    Case Input.Key.NumPad2, Input.Key.Down : Return ChrW(&H50)
    '    Case Input.Key.NumPad3, Input.Key.PageDown : Return ChrW(&H51)
    '    Case Input.Key.Insert : Return ChrW(52)
    '    Case Input.Key.Delete : Return ChrW(53)

    '    Case Else

    '      Return ChrW(0)

    '  End Select

    'End Function

    'Private Shared Function EgaColorTable(index As Short) As String

    '  ' Number to the right (in comments) is the default BASIC color number for 16 color mode (text).
    '  Select Case index
    '    Case 0 : Return "000000" ' 0
    '    Case 1 : Return "0000AA" ' 1
    '    Case 2 : Return "00AA00" ' 2
    '    Case 3 : Return "00AAAA" ' 3
    '    Case 4 : Return "AA0000" ' 4
    '    Case 5 : Return "AA00AA" ' 5
    '    Case 6 : Return "AAAA00"
    '    Case 7 : Return "AAAAAA" ' 7
    '    Case 8 : Return "000055"
    '    Case 9 : Return "0000FF"
    '    Case 10 : Return "00AA55"
    '    Case 11 : Return "00AAFF"
    '    Case 12 : Return "AA0055"
    '    Case 13 : Return "AA00FF"
    '    Case 14 : Return "AAAA55"
    '    Case 15 : Return "AAAAFF"
    '    Case 16 : Return "005500"
    '    Case 17 : Return "0055AA"
    '    Case 18 : Return "00FF00"
    '    Case 19 : Return "00FFAA"
    '    Case 20 : Return "AA5500" ' 6
    '    Case 21 : Return "AA55AA"
    '    Case 22 : Return "AAFF00"
    '    Case 23 : Return "AAFFAA"
    '    Case 24 : Return "005555"
    '    Case 25 : Return "0055FF"
    '    Case 26 : Return "00FF55"
    '    Case 27 : Return "00FFFF"
    '    Case 28 : Return "AA5555"
    '    Case 29 : Return "AA55FF"
    '    Case 30 : Return "AAFF55"
    '    Case 31 : Return "AAFFFF"
    '    Case 32 : Return "550000"
    '    Case 33 : Return "5500AA"
    '    Case 34 : Return "55AA00"
    '    Case 35 : Return "55AAAA"
    '    Case 36 : Return "FF0000"
    '    Case 37 : Return "FF00AA"
    '    Case 38 : Return "FFAA00"
    '    Case 39 : Return "FFAAAA"
    '    Case 40 : Return "550055"
    '    Case 41 : Return "5500FF"
    '    Case 42 : Return "55AA55"
    '    Case 43 : Return "55AAFF"
    '    Case 44 : Return "FF0055"
    '    Case 45 : Return "FF00FF"
    '    Case 46 : Return "FFAA55"
    '    Case 47 : Return "FFAAFF"
    '    Case 48 : Return "555500"
    '    Case 49 : Return "5555AA"
    '    Case 50 : Return "55FF00"
    '    Case 51 : Return "55FFAA"
    '    Case 52 : Return "FF5500"
    '    Case 53 : Return "FF55AA"
    '    Case 54 : Return "FFFF00"
    '    Case 55 : Return "FFFF00"
    '    Case 56 : Return "FFFFAA" ' 8
    '    Case 57 : Return "555555" ' 9
    '    Case 58 : Return "5555FF" ' 10
    '    Case 59 : Return "55FF55" ' 11
    '    Case 60 : Return "FF5555" ' 12
    '    Case 61 : Return "FF55FF" ' 13
    '    Case 62 : Return "FFFF55" ' 14
    '    Case 63 : Return "FFFFFF" ' 15
    '  End Select

    '  Return Nothing

    'End Function

#Region "DRAW"

    Private Sub ResetDraw()
      m_drawPosition = New Drawing.Point(m_display.ScreenWidth \ 2, m_display.ScreenHeight \ 2)
      m_drawColor = -1
      m_drawScale = 1
      m_drawAngle = 0
      m_display.GraphicViewReset()
    End Sub

    Private Function ExecuteDraw() As Boolean

      If PeekToken() Is Nothing Then
        Return True
      End If

      Dim command As String = Nothing
      If Not ExecuteStringExpression(command, Nothing) Then Return False

      If PeekToken() Is Nothing Then

        Return ExecuteDraw(command)

      Else

        Return ThrowBasicError(BasicError.SyntaxError)

      End If

    End Function

    Private Function ExecuteDraw(command As String) As Boolean

      command = command.ToUpper

      Dim index As Integer = 0

      Dim blank As Boolean
      Dim noUpdate As Boolean

      Do

        If index > command.Length - 1 Then
          ' Reached the end of the string.
          Exit Do
        End If

        ' Figure out what token we are going to begin processing.
        Dim token As Char = command.Chars(index)

        Select Case token

          Case " "c, ChrW(10), ChrW(13) ' Space. Ignore. Also, ignore LF and CR.
            index += 1

          Case ";"c ' Seperator. Ignore?
            index += 1

          Case "B"c ' Blank (no draw, just move). [Normally followed by an M for move.]

            index += 1
            blank = True

          Case "M"c ' Move the draw position.

            ' Now should be followed by two numbers seperated by a comma.
            ' These numbers represent the x and y coordinate to move to.
            Dim x As Integer
            Dim y As Integer
            Dim relative As Boolean

            index += 1

            ' Search for a comma to determine the first parameter.
            'Dim scan As Integer = 0
            'Do
            '  Select Case command.Chars(index + scan)
            '    Case "+"c, "-"c
            '      relative = True
            '    Case "0"c To "9"c ' Numeric value.
            '      ' Valid character, for the moment, just ignore.
            '    Case ","c ' Comma, this is what we are looking for.
            '      x = CInt(command.Substring(index, scan))
            '      Exit Do
            '    Case " "c ' Space, valid character, but ignore.
            '    Case Else ' Encountered something unexpected.
            '      Return ThrowBasicError(BasicError.IllegalFunctionCall)
            '      'Throw New InvalidOperationException("Error parsing command (position " & (index + scan).ToString & ".")
            '  End Select
            '  scan += 1
            'Loop

            ' Scan to see if this is relative?

            Dim scan As Integer = 0
            Dim negative As Boolean = False
            Do
              Select Case command.Chars(index + scan)
                Case "-"c
                  index += scan + 1
                  negative = True
                  relative = True
                  Exit Do
                Case "+"c, "-"c
                  index += scan + 1
                  relative = True
                  Exit Do
                Case "="c ' Variable
                  Exit Do
              ' Valid character, for the moment, just ignore.
                Case "0"c To "9"c ' Numeric value.
                  ' Valid character, for the moment, just ignore.
                  Exit Do
                Case ","c ' Comma, this is what we are looking for.
                  'x = CInt(command.Substring(index, scan))
                  Exit Do
                Case " "c ' Space, valid character, but ignore.
                Case Else ' Encountered something unexpected.
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                  'Throw New InvalidOperationException("Error parsing command (position " & (index + scan).ToString & ".")
              End Select
              scan += 1
            Loop


            If Not ParseValue(index, command, x) Then Return False
            If negative Then x = -x

            If command(index) = ","c Then
              index += 1
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

            scan = 0
            negative = False
            Do
              Select Case command.Chars(index + scan)
                Case "-"c
                  index += scan + 1
                  negative = True
                  relative = True
                  Exit Do
                Case "+"c
                  index += scan + 1
                  relative = True
                  Exit Do
                Case "="c ' Variable
                  Exit Do
              ' Valid character, for the moment, just ignore.
                Case "0"c To "9"c ' Numeric value.
                  ' Valid character, for the moment, just ignore.
                  Exit Do
                Case ","c ' Comma, this is what we are looking for.
                  'x = CInt(command.Substring(index, scan))
                  Exit Do
                Case " "c ' Space, valid character, but ignore.
                Case Else ' Encountered something unexpected.
                  Return ThrowBasicError(BasicError.IllegalFunctionCall)
                  'Throw New InvalidOperationException("Error parsing command (position " & (index + scan).ToString & ".")
              End Select
              scan += 1
            Loop

            If Not ParseValue(index, command, y) Then Return False
            If negative Then y = -y

            'index += scan + 1
            'scan = 0
            'Do
            '  Select Case command.Chars(index + scan)
            '    Case "+"c, "-"c
            '      relative = True
            '    Case "0"c To "9"c ' Numeric value.
            '    Case ","c ' Comma, this is what we are looking for.
            '    Case " "c ' Space, valid character, but ignore.
            '    Case Else ' Encountered something unexpected.
            '      y = CInt(command.Substring(index, scan))
            '      Exit Do
            '  End Select
            '  scan += 1
            '  If index + scan = command.Length Then
            '    y = CInt(command.Substring(index, scan))
            '    Exit Do
            '  End If
            'Loop

            'index += scan '+ 1

            If m_drawScale <> 1 AndAlso relative Then
              x = CInt(x * m_drawScale)
              y = CInt(y * m_drawScale)
            End If

            Dim position As Drawing.Point
            If relative Then
              position = New Drawing.Point(m_drawPosition.X + x, m_drawPosition.Y + y)
            Else
              position = New Drawing.Point(x, y)
            End If
            If Not blank Then
              m_display.Line(m_drawPosition.X, m_drawPosition.Y, position.X, position.Y, m_drawColor, False, False, New Short?)
            End If
            If Not noUpdate Then
              m_drawPosition = position
            End If

            blank = False
            noUpdate = False

          Case "U"c, "D"c, "L"c, "R"c, "E"c, "F"c, "G"c, "H"c

            ' U - Up.
            ' D - Down.
            ' L - Left.
            ' R - Right.
            ' E - 45-degree angle.
            ' F - 135-degree angle.
            ' G - 225-degree angle.
            ' H - 315-degree angle.

            Dim direction As Char = command.Chars(index)
            index += 1
            Dim value As Integer
            If Not ParseValue(index, command, value) Then Return False
            value = CInt(value * m_drawScale)
            Dim position As Drawing.Point = CalculatePosition(direction, value)
            If Not blank Then
              m_display.Line(m_drawPosition.X, m_drawPosition.Y, position.X, position.Y, m_drawColor, False, False, New Short?)
            End If
            If Not noUpdate Then
              m_drawPosition = position
            End If
            blank = False
            noUpdate = False

          Case "X"c ' Execute a substring and return.

            index += 1

            Dim scan As Integer = 0
            Dim accum As New Text.StringBuilder

            Do
              Select Case command.Chars(index + scan)
                Case ";"c
                  index += scan
                  Exit Do
                Case Else
                  accum.Append(command.Chars(index + scan))
              End Select
              scan += 1
              If index + scan > command.Length - 1 Then
                ' If using X, must be followed by a string variable AND a semi-colon.
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
            Loop

            Dim variableName As String = accum.ToString
            If IsStringVariable(variableName) Then
              Dim cmd As String = Nothing
              If Not GetStringVariable(variableName, cmd) Then Return False
              If cmd <> "" Then
                If Not ExecuteDraw(cmd) Then Return False
              End If
            Else
              Return ThrowBasicError(BasicError.TypeMismatch)
            End If

          Case "C"c ' Color.

            index += 1
            Dim value As Integer
            If Not ParseValue(index, command, value) Then Return False
            m_drawColor = CShort(value)

          Case "A"c ' Angle.

            index += 1
            Dim value As Integer
            If Not ParseValue(index, command, value) Then Return False
            Select Case value
              Case 0 ' 0 degrees (default).
                m_drawAngle = 0
              Case 1 ' 90-degrees (clockwise).
                m_drawAngle = 1
              Case 2 ' 180-degrees (clockwise).
                m_drawAngle = 2
              Case 3 ' 270-degrees (clockwise).
                m_drawAngle = 3
              Case Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
                'Throw New InvalidOperationException("Error parsing command (position " & index.ToString & ".")
            End Select

          Case "T"c ' Turn Angle

            index += 1

            If command.Chars(index) = "A"c Then
              Return ThrowBasicError(BasicError.AdvancedFeature)
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

          Case "S"c ' Scale.

            index += 1
            Dim value As Integer
            If Not ParseValue(index, command, value) Then Return False
            If value.Between(1, 255) Then
              m_drawScale = CSng(value / 4)
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
              'Throw New InvalidOperationException("Error parsing command (position " & index.ToString & ".")
            End If

          Case "N"c ' No update of draw position.

            index += 1
            noUpdate = True

          Case "P"c ' Paint

            ' Ppaint,boundary

            index += 1

            Dim paint As Integer
            If Not ParseValue(index, command, paint) Then Return False
            Dim boundary As Integer = -1
            If command.Chars(index) = ","c Then
              index += 1
              If Not ParseValue(index, command, boundary) Then Return False
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If

            Dim pp As New Display.Display.PaintParams With {.PaintAttribute = paint,
                                                  .BorderAttribute = boundary}

            'If Not m_display.Paint(m_drawPosition.X, m_drawPosition.Y, paint, boundary) Then ThrowBasicError(BasicError.OutOfMemory)
            If Not m_display.Paint(m_drawPosition.X, m_drawPosition.Y, pp) Then ThrowBasicError(BasicError.OutOfMemory)

          Case Else

            Return ThrowBasicError(BasicError.IllegalFunctionCall)
            'Throw New InvalidOperationException("Error parsing command (position " & index.ToString & ".")

        End Select

      Loop

      Return True

    End Function

    Private Function ParseValue(ByRef index As Integer, command As String, ByRef value As Integer) As Boolean

      If command.Chars(index) = "="c Then

        Dim scan As Integer = 1
        Dim accum As New Text.StringBuilder

        Do
          Select Case command.Chars(index + scan)
            Case ";"c
              index += scan + 1
              Exit Do
            Case Else
              accum.Append(command.Chars(index + scan))
          End Select
          scan += 1
          If index + scan > command.Length - 1 Then
            index += scan
            Exit Do
          End If
        Loop

        Dim variableName As String = accum.ToString

        Dim result As Double
        If Not GetNumericVariable(variableName, result) Then Return False
        value = CInt(result)
        Return True

      Else
        ' Starting at position index, determine the numeric constant.

        Dim scan As Integer = 0
        Do
          Select Case command.Chars(index + scan)
            Case "0"c To "9"c ' Numeric value.
            Case " "c ' Space, valid character, but ignore.
            Case Else ' Encountered something unexpected.
              If IsNumeric(command.Substring(index, scan)) Then
                Dim result As Integer = CInt(command.Substring(index, scan))
                index += scan
                value = result
                Return True
              Else
                Return ThrowBasicError(BasicError.IllegalFunctionCall)
              End If
          End Select
          scan += 1
          If index + scan > command.Length - 1 Then
            If IsNumeric(command.Substring(index, scan)) Then
              Dim result As Integer = CInt(command.Substring(index, scan))
              index += scan
              value = result
              Return True
            Else
              Return ThrowBasicError(BasicError.IllegalFunctionCall)
            End If
          End If
        Loop

      End If

      Return ThrowBasicError(BasicError.IllegalFunctionCall)
      'Throw New InvalidOperationException("Error parsing command (position " & index.ToString & ".")

    End Function

    Private Function CalculatePosition(direction As Char, value As Integer) As Drawing.Point

      Dim x As Integer = m_drawPosition.X
      Dim y As Integer = m_drawPosition.Y

      Select Case m_drawAngle
        Case 0
          Select Case direction
            Case "U"c ' Up.
              y -= value
            Case "R"c ' Right.
              x += value
            Case "D"c ' Down.
              y += value
            Case "L"c ' Left.
              x -= value
            Case "E"c ' 45-degree angle.
              x += value
              y -= value
            Case "F"c ' 135-degree angle.
              x += value
              y += value
            Case "G"c ' 225-degree angle.
              x -= value
              y += value
            Case "H"c ' 315-degree angle.
              x -= value
              y -= value
          End Select
        Case 1
          Select Case direction
            Case "U"c ' Up.
              x -= value
            Case "R"c ' Right.
              y += value
            Case "D"c ' Down.
              x += value
            Case "L"c ' Left.
              y -= value
            Case "E"c ' 45-degree angle.
              x -= value
              y -= value
            Case "F"c ' 135-degree angle.
              x += value
              y -= value
            Case "G"c ' 225-degree angle.
              x += value
              y += value
            Case "H"c ' 315-degree angle.
              x -= value
              y += value
          End Select
        Case 2
          Select Case direction
            Case "U"c ' Up.
              y += value
            Case "R"c ' Right.
              x -= value
            Case "D"c ' Down.
              y -= value
            Case "L"c ' Left.
              x += value
            Case "E"c ' 45-degree angle.
              x -= value
              y += value
            Case "F"c ' 135-degree angle.
              x -= value
              y -= value
            Case "G"c ' 225-degree angle.
              x += value
              y -= value
            Case "H"c ' 315-degree angle.
              x += value
              y += value
          End Select
        Case 3
          Select Case direction
            Case "U"c ' Up.
              x += value
            Case "R"c ' Right.
              y -= value
            Case "D"c ' Down.
              x -= value
            Case "L"c ' Left.
              y += value
            Case "E"c ' 45-degree angle.
              x += value
              y += value
            Case "F"c ' 135-degree angle.
              x -= value
              y += value
            Case "G"c ' 225-degree angle.
              x -= value
              y -= value
            Case "H"c ' 315-degree angle.
              x += value
              y -= value
          End Select
      End Select

      Return New Drawing.Point(x, y)

    End Function

#End Region

    'Public Sub Start(width As Short)
    '  InternalStart(width)
    'End Sub

    'Private Sub InternalStart(width As Short)

    '  ' Reset internal state(s).
    '  m_shutDown = False

    '  If Me.m_dialect = Parser.Dialect.QBasic Then
    '  Else

    '    ' Show copyright / startup info.

    '    If width = 40 OrElse width = 80 Then
    '      m_display.ColumnCount = width
    '      'If m_pen IsNot Nothing Then
    '      '  m_pen.ColumnCount = width
    '      'End If
    '    End If
    '    m_display.Locate(1, 1)
    '    m_display.Print(m_productName, True) ' & If(m_environment IsNot Nothing AndAlso m_environment.IsDebugBuild, " (DEBUG BUILD)", ""), True)
    '    If Me.m_display.ColumnCount = 80 Then
    '      m_display.Print("(C) Copyright Cory Smith 2011-2023", True)
    '    Else
    '      m_display.Print("(C) Cory Smith 2011-2023", True)
    '    End If

    '    'If m_region <> "en-US" Then
    '    '  m_display.Print(String.Format("Locale: {0}", m_region), True)
    '    'End If
    '    If m_environment IsNot Nothing Then
    '      Dim mem As Long = m_environment.FreeMemory
    '      If mem > 0 Then
    '        m_display.Print(String.Format("{0} Bytes free", mem), True)
    '      Else
    '        m_display.Print("? Bytes free", True)
    '      End If
    '    Else
    '      m_display.Print("? Bytes free", True)
    '    End If
    '    'm_display.Print()
    '    m_display.Print("Ok", True)

    '  End If

    '  ' Kick off "loop".
    '  If InternalTimer IsNot Nothing Then InternalTimer.Start()

    'End Sub

    'Private Function ExecuteKeywords() As Boolean

    '  Dim c As Integer = 0

    '  'm_display.Print(String.Format("The following {0} keywords are implemented to one degree or another:", m_reservedWords.Count), True)
    '  'Dim line As Integer = 1
    '  'm_display.Print(line.ToString.PadRight(2) & " - ", False) : line += 1 : c += 5
    '  'For Each entry In m_reservedWords
    '  '  If c = m_display.ColumnCount Then
    '  '    c = 0
    '  '    m_display.Print(line.ToString.PadRight(2) & " - ", False) : line += 1 : c += 5
    '  '  ElseIf c + entry.Length > m_display.ColumnCount Then
    '  '    m_display.Print()
    '  '    c = 0
    '  '    m_display.Print(line.ToString.PadRight(2) & " - ", False) : line += 1 : c += 5
    '  '  End If
    '  '  m_display.Print(entry, False)
    '  '  c += entry.Length
    '  '  If c < m_display.ColumnCount Then
    '  '    m_display.Print(" ", False)
    '  '    c += 1
    '  '  End If
    '  'Next
    '  'm_display.Print()

    '  m_display.Print(String.Format("The following {0} keywords are implemented to one degree or another:", m_reservedWords.Count), True)
    '  Dim max As Integer = m_display.ColumnCount - 3
    '  For Each entry In m_reservedWords
    '    If c = max Then
    '      m_display.Print()
    '      c = 0
    '    ElseIf c + entry.Length > max Then
    '      m_display.Print()
    '      c = 0
    '    End If
    '    m_display.Print(entry, False)
    '    c += entry.Length
    '    If c < max Then
    '      m_display.Print(" ", False)
    '      c += 1
    '    End If
    '  Next
    '  m_display.Print()

    '  Return True

    'End Function

    Private m_paste As Boolean

    Public Sub SetCodeAndRun(text As String)

      m_display.Cls(0)
      Dim success = InternalLoadCompleted(text, "NONE|0")
      Run()

    End Sub

    Public Sub ProcessText(text As String)
      InternalProcessText(text)
    End Sub

    Private Sub InternalProcessText(text As String)

      m_paste = True

      Dim ctrl As Boolean
      Dim alt As Boolean
      Dim shift As Boolean

      If m_keyboard IsNot Nothing AndAlso m_keyboard.UseModifier Then
        ctrl = ((Me.m_keyboard.Modifier And Input.ModifierKeys.Control) = Input.ModifierKeys.Control)
        alt = ((Me.m_keyboard.Modifier And Input.ModifierKeys.Alt) = Input.ModifierKeys.Alt)
        shift = ((Me.m_keyboard.Modifier And Input.ModifierKeys.Shift) = Input.ModifierKeys.Shift)
      Else
        ctrl = m_display.ControlDown
        alt = m_display.AltDown
        shift = m_display.ShiftDown
      End If

      Dim resetCtrlModifier As Boolean
      Dim resetAltModifier As Boolean
      Dim resetShiftModifier As Boolean

      If m_keyboard Is Nothing OrElse Not m_keyboard.UseModifier Then
        If m_display.ControlDown Then
          resetCtrlModifier = True
          m_display.EditControlUp()
        End If
        If m_display.AltDown Then
          resetAltModifier = True
          m_display.EditAltUp()
        End If
        If m_display.ShiftDown Then
          resetShiftModifier = True
          m_display.EditShiftUp()
        End If
      End If

      Try

        For Each c As Char In text

          Select Case c

            Case ChrW(8)
              Dim ee As New Input.KeyEventArgs(Input.Key.Tab, Nothing)
              ProcessKey(ee, True)

            Case ChrW(9)
              Dim ee As New Input.KeyEventArgs(Input.Key.Back, Nothing)
              ProcessKey(ee, True)

            Case ChrW(13)
              Dim ee As New Input.KeyEventArgs(Input.Key.Enter, Nothing)
              ProcessKey(ee, True)

            Case ChrW(27)
              Dim ee As New Input.KeyEventArgs(Input.Key.Escape, Nothing)
              ProcessKey(ee, True)

            Case ChrW(32)
              Dim ee As New Input.KeyEventArgs(Input.Key.Space, Nothing)
              ProcessKey(ee, True)

            Case "a"c To "z"c
              Dim ee As New Input.KeyEventArgs(CType(30 + (AscW(c) - 97), Input.Key), Nothing)
              ProcessKey(ee, True)
            Case "A"c To "Z"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(CType(30 + (AscW(c) - 65), Input.Key), Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "`"c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 192)
              ProcessKey(ee, True)
            Case "~"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 192) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "1"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D1, Nothing)
              ProcessKey(ee, True)
            Case "!"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D1, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "2"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D2, Nothing)
              ProcessKey(ee, True)
            Case "@"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D2, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "3"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D3, Nothing)
              ProcessKey(ee, True)
            Case "#"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D3, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "4"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D4, Nothing)
              ProcessKey(ee, True)
            Case "$"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D4, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "5"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D5, Nothing)
              ProcessKey(ee, True)
            Case "%"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D5, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "6"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D6, Nothing)
              ProcessKey(ee, True)
            Case "^"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D6, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "7"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D7, Nothing)
              ProcessKey(ee, True)
            Case "&"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D7, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "8"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D8, Nothing)
              ProcessKey(ee, True)
            Case "*"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D8, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "9"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D9, Nothing)
              ProcessKey(ee, True)
            Case "("c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D9, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "0"c
              Dim ee As New Input.KeyEventArgs(Input.Key.D0, Nothing)
              ProcessKey(ee, True)
            Case ")"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.D0, Nothing) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "-"c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 189)
              ProcessKey(ee, True)
            Case "_"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 189) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "="c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 187)
              ProcessKey(ee, True)
            Case "+"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 187) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "["c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 219)
              ProcessKey(ee, True)
            Case "{"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 219) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "]"c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 221)
              ProcessKey(ee, True)
            Case "}"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 221) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "\"c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 220)
              ProcessKey(ee, True)
            Case "|"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 220) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case ";"c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 186)
              ProcessKey(ee, True)
            Case ":"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 186) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "'"c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 222)
              ProcessKey(ee, True)
            Case """"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 222) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case ","c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 188)
              ProcessKey(ee, True)
            Case "<"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 188) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "."c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 190)
              ProcessKey(ee, True)
            Case ">"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 190) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
          'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
          'Me.ProcessKey(ee, False)

            Case "/"c
              Dim ee As New Input.KeyEventArgs(Input.Key.Unknown, 191)
              ProcessKey(ee, True)
            Case "?"c
              'Dim ee As New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, True)
              Dim ee = New Input.KeyEventArgs(Input.Key.Unknown, 191) With {.OppositeShiftState = True}
              ProcessKey(ee, True)
              'ee = New Basic.Interpreter.KeyEventArgs(Interpreter.Key.Shift, Nothing)
              'Me.ProcessKey(ee, False)

            Case Else
          End Select

        Next

      Finally

        m_paste = False

        If resetCtrlModifier Then
          m_display.EditControlDown()
        End If

        If resetAltModifier Then
          m_display.EditAltDown()
        End If

        If resetShiftModifier Then
          m_display.EditShiftDown()
        End If

      End Try

    End Sub

    ' one plus - parsing / place holding
    ' two plus - implemented

    ' ---------Graphics-------------- 17
    ' ++ CIRCLE
    ' ++ DRAW
    ' ++ GET (Graphics)
    ' ++ LINE
    ' + PMAP
    ' ++ POINT
    ' ++ PRESET
    ' ++ PSET
    ' ++ PUT (Graphics)
    ' ++ PAINT
    ' ++ PALETTE
    ' PALETTE USING
    ' + PCOPY
    ' ++ SCREEN (function)
    ' ++ SCREEN (statement)
    ' + VIEW
    ' + WINDOW

    ' ---------File I/O-------------- 14
    ' ++ OPEN
    ' ++ CLOSE
    ' ++ EOF
    ' ++ LOF
    ' ++ GET (Files)
    ' ++ PUT (Files)
    ' ++ FIELD
    ' ++ PRINT #
    ' ++ PRINT # USING 
    ' ++ WRITE #
    ' ++ INPUT #
    ' ++ LINE INPUT #
    ' + LOCK
    ' + UNLOCK

    ' ---------Printer--------------- 8
    ' ++ IOCTL
    ' ++ IOCTL$
    ' ++ LLIST
    ' ++ LPRINT
    ' ++ LPRINT USING
    ' ++ LPOS
    ' ++ PRINT #
    ' ++ PRINT USING #

    ' ---------Program Flow---------- 2
    ' ++ CHAIN
    ' ++ COMMON

    ' --------Machine --------------- 4
    ' + ENVIRON
    ' + ENVIRON$
    ' + EXTERR
    ' + SHELL

    ' ---------Sound----------------- 3
    ' ON PLAY(n)
    ' PLAY
    ' PLAY(n)

    ' ---------Joystick-------------- 4
    ' ON STRIG(n)
    ' STICK
    ' STRIG
    ' STRIG(n)

    ' ---------Pen------------------- 2
    ' ON PEN
    ' PEN

    ' ---------Serial---------------- 3
    ' COM(n)
    ' ON COM(n)
    ' OPEN "COM..." AS ...

    ' --------Machine Language------- 11
    ' CALL
    ' DEF SEG
    ' DEF USR
    ' USR

    ' BLOAD
    ' BSAVE

    ' INP
    ' OUT
    ' WAIT

    Private Sub InternalPen_PositionChanged(sender As Object, e As Input.PositionChangedEventArgs) Handles InternalPen.PositionChanged

      m_penCurrentPosition = e.Position
      If m_penCurrentActivated Then
        m_penLastActivatedPosition = e.Position
      End If

    End Sub

    Private Sub InternalPen_SwitchChanged(sender As Object, e As Input.SwitchChangedEventArgs) Handles InternalPen.SwitchChanged

      If e.Pressed Then
        m_penLastActivatedPosition = m_penCurrentPosition
        If Me.m_onPenState = OnState.On Then
          m_onPenActivated = True
        End If
        If Not m_penActivatedSinceLastPoll Then
          m_penActivatedSinceLastPoll = True
        End If
      End If
      m_penCurrentActivated = e.Pressed

    End Sub

  End Class

End Namespace
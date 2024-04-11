Imports System.Timers
Imports System.Drawing
Imports System.IO
Imports System.IO.Compression
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports System.Runtime.InteropServices.OSPlatform
Imports System.Runtime.InteropServices.RuntimeInformation
Imports System.Text

Imports VbPixelGameEngine

Imports QBLib.Video
Imports Basic.Parser
Imports Basic

Friend Module Program

  Sub Main()
    Dim demo As New QBasic
    If demo.Construct(640, 400, 1, 1) Then ', False, True) Then
      demo.ShowEngineName = False : demo.ShowIPS = False
      demo.Start()
    End If
  End Sub

End Module

Friend Class QBasic
  Inherits PixelGameEngine

  Private m_selected As Integer
  Private m_context As IContext

  Private WithEvents Menu As MainMenu
  Private ReadOnly m_help As New HelpPanel
  Private WithEvents Document1 As New DocumentPanel
  Private WithEvents Document2 As New DocumentPanel
  Private ReadOnly m_immediate As New ImmediatePanel
  Private ReadOnly m_statusPanel As New StatusPanel

  Private m_t As Single

  'Private WithEvents InterpreterTimer As New Timer(1)
  'Private WithEvents DisplayTimer As New Timer(1)

  'Private WithEvents Interpreter As Interactive
  'Private ReadOnly Display As New Display(DisplayTimer)

  Private ReadOnly m_pathspec As String
  Private m_path As String

  Private m_runner As Threading.Thread

  Private m_scrn() As Integer
  Private m_scrn0() As UShort
  Private m_buffer() As Pixel
  Private m_fg As Integer
  Private m_bg As Integer
  Private m_cr As Integer
  Private m_cc As Integer

  Friend Sub New()
    AppName = "Community QBasic"
    m_pathspec = System.IO.Path.Combine(System.Environment.CurrentDirectory(), If(IsOSPlatform(Windows), "*.BAS", "*.*"))
  End Sub

  <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
  Private Shared Function SendMessage(hWnd As IntPtr, Msg As UInteger, wParam As UInteger, lParam As IntPtr) As IntPtr
  End Function

  Protected Overrides Function OnUserCreate() As Boolean

    Dim w As Integer, h As Integer
    Dim result = GetScreenSize()
    w = result.w : h = result.h

    Select Case h
      Case <= 1080
      Case <= 1440
        IncreasePixelSize()
      Case Else
        IncreasePixelSize()
        IncreasePixelSize()
    End Select

    If OperatingSystem.IsWindowsVersionAtLeast(7) Then
      Dim assem = Assembly.GetExecutingAssembly
      Dim ico = Icon.ExtractAssociatedIcon(assem.Location)
      Const WM_SETICON As UInteger = &H80
      Const ICON_SMALL As UInteger = 0
      Const ICON_BIG As UInteger = 1
      Dim unused = SendMessage(m_hWnd, WM_SETICON, ICON_SMALL, ico.Handle)
      unused = SendMessage(m_hWnd, WM_SETICON, ICON_BIG, ico.Handle)
    End If

    Init()

    Menu = New MainMenu With {
      .Items = {New MenuItem("&File") With {
        .Items = {New MenuItem("&New", "Removes currently loaded file from memory"),
                  New MenuItem("&Open...", "Loads new file into memory"),
                  New MenuItem("&Save", "Saves current file"),
                  New MenuItem("Save &As...", "Saves current file with specified name"),
                  New MenuItem("-"),
                  New MenuItem("&Print...", "Prints specified text"),
                  New MenuItem("-"),
                  New MenuItem("E&xit", "Exits editor and returns to DOS")}.ToList},
        New MenuItem("&Edit") With {
        .Items = {New MenuItem("Cu&t", "Deletes selected text and copies it to buffer") With {.Hotkey = "Shift+Del", .Enabled = False},
                  New MenuItem("&Copy", "Copies selected text to buffer") With {.Hotkey = "Ctrl+Ins", .Enabled = False},
                  New MenuItem("&Paste", "Inserts buffer contents at current location") With {.Hotkey = "Shift+Ins", .Enabled = False},
                  New MenuItem("Cl&ear", "Deletes selected text without copying it to buffer") With {.Hotkey = "Del", .Enabled = False},
                  New MenuItem("-"),
                  New MenuItem("New &SUB...", "Opens a window for a new subprogram"),
                  New MenuItem("New &FUNCTION...", "Opens a window for a new FUNCTION procedure")}.ToList},
        New MenuItem("&View") With {
        .Items = {New MenuItem("&SUBs...", "Displays a loaded SUB or FUNCTION") With {.Hotkey = "F2"},
                  New MenuItem("S&plit", "Divides screen into two View Windows"),
                  New MenuItem("&Output Screen", "Displays output screen") With {.Hotkey = "F4"}}.ToList},
        New MenuItem("&Search") With {
        .Items = {New MenuItem("&Find...", "Finds specified text"),
                  New MenuItem("&Repeat Last Find", "Finds next occurrence of text specified in previous search") With {.Hotkey = "F3"},
                  New MenuItem("&Change...", "Finds and changes specified text")}.ToList},
        New MenuItem("&Run") With {
        .Items = {New MenuItem("&Start", "Runs current program") With {.Hotkey = "Shift+F5"},
                  New MenuItem("&Restart", "Clears variables in preparation for restarting single stepping"),
                  New MenuItem("&Continue", "Continues execution after a break") With {.Hotkey = "F5"}}.ToList},
        New MenuItem("&Debug") With {
        .Items = {New MenuItem("&Step", "Executes next program") With {.Hotkey = "F8"},
                  New MenuItem("&Procedure Step", "Executes next program statement, tracing over procedure calls") With {.Hotkey = "F10"},
                  New MenuItem("-"),
                  New MenuItem("&Trace On", "Highlights statement currently executing"),
                  New MenuItem("-"),
                  New MenuItem("Toggle &Breakpoint", "Sets/clears breakpoint at cursor location") With {.Hotkey = "F9"},
                  New MenuItem("&Clear All Breakpoints", "Removes all breakoints"),
                  New MenuItem("Set &Next Statement", "Makes the statement at the cursor the next statement to execute") With {.Enabled = False}}.ToList},
        New MenuItem("&Options") With {
        .Items = {New MenuItem("&Display...", "Changes display attributes"),
                  New MenuItem("Help &Path...", "Sets search path for Help files"),
                  New MenuItem("&Syntax Checking", "Turns editor's syntax checking on or off.") With {.Checked = True}}.ToList},
        New MenuItem("&Help") With {
        .Items = {New MenuItem("&Index", "Displays help index"),
                  New MenuItem("&Contents", "Displays help table of contents"),
                  New MenuItem("Topic:", "Displays information about the Basic keyword the cursor is on") With {.Enabled = False, .Hotkey = "F1"},
                  New MenuItem("Using &Help", "Displays information about how to use online help") With {.Hotkey = "Shift+F1"},
                  New MenuItem("-"),
                  New MenuItem("&About...", "Displays product version and copyright information")}.ToList}}.ToList}

    Dim includeFbc = False
    Dim includeQbjs = True

    If includeQbjs OrElse includeFbc Then
      Menu.Items(4).Items.Add(New MenuItem("-"))
    End If
    If includeFbc Then
      Menu.Items(4).Items.Add(New MenuItem("Make E&XE File (FBC)...", "Creates executable file on disk"))
    End If
    If includeQbjs Then
      Menu.Items(4).Items.Add(New MenuItem("In &Web browser (QBJS)...", "Launch using QBJS in default web browser"))
    End If

    Menu.CalculateOffsets()

    Document1.Focused = True
    Document2.Visible = False

    m_context = New WelcomeDialog()

    DrawScreen()

    'InterpreterTimer.Enabled = True

    'Dim env As IEnvironment = Nothing
    'Dim fs As IVirtualFileSystem = New WindowsFileSystem
    'Dim snd As ISound = Nothing
    'Dim kbd As IKeyboard = Nothing
    'Dim gpio As IGpio = Nothing

    'Interpreter = New Interactive(Dialect.QBasic,
    '                              env,
    '                              Display,
    '                              InterpreterTimer,
    '                              fs,
    '                              snd,
    '                              kbd,
    '                              gpio,
    '                              "QBasic",
    '                              1000,
    '                              "en-US")

    Return True

  End Function

  Private Sub DrawScreen()

    m_help.Render()
    Document1.Render()
    Document2.Render()
    m_immediate.Render()

    If Menu.Focused AndAlso Menu.Expanded Then
      m_statusPanel.Dialog = False
      m_statusPanel.Text = $"F1=Help {ChrW(179)} {If(Menu.Items(Menu.Selected).Items(Menu.SubSelected).Help, "Help not defined")}"
    ElseIf m_context IsNot Nothing Then
      m_statusPanel.Dialog = True
      m_statusPanel.Text = "F1=Help   Enter=Execute   Esc=Cancel   Tab=Next Field   Arrow=Next Item"
    Else
      m_statusPanel.Dialog = False
      m_statusPanel.Text = "<Shift+F1=Help> <F6=Window> <F2=Subs> <F5=Run> <F8=Step>"
    End If

    If m_help.Focused Then
      m_statusPanel.DocumentRow = m_help.DocumentRow
      m_statusPanel.DocumentCol = m_help.DocumentCol
    ElseIf Document1.Focused Then
      m_statusPanel.DocumentRow = Document1.DocumentRow
      m_statusPanel.DocumentCol = Document1.DocumentCol
    ElseIf Document2.Focused Then
      m_statusPanel.DocumentRow = Document2.DocumentRow
      m_statusPanel.DocumentCol = Document2.DocumentCol
    ElseIf m_immediate.Focused Then
      m_statusPanel.DocumentRow = m_immediate.DocumentRow
      m_statusPanel.DocumentCol = m_immediate.DocumentCol
    End If
    m_statusPanel.Render()

    Menu.Render()

    m_context?.Render()

  End Sub

  Private m_exit As Boolean
  Private m_isAlt As Boolean
  Private m_isK As Boolean
  Private m_isQ As Boolean
  Private m_isCtrlP As Boolean

  Private ReadOnly m_subs As New List(Of String)

  Protected Overrides Function OnUserUpdate(elapsedTime As Single) As Boolean

    m_t += elapsedTime

    Dim cursorVisible = True

    Dim keys = GetPressed()

    Dim isAlt = GetKey(Key.ALT).Held OrElse GetKey(Key.ALT).Pressed
    Dim isShift = GetKey(Key.SHIFT).Held OrElse GetKey(Key.SHIFT).Pressed
    Dim isControl = GetKey(Key.CTRL).Held OrElse GetKey(Key.CTRL).Pressed

    ' These keys are "always active".
    If keys IsNot Nothing Then
      For index = keys.Count - 1 To 0 Step -1
        If Not isControl AndAlso Not isAlt AndAlso Not isShift Then
          Select Case keys(index)
            Case ConsoleKey.Insert
              ToggleInsertMode()
              keys.RemoveAt(index)
            Case ConsoleKey.F11
              ToggleFullScreen()
              If Not IsFullScreen Then
                ' Whenever we restore the window, will need to "reattach" the icon in the title bar.
                If OperatingSystem.IsWindowsVersionAtLeast(7) Then
                  Dim assem = Assembly.GetExecutingAssembly
                  Dim ico = Icon.ExtractAssociatedIcon(assem.Location)
                  Const WM_SETICON As UInteger = &H80
                  Const ICON_SMALL As UInteger = 0
                  Const ICON_BIG As UInteger = 1
                  Dim unused = SendMessage(m_hWnd, WM_SETICON, ICON_SMALL, ico.Handle)
                  unused = SendMessage(m_hWnd, WM_SETICON, ICON_BIG, ico.Handle)
                Else
                  'TODO: How to set the icon on Linux?
                End If
              End If
              keys.RemoveAt(index)
            Case Else
          End Select
        ElseIf isControl AndAlso Not isAlt AndAlso Not isShift Then
          Select Case keys(index)
            Case ConsoleKey.V ' Insert and Overstrike
              ToggleInsertMode()
              keys.RemoveAt(index)
            Case ConsoleKey.C
              If m_running Then
                s_cancelTokenSource.Cancel()
                m_running = False
              End If
            Case ConsoleKey.Attention
              If m_running Then
                s_cancelTokenSource.Cancel()
                m_running = False
              End If
            Case Else
              Stop
          End Select
        ElseIf isControl AndAlso isAlt AndAlso isShift Then
          Select Case keys(index)
            Case ConsoleKey.Add, ConsoleKey.OemPlus
              IncreasePixelSize()
              keys.RemoveAt(index)
            Case ConsoleKey.Subtract, ConsoleKey.OemMinus
              DecreasePixelSize()
              keys.RemoveAt(index)
            Case Else
          End Select
        End If
      Next
    End If

    If m_context IsNot Nothing Then
      m_context.Render()
      If Not m_context.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift) Then
        If TypeOf m_context Is SavePrompt Then
          m_selected = CType(m_context, SavePrompt).Selected
        ElseIf TypeOf m_context Is WelcomeDialog Then
          m_selected = CType(m_context, WelcomeDialog).Selected
          If m_selected = 0 Then
            m_help.Title = "Survival Guide"
            m_help.Content = <content><![CDATA[
Using QBasic:

  * To activate the QBasic menu bar, press Alt.
  * To activate menus and commands, press the highlighted letter.
  * To move between menus and command, use the direction keys.
  * To get help on a selected keyword, command, or dialog box, press F1.
  * To exit Help, press Esc.

Browsing the QBasic Help system:

  * To select one of the following topics, press the Tab key or the first
    letter of the topic. Then press the Enter key to see:

    <Index>         The Index for QBasic Help
    <Contents>      The Table of Contents for QBasic Help topics
    <Using Help>    Information on using QBasic Help

Tip: These topics are also available from the Help menu.
]]></content>.Value
            ShowHelp()
          End If
        ElseIf TypeOf m_context Is OpenDialog Then
          Dim o = CType(m_context, OpenDialog)
          Select Case o.DialogResult
            Case DialogResult.Ok
              m_path = o.Path
              If System.IO.File.Exists(m_path) Then

                Document1.Clear()
                Document2.Clear()

                Dim code = System.IO.File.ReadAllText(m_path)
                Document1.Text = code
                Document1.Title = System.IO.Path.GetFileName(m_path)

                ' B#
                Dim tree = QB.CodeAnalysis.Syntax.SyntaxTree.Parse(code)
                Document2.Text = tree.Root.ToString
                Document2.Title = "Tree"

                ' GW
                Dim p As Basic.Parser.Parser
                Using s As New MemoryStream()
                  Dim buffer() = code.ToByteArray
                  s.Write(buffer, 0, buffer.Length)
                  s.Seek(0, SeekOrigin.Begin)
                  p = New Parser(s)
                End Using

                'm_subs.Clear()
                For Each line In p.Lines
                  For Each statement In line.Statements
                    Dim token = If(statement?.Tokens?.Count > 0, statement.Tokens(0), Nothing)
                    If token IsNot Nothing Then
                      Select Case token.Keyword
                        Case "END"
                          Dim nextToken = If(statement?.Tokens?.Count > 1, statement.Tokens(1), Nothing)
                          If nextToken Is Nothing Then
                          ElseIf nextToken?.Keyword = "FUNCTION" Then
                          ElseIf nextToken?.Keyword = "IF" Then
                          ElseIf nextToken?.Keyword = "SELECT" Then
                          ElseIf nextToken?.Keyword = "SUB" Then
                          ElseIf nextToken?.Keyword = "TYPE" Then
                          Else
                            Stop
                          End If
                        Case "BEEP"
                        Case "CALL", "CIRCLE", "CLS", "COLOR"
                        Case "DEFINT", "DEF", "DIM"
                        Case "ELSE", "END"
                        Case "FOR"
                        Case "GET", "GOSUB"
                        Case "IF", "INPUT"
                        Case "LET", "LINE", "LOCATE"
                        Case "NEXT"
                        Case "ON"
                        Case "PAINT", "PALETTE", "PLAY", "POKE", "PRINT", "PSET", "PUT"
                        Case "RANDOMIZE", "READ", "REM", "RESTORE", "RESUME", "RETURN"
                        Case "SCREEN"
                        Case "VIEW"
                        Case "WEND", "WHILE", "WIDTH"
                        Case "SUB", "FUNCTION"
                          token = If(statement?.Tokens?.Count > 1, statement.Tokens(1), Nothing)
                          If TypeOf token Is IdentifierToken Then
                            m_subs.Add(token.ToString)
                          End If
                        Case Else
                          Debug.WriteLine(token.Keyword)
                      End Select
                    End If
                    Exit For
                  Next
                Next

              End If
            Case DialogResult.Cancel
            Case DialogResult.Help
            Case Else
          End Select
        ElseIf TypeOf m_context Is SaveAsDialog Then
          Dim o = CType(m_context, SaveAsDialog)
          Select Case o.DialogResult
            Case DialogResult.Ok
              m_path = o.Path
              System.IO.File.WriteAllText(m_path, Document1.Text)
              Document1.Title = System.IO.Path.GetFileName(m_path)
              Document1.Changed = False
            Case DialogResult.Cancel
            Case DialogResult.Help
            Case Else
          End Select
        End If
        m_context = Nothing : DrawScreen()
      End If

    Else

      If m_running Then 'Interpreter.IsRunning Then

        If m_runner.IsAlive Then
          ' need to feed the keys into the runner
          m_scrn0 = Nothing
          m_scrn = Nothing
          m_buffer = Nothing
        Else
          If m_scrn0 Is Nothing Then
            'TODO: Take a "snapshot" of the screen
            '      Capture the current cursor location
            '      Capture the current foreground and background colors
            '      Capture the current screen mode? Width?
            ReDim m_scrn0(Screen0.Length - 1)
            Array.Copy(Screen0, 0, m_scrn0, 0, Screen0.Length)
            ReDim m_buffer(Buffer.Length - 1)
            Array.Copy(Buffer, 0, m_buffer, 0, Buffer.Length)
            m_fg = QBLib.Video.m_fgColor
            m_bg = QBLib.Video.m_bgColor
            m_cr = QBLib.Video.CursorRow
            m_cc = QBLib.Video.CursorCol
            LOCATE(25, 1) : PRINT("Press any key to continue", True)
          End If
          If keys IsNot Nothing Then
            s_cancelTokenSource.Cancel()
            m_running = False
            'For Each key In keys
            '  Select Case key
            '    Case ConsoleKey.Escape
            '      s_cancelTokenSource.Cancel()
            '      m_running = False
            '    Case Else
            '  End Select
            'Next
          End If
        End If

      Else

        If Not m_isAlt Then
          m_isAlt = GetKey(Key.ALT).Pressed
          Menu.AltPressed = True
        Else
          m_isAlt = GetKey(Key.ALT).Held
          If Not m_isAlt AndAlso Menu.AltPressed Then Menu.ShowAccelerators = True
        End If

        If Not Menu.ShowAccelerators AndAlso
           Not Menu.Focused Then

          ' End+Enter - Insert Line below???
          ' End,Enter - Insert a blank line below the cursor position
          ' Home,Ctrl+N - Insert a blank line above the cursor position (WordStar)

          Dim handled = False

          If keys IsNot Nothing Then

            For Each key In keys

              If m_isCtrlP Then
                If isControl Then
                  Select Case key
                    Case Else
                      m_isCtrlP = False
                  End Select
                Else
                  m_isCtrlP = False
                End If
                handled = True
              ElseIf m_isK Then
                Select Case key
                  Case ConsoleKey.D0, ConsoleKey.D1, ConsoleKey.D2, ConsoleKey.D3 ' Set bookmarks (maximum of 4)
                  Case Else
                    m_isK = False
                End Select
                handled = True
              ElseIf m_isQ Then
                Select Case key
                  Case ConsoleKey.D0, ConsoleKey.D1, ConsoleKey.D2, ConsoleKey.D3 ' Go to set bookmarks
                  Case ConsoleKey.A ' Search for and replace text
                  Case ConsoleKey.D ' Move cursor to end of line (WordStar)
                  Case ConsoleKey.E ' Move cursor to top of window (WordStar)
                  Case ConsoleKey.F ' Search for text (WordStar)
                  Case ConsoleKey.S ' Cursor to beginning of current line (WordStar)
                  Case ConsoleKey.X ' Move cursor to bottom of window (WordStar)
                  Case ConsoleKey.Y ' Cut to end of line (WordStar)
                  Case Else
                    m_isQ = False
                End Select
                handled = True
              ElseIf isShift Then
                Select Case key
                  Case ConsoleKey.F1 ' Help on QBasic Help
                    UsingHelpAction()
                    handled = True
                  Case ConsoleKey.F2 ' Display the next procedure
                    handled = True
                  Case ConsoleKey.F5 ' Start program execution from beginning
                    StartAction()
                    handled = True
                  Case ConsoleKey.F6 ' Make the previous window the active window
                    If Document1.Focused Then
                      If m_help.Visible Then
                        FocusHelp()
                      ElseIf m_immediate.Visible Then
                        FocusImmediate()
                      ElseIf Document2.Visible Then
                        FocusDocument2()
                      Else
                        FocusDocument1()
                      End If
                    ElseIf Document2.Visible AndAlso Document2.Focused Then
                      If Document1.Visible Then
                        FocusDocument1()
                      ElseIf m_help.Visible Then
                        FocusHelp()
                      ElseIf m_immediate.Visible Then
                        FocusImmediate()
                      Else
                        FocusDocument2()
                      End If
                    ElseIf m_immediate.Visible AndAlso m_immediate.Focused Then
                      If Document2.Visible Then
                        FocusDocument2()
                      ElseIf Document1.Visible Then
                        FocusDocument1()
                      ElseIf m_help.Visible Then
                        FocusHelp()
                      Else
                        FocusImmediate()
                      End If
                    ElseIf m_help.Visible AndAlso m_help.Focused Then
                      If m_immediate.Visible Then
                        FocusImmediate()
                      ElseIf Document2.Visible Then
                        FocusDocument2()
                      ElseIf Document1.Visible Then
                        FocusDocument1()
                      Else
                        FocusHelp()
                      End If
                    End If
                    handled = True
                  Case Else
                End Select
              ElseIf isShift AndAlso isControl Then
                Select Case key
                  Case ConsoleKey.UpArrow ' Select Words
                    handled = True
                  Case ConsoleKey.DownArrow ' Select Words
                    handled = True
                  Case ConsoleKey.LeftArrow ' Select Words
                    handled = True
                  Case ConsoleKey.RightArrow ' Select Words
                  Case ConsoleKey.Home ' Select to beginning of file
                    handled = True
                  Case ConsoleKey.End ' Select to end of file
                    handled = True
                  Case Else
                End Select
              ElseIf isControl Then
                Select Case key
                  Case ConsoleKey.F2 ' Display the previous procedure
                    handled = True
                  Case ConsoleKey.F10 ' Switch between multiple windows and full-screen active window
                    ZoomEditorAction()
                    handled = True
                  Case Else
                End Select
              ElseIf m_isAlt Then
                Select Case key
                  Case ConsoleKey.Add ' Increase size of active window
                    'NOTE: The OemPlus doesn't seem to work for this purpose in the original; just beeps.
                    Menu.AltPressed = False
                    Dim max = 24
                    Dim visibleCount = 3
                    If Not m_help.Visible Then visibleCount -= 1
                    If Not Document2.Visible Then visibleCount -= 1
                    If Not m_immediate.Visible Then visibleCount -= 1
                    max -= visibleCount
                    If m_help.Visible AndAlso m_help.Focused AndAlso m_help.EditorHeight < max Then
                      If Document1.Visible AndAlso Document1.EditorHeight > 2 Then ' Shrink Document1?
                        Document1.EditorHeight -= 1 : Document1.EditorTop += 1
                        m_help.EditorHeight += 1
                      ElseIf Document2.Visible AndAlso Document2.EditorHeight > 2 Then ' Shrink Document2?
                        Document2.EditorHeight -= 1 : Document2.EditorTop += 1
                        If Document1.Visible Then Document1.EditorTop += 1 ' If shrink, move Document1 down.
                        m_help.EditorHeight += 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight > 2 Then ' Shrink Immediate?
                        m_immediate.EditorTop += 1
                        m_immediate.EditorHeight -= 1
                        If Document1.Visible Then Document1.EditorTop += 1 ' If shrink, move Document1 down.
                        If Document2.Visible Then Document2.EditorTop += 1 ' If shrink, move Document2 down.
                        m_help.EditorHeight += 1
                      End If
                    ElseIf Document1.Visible AndAlso Document1.Focused AndAlso Document1.EditorHeight < max Then
                      If Document2.Visible AndAlso Document2.EditorHeight > 2 Then
                        Document2.EditorTop += 1 : Document2.EditorHeight -= 1
                        Document1.EditorHeight += 1
                      ElseIf m_help.Visible AndAlso m_help.EditorHeight > 2 Then
                        m_help.EditorHeight -= 1
                        Document1.EditorTop -= 1 : Document1.EditorHeight += 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight > 2 Then
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                        Document1.EditorHeight += 1
                      End If
                    ElseIf Document2.Visible AndAlso Document2.Focused AndAlso Document2.EditorHeight < max Then
                      If Document1.Visible AndAlso Document1.EditorHeight > 2 Then
                        Document1.EditorHeight -= 1
                        Document2.EditorTop -= 1 : Document2.EditorHeight += 1
                      ElseIf m_help.Visible AndAlso m_help.EditorHeight > 2 Then
                        m_help.EditorHeight -= 1
                        If Document1.Visible Then Document1.EditorTop -= 1
                        Document2.EditorTop -= 1 : Document2.EditorHeight += 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight > 2 Then
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                        Document2.EditorHeight += 1
                      End If
                    ElseIf m_immediate.Visible AndAlso m_immediate.Focused AndAlso m_immediate.EditorHeight < 12 Then
                      If Document2.Visible AndAlso Document2.EditorHeight > 2 Then
                        Document2.EditorHeight -= 1
                        m_immediate.EditorTop -= 1 : m_immediate.EditorHeight += 1
                      ElseIf Document1.Visible AndAlso Document1.EditorHeight > 2 Then
                        Document1.EditorHeight -= 1
                        If Document2.Visible Then Document2.EditorTop -= 1
                        m_immediate.EditorTop -= 1 : m_immediate.EditorHeight += 1
                      ElseIf m_help.Visible AndAlso m_help.EditorHeight > 2 Then
                        m_help.EditorHeight -= 1
                        If Document1.Visible Then Document1.EditorTop -= 1
                        If Document2.Visible Then Document2.EditorTop -= 1
                        m_immediate.EditorTop -= 1 : m_immediate.EditorHeight += 1
                      End If
                    End If
                    handled = True
                  Case ConsoleKey.Subtract, ConsoleKey.OemMinus ' Decrease size of active window
                    Menu.AltPressed = False
                    If m_help.Focused AndAlso m_help.EditorHeight > 2 Then
                      m_help.EditorHeight -= 1
                      Document1.EditorTop -= 1 : Document1.EditorHeight += 1
                    ElseIf Document1.Focused AndAlso Document1.EditorHeight > 2 Then
                      If m_help.Visible Then
                        m_help.EditorHeight += 1
                        Document1.EditorTop += 1 : Document1.EditorHeight -= 1
                      ElseIf Document2.Visible Then
                        Document1.EditorHeight -= 1
                        Document2.EditorTop -= 1 : Document2.EditorHeight += 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight < 12 Then
                        Document1.EditorHeight -= 1
                        m_immediate.EditorTop -= 1 : m_immediate.EditorHeight += 1
                      End If
                    ElseIf Document2.Focused AndAlso Document2.EditorHeight > 2 Then
                      If Document1.Visible Then
                        Document1.EditorHeight += 1
                        Document2.EditorTop += 1 : Document2.EditorHeight -= 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight < 12 Then
                        Document2.EditorHeight -= 1
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight += 1
                      End If
                    ElseIf m_immediate.Focused AndAlso m_immediate.EditorHeight > 2 Then
                      If Document2.Visible Then
                        Document2.EditorHeight += 1
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                      ElseIf Document1.Visible Then
                        Document1.EditorHeight += 1
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                      ElseIf m_help.Visible Then
                        m_help.EditorHeight += 1
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                      End If
                    End If
                    handled = True
                  Case ConsoleKey.F, ConsoleKey.E, ConsoleKey.V, ConsoleKey.S, ConsoleKey.R, ConsoleKey.D, ConsoleKey.O, ConsoleKey.H
                    Menu.ShowAccelerators = True
                    Menu.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift)
                    handled = True
                  Case Else
                End Select
              ElseIf Not isControl AndAlso Not isAlt AndAlso Not isShift Then
                Select Case key

                  Case ConsoleKey.Escape
                    If Menu.AltPressed AndAlso Menu.Focused Then
                      Menu.Reset()
                      handled = True
                    End If

                  Case ConsoleKey.F1 ' Help on keywords or topics (or right mouse click on word)
                    'TODO: if in a document and on a keyword, open help.
                    TopicAction()
                    handled = True
                  Case ConsoleKey.F2 ' Display a list of loaded SUB procedures
                    SubsAction()
                    handled = True
                  Case ConsoleKey.F3 ' Repeat find for same text
                    RepeatLastFindAction()
                    handled = True
                  Case ConsoleKey.F4 ' Switch between the output screen and the View window
                    OutputScreenAction()
                    handled = True
                  Case ConsoleKey.F5 ' Continue running
                    RestartAction()
                    handled = True
                  Case ConsoleKey.F6 ' Make the next window the active window
                    If Document1.Focused Then
                      If Document2.Visible Then
                        FocusDocument2()
                      ElseIf m_immediate.Visible Then
                        FocusImmediate()
                      ElseIf m_help.Visible Then
                        FocusHelp()
                      Else
                        FocusDocument1()
                      End If
                    ElseIf Document2.Visible AndAlso Document2.Focused Then
                      If m_immediate.Visible Then
                        FocusImmediate()
                      ElseIf m_help.Visible Then
                        FocusHelp()
                      ElseIf Document1.Visible Then
                        FocusDocument1()
                      Else
                        FocusDocument2()
                      End If
                    ElseIf m_immediate.Visible AndAlso m_immediate.Focused Then
                      If m_help.Visible Then
                        FocusHelp()
                      ElseIf Document1.Visible Then
                        FocusDocument1()
                      ElseIf Document2.Visible Then
                        FocusDocument2()
                      Else
                        FocusImmediate()
                      End If
                    ElseIf m_help.Visible AndAlso m_help.Focused Then
                      If Document1.Visible Then
                        FocusDocument1()
                      ElseIf Document2.Visible Then
                        FocusDocument2()
                      ElseIf m_immediate.Visible Then
                        FocusImmediate()
                      Else
                        FocusHelp()
                      End If
                    End If
                    handled = True
                  Case ConsoleKey.F7 ' Execute to cursor
                    handled = True
                  Case ConsoleKey.F8 ' Single step
                    StepAction()
                    handled = True
                  Case ConsoleKey.F9 ' Toggle breakpoint
                    ToggleBreakpointAction()
                    handled = True
                  Case ConsoleKey.F10 ' Procedure step
                    ProcedureStepAction()
                    handled = True

                  Case Else
                End Select
              End If

            Next

          End If

          If cursorVisible Then
            If m_help.Focused Then
              cursorVisible = m_help.EditorHeight > 2
            ElseIf Document1.Focused Then
              cursorVisible = Document1.EditorHeight > 2
            ElseIf Document2.Focused Then
              cursorVisible = Document2.EditorHeight > 2
            ElseIf m_immediate.Focused Then
              cursorVisible = m_immediate.EditorHeight > 2
            Else
            End If
          End If

          If cursorVisible AndAlso Not handled Then
            If m_help.Focused Then
              If Not m_help.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift) Then
                HideHelp()
              End If
            ElseIf Document1.Focused Then
              Document1.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift)
            ElseIf Document2.Focused Then
              Document2.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift)
            ElseIf m_immediate.Focused Then
              Document1.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift)
            Else
            End If
          End If

        Else
          cursorVisible = False

          Menu.ProcessKeys(keys, Me.CapsLock, isControl, isAlt, isShift)
        End If

      End If

    End If

    If m_running Then 'Interpreter.IsRunning Then

      If g_display IsNot Nothing Then

        Dim p = g_display.Pixels(g_display.VisualPage)
        Dim w = g_display.ScreenWidth
        Dim h = g_display.ScreenHeight
        Dim pxl As Pixel
        For y = 0 To h - 1
          For x = 0 To w - 1
            Dim c = p(y * w + x)
            pxl.I = c
            If pxl.R <> 0 OrElse pxl.B <> 0 Then ' swap the red and blue
              Dim b = pxl.R : pxl.R = pxl.B : pxl.B = b
            End If
            Draw(x, y, pxl)
          Next
        Next

      Else

        Dim w = 640
        Dim h = 400
        For y = 0 To h - 1
          For x = 0 To w - 1
            Dim c = Buffer(y * w + x)
            Draw(x, y, c)
          Next
        Next

        'For r = 0 To 24
        '  For c = 0 To 79

        '    Dim index = (r * 80) + c
        '    Dim ch = CByte(Screen0(index) And &HFF)
        '    Dim clr = ((Screen0(index) And &HFF00) \ 256) And &HFF
        '    Dim map = CharMap(m_textH, ch)

        '    Dim x = c * m_textW
        '    Dim y = r * m_textH

        '    Dim fg, bg As Integer
        '    SplitColor(clr, fg, bg)

        '    Dim fgc = m_palette(fg)
        '    Dim bgc = m_palette(bg)

        '    For dy = 0 To m_textH - 1
        '      Draw(x + 0, dy + y, If((map(dy) And 1) > 0, fgc, bgc))
        '      Draw(x + 1, dy + y, If((map(dy) And 2) > 0, fgc, bgc))
        '      Draw(x + 2, dy + y, If((map(dy) And 4) > 0, fgc, bgc))
        '      Draw(x + 3, dy + y, If((map(dy) And 8) > 0, fgc, bgc))
        '      Draw(x + 4, dy + y, If((map(dy) And 16) > 0, fgc, bgc))
        '      Draw(x + 5, dy + y, If((map(dy) And 32) > 0, fgc, bgc))
        '      Draw(x + 6, dy + y, If((map(dy) And 64) > 0, fgc, bgc))
        '      Draw(x + 7, dy + y, If((map(dy) And 128) > 0, fgc, bgc))
        '    Next

        '  Next
        'Next

      End If

    Else
      DrawScreen()
      ' Draws pixels to screen...
      For r = 0 To 24
        For c = 0 To 79

          Dim index = (r * 80) + c
          Dim ch = CByte(Screen0(index) And &HFF)
          Dim clr = ((Screen0(index) And &HFF00) \ 256) And &HFF
          Dim map = CharMap(m_textH, ch)

          Dim x = c * m_textW
          Dim y = r * m_textH

          Dim fg, bg As Integer
          SplitColor(clr, fg, bg)

          Dim fgc = m_palette(fg)
          Dim bgc = m_palette(bg)

          For dy = 0 To m_textH - 1
            Draw(x + 0, dy + y, If((map(dy) And 1) > 0, fgc, bgc))
            Draw(x + 1, dy + y, If((map(dy) And 2) > 0, fgc, bgc))
            Draw(x + 2, dy + y, If((map(dy) And 4) > 0, fgc, bgc))
            Draw(x + 3, dy + y, If((map(dy) And 8) > 0, fgc, bgc))
            Draw(x + 4, dy + y, If((map(dy) And 16) > 0, fgc, bgc))
            Draw(x + 5, dy + y, If((map(dy) And 32) > 0, fgc, bgc))
            Draw(x + 6, dy + y, If((map(dy) And 64) > 0, fgc, bgc))
            Draw(x + 7, dy + y, If((map(dy) And 128) > 0, fgc, bgc))
          Next

        Next
      Next

      ' Blinking cursor...
      If cursorVisible AndAlso m_cursorVisible Then
        If CInt(Fix(m_t * 8)) Mod 2 = 0 Then

          Dim cc = QBLib.Video.CursorCol
          Dim cr = QBLib.Video.CursorRow
          If m_context IsNot Nothing Then
            cc = m_context.CursorCol
            cr = m_context.CursorRow
          ElseIf m_help.Focused Then
            cc = m_help.CursorCol
            cr = m_help.CursorRow
          ElseIf Document1.Focused Then
            cc = Document1.CursorCol
            cr = Document1.CursorRow
          ElseIf Document2.Focused Then
            cc = Document2.CursorCol
            cr = Document2.CursorRow
          ElseIf m_immediate.Focused Then
            cc = m_immediate.CursorCol
            cr = m_immediate.CursorRow
          Else
          End If

          Dim strt = CursorStart
          Dim stp = CursorStop

          If stp = 0 Then ' RULE: If stop=0, nothing...
            If strt = 0 Then ' ...unless start=0
              ' draw a single line at top of block
            Else
              strt = -1
            End If
          ElseIf stp < strt Then ' RULE: If stop<start, from stop to bottom
            strt = stp : stp = 14
          ElseIf strt = stp Then ' RULE: If start=stop, single line at start...
            If strt <= 3 Then
            Else '...(or bottom if start=>4)
              strt = 14 : stp = 14
            End If
          ElseIf strt >= 3 Then ' RULE: Calcuate difference between start and stop
            If stp - strt = 1 Then ' If difference is 1, draw two lines at bottom
              strt = 14 : stp = 14
            ElseIf stp - strt = 2 Then ' If difference is 2, draw two lines at bottom
              strt = 13 : stp = 14
            Else ' Draw "half" cursor...
              strt = 7 : stp = 14
            End If
          ElseIf strt <= 1 Then ' RULE: If start<=1 then...
            If stp <= 3 Then ' ... start to difference + 1
              stp = stp - strt + 1
            Else ' ...start to bottom...
              stp = 14
            End If
          ElseIf stp = 2 Then ' RULE: If start=2...
            If stp = 3 Then ' If stop=3, then 2 to 3
            ElseIf stp = 4 Then ' If stop=4, then 3 lines at bottom
              strt = 12
              stp = 14
            Else ' Otherwise 2 to bottom
              strt = 2
              stp = 14
            End If
          End If

          If strt > -1 Then
            Dim x = (cc - 1) * m_textW
            Dim y = (cr - 1) * m_textH
            Dim c = SCREEN(cr, cc, 1)
            Dim fg, bg As Integer
            SplitColor(c, fg, bg)
            Dim cclr = Presets.Gray
            If bg = 8 Then cclr = Presets.Black
            For i = strt To stp
              DrawLine(x, y + i, x + 7, y + i, cclr)
            Next
          End If

        End If
      End If

    End If

    Return Not m_exit

  End Function

  Private Sub ShowHelp()
    m_help.EditorTop = 2 : m_help.EditorHeight = 21
    Dim row = 22
    If Document1.Visible Then
      Document1.EditorTop = row
      If Document2.Visible Then
        If m_immediate.Visible Then
          Document2.EditorTop = 23
          m_immediate.EditorTop = 24
          Document2.EditorHeight = 2
          Document1.EditorHeight = 2
          m_immediate.EditorHeight = 2
        Else
          Document2.EditorTop = 24
          Document2.EditorHeight = 2
          Document1.EditorHeight = 3
        End If
      Else
        If m_immediate.Visible Then
          m_immediate.EditorTop = 24
          Document1.EditorHeight = 3
          m_immediate.EditorHeight = 2
        Else
          Document1.EditorHeight = 4
        End If
      End If
    End If
    m_help.Visible = True : FocusHelp()
  End Sub

  Private Sub HideHelp()
    m_help.Visible = False
    Dim h = (Document1.EditorTop - 1) + Document1.EditorHeight
    Document1.EditorTop = 2
    Document1.EditorHeight = h
    FocusDocument1()
  End Sub

  Private Sub FocusDocument1()
    m_help.Focused = False
    m_help.ScrollBars = False
    m_immediate.Focused = False
    Document2.Focused = False
    Document2.ScrollBars = False
    Document1.Focused = True
    Document1.ScrollBars = True
    QBLib.Video.LOCATE(Document1.EditorTop + 1, Document1.EditorLeft + 1)
  End Sub

  Private Sub FocusDocument2()
    m_help.Focused = False
    m_help.ScrollBars = False
    m_immediate.Focused = False
    Document1.Focused = False
    Document1.ScrollBars = False
    Document2.Focused = True
    Document2.ScrollBars = True
    QBLib.Video.LOCATE(Document2.EditorTop + 1, Document2.EditorLeft + 1)
  End Sub

  Private Sub FocusHelp()
    Document1.Focused = False
    Document1.ScrollBars = False
    Document2.Focused = False
    Document2.ScrollBars = False
    m_immediate.Focused = False
    m_help.Focused = True
    m_help.ScrollBars = True
    QBLib.Video.LOCATE(m_help.EditorTop + 1, m_help.EditorLeft + 1)
  End Sub

  Private Sub FocusImmediate()
    m_help.Focused = False
    m_help.ScrollBars = False
    Document1.Focused = False
    Document1.ScrollBars = False
    Document2.Focused = False
    Document2.ScrollBars = False
    m_immediate.Focused = True
    QBLib.Video.LOCATE(m_immediate.EditorTop + 1, m_immediate.EditorLeft + 1)
  End Sub

  Private Function GetPressed() As List(Of ConsoleKey)

    Dim result As New List(Of ConsoleKey)

    If GetKey(Key.LEFT).Pressed Then result.Add(ConsoleKey.LeftArrow)
    If GetKey(Key.RIGHT).Pressed Then result.Add(ConsoleKey.RightArrow)
    If GetKey(Key.UP).Pressed Then result.Add(ConsoleKey.UpArrow)
    If GetKey(Key.DOWN).Pressed Then result.Add(ConsoleKey.DownArrow)
    If GetKey(Key.HOME).Pressed Then result.Add(ConsoleKey.Home)
    If GetKey(Key.END).Pressed Then result.Add(ConsoleKey.End)
    If GetKey(Key.PGUP).Pressed Then result.Add(ConsoleKey.PageUp)
    If GetKey(Key.PGDN).Pressed Then result.Add(ConsoleKey.PageDown)
    If GetKey(Key.INS).Pressed Then result.Add(ConsoleKey.Insert)
    If GetKey(Key.DEL).Pressed Then result.Add(ConsoleKey.Delete)
    If GetKey(Key.BACK).Pressed Then result.Add(ConsoleKey.Backspace)
    If GetKey(Key.TAB).Pressed Then result.Add(ConsoleKey.Tab)
    If GetKey(Key.ENTER).Pressed Then result.Add(ConsoleKey.Enter)

    If GetKey(Key.NP_ADD).Pressed Then result.Add(ConsoleKey.Add)
    If GetKey(Key.NP_SUB).Pressed Then result.Add(ConsoleKey.Subtract)
    If GetKey(Key.NP_MUL).Pressed Then result.Add(ConsoleKey.Multiply)
    If GetKey(Key.NP_DIV).Pressed Then result.Add(ConsoleKey.Divide)

    If GetKey(Key.OEM_1).Pressed Then result.Add(ConsoleKey.Oem1) ' ;: (186)
    If GetKey(Key.EQUALS).Pressed Then result.Add(ConsoleKey.OemPlus) ' =+ (187)
    If GetKey(Key.COMMA).Pressed Then result.Add(ConsoleKey.OemComma) ' ,< (188)
    If GetKey(Key.MINUS).Pressed Then result.Add(ConsoleKey.OemMinus) ' -_ (189)
    If GetKey(Key.PERIOD).Pressed Then result.Add(ConsoleKey.OemPeriod) ' .> (190)
    If GetKey(Key.OEM_2).Pressed Then result.Add(ConsoleKey.Oem2) ' /?
    If GetKey(Key.OEM_3).Pressed Then result.Add(ConsoleKey.Oem3) ' `~
    If GetKey(Key.OEM_4).Pressed Then result.Add(ConsoleKey.Oem4) ' [{
    If GetKey(Key.OEM_5).Pressed Then result.Add(ConsoleKey.Oem5) ' \|
    If GetKey(Key.OEM_6).Pressed Then result.Add(ConsoleKey.Oem6) ' ]}
    If GetKey(Key.OEM_7).Pressed Then result.Add(ConsoleKey.Oem7) ' '"

    If GetKey(Key.ESCAPE).Pressed Then result.Add(ConsoleKey.Escape)

    If GetKey(Key.SCROLL).Pressed Then result.Add(ConsoleKey.Attention)
    If GetKey(Key.PAUSE).Pressed Then result.Add(ConsoleKey.Attention)

    If GetKey(Key.F1).Pressed Then result.Add(ConsoleKey.F1)
    If GetKey(Key.F2).Pressed Then result.Add(ConsoleKey.F2)
    If GetKey(Key.F3).Pressed Then result.Add(ConsoleKey.F3)
    If GetKey(Key.F4).Pressed Then result.Add(ConsoleKey.F4)
    If GetKey(Key.F5).Pressed Then result.Add(ConsoleKey.F5)
    If GetKey(Key.F6).Pressed Then result.Add(ConsoleKey.F6)
    If GetKey(Key.F7).Pressed Then result.Add(ConsoleKey.F7)
    If GetKey(Key.F8).Pressed Then result.Add(ConsoleKey.F8)
    If GetKey(Key.F9).Pressed Then result.Add(ConsoleKey.F9)
    If GetKey(Key.F10).Pressed Then result.Add(ConsoleKey.F10)
    If GetKey(Key.F11).Pressed Then result.Add(ConsoleKey.F11)
    If GetKey(Key.F12).Pressed Then result.Add(ConsoleKey.F12)

    If GetKey(Key.SPACE).Pressed Then result.Add(ConsoleKey.Spacebar)
    If GetKey(Key.A).Pressed Then result.Add(ConsoleKey.A)
    If GetKey(Key.B).Pressed Then result.Add(ConsoleKey.B)
    If GetKey(Key.C).Pressed Then result.Add(ConsoleKey.C)
    If GetKey(Key.D).Pressed Then result.Add(ConsoleKey.D)
    If GetKey(Key.E).Pressed Then result.Add(ConsoleKey.E)
    If GetKey(Key.F).Pressed Then result.Add(ConsoleKey.F)
    If GetKey(Key.G).Pressed Then result.Add(ConsoleKey.G)
    If GetKey(Key.H).Pressed Then result.Add(ConsoleKey.H)
    If GetKey(Key.I).Pressed Then result.Add(ConsoleKey.I)
    If GetKey(Key.J).Pressed Then result.Add(ConsoleKey.J)
    If GetKey(Key.K).Pressed Then result.Add(ConsoleKey.K)
    If GetKey(Key.L).Pressed Then result.Add(ConsoleKey.L)
    If GetKey(Key.M).Pressed Then result.Add(ConsoleKey.M)
    If GetKey(Key.N).Pressed Then result.Add(ConsoleKey.N)
    If GetKey(Key.O).Pressed Then result.Add(ConsoleKey.O)
    If GetKey(Key.P).Pressed Then result.Add(ConsoleKey.P)
    If GetKey(Key.Q).Pressed Then result.Add(ConsoleKey.Q)
    If GetKey(Key.R).Pressed Then result.Add(ConsoleKey.R)
    If GetKey(Key.S).Pressed Then result.Add(ConsoleKey.S)
    If GetKey(Key.T).Pressed Then result.Add(ConsoleKey.T)
    If GetKey(Key.U).Pressed Then result.Add(ConsoleKey.U)
    If GetKey(Key.V).Pressed Then result.Add(ConsoleKey.V)
    If GetKey(Key.W).Pressed Then result.Add(ConsoleKey.W)
    If GetKey(Key.X).Pressed Then result.Add(ConsoleKey.X)
    If GetKey(Key.Y).Pressed Then result.Add(ConsoleKey.Y)
    If GetKey(Key.Z).Pressed Then result.Add(ConsoleKey.Z)

    If GetKey(Key.K0).Pressed Then result.Add(ConsoleKey.D0)
    If GetKey(Key.K1).Pressed Then result.Add(ConsoleKey.D1)
    If GetKey(Key.K2).Pressed Then result.Add(ConsoleKey.D2)
    If GetKey(Key.K3).Pressed Then result.Add(ConsoleKey.D3)
    If GetKey(Key.K4).Pressed Then result.Add(ConsoleKey.D4)
    If GetKey(Key.K5).Pressed Then result.Add(ConsoleKey.D5)
    If GetKey(Key.K6).Pressed Then result.Add(ConsoleKey.D6)
    If GetKey(Key.K7).Pressed Then result.Add(ConsoleKey.D7)
    If GetKey(Key.K8).Pressed Then result.Add(ConsoleKey.D8)
    If GetKey(Key.K9).Pressed Then result.Add(ConsoleKey.D9)

    If result.Count = 0 Then Return Nothing Else Return result

  End Function

  Private Async Sub Menu_OnClick(sender As Object, e As MenuClickEventArgs) Handles Menu.OnClick

    Select Case e.Item.Text
      Case "New" : Await NewActionAsync()
      Case "Open..." : OpenAction()
      Case "Save" : SaveAction()
      Case "Save As..." : SaveAsAction()
      Case "Print..." : PrintAction()
      Case "Exit" : Await ExitActionAsync()
      Case "Cut" : CutAction()
      Case "Copy" : CopyAction()
      Case "Paste" : PasteAction()
      Case "Clear" : ClearAction()
      Case "New SUB..." : NewSubAction()
      Case "New FUNCTION..." : NewFunctionAction()
      Case "SUBs..." : SubsAction()
      Case "Split" : SplitAction()
      Case "Output Screen" : OutputScreenAction()
      Case "Find..." : FindAction()
      Case "Repeat Last Find" : RepeatLastFindAction()
      Case "Change..." : ChangeAction()
      Case "Start" : StartAction()
      Case "Restart" : RestartAction()
      Case "Continue" : ContinueAction()
      Case "Make EXE File (FBC)..." : MakeExeFileFbcAction()
      Case "In Web browser (QBJS)..." : LaunchInWebBrowserAction()
      Case "Step" : StepAction()
      Case "Procedure Step" : ProcedureStepAction()
      Case "Trace On" : TraceOnAction()
      Case "Toggle Breakpoint" : ToggleBreakpointAction()
      Case "Clear All Breakpoints" : ClearAllBreakpointsAction()
      Case "Set Next Statement" : SetNextStatementAction()
      Case "Display..." : DisplayAction()
      Case "Help Path..." : HelpPathAction()
      Case "Syntax Checking" : SyntaxCheckingAction()
      Case "Index" : IndexAction()
      Case "Contents" : ContentsAction()
      Case "Using Help" : UsingHelpAction()
      Case "About..."
        AboutAction()
      Case Else
        If e.Item.Text.StartsWith("Topic:") Then
          TopicAction()
        Else
          m_context = New MessageDialog($"Unhandled menu '{e.Item.Text}'")
        End If
    End Select
  End Sub

#Region "Actions"

  Private Async Function NewActionAsync() As Task
    If Document1.Changed Then
      m_selected = 0
      Do
        m_context = New SavePrompt(m_selected)
        While m_context IsNot Nothing
          Await Task.Delay(1)
        End While
        Select Case Math.Abs(m_selected)
          Case 0 ' Yes
            ' Save file...
            Exit Do
          Case 1 ' No
            Exit Do
          Case 2 ' Cancel
            Return
          Case 3 ' Help
            m_context = Nothing
            Dim body = "Your file has either never been saved or has not been saved
since it last changed. Either:
  - Choose <Yes> to save the file, preserving your
    changes.
  - Choose <No> to discard your changes.
  - Choose <Cancel> to cancel the command, leaving
    everything as it was before you chose this command.".Replace("-"c, ChrW(254))
            m_context = New HelpDialog("File not saved", body)
            While m_context IsNot Nothing
              Await Task.Delay(1)
            End While
            If m_selected < 0 Then m_selected = 0
          Case Else
        End Select
      Loop
    End If
    Document1.Clear()
    Document2.Clear()
    DrawScreen()
  End Function

  Private Sub OpenAction()
    m_context = New OpenDialog(m_pathspec)
  End Sub

  Private Sub SaveAction()
    If Document1.Title = "Untitled" Then
      SaveAsAction()
    Else
      System.IO.File.WriteAllText(m_path, Document1.Text)
      Document1.Changed = False
    End If
  End Sub

  Private Sub SaveAsAction()
    m_context = New SaveAsDialog(m_pathspec, If(Document1.Title <> "Untitled", Document1.Title, "*.BAS"))
  End Sub

  Private Sub PrintAction()
    m_context = New PrintDialog()
  End Sub

  Private Async Function ExitActionAsync() As Task
    If Document1.Changed Then
      m_selected = 0
      Do
        m_context = New SavePrompt(m_selected)
        While m_context IsNot Nothing
          Await Task.Delay(1)
        End While
        Select Case Math.Abs(m_selected)
          Case 0 ' Yes
            ' Save file...
            Exit Do
          Case 1 ' No
            Exit Do
          Case 2 ' Cancel
            Return
          Case 3 ' Help
            m_context = Nothing
            Dim body = "Your file has either never been saved or has not been saved
since it last changed. Either:
  - Choose <Yes> to save the file, preserving your
    changes.
  - Choose <No> to discard your changes.
  - Choose <Cancel> to cancel the command, leaving
    everything as it was before you chose this command.".Replace("-"c, ChrW(254))
            m_context = New HelpDialog("File not saved", body)
            While m_context IsNot Nothing
              Await Task.Delay(1)
            End While
            If m_selected < 0 Then m_selected = 0
          Case Else
        End Select
      Loop
    End If
    m_exit = True
  End Function

  Private Sub CutAction()
    If Document1.CanCutSelection Then
      Document1.CutSelection()
    End If
  End Sub

  Private Sub CopyAction()
    If Document1.CanCopySelection Then
      Document1.CopySelection()
    End If
  End Sub

  Private Sub PasteAction()
    If Document1.CanPaste Then
      Document1.Paste()
    End If
  End Sub

  Private Sub ClearAction()
    If Document1.CanDeleteSelection Then
      Document1.DeleteSelection()
    End If
  End Sub

  Private Sub NewSubAction()
    m_context = New NewSubDialog()
  End Sub

  Private Sub NewFunctionAction()
    m_context = New NewFunctionDialog
  End Sub

  Private Sub TopicAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub UsingHelpAction()
    m_help.Title = "Using Help"
    m_help.Content = <content><![CDATA[  <Contents>  <Index>  <Back>
---
Using QBasic Help:

  * To get help on a Basic keyword, place the cursor on it and press F1
    or click the right mouse button.

  * To get help on a QBasic menu, command, or dialog box, place the cursor
    on the menu item or <Help> button and press F1.

  * To view QBasic Help topics, press Alt+H, then press the highlighted
    letter to choose a command.

  * To move the cursor into the Help window, press Shift+F6.

  * To scroll the help information, press PgDn or PgUp.

  * To copy help information (such as a programming example) to the
    View window, use the commands on the QBasic Edit menu.

  * To close the Help window, press Esc.

In the Help window, you can display information on:

  * Help topics (identified by highlighted arrowheads < >)

  * Basic keywords

To move the cursor to a Help topic, press Tab or the first letter of the
topic. To display the topic or keyword information, place the cursor
anywhere on the topic or keyword and press F1 or click the right mouse
button.

QBasic saves the last 20 Help topics you viewed. To review them press
Alt+F1 or click the <Back> button repeatedly.
]]></content>.Value
    ShowHelp()
  End Sub

  Private Sub ContentsAction()
    m_help.Title = "Table of Contents"
    m_help.Content = <content><![CDATA[  <Contents>  <Index>  <Back>
---

  +Orientation-------------------------+ +Keys--------------------------+
  | <Using Help>                       | | <Shortcut Keys Summary>      |
  | <Using Menus and Commands>         | | <Editing Keys>               |
  | <Using a Dialog Box>               | | <View and Search Keys>       |
  | <Syntax Conventions>               | | <Run and Debug Keys>         |
  +------------------------------------+ | <Help Keys>                  |
  +Using QBasic------------------------+ +------------------------------+ 
  | <QBasic Command Line>              | +Quick Reference---------------+
  | <Basic Character Set>              | | <ASCII Character Codes>      |
  | <Keywords by Programming Task>     | | <Keyboard Scan Codes>        |
  | <QBasic Environment Limits>        | | <Run-Time Error Codes>       |
  | <Version Differences>              | | <Copyright and Trademarks>   |
  | <Converting BASICA Programs>       | +------------------------------+ 
  | <Beyond QBasic>                    | 
  +------------------------------------+

]]></content>.Value
    ShowHelp()
  End Sub

  Private Sub IndexAction()
    m_help.Title = "QBasic Online Help Index"
    m_help.Content = <content><![CDATA[  <Contents>  <Index>  <Back>
---
To get help on a QBasic keyword in the list below:
    1. Press the key of the first letter of the keyword.
    2. Use the direction keys to move the cursor to the keyword.
    3. Press F1 to display the help text in the Help window.
                                                                  +===+
==================================================================| A |
                                                                  +===+
  ABS Function                       APPEND Keyword
  ABSOLUTE Keyword                   AS Keyword
  ACCESS Keyword                     ASC Function
  AND Operator                       ATN Function
  ANY Keyword
                                                                  +===+
==================================================================| B |
                                                                  +===+
  BASE Keyword                       BLOAD Statement
  Basic Character Set                Boolean Operators
  BEEP Statement                     BSAVE Statement
  BINARY Keyword
                                                                  +===+
==================================================================| C |
                                                                  +===+
  CALL Statement                     COLOR Statement
  CALL ABSOLUTE Statement            COM Statement
  CASE Keyword                       COMMON Statement
  ...

]]></content>.Value
    ShowHelp()
  End Sub

  Private Sub SyntaxCheckingAction()
    ' Toggle value
    Menu.Items(6).Items(2).Checked = Not Menu.Items(6).Items(2).Checked
    DrawScreen()
  End Sub

  Private Sub HelpPathAction()
    m_context = New HelpPathDialog()
  End Sub

  Private Sub DisplayAction()
    m_context = New DisplayDialog()
  End Sub

  Private Sub SetNextStatementAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ClearAllBreakpointsAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ToggleBreakpointAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub TraceOnAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ProcedureStepAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub StepAction()
    Dim tree = QB.Interpreter.DebugTree(Document1.Text)
    Document1.Text = tree
    'm_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ContinueAction()
    m_runner = New Threading.Thread(AddressOf RunnerBs)
    m_runner.Start()
  End Sub

  Private Sub RestartAction()
    m_runner = New Threading.Thread(AddressOf RunnerBs)
    m_runner.Start()
  End Sub

  Private Sub StartAction()
    m_runner = New Threading.Thread(AddressOf RunnerBs)
    m_runner.Start()
  End Sub

  Private Sub RunnerGw()

    s_cancelTokenSource = New System.Threading.CancellationTokenSource()
    s_cancelToken = s_cancelTokenSource.Token
    m_running = True

    'InterpreterTimer.Enabled = True

    Dim env As Basic.Environment.IEnvironment = Nothing
    Dim fs As Basic.IO.IVirtualFileSystem = New WindowsFileSystem
    Dim snd As Basic.Audio.ISound = Nothing
    Dim kbd As Basic.Input.IKeyboard = Nothing
    Dim gpio As Basic.IO.IGpio = Nothing
    'm_display = New Display()

    If m_scrn0 IsNot Nothing Then
      Array.Copy(m_scrn0, 0, Screen0, 0, Screen0.Length)
      Array.Copy(m_buffer, 0, Buffer, 0, Buffer.Length)
      'QBLib.Video.m_fgColor = m_fg
      'QBLib.Video.m_bgColor = m_bg
      'QBLib.Video.m_cursorRow = m_cr
      'QBLib.Video.m_cursorCol = m_cc
      QBLib.Video.COLOR(m_fg, m_bg)
      QBLib.Video.LOCATE(m_cr, m_cc)
      m_scrn0 = Nothing
      m_buffer = Nothing
    Else
      QBLib.Video.CLS()
      QBLib.Video.COLOR(8, 0)
    End If

    g_display = New Display
    Dim interpreter = New Basic.Interactive(env,
                                            g_display,
                                            Nothing,
                                            snd,
                                            kbd,
                                            "en-US") With {.Source = Document1.Text}
    interpreter.Run()

    'm_display = Nothing

  End Sub

  Private Sub RunnerBs()
    g_display = Nothing : m_running = True
    s_cancelTokenSource = New System.Threading.CancellationTokenSource()
    s_cancelToken = s_cancelTokenSource.Token
    If m_scrn0 IsNot Nothing Then
      Array.Copy(m_scrn0, 0, Screen0, 0, Screen0.Length)
      Array.Copy(m_buffer, 0, Buffer, 0, Buffer.Length)
      'QBLib.Video.m_fgColor = m_fg
      'QBLib.Video.m_bgColor = m_bg
      'QBLib.Video.m_cursorRow = m_cr
      'QBLib.Video.m_cursorCol = m_cc
      QBLib.Video.COLOR(m_fg, m_bg)
      QBLib.Video.LOCATE(m_cr, m_cc)
      m_scrn0 = Nothing
      m_buffer = Nothing
    Else
      QBLib.Video.COLOR(8, 0)
      QBLib.Video.CLS()
    End If
    Dim i = New QB.Interpreter()
    i.Run(Document1.Text)
  End Sub

  Private Sub MakeExeFileFbcAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub LaunchInWebBrowserAction()

    Dim compress = True

    If Document1.Text <> "" Then 'Title <> "Untitled" Then

      Dim decoded = Document1.Text
      decoded = decoded.Replace(vbCrLf, vbLf)
      Dim bytes = Encoding.UTF8.GetBytes(decoded)

      Dim buffer() As Byte
      Using ms = New MemoryStream()
        Using gs = New GZipStream(ms, CompressionMode.Compress, True)
          gs.Write(bytes, 0, bytes.Length)
        End Using
        ms.Position = 0
        ReDim buffer(CInt(ms.Length - 1))
        ms.Read(buffer, 0, buffer.Length)
      End Using

      Dim encoded = Convert.ToBase64String(If(compress, buffer, bytes))
      'Dim url = $"https://qbjs.org/#code={encoded}"
      Dim url = $"https://boxgaming.github.io/qbjs/#gzcode={encoded}"

      If False Then ' For testing...

        Dim dBuffer = Convert.FromBase64String(encoded)
        Using ms = New MemoryStream(dBuffer)
          Using gs = New GZipStream(ms, CompressionMode.Decompress)
            Using ds = New MemoryStream()
              gs.CopyTo(ds)
              bytes = ds.ToArray
            End Using
          End Using
        End Using

        decoded = Encoding.UTF8.GetString(bytes)

        Document1.Text = $"'decoded
{decoded}"

      End If

      Dim p As New Process
      p.StartInfo.UseShellExecute = True
      p.StartInfo.FileName = url
      p.Start()
      'Process.Start("cmd", "/C start" + " " + url)
    End If

  End Sub

  Private Sub ChangeAction()
    m_context = New ChangeDialog()
  End Sub

  Private Sub RepeatLastFindAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub FindAction()
    m_context = New FindDialog()
  End Sub

  Private Sub OutputScreenAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub SplitAction()
    Dim visible = Document2.Visible
    If visible Then
      Document2.Visible = False
      Document1.EditorHeight += Document2.EditorHeight - 1 '21
      If Not Document1.Focused AndAlso Not m_immediate.Focused Then
        FocusDocument1()
      End If
    Else
      Dim h = Document1.EditorHeight - 1
      Dim h2 = h \ 2
      Dim h1 = h - h2
      Document1.EditorHeight = h1 + 1
      Document2.EditorTop = 2 + h1
      Document2.EditorHeight = h2 + 1
      Document2.ScrollBars = False
      Document2.Visible = True
    End If
  End Sub

  Private Sub SubsAction()
    Dim dlg = New SubsDialog(Document1.Title, m_subs)
    m_context = dlg
  End Sub

  Private Sub AboutAction()
    m_context = New AboutDialog()
  End Sub

  Private m_windowFocused As Boolean

  Private m_help_visible As Boolean
  Private m_help_top As Integer
  Private m_help_height As Integer
  Private m_document1_visible As Boolean
  Private m_document1_top As Integer
  Private m_document1_height As Integer
  Private m_document2_visible As Boolean
  Private m_document2_top As Integer
  Private m_document2_height As Integer
  Private m_immediate_visible As Boolean
  Private m_immediate_top As Integer
  Private m_immediate_height As Integer

  Private Sub ZoomEditorAction()
    If m_windowFocused Then
      m_help.Visible = m_help_visible
      m_help.EditorTop = m_help_top
      m_help.EditorHeight = m_help_height
      Document1.Visible = m_document1_visible
      Document1.EditorTop = m_document1_top
      Document1.EditorHeight = m_document1_height
      Document2.Visible = m_document2_visible
      Document2.EditorTop = m_document2_top
      Document2.EditorHeight = m_document2_height
      m_immediate.Visible = m_immediate_visible
      m_immediate.EditorTop = m_immediate_top
      m_immediate.EditorHeight = m_immediate_height
      m_windowFocused = False
    Else
      m_help_visible = m_help.Visible
      m_help_top = m_help.EditorTop
      m_help_height = m_help.EditorHeight
      m_document1_visible = Document1.Visible
      m_document1_top = Document1.EditorTop
      m_document1_height = Document1.EditorHeight
      m_document2_visible = Document2.Visible
      m_document2_top = Document2.EditorTop
      m_document2_height = Document2.EditorHeight
      m_immediate_visible = m_immediate.Visible
      m_immediate_top = m_immediate.EditorTop
      m_immediate_height = m_immediate.EditorHeight
      If m_help.Focused Then
        m_help.EditorTop = 2
        m_help.EditorHeight = 24
        Document1.Visible = False
        Document2.Visible = False
        m_immediate.Visible = False
        m_windowFocused = True
      ElseIf Document1.Focused Then
        m_help.Visible = False
        Document1.EditorTop = 2
        Document1.EditorHeight = 24
        Document2.Visible = False
        m_immediate.Visible = False
        m_windowFocused = True
      ElseIf Document2.Focused Then
        m_help.Visible = False
        Document1.Visible = False
        Document2.EditorTop = 2
        Document2.EditorHeight = 24
        m_immediate.Visible = False
        m_windowFocused = True
      Else
        'NOTE: Not valid if currently focused on the Immediate window.
        'm_help.Visible = False
        'm_document1.Visible = False
        'm_document2.Visible = False
        'm_immediate.EditorTop = 2
        'm_immediate.EditorHeight = 24
      End If
    End If
  End Sub

  Private Sub Document1_SelectionChanged() Handles Document1.SelectionChanged
    Menu.Items(1).Items(0).Enabled = Document1.CanCutSelection ' Cut
    Menu.Items(1).Items(1).Enabled = Document1.CanCopySelection ' Copy
    Menu.Items(1).Items(2).Enabled = Document1.CanPaste ' Paste
    Menu.Items(1).Items(3).Enabled = Document1.CanDeleteSelection ' Clear
  End Sub

#End Region

End Class
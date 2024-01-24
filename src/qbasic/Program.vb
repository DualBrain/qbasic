Imports VbPixelGameEngine
Imports QB.Video
Imports System.Timers
Imports System.Drawing.Text

Friend Module Program

  Sub Main()
    Dim demo As New QBasic
    If demo.Construct(640, 400, 2, 2) Then ', False, True) Then
      demo.ShowEngineName = False : demo.ShowFPS = False
      demo.Start()
    End If
  End Sub

End Module

Friend Class QBasic
  Inherits PixelGameEngine

  Private m_selected As Integer
  Private m_context As IContext

  Private WithEvents Menu As MainMenu
  Private ReadOnly m_document1 As New DocumentPanel
  Private ReadOnly m_document2 As New DocumentPanel
  Private ReadOnly m_immediate As New ImmediatePanel
  Private ReadOnly m_statusPanel As New StatusPanel

  Private m_t As Single

  Private WithEvents WorkTimer As New Timer(1)

  Friend Sub New()
    AppName = "QBasic?"
  End Sub

  Private m_workTimerActive As Boolean
  Private Sub WorkTimer_Elapsed(sender As Object, e As ElapsedEventArgs) Handles WorkTimer.Elapsed

    If m_workTimerActive Then Return
    m_workTimerActive = True
    Try
      'Dim value = $"{Now:HH:mm:ss}.{Now.Millisecond:000}"
      'QPrintRC(value, 1, 55, 14, 8)
    Finally
      m_workTimerActive = False
    End Try

  End Sub

  Protected Overrides Function OnUserCreate() As Boolean

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

    Menu.CalculateOffsets()

    m_document1.Focused = True
    m_document2.Visible = False

    DrawScreen()

    WorkTimer.Enabled = True

    Return True

  End Function

  Private Sub DrawScreen()

    m_document1.Render()
    m_document2.Render()
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

    If m_document1.Focused Then
      m_statusPanel.DocumentRow = m_document1.DocumentRow
      m_statusPanel.DocumentCol = m_document1.DocumentCol
    ElseIf m_document2.Focused Then
      m_statusPanel.DocumentRow = m_document2.DocumentRow
      m_statusPanel.DocumentCol = m_document2.DocumentCol
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

  Protected Overrides Function OnUserUpdate(elapsedTime As Single) As Boolean

    m_t += elapsedTime

    Dim cursorVisible = True

    Dim keys = GetPressed()

    Dim isAlt = GetKey(Key.ALT).Held OrElse GetKey(Key.ALT).Pressed
    Dim isShift = GetKey(Key.SHIFT).Held OrElse GetKey(Key.SHIFT).Pressed
    Dim isControl = GetKey(Key.CTRL).Held OrElse GetKey(Key.CTRL).Pressed

    If m_context IsNot Nothing Then
      m_context.Render()
      If Not m_context.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift) Then
        If TypeOf m_context Is SavePrompt Then
          m_selected = CType(m_context, SavePrompt).Selected
        End If
        m_context = Nothing : DrawScreen()
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
                  handled = True
                Case ConsoleKey.F2 ' Display the next procedure
                  handled = True
                Case ConsoleKey.F5 ' Start program execution from beginning
                  handled = True
                Case ConsoleKey.F6 ' Make the previous window the active window
                  If m_document1.Focused Then
                    If m_immediate.Visible Then
                      FocusImmediate()
                    ElseIf m_document2.Visible Then
                      FocusDocument2()
                    End If
                  ElseIf m_document2.Visible AndAlso m_document2.Focused Then
                    If m_document1.Visible Then
                      FocusDocument1()
                    ElseIf m_immediate.Visible Then
                      FocusImmediate()
                    End If
                  ElseIf m_immediate.Visible AndAlso m_immediate.Focused Then
                    If m_document2.Visible Then
                      FocusDocument2()
                    Else
                      FocusImmediate()
                    End If
                  End If
                  handled = True
                Case ConsoleKey.UpArrow ' Select Character/lines
                  handled = True
                Case ConsoleKey.DownArrow ' Select Character/lines
                  handled = True
                Case ConsoleKey.LeftArrow ' Select Character/lines
                  handled = True
                Case ConsoleKey.RightArrow ' Select Character/lines
                  handled = True
                Case ConsoleKey.Insert ' Insert From Clipboard
                  handled = True
                Case ConsoleKey.Delete ' Cut selected text
                  handled = True
                Case ConsoleKey.PageUp ' Select Screen up
                  handled = True
                Case ConsoleKey.PageDown ' Select Screen down
                  handled = True
                Case ConsoleKey.Tab ' Delete leading spaces from selected lines
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
                  handled = True
                Case ConsoleKey.A ' Word Left (WordStar)
                  handled = True
                Case ConsoleKey.C ' Page down (WordStar)
                  handled = True
                Case ConsoleKey.D ' Cursor right (WordStar)
                  handled = True
                Case ConsoleKey.E ' Cursor up (WordStar)
                  handled = True
                Case ConsoleKey.F ' Word Right (WordStar)
                  handled = True
                Case ConsoleKey.G ' Delete selected text / Delete one character at the cursor (WordStar)
                  handled = True
                Case ConsoleKey.H ' Delete one character to the left of the cursor (WordStar)
                  handled = True
                Case ConsoleKey.J ' Cursor to beginning of next line (WordStar)
                  handled = True
                Case ConsoleKey.K '...
                  m_isK = True
                  handled = True
                Case ConsoleKey.L ' Repeat find for same text
                  handled = True
                Case ConsoleKey.N ' Insert Line above
                  handled = True
                Case ConsoleKey.P ' Insert special characters...
                  m_isCtrlP = True
                  handled = True
                Case ConsoleKey.Q '... 
                  m_isQ = True
                  handled = True
                Case ConsoleKey.R ' Page up (WordStar)
                  handled = True
                Case ConsoleKey.S ' Cursor left (WordStar)
                  handled = True
                Case ConsoleKey.T ' Delete the rest of the word the cursor is on (WordStar)
                  handled = True
                Case ConsoleKey.V ' Switch between insert and overstrike modes (WordStar)
                  handled = True
                Case ConsoleKey.W ' Scroll up (WordStar)
                  handled = True
                Case ConsoleKey.X ' Cursor dn (WordStar)
                  handled = True
                Case ConsoleKey.Y ' Cut current line
                  handled = True
                Case ConsoleKey.Z ' Scroll dn (WordStar)
                  handled = True
                Case ConsoleKey.Enter ' Cursor to beginning of next line
                  handled = True
                Case ConsoleKey.Insert ' Copy to Clipboard
                  handled = True
                Case ConsoleKey.LeftArrow ' Word Left
                  handled = True
                Case ConsoleKey.RightArrow ' Word Right
                  handled = True
                Case ConsoleKey.UpArrow ' Scroll up
                  handled = True
                Case ConsoleKey.DownArrow ' Scroll dn
                  handled = True
                Case ConsoleKey.PageUp ' Left one window
                  handled = True
                Case ConsoleKey.PageDown ' Right one window
                  handled = True
                Case Else
              End Select
            ElseIf m_isAlt Then
              Select Case key
                Case ConsoleKey.Add ' Increase size of active window
                  Menu.AltPressed = False
                  Dim max = 24
                  If m_document2.Visible AndAlso m_immediate.Visible Then
                    max -= 2
                  ElseIf m_document2.Visible OrElse m_immediate.Visible Then
                    max -= 1
                  End If
                  If m_document1.Focused Then
                    If m_document1.Height < max Then
                      If m_document2.Visible Then
                        If m_document2.Height > 2 Then
                          m_document1.Height += 1
                          m_document2.Row += 1 : m_document2.Height -= 1
                        ElseIf m_immediate.Height > 2 Then
                          m_document1.Height += 1
                          m_document2.Row += 1
                          m_immediate.Row += 1 : m_immediate.Height -= 1
                        End If
                      ElseIf m_immediate.Visible Then
                        If m_immediate.Height > 2 Then
                          m_document1.Height += 1
                          m_immediate.Row += 1 : m_immediate.Height -= 1
                        End If
                      End If
                    End If
                  ElseIf m_document2.Focused Then
                    If m_document2.Height < max Then
                      If m_document1.Height > 2 Then
                        m_document1.Height -= 1
                        m_document2.Row -= 1 : m_document2.Height += 1
                      ElseIf m_immediate.Height > 2 Then
                        m_document2.Row += 1 : m_document2.Height += 1
                        m_immediate.Row += 1 : m_immediate.Height -= 1
                      End If
                    End If
                  ElseIf m_immediate.Focused Then
                    If m_immediate.Height < 12 Then
                      If m_document2.Visible Then
                        If m_document2.Height > 2 Then
                          m_document2.Height -= 1
                          m_immediate.Row -= 1 : m_immediate.Height += 1
                        ElseIf m_document1.Height > 2 Then
                          m_document1.Height -= 1
                          m_document2.Row -= 1
                          m_immediate.Row -= 1 : m_immediate.Height += 1
                        End If
                      Else
                        If m_document1.Height > 2 Then
                          m_document1.Height -= 1
                          m_immediate.Row -= 1 : m_immediate.Height += 1
                        End If
                      End If
                    End If
                  End If
                  handled = True
                Case ConsoleKey.Subtract ' Decrease size of active window
                  Menu.AltPressed = False
                  If m_document1.Focused Then
                    If m_document1.Height > 2 Then
                      If m_document2.Visible Then
                        m_document1.Height -= 1
                        m_document2.Row -= 1 : m_document2.Height += 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.Height < 12 Then
                        m_document1.Height -= 1
                        m_immediate.Row -= 1 : m_immediate.Height += 1
                      End If
                    End If
                  ElseIf m_document2.Focused Then
                    If m_document1.Height > 2 Then
                      m_document2.Height -= 1
                      m_document2.Row -= 1 : m_document2.Height += 1
                    ElseIf m_immediate.Visible Then
                      If m_immediate.Height > 2 AndAlso m_immediate.Height < 12 Then
                        m_document2.Height += 1
                        m_immediate.Row += 1 : m_immediate.Height -= 1
                      End If
                    End If
                  ElseIf m_immediate.Focused Then
                    If m_document2.Visible Then
                    End If
                  End If
                  handled = True
                Case ConsoleKey.F, ConsoleKey.E, ConsoleKey.V, ConsoleKey.S, ConsoleKey.R, ConsoleKey.D, ConsoleKey.O, ConsoleKey.H
                  Menu.ShowAccelerators = True
                  Menu.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift)
                  handled = True
                Case Else
              End Select
            ElseIf Not isShift AndAlso Not isControl AndAlso Not isShift Then
              Select Case key

                Case ConsoleKey.Escape
                  If Menu.AltPressed Then
                    Menu.Reset()
                    handled = True
                  End If

                Case ConsoleKey.F1 ' Help on keywords or topics (or right mouse click on word)
                  handled = True
                Case ConsoleKey.F2 ' Display a list of loaded SUB procedures
                  handled = True
                Case ConsoleKey.F3 ' Repeat find for same text
                  handled = True
                Case ConsoleKey.F4 ' Switch between the output screen and the View window
                  handled = True
                Case ConsoleKey.F5 ' Continue running
                  handled = True
                Case ConsoleKey.F6 ' Make the next window the active window
                  If m_document1.Focused Then
                    If m_document2.Visible Then
                      FocusDocument2()
                    ElseIf m_immediate.Visible Then
                      FocusImmediate()
                    End If
                  ElseIf m_document2.Visible AndAlso m_document2.Focused Then
                    If m_immediate.Visible Then
                      FocusImmediate()
                    ElseIf m_document1.Visible Then
                      FocusDocument1()
                    End If
                  ElseIf m_immediate.Visible AndAlso m_immediate.Focused Then
                    FocusDocument1()
                  End If
                  handled = True
                Case ConsoleKey.F7 ' Execute to cursor
                  handled = True
                Case ConsoleKey.F8 ' Single step
                  handled = True
                Case ConsoleKey.F9 ' Toggle breakpoint
                  handled = True
                Case ConsoleKey.F10 ' Procedure step
                  handled = True

                Case ConsoleKey.Backspace ' Delete one character to the left of the cursor
                  'handled = True
                Case ConsoleKey.Insert ' Switch to insert/overstrike
                  'handled = True
                Case ConsoleKey.Home ' Move cursor to first indention level of the current line
                  'handled = True
                Case ConsoleKey.End ' Move cursor to end of line
                  'handled = True
                Case ConsoleKey.Delete ' Erase selected text / Delete one character at the cursor
                  'handled = True
                Case ConsoleKey.PageUp ' Page up
                  'handled = True
                Case ConsoleKey.PageDown ' Page Down
                  'handled = True

                Case ConsoleKey.UpArrow ': m_codeRow -= 1 : If m_codeRow < 1 Then m_codeRow = 1 ' Cursor Up
                  'handled = True
                Case ConsoleKey.DownArrow ': m_codeRow += 1 : If m_codeRow > 18 Then m_codeRow = 18 ' Cursor Dn
                  'handled = True
                Case ConsoleKey.LeftArrow ': m_codeCol -= 1 : If m_codeCol < 1 Then m_codeCol = 1 ' Cursor Left
                  'handled = True
                Case ConsoleKey.RightArrow ': m_codeCol += 1 : If m_codeCol > 78 Then m_codeCol = 78 ' Cursor Right
                  'handled = True
                Case ConsoleKey.Spacebar ': m_code &= " "c : m_codeCol += 1
                  'handled = True

                Case ConsoleKey.A To ConsoleKey.Z ': m_code &= ChrW(key) : m_codeCol += 1
                  'handled = True

                Case Else
              End Select
            End If

          Next

        End If

        If cursorVisible Then
          If m_document1.Focused Then
            cursorVisible = m_document1.Height > 2
          ElseIf m_document2.Focused Then
            cursorVisible = m_document2.Height > 2
          ElseIf m_immediate.Focused Then
            cursorVisible = m_immediate.Height > 2
          Else
          End If
        End If

        If cursorVisible AndAlso Not handled Then
          If m_document1.Focused Then
            m_document1.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift)
          ElseIf m_document2.Focused Then
            m_document1.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift)
          ElseIf m_immediate.Focused Then
            m_document1.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift)
          Else
          End If
        End If

      Else
        cursorVisible = False

        Menu.ProcessKeys(keys, Me.CapsLock, isControl, isAlt, isShift)
      End If

    End If

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

        Dim cc = m_cursorCol
        Dim cr = m_cursorRow
        If m_document1.Focused Then
          cc = m_document1.CursorCol
          cr = m_document1.CursorRow
        ElseIf m_document2.Focused Then
          cc = m_document2.CursorCol
          cr = m_document2.CursorRow
        ElseIf m_immediate.Focused Then
          'cc = m_immediate.CursorCol
          'cr = m_immediate.CursorRow
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

    Return Not m_exit

  End Function

  Private Sub FocusDocument1()
    m_immediate.Focused = False
    m_document2.Focused = False
    m_document2.ScrollBars = False
    m_document1.Focused = True
    m_document1.ScrollBars = True
    m_cursorRow = m_document1.Row + 1
    m_cursorCol = m_document1.Col + 1
  End Sub

  Private Sub FocusDocument2()
    m_immediate.Focused = False
    m_document1.Focused = False
    m_document1.ScrollBars = False
    m_document2.Focused = True
    m_document2.ScrollBars = True
    m_cursorRow = m_document2.Row + 1
    m_cursorCol = m_document2.Col + 1
  End Sub

  Private Sub FocusImmediate()
    m_document1.Focused = False
    m_document1.ScrollBars = False
    m_document2.Focused = False
    m_document2.ScrollBars = False
    m_immediate.Focused = True
    m_cursorRow = m_immediate.Row + 1
    m_cursorCol = m_immediate.Col + 1
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
    If m_document1.Changed Then
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
    m_document1.Clear()
    m_document2.Clear()
    DrawScreen()
  End Function

  Private Sub OpenAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub SaveAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub SaveAsAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub PrintAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Async Function ExitActionAsync() As Task
    If m_document1.Changed Then
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
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub CopyAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub PasteAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ClearAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub NewSubAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub NewFunctionAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub TopicAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub UsingHelpAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ContentsAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub IndexAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub SyntaxCheckingAction()
    ' Toggle value
    Menu.Items(6).Items(2).Checked = Not Menu.Items(6).Items(2).Checked
    DrawScreen()
  End Sub

  Private Sub HelpPathAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub DisplayAction()
    m_context = New MessageDialog("Not implemented.")
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
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ContinueAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub RestartAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub StartAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ChangeAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub RepeatLastFindAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub FindAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub OutputScreenAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub SplitAction()
    Dim visible = m_document2.Visible
    If visible Then
      m_document2.Visible = False
      m_document1.Height += m_document2.Height - 1 '21
      If Not m_document1.Focused AndAlso Not m_immediate.Focused Then
        FocusDocument1()
      End If
    Else
      Dim h = m_document1.Height - 1
      Dim h2 = h \ 2
      Dim h1 = h - h2
      m_document1.Height = h1 + 1
      m_document2.Row = 2 + h1
      m_document2.Height = h2 + 1
      m_document2.ScrollBars = False
      m_document2.Visible = True
    End If
  End Sub

  Private Sub SubsAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub AboutAction()
    m_context = New AboutDialog()
  End Sub

#End Region

End Class
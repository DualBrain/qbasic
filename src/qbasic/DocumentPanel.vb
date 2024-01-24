Imports System.ComponentModel.Design
Imports QB.Video
Imports VbPixelGameEngine

Public Class DocumentPanel
  Inherits PgeX
  Implements IContext

  Private Structure BlkCoords                              ' Structure for Block coordinates
    Dim B As Integer                             ' Block coordinates
    Dim PB As Integer                            ' Previous block coordinates
    Dim P As Integer                             ' Paint coordinates
    Dim PP As Integer                            ' Previous paint coordinates
  End Structure

  Private Structure EditInfo

    Public Rows As Integer             ' Text Window Rows
    Public Wide As Integer             ' Text Window Columns
    'Public Wrap As Integer             ' Right Margin for Word Wrap (0 for No Wrap)
    Public HTab As Integer             ' Horizontal Tab spaces
    Public AColor As Integer           ' Window Color
    'Public Frame As Boolean            ' Display Frame Flag

    Public LSCol As Integer            ' Left Screen Column of editable window
    Public LC As Integer               ' Left Text Column
    Public CurCol As Integer           ' Current text column
    Public TSRow As Integer            ' Top Screen Row of editable window
    Public TL As Integer               ' Top Text Line
    Public CurLine As Integer          ' Current text Line

    ' Contains the "highlight"
    Public UlCRow As Integer           ' Upper Left Block Row
    Public UlCCol As Integer           ' Upper Left Block Column
    Public BrCRow As Integer           ' Lower Right Block Row
    Public BrCCol As Integer           ' Lower Right Block Column
    Public CBlock As Boolean           ' Column Block Flag

    Public WasMarked As Boolean        ' Flag that something has been marked
    Public DelBlock As Boolean         ' Flag to Delete highlighted block
    Public InsBlock As Boolean         ' Flag to Paste Buffer contents
    Public Text2Paste As Boolean       ' Flag that something is in the Paste buffer
    Public CopyBlock As Boolean        ' Flag to paste the buffer now

    Public Presses As Integer          ' Mouse presses.                Read Only!
    Public MRow As Integer             ' Mouse Screen Row.             Read Only!
    Public MCol As Integer             ' Mouse Screen Column.          Read Only!

    Public UnKnownKey As Boolean       ' Flag, True if key pressed but not handled
    Public Insert As Boolean          ' Insert State (False = Overtype, True = Insert)
    Public Changed As Boolean          ' Flag - (True means text was edited)
    Public LCount As Integer           ' Number of text lines.         Read Only!
    Public MErr As Integer             ' Error flag  (1 = Out of memory, 2 = Too many lines)

  End Structure

  Private Const NumPad As String = "12346789"              ' Shifted arrow keys
  Private Const NumPad2 As String = "stOPQKMGHK"           ' Unshifted arrow keys
  Private Const SkipTbl As String = " ^*()[]-=+,.<>/\"     ' Word delimiters
  Private Const ZERO As String = ChrW(0)
  Private Const ONE As String = ChrW(1)

  Public Property Title As String = "Untitled"
  Public Property Text As String

  Public Property Row As Integer = 2
  Public Property Col As Integer = 1
  Public Property Width As Integer = 80
  Public Property Height As Integer = 21

  Public Property ScrollBars As Boolean = True

  Public Property Focused As Boolean = True

  Public Property Visible As Boolean = True

  Public Sub New()

    m_action = 1

    m_ed.Insert = True
    m_ed.Rows = Height - 2
    m_ed.Wide = Width - 2
    'm_ed.Wrap = 0 ' No word wrap
    m_ed.HTab = 2
    m_ed.AColor = OneColor(8, 1)
    'm_ed.Frame = False
    m_ed.TSRow = 3
    m_ed.LSCol = 2
    Clear()

    'WrapWas = m_ed.Wrap
    m_ed.HTab = MaxInt(m_ed.HTab, 1)            ' Horizontal Tab spaces
    tMarg = 1
    bMarg = 1
    'insrt = m_ed.Insert
    'If Not insrt Then insrt = True             'Insert mode set to ON
    'make 1 to default to OT
    ctrlP = 32

    mouseThere = 0                           'Assume Mouse code absent
    blocksThere = 0                          'Assume Block code absent

    BScan = 12
    LOCATE(, , 0, BScan - 2, BScan)

    nPixLines = 8
    ScrRows = 25

    m_ed.TSRow = Row + 1 'CSRLIN()       'Calc the top line of window
    m_ed.LSCol = Col + 1 'POS(0)       'Calc Left margin of window
    'Both are to inside of frame
    m_ed.Rows = MinInt(MaxInt(m_ed.Rows, 3), ScrRows - CSRLIN() + 1)
    m_ed.Wide = MinInt(MaxInt(m_ed.Wide, 11), 80 - POS(0) + 1)

    If m_ed.LC <= 0 Then m_ed.LC = 1             'Set window column to 1
    If m_ed.CurCol <= 0 Then m_ed.CurCol = 1     'establish cursor column at 1
    If m_ed.TL <= 0 Then m_ed.TL = 1             'Top of window row number
    If m_ed.CurLine <= 0 Then m_ed.CurLine = 1   'make top line the cursor col.
    WindowLimits()

  End Sub

  Public ReadOnly Property Changed As Boolean
    Get
      Return m_ed.Changed
    End Get
  End Property

  Public ReadOnly Property CursorRow As Integer
    Get
      Return m_ed.TSRow + m_ed.CurLine - m_ed.TL
    End Get
  End Property

  Public ReadOnly Property CursorCol As Integer
    Get
      Return m_ed.LSCol + (m_ed.CurCol - m_ed.LC)
    End Get
  End Property

  Public ReadOnly Property DocumentRow As Integer
    Get
      Return m_ed.CurLine
    End Get
  End Property

  Public ReadOnly Property DocumentCol As Integer
    Get
      Return m_ed.CurCol
    End Get
  End Property

  Public Sub Render() Implements IContext.Render

    If Not Visible Then Exit Sub

    Dim lrRow = Row + Height - 1
    Dim lrCol = Col + Width - 1

    ' Box
    Box0(Row, Col, Row + Height - 1, lrCol, 1, OneColor(8, 1))

    If Row > 2 Then
      HLine(Row, Col, lrCol, 1, OneColor(8, 1))
    End If

    ' Content Area
    ClearScr0(Row + 1, Col + 1, lrRow - 1, lrCol - 1, OneColor(8, 1))

    If m_action = 3 Then
      'Dim ky As String
      'QEdit(m_document, ky, m_action, m_ed)
      LOCATE(m_ed.TSRow, m_ed.LSCol, 0)
      Call HideCursor()
      Call APrint0(m_document.ToArray,
                   m_ed.TL - 1,
                   m_ed.Rows,
                   m_ed.LC,
                   m_ed.Wide,
                   m_ed.AColor)
      LOCATE(m_ed.TSRow + m_ed.CurLine - m_ed.TL, m_ed.LSCol + (m_ed.CurCol - m_ed.LC), 1)
      Call ShowCursor()
    End If

    If Me.Text IsNot Nothing Then
      Dim lines = Me.Text.Split(vbLf)
      For index = 0 To lines.Length - 1
        QPrintRC(lines(index), Row + 1 + index, Col + 1, OneColor(8, 1))
      Next
    End If

    ' Expand/Collapse Tool
    QPrintRC(ChrW(180), Row, lrCol - 4, OneColor(8, 1))
    QPrintRC(ChrW(24), Row, lrCol - 3, OneColor(1, 8))
    QPrintRC(ChrW(195), Row, lrCol - 2, OneColor(8, 1))

    ' Scrollbars
    If ScrollBars Then
      'TODO: Determine current position within the scrollbars...
      If Height > 5 Then VScrollBar(Row + 1, lrCol, lrRow - 2)
      If Height > 3 Then HScrollBar(lrRow - 1, Col + 1, lrCol - 1)
    End If

    ' Title
    Dim title = $" {Me.Title} "
    Dim titleOffset = (Width - title.Length) \ 2
    QPrintRC(title, Row, Col + titleOffset, OneColor(1, 8))

  End Sub

  Public Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys
    Editor(keys, capsLock, ctrl, alt, shift)
    Return True
  End Function

  Private m_action As Integer = 1
  Private ReadOnly m_document As New List(Of String)
  Private m_ed As EditInfo

  Public Sub Clear()
    m_document.Clear()
    m_document.Add("")
    m_ed.CurLine = 1
    m_ed.CurCol = 1
  End Sub

  Private reprint As Boolean
  'Private insrt As Boolean
  Private WrapWas As Integer

  Private BScan, ScrRows As Integer
  Private N As Integer
  Private arrayEnd, HiClr, X, OldVrtPtr, OldHorPtr, LastCurLine, LastCurCol As Integer
  Private B, Extra, I, Lin, LinLen, P, VrtPtr, HorPtr As Integer
  Private CurLine, CurCol, par, tempS As String

  Private clipBd() As Integer
  Private LineBuf As String
  Private ScrBuf() As UShort
  Private Ruler() As String
  Private WC(3, 1) As Integer
  Private Hlp() As String
  Private HelpScr() As UShort

  ' ----

  Dim clipBd1, clipBd2, rightSide As String
  Dim delay, lmTime, mTime As Single
  Dim ok As Boolean
  Dim bBytes, clipBytes As Long
  Dim sz As Integer
  Dim saveT, saveC, lmRow, lmCol, scrVrtPtr, scrHorPtr, numRows, numCols, free As Integer
  Dim frstBlkKey, blkRow, blkCol, lpCol, rpCol, wasPressed, mouseAct As Integer
  Dim bMarg, ctrlP, blkClr, lineWas, lineEd, markBlock, lastC As Integer
  Dim y As Integer
  'Dim scrnRows As integer

  Dim colBlock As Boolean
  Dim mouseThere As Integer
  Dim blocksThere As Integer
  Dim nPixLines As Integer
  Dim tMarg As Integer

  Private Sub Editor(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean)

    If m_action < 3 OrElse m_action = 4 Then

      '----- Find the last element being used in the text array

      arrayEnd = m_document.Count - 1
      m_ed.LCount = FindLast(m_document.ToArray, arrayEnd, arrayEnd)

      'If m_ed.Wrap <> 0 Then                              'If word wrap is on, use "¶" paragraph marker
      '  par = ChrW(20)                          ' (you could use CHR$(255) for invisible marker)
      '  m_ed.Wrap = MaxInt(MinInt(m_ed.Wrap, 255), 10)
      'Else                                              'Otherwise,
      par = ""                                        '  use a null character
      'End If

      '----- If word wrap is on, format all the text.

      'If m_ed.Wrap <> 0 Then
      '  Dim wrapLine = 1
      '  If m_action < 3 Then
      '    wrapLine = 1
      '    WrapAll(m_document, m_ed, wrapLine)
      '  Else
      '    wrapLine = m_ed.CurLine
      '    WrapUp(m_document, m_ed, wrapLine)
      '    WrapDown(m_document, m_ed, wrapLine)
      '  End If
      '  WrapWas = m_ed.Wrap
      'End If

      '----- Insert margin markers in ruler line.

      HiClr = m_ed.AColor                                 'Determine high intensity Clr
      Dim temp As Integer
      Call SplitColor(m_ed.AColor, X, temp)   'Split color # into Fg & Bg
      If X < 8 Then HiClr = m_ed.AColor + 8
      If m_ed.AColor = 112 Then
        blkClr = 7
      Else
        blkClr = 112
      End If

      InitWindow()                                    'Display the window
      CsrSize()                                         'Set the cursor size

    End If

    'LOCATE(m_ed.TSRow + m_ed.CurLine - m_ed.TL, m_ed.LSCol + (m_ed.CurCol - m_ed.LC), 1)

    m_ed.UnKnownKey = False
    lineWas = m_ed.CurLine

    ' Handle keyboard/mouse
    If keys IsNot Nothing Then
      For Each key In keys
        If ctrl AndAlso Not alt AndAlso Not shift Then
          Select Case key
            Case ConsoleKey.Home
              m_ed.CurLine = 1 : m_ed.CurCol = 1
            Case ConsoleKey.End
              m_ed.CurLine = m_document.Count + 1
              'm_ed.CurCol = m_document(m_ed.CurLine - 1).Length + 1
            Case Else
          End Select
        End If
        If Not ctrl AndAlso Not alt AndAlso shift Then
          Select Case key
            Case ConsoleKey.LeftArrow
            Case ConsoleKey.RightArrow
            Case ConsoleKey.UpArrow
            Case ConsoleKey.DownArrow
            Case ConsoleKey.Home
            Case ConsoleKey.End
            Case ConsoleKey.PageUp
            Case ConsoleKey.PageDown
            Case ConsoleKey.Insert
            Case ConsoleKey.Delete
            Case Else
          End Select
        End If
        If Not ctrl AndAlso Not alt Then ' shift can be in either state
          Select Case key

            Case ConsoleKey.A To ConsoleKey.Z,
                 ConsoleKey.Spacebar,
                 ConsoleKey.D0 To ConsoleKey.D9,
                 ConsoleKey.Multiply, ConsoleKey.Divide, ConsoleKey.Add, ConsoleKey.Subtract,
                 ConsoleKey.Oem1, ConsoleKey.Oem2, ConsoleKey.Oem3, ConsoleKey.Oem4, ConsoleKey.Oem5, ConsoleKey.Oem6, ConsoleKey.Oem7,
                 ConsoleKey.OemPlus, ConsoleKey.OemMinus,
                 ConsoleKey.OemComma, ConsoleKey.OemPeriod

              If m_ed.Insert Then
                If m_ed.CurCol <= m_document(m_ed.CurLine - 1).Length Then
                  Dim leftSide = If(m_ed.CurCol > 0, m_document(m_ed.CurLine - 1).Substring(0, m_ed.CurCol - 1), "")
                  Dim rightSide = If(m_ed.CurCol <= m_document(m_ed.CurLine - 1).Length, m_document(m_ed.CurLine - 1).Substring(m_ed.CurCol - 1), "")
                  m_document(m_ed.CurLine - 1) = leftSide & GetChar(key, capsLock, shift) & rightSide
                Else
                  m_document(m_ed.CurLine - 1) &= GetChar(key, capsLock, shift)
                End If
                m_ed.CurCol += 1
                m_ed.Changed = True
              Else
                Stop
              End If

            Case ConsoleKey.Tab
              If m_ed.CurCol <= m_document(m_ed.CurLine - 1).Length Then
                Dim leftSide = If(m_ed.CurCol > 0, m_document(m_ed.CurLine - 1).Substring(0, m_ed.CurCol - 1), "")
                Dim rightSide = If(m_ed.CurCol <= m_document(m_ed.CurLine - 1).Length, m_document(m_ed.CurLine - 1).Substring(m_ed.CurCol - 1), "")
                m_document(m_ed.CurLine - 1) = leftSide & Space(m_ed.HTab) & rightSide
                m_ed.Changed = True
              End If
              m_ed.CurCol += m_ed.HTab

            Case ConsoleKey.LeftArrow
              If m_ed.CurCol > 1 Then m_ed.CurCol -= 1

            Case ConsoleKey.RightArrow
              'TODO: Need to handle scrolling within window...
              m_ed.CurCol += 1

            Case ConsoleKey.UpArrow
              If m_ed.CurLine > 1 Then m_ed.CurLine -= 1

            Case ConsoleKey.DownArrow
              'TODO: Need to handle scrolling within window...
              m_ed.CurLine += 1

            Case ConsoleKey.Home
              m_ed.CurCol = 1

            Case ConsoleKey.End
              If m_ed.CurLine <= m_document.Count Then
                m_ed.CurCol = m_document(m_ed.CurLine - 1).Length + 1
              Else
                m_ed.CurCol = 1
              End If

            Case ConsoleKey.PageUp
            Case ConsoleKey.PageDown

            Case ConsoleKey.Insert
              m_ed.Insert = Not m_ed.Insert
              CsrSize()

            Case ConsoleKey.Backspace
              If m_ed.CurCol = 1 Then
                If m_ed.CurLine > 1 AndAlso m_ed.CurLine <= m_document.Count Then
                  Dim l = m_document(m_ed.CurLine - 2).Length
                  m_document(m_ed.CurLine - 2) = m_document(m_ed.CurLine - 2) + LTrim(m_document(m_ed.CurLine - 1))
                  m_document.RemoveAt(m_ed.CurLine - 1)
                  m_ed.CurLine -= 1 : m_ed.CurCol = l + 1
                  m_ed.Changed = True
                End If
              Else
                If m_ed.CurCol <= m_document(m_ed.CurLine - 1).Length + 1 Then
                  Dim leftSide = If(m_ed.CurCol > 1, m_document(m_ed.CurLine - 1).Substring(0, m_ed.CurCol - 2), "")
                  Dim rightSide = If(m_ed.CurCol <= m_document(m_ed.CurLine - 1).Length, m_document(m_ed.CurLine - 1).Substring(m_ed.CurCol - 1), "")
                  m_document(m_ed.CurLine - 1) = leftSide & rightSide
                  m_ed.Changed = True
                End If
                m_ed.CurCol -= 1
              End If

            Case ConsoleKey.Delete
              If m_ed.CurCol > m_document(m_ed.CurLine - 1).Length Then
                If m_ed.CurLine < m_document.Count Then
                  m_document(m_ed.CurLine - 1) = m_document(m_ed.CurLine - 1) + LTrim(m_document(m_ed.CurLine))
                  m_document.RemoveAt(m_ed.CurLine)
                  m_ed.Changed = True
                End If
              Else
                m_document(m_ed.CurLine - 1) = Left(m_document(m_ed.CurLine - 1), m_ed.CurCol - 1) + Mid(m_document(m_ed.CurLine - 1), m_ed.CurCol + 1)
                m_ed.Changed = True
              End If

            Case ConsoleKey.Enter
              If m_ed.CurLine >= m_document.Count Then
                m_document.Add("")
              Else
                m_document.Insert(If(m_ed.CurCol = 1, m_ed.CurLine - 1, m_ed.CurLine), "")
              End If
              If m_ed.CurCol > 1 AndAlso m_ed.CurCol <= m_document(m_ed.CurLine - 1).Length Then
                m_document(m_ed.CurLine) = m_document(m_ed.CurLine - 1).Substring(m_ed.CurCol - 1)
                m_document(m_ed.CurLine - 1) = m_document(m_ed.CurLine - 1).Substring(0, m_ed.CurCol - 1)
              End If
              m_ed.CurLine += 1 : m_ed.CurCol = 1
              m_ed.Changed = True

            Case ConsoleKey.Escape
            Case Else
          End Select
        End If
      Next
    End If

    If lineEd <> 0 AndAlso m_ed.CurLine <> lineWas Then
      m_document(lineWas) = RTrim(m_document(lineWas))
      lineEd = 0
    End If

    If m_action = 1 OrElse m_action = 2 OrElse m_action = 4 Then
      m_action = 3                                      'Reset Action code to 3 (idle)
    End If

  End Sub

  Private Shared Function GetChar(key As ConsoleKey, capsLock As Boolean, shift As Boolean) As Char
    Dim result = "?"c
    Select Case key

      Case ConsoleKey.Oem1 ' ;: (186)
        If shift Then result = ":"c Else result = ";"c
      Case ConsoleKey.OemPlus ' =+ (187)
        If shift Then result = "+"c Else result = "="c
      Case ConsoleKey.OemComma ' ,< (188)
        If shift Then result = "<"c Else result = ","c
      Case ConsoleKey.OemMinus ' -_ (189)
        If shift Then result = "_"c Else result = "-"c
      Case ConsoleKey.OemPeriod ' .> (190)
        If shift Then result = ">"c Else result = "."c
      Case ConsoleKey.Oem2 ' /?
        If shift Then result = "?"c Else result = "/"c
      Case ConsoleKey.Oem3 ' `~
        If shift Then result = "~"c Else result = "`"c
      Case ConsoleKey.Oem4 ' [{ (219)
        If shift Then result = "{"c Else result = "["c
      Case ConsoleKey.Oem5 ' \| (220)
        If shift Then result = "|"c Else result = "\"c
      Case ConsoleKey.Oem6 ' ]} (221)
        If shift Then result = "}"c Else result = "]"c
      Case ConsoleKey.Oem7 ' '" (222)
        If shift Then result = """"c Else result = "'"c

      Case ConsoleKey.Spacebar
        result = " "c

      Case ConsoleKey.A To ConsoleKey.Z

        result = ChrW(key)
        If capsLock Then
          If shift Then result = CChar($"{result}".ToLower)
        Else
          If Not shift Then result = CChar($"{result}".ToLower)
        End If

      Case ConsoleKey.Add : Return "+"c
      Case ConsoleKey.Multiply : Return "*"c
      Case ConsoleKey.Divide : Return "/"c
      Case ConsoleKey.Subtract : Return "-"c

      Case ConsoleKey.D1 : If shift Then Return "!"c Else Return "1"c
      Case ConsoleKey.D2 : If shift Then Return "@"c Else Return "2"c
      Case ConsoleKey.D3 : If shift Then Return "#"c Else Return "3"c
      Case ConsoleKey.D4 : If shift Then Return "$"c Else Return "4"c
      Case ConsoleKey.D5 : If shift Then Return "%"c Else Return "5"c
      Case ConsoleKey.D6 : If shift Then Return "^"c Else Return "6"c
      Case ConsoleKey.D7 : If shift Then Return "&"c Else Return "7"c
      Case ConsoleKey.D8 : If shift Then Return "*"c Else Return "8"c
      Case ConsoleKey.D9 : If shift Then Return "("c Else Return "9"c
      Case ConsoleKey.D0 : If shift Then Return ")"c Else Return "0"c

      Case Else
        Stop
    End Select
    Return result
  End Function

  'Private Sub QEdit(array As List(Of String), ByRef ky As String, ByRef action As Integer, ByRef ed As EditInfo)

  '  Const MOUSE_KEY As String = ZERO + Chr(3)              'Dummy key for mouse actions
  '  Const MOUSE_KEY_2 As String = ZERO + Chr(4)

  '  ed.MErr = 0                         ' No errors yet

  '  Dim Bl(3) As BlkCoords

  '  '----- Action of 0 or 1 - Save the underlying screen
  '  If action < 2 Then

  '    WrapWas = ed.Wrap
  '    ed.HTab = MaxInt(ed.HTab, 1)            ' Horizontal Tab spaces
  '    tMarg = 1
  '    bMarg = 1
  '    Insrt = ed.InsStat
  '    If Insrt = 0 Then Insrt = -1             'Insert mode set to ON
  '    'make 1 to default to OT
  '    ctrlP = 32

  '    CurLine$ = Space(4)                     'Used to display Cursor Pos.
  '    CurCol$ = Space(3)

  '    '----- Compose the Ruler line string
  '    ReDim Ruler(0 To 1) 'As String * 260      'Put it out in far memory
  '    Ruler(0) = Space(260)
  '    Ruler(1) = Space(260)

  '    For N = 0 To 250 Step 10                 'Compose Ruler line
  '      MID$(Ruler(0), N + 1) = Mid(Str(N Mod 100), 2, 1)
  '      For X = 1 To 9
  '        If X = 5 Then
  '          MID$(Ruler(0), N + X + 1) = ChrW(197)
  '        Else
  '          MID$(Ruler(0), N + X + 1) = ChrW(194)
  '        End If
  '      Next
  '    Next

  '    mouseThere = 0                           'Assume Mouse code absent
  '    blocksThere = 0                          'Assume Block code absent


  '    '----- Determine bottom cursor scan line.
  '    Select Case Monitor%()                     'Determine the monitor type
  '      Case 3, 5, 10                         'CGA, EGA monitors
  '        BScan = 7                          'Set bottom scan line
  '      Case Else                             'All other monitors
  '        BScan = 12
  '    End Select
  '    LOCATE(, , 0, BScan - 1, BScan)

  '    nPixLines = 8
  '    Call GetVMode(0, 0, 0, ScrRows, 80)      'Get current line mode
  '    'Fix for OS2 DOS
  '    If ScrRows = 50 AndAlso DOSVer%() = 1000 Then nPixLines = 7

  '    ed.TSRow = CSRLIN() - If(ed.Frame, -1, 0)      'Calc the top line of window
  '    ed.LSCol = POS(0) - If(ed.Frame, -1, 0)      'Calc Left margin of window
  '    'Both are to inside of frame
  '    ed.Rows = MinInt%(MaxInt%(ed.Rows, 3), (ScrRows + (If(ed.Frame, -1, 0) * 2)) - CSRLIN() + 1)
  '    ed.Wide = MinInt%(MaxInt%(ed.Wide, 11), (80 + (If(ed.Frame, -1, 0) * 2)) - POS(0) + 1)

  '    '----- Save current window coordinates for later resizing
  '    Dim WC(3, 1) As Integer
  '    WC(0, 1) = ed.TSRow + CInt(ed.Frame)    'Top line
  '    WC(1, 1) = ed.LSCol + CInt(ed.Frame)    'Left margin
  '    WC(2, 1) = WC(0, 1) + ed.Rows + (2 * CInt(ed.Frame)) - 1 'Bottom line of window
  '    WC(3, 1) = WC(1, 1) + ed.Wide + (2 * CInt(ed.Frame)) - 1 'Right margin of window

  '    '----- Dim array to hold the underlying screen and save it.
  '    'ReDim ScrBuf(1 To (ScrRows * 80) + 2)
  '    ReDim ScrBuf(0 To ((ScrRows * 80) + 2) - 1)
  '    Call MScrnSave(1, 1, ScrRows, 80, ScrBuf)

  '    If ed.LC <= 0 Then ed.LC = 1             'Set window column to 1
  '    If ed.CurCol <= 0 Then ed.CurCol = 1     'establish cursor column at 1
  '    If ed.TL <= 0 Then ed.TL = 1             'Top of window row number
  '    If ed.CurLine <= 0 Then ed.CurLine = 1   'make top line the cursor col.
  '    WindowLimits(ed)

  '  End If

  '  '----- Action of 5 - Restore the underlying screen
  '  If action = 5 Then
  '    CleanUp()                            'Restore the screen
  '    action = 1                               'Reset action to 1
  '    Exit Sub                                 'Bail out
  '  End If

  '  '----- Action of 0, 1, 2 - Display the edit window

  '  If action < 3 OrElse action = 4 Then

  '    '----- Find the last element being used in the text array

  '    arrayEnd = array.Count - 1
  '    ed.LCount = FindLast(array.ToArray, arrayEnd, arrayEnd)

  '    If ed.Wrap <> 0 Then                              'If word wrap is on, use "¶" paragraph marker
  '      par = ChrW(20)                          ' (you could use CHR$(255) for invisible marker)
  '      ed.Wrap = MaxInt(MinInt(ed.Wrap, 255), 10)
  '    Else                                              'Otherwise,
  '      par = ""                                        '  use a null character
  '    End If

  '    '----- If word wrap is on, format all the text.

  '    If ed.Wrap <> 0 Then
  '      WrapLine = 1
  '      If action < 3 Then
  '        WrapLine = 1
  '        WrapAll(array, ed)
  '      Else
  '        WrapLine = ed.CurLine
  '        WrapUp(array, ed)
  '        WrapDown(array, ed)
  '      End If
  '      WrapWas = ed.Wrap
  '    End If

  '    '----- Insert margin markers in ruler line.

  '    Ruler(1) = Ruler(0)
  '    If ed.Wrap <> 0 Then
  '      MID$(Ruler(1), 2) = Chr(16)
  '      MID$(Ruler(1), ed.Wrap + 1) = Chr(17)
  '    End If

  '    HiClr = ed.AColor                                 'Determine high intensity Clr
  '    Call SplitColor(ed.AColor, X, Temp)   'Split color # into Fg & Bg
  '    If X < 8 Then HiClr = ed.AColor + 8
  '    If ed.AColor = 112 Then
  '      blkClr = 7
  '    Else
  '      blkClr = 112
  '    End If

  '    InitWindow(ed)                                    'Display the window
  '    CsrSize()                                         'Set the cursor size

  '  End If

  '  '----- Main editing (key processing) loop
  '  Do

  '    '----- Position the cursor
  '    LOCATE(ed.TSRow + ed.CurLine - ed.TL, ed.LSCol + (ed.CurCol - ed.LC), 1)

  '    ed.UnKnownKey = False
  '    lineWas = ed.CurLine

  '    '----- Handle regular keys

  '    Dim isCtrl As Boolean

  '    If True Then

  '      If m_keys IsNot Nothing Then
  '        For Each key In m_keys

  '          If isCtrl Then
  '            Select Case key
  '              Case ConsoleKey.Y ' ----- Ctrl Y - Delete a Line
  '                If ed.CurLine <= ed.LCount Then 'Disallow if we're past the last line
  '                  'Calc the screen line number
  '                  X = ed.TSRow + ed.CurLine - ed.TL
  '                  'Paint the line before we delete it  just for effect
  '                  Call MPaintBox(X, ed.LSCol, X, ed.LSCol + ed.Wide - 1, blkClr)
  '                  Temp = ed.CurLine                   'Delete the current line
  '                  DeleteLine(array, ed)
  '                  Pause(1)                            'Wait an 1/18 of a second
  '                  reprint = -1                        'Set flag to re-print window
  '                  If ed.LCount <> 0 Then ed.Changed = True
  '                End If
  '              Case ConsoleKey.N '----- Ctrl N - Insert a Line
  '                Temp = ed.CurLine
  '                tempS = ""
  '                InsertLine(array, ed)
  '                reprint = -1                          'Set flag to re-print window
  '                If ed.LCount <> 0 Then ed.Changed = True
  '              Case ConsoleKey.P '----- Ctrl P - Embed characters
  '                ctrlP = 0
  '                lineEd = -1
  '              Case Else
  '                ed.UnKnownKey = True
  '            End Select
  '          Else

  '            Select Case key
  '              Case ConsoleKey.Backspace
  '                If ed.CurCol > 1 Then           'If not at column 1,
  '                  ed.CurCol -= 1                '  decrement the cursor posit.
  '                  If Insrt = -1 Then            'If in Insert mode, drag the line 1 char. left
  '                    array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1) + Mid(array(ed.CurLine), ed.CurCol + 1)
  '                    If Len(array(ed.CurLine)) <> 0 Then ed.Changed = True
  '                    If ed.Wrap <> 0 Then
  '                      WrapLine = ed.CurLine
  '                      WrapUp(array, ed)         'Wrap words up
  '                    End If
  '                  ElseIf ed.CurCol = QPLen(array(ed.CurLine)) Then 'Not in Insert mode, blank char
  '                    array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1)
  '                    ed.Changed = True
  '                  ElseIf ed.CurCol < QPLen(array(ed.CurLine)) Then
  '                    MID$(array(ed.CurLine), ed.CurCol, 1) = " "
  '                    ed.Changed = True
  '                    lineEd = -1
  '                  End If                        'Put edited line in a buffer
  '                  If ed.CurCol < ed.LC Then     'If we're at left window col,
  '                    ed.LC -= 1                  '  decrement window column
  '                    reprint = -1
  '                  Else
  '                    REM LSet LineBuf$ = Mid(Array$(Ed.CurLine), Ed.LC)
  '                    LOCATE(, ed.LSCol, 0)        'Print the edited line
  '                    Call MQPrint(LineBuf$, ed.AColor)
  '                  End If
  '                ElseIf ed.CurLine > 1 Then 'If in Insert mode, go up
  '                  Temp = FnSpaces2Pad%(array(ed.CurLine - 1))
  '                  'Copy line to line above
  '                  ed.CurCol = QPLen%(array(ed.CurLine - 1)) + Temp + 1
  '                  array(ed.CurLine - 1) = array(ed.CurLine - 1) + Space(Temp) + array(ed.CurLine)
  '                  'Update window column
  '                  ed.LC = MaxInt%(ed.CurCol - ed.Wide + 1, ed.LC)
  '                  Temp = ed.CurLine             'Delete the current line
  '                  DeleteLine(array, ed)
  '                  ed.CurLine -= 1
  '                  CursorUp(array, ed)           'Back up a line
  '                  reprint = -1                  'Set flag to re-print window
  '                  If ed.Wrap <> 0 Then
  '                    WrapLine = ed.CurLine
  '                    WrapDown(array, ed)         'Wrap words down
  '                  End If
  '                  ed.Changed = True
  '                End If
  '              Case ConsoleKey.Enter
  '                If ed.CurLine < arrayEnd Then
  '                  X = Blanks%(array(ed.CurLine)) + 1
  '                  If Insrt = -1 Then 'If in Insert mode, insert a new line below.
  '                    If ed.LCount < arrayEnd Then
  '                      'New line is right of cursor
  '                      tempS = Space(MinInt%(X, ed.CurCol) - 1) + RTrim(Mid(array(ed.CurLine), ed.CurCol))
  '                      Temp = ed.CurLine + 1
  '                      InsertLine(array, ed)
  '                      'Current line is left of cursor & paragraph marker
  '                      array(ed.CurLine) = RTrim(Left(array(ed.CurLine), ed.CurCol - 1))
  '                      ed.Changed = True
  '                    End If
  '                  End If
  '                  If X > 1 OrElse QPLen%(array(ed.CurLine)) <> 0 Then ed.CurCol = X
  '                  ed.CurLine = MinInt%(ed.CurLine + 1, arrayEnd)
  '                  WindowLimits(ed)
  '                  reprint = -1                'Set flag to re-print window
  '                End If
  '              Case ConsoleKey.Tab
  '                'Make current column "Ed.HTab"
  '                '  to the right
  '                N = ((ed.CurCol + ed.HTab - 1) \ ed.HTab) * ed.HTab + 1
  '                N = MinInt%(N, 255 - ed.HTab)
  '                If N <= ed.Wrap Or ed.Wrap = 0 Then
  '                  'If in Insert mode
  '                  If Insrt = -1 And ed.CurCol < QPLen%(array(ed.CurLine)) Then
  '                    '  insert "Ed.HTab" spaces
  '                    array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1) + Space(N - ed.CurCol) + Mid(array(ed.CurLine), ed.CurCol)
  '                    If ed.Wrap <> 0 Then
  '                      WrapLine = ed.CurLine
  '                      WrapDown(array, ed)        'Wrap words down
  '                    End If
  '                    ed.Changed = True
  '                  End If
  '                  ed.CurCol = N
  '                  'Is it off the right of window?
  '                  If ed.CurCol > ed.LC + ed.Wide - 1 Then
  '                    ed.LC += ed.HTab  'Make it "Ed.HTab" to the right
  '                    reprint = -1             'Set flag to re-print window
  '                  Else                        'Move to the left margin
  '                    LOCATE(, ed.LSCol, 0)
  '                    'Put new line in buffer
  '                    REM LSet LineBuf$ = Mid(Array$(Ed.CurLine), Ed.LC)
  '                    'Print the buffer
  '                    Call MQPrint(LineBuf$, ed.AColor)
  '                  End If
  '                End If
  '              Case ConsoleKey.Escape
  '                If action = 0 Then            'If not using Action flag,
  '                  CleanUp()                   '  restore the screen and Clean up memory
  '                  Exit Do                     ' see ya later oscillator
  '                Else
  '                  ed.UnKnownKey = True
  '                End If
  '              Case ConsoleKey.Spacebar,
  '                   ConsoleKey.A To ConsoleKey.Z,
  '                   ConsoleKey.D0 To ConsoleKey.D9,
  '                   ConsoleKey.OemPlus To ConsoleKey.OemPeriod

  '                X = QPLen%(array(ed.CurLine)) 'See if line is long enough
  '                Select Case key
  '                  Case ConsoleKey.Spacebar,
  '                       ConsoleKey.A To ConsoleKey.Z,
  '                       ConsoleKey.D0 To ConsoleKey.D9
  '                    ky = ChrW(key)
  '                  Case ConsoleKey.OemPeriod : ky = "."
  '                  Case ConsoleKey.OemPlus : ky = "+"
  '                  Case ConsoleKey.OemComma : ky = ","
  '                  Case ConsoleKey.OemMinus : ky = "-"
  '                  Case Else
  '                    ky = "?"
  '                End Select
  '                If X < 256 And ed.CurCol < 256 Then
  '                  If ed.CurLine < arrayEnd Or ed.Wrap = 0 Or X < ed.Wrap Then
  '                    If ed.CurCol > X Then    'Make space for new character
  '                      array(ed.CurLine) = array(ed.CurLine) + Space(ed.CurCol - X)
  '                    End If
  '                    'Update line count if past
  '                    ed.LCount = MaxInt%(ed.LCount, ed.CurLine) ' end
  '                    'Insert character in line
  '                    If Insrt = -1 And ed.CurCol <= X Then
  '                      array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1) + ky + Mid(array(ed.CurLine), ed.CurCol)
  '                      Call MQPrint(Mid(array(ed.CurLine), ed.CurCol, ed.Wide - (ed.CurCol - ed.LC)), ed.AColor)
  '                      If ed.Wrap <> 0 Then
  '                        WrapLine = ed.CurLine
  '                        WrapDown(array, ed)
  '                      End If
  '                    Else                     'Concat character to end
  '                      MID$(array(ed.CurLine), ed.CurCol, 1) = ky
  '                      Call MQPrint(ky, ed.AColor)  'Print the character
  '                      If ed.Wrap <> 0 AndAlso ed.CurCol > ed.Wrap Then
  '                        WrapLine = ed.CurLine
  '                        WrapDown(array, ed)
  '                      End If
  '                    End If

  '                    ed.CurCol += 1 'Update the current column
  '                    WindowLimits(ed)
  '                    ed.Changed = True          'We just changed the text
  '                    lineEd = -1
  '                  End If
  '                End If

  '                ctrlP = 32

  '              Case Else
  '                ed.UnKnownKey = True
  '            End Select

  '          End If

  '        Next
  '      End If

  '    Else

  '      If QPLen%(ky) = 1 Then

  '        Select Case ASCII%(ky)

  '            '----- All Non Control Characters
  '          Case Is >= ctrlP

  '            X = QPLen%(array(ed.CurLine)) 'See if line is long enough
  '            If X < 256 And ed.CurCol < 256 Then
  '              If ed.CurLine < arrayEnd Or ed.Wrap = 0 Or X < ed.Wrap Then
  '                If ed.CurCol > X Then    'Make space for new character
  '                  array(ed.CurLine) = array(ed.CurLine) + Space(ed.CurCol - X)
  '                End If
  '                'Update line count if past
  '                ed.LCount = MaxInt%(ed.LCount, ed.CurLine) ' end
  '                'Insert character in line
  '                If Insrt = -1 And ed.CurCol <= X Then
  '                  array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1) + ky + Mid(array(ed.CurLine), ed.CurCol)
  '                  Call MQPrint(Mid(array(ed.CurLine), ed.CurCol, ed.Wide - (ed.CurCol - ed.LC)), ed.AColor)
  '                  If ed.Wrap <> 0 Then
  '                    WrapLine = ed.CurLine
  '                    WrapDown(array, ed)
  '                  End If
  '                Else                     'Concat character to end
  '                  MID$(array(ed.CurLine), ed.CurCol, 1) = ky
  '                  Call MQPrint(ky, ed.AColor)  'Print the character
  '                  If ed.Wrap <> 0 AndAlso ed.CurCol > ed.Wrap Then
  '                    WrapLine = ed.CurLine
  '                    WrapDown(array, ed)
  '                  End If
  '                End If

  '                ed.CurCol += 1 'Update the current column
  '                WindowLimits(ed)
  '                ed.Changed = True          'We just changed the text
  '                lineEd = -1
  '              End If
  '            End If

  '            ctrlP = 32


  '            '----- Back Space
  '          Case 8
  '            If ed.CurCol > 1 Then          'If not at column 1,
  '              ed.CurCol -= 1   '  decrement the cursor posit.
  '              If Insrt = -1 Then          'If in Insert mode, drag the
  '                '  line 1 char. left
  '                array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1) + Mid(array(ed.CurLine), ed.CurCol + 1)
  '                If Len(array(ed.CurLine)) <> 0 Then ed.Changed = True
  '                If ed.Wrap <> 0 Then
  '                  WrapLine = ed.CurLine
  '                  WrapUp(array, ed)          'Wrap words up
  '                End If
  '                'Not in Insert mode, blank char
  '              ElseIf ed.CurCol = QPLen%(array(ed.CurLine)) Then
  '                array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1)
  '                ed.Changed = True
  '              ElseIf ed.CurCol < QPLen%(array(ed.CurLine)) Then
  '                MID$(array(ed.CurLine), ed.CurCol, 1) = " "
  '                ed.Changed = True
  '                lineEd = -1
  '              End If                      'Put edited line in a buffer
  '              If ed.CurCol < ed.LC Then  'If we're at left window col,
  '                ed.LC -= 1        '  decrement window column
  '                reprint = -1
  '              Else
  '                REM LSet LineBuf$ = Mid(Array$(Ed.CurLine), Ed.LC)
  '                LOCATE(, ed.LSCol, 0)        'Print the edited line
  '                Call MQPrint(LineBuf$, ed.AColor)
  '              End If
  '              'If in Insert mode, go up
  '            ElseIf ed.CurLine > 1 Then

  '              Temp = FnSpaces2Pad%(array(ed.CurLine - 1))
  '              'Copy line to line above
  '              ed.CurCol = QPLen%(array(ed.CurLine - 1)) + Temp + 1
  '              array(ed.CurLine - 1) = array(ed.CurLine - 1) + Space(Temp) + array(ed.CurLine)
  '              'Update window column
  '              ed.LC = MaxInt%(ed.CurCol - ed.Wide + 1, ed.LC)

  '              Temp = ed.CurLine           'Delete the current line
  '              DeleteLine(array, ed)

  '              ed.CurLine -= 1
  '              CursorUp(array, ed)              'Back up a line
  '              reprint = -1                'Set flag to re-print window
  '              If ed.Wrap <> 0 Then
  '                WrapLine = ed.CurLine
  '                WrapDown(array, ed)           'Wrap words down
  '              End If
  '              ed.Changed = True
  '            End If

  '            '----- Enter
  '          Case 13
  '            If ed.CurLine < arrayEnd Then

  '              X = Blanks%(array(ed.CurLine)) + 1
  '              'If in Insert mode, insert a
  '              If Insrt = -1 Then
  '                '  new line below.
  '                If ed.LCount < arrayEnd Then
  '                  'New line is right of cursor
  '                  tempS = Space(MinInt%(X, ed.CurCol) - 1) + RTrim(Mid(array(ed.CurLine), ed.CurCol))
  '                  Temp = ed.CurLine + 1
  '                  InsertLine(array, ed)
  '                  'Current line is left of
  '                  '  cursor & paragraph marker
  '                  array(ed.CurLine) = RTrim(Left(array(ed.CurLine), ed.CurCol - 1))
  '                  ed.Changed = True
  '                End If
  '              End If

  '              If X > 1 OrElse QPLen%(array(ed.CurLine)) <> 0 Then ed.CurCol = X
  '              ed.CurLine = MinInt%(ed.CurLine + 1, arrayEnd)
  '              WindowLimits(ed)

  '              reprint = -1                'Set flag to re-print window
  '            End If

  '            '----- TAB
  '          Case 9
  '            'Make current column "Ed.HTab"
  '            '  to the right
  '            N = ((ed.CurCol + ed.HTab - 1) \ ed.HTab) * ed.HTab + 1
  '            N = MinInt%(N, 255 - ed.HTab)
  '            If N <= ed.Wrap Or ed.Wrap = 0 Then
  '              'If in Insert mode
  '              If Insrt = -1 And ed.CurCol < QPLen%(array(ed.CurLine)) Then
  '                '  insert "Ed.HTab" spaces
  '                array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1) + Space(N - ed.CurCol) + Mid(array(ed.CurLine), ed.CurCol)
  '                If ed.Wrap <> 0 Then
  '                  WrapLine = ed.CurLine
  '                  WrapDown(array, ed)        'Wrap words down
  '                End If
  '                ed.Changed = True
  '              End If

  '              ed.CurCol = N
  '              'Is it off the right of window?
  '              If ed.CurCol > ed.LC + ed.Wide - 1 Then
  '                ed.LC += ed.HTab  'Make it "Ed.HTab" to the right
  '                reprint = -1             'Set flag to re-print window
  '              Else                        'Move to the left margin
  '                LOCATE(, ed.LSCol, 0)
  '                'Put new line in buffer
  '                REM LSet LineBuf$ = Mid(Array$(Ed.CurLine), Ed.LC)
  '                'Print the buffer
  '                Call MQPrint(LineBuf$, ed.AColor)
  '              End If
  '            End If

  '            '----- Escape
  '          Case 27
  '            If action = 0 Then             'If not using Action flag,
  '              CleanUp()               '  restore the screen and
  '              'Clean up memory
  '              Exit Do                     'see ya later oscillator
  '            Else
  '              ed.UnKnownKey = True
  '            End If


  '            '----- Ctrl Y - Delete a Line
  '          Case 25
  '            If ed.CurLine <= ed.LCount Then 'Disallow if we're past the last line
  '              'Calc the screen line number
  '              X = ed.TSRow + ed.CurLine - ed.TL
  '              'Paint the line before we delete it
  '              '  just for effect
  '              Call MPaintBox(X, ed.LSCol, X, ed.LSCol + ed.Wide - 1, blkClr)

  '              Temp = ed.CurLine           'Delete the current line
  '              DeleteLine(array, ed)

  '              Pause(1)                     'Wait an 1/18 of a second
  '              reprint = -1                'Set flag to re-print window
  '              If ed.LCount <> 0 Then ed.Changed = True
  '            End If


  '            '----- Ctrl N - Insert a Line
  '          Case 14
  '            Temp = ed.CurLine
  '            tempS = ""
  '            InsertLine(array, ed)
  '            reprint = -1                   'Set flag to re-print window
  '            If ed.LCount <> 0 Then ed.Changed = True


  '            '----- Ctrl P - Imbed characters
  '          Case 16
  '            ctrlP = 0
  '            lineEd = -1

  '          Case Else
  '            ed.UnKnownKey = True

  '        End Select


  '        X& = 32768 'FRE("")
  '        If X& < 1280 Then
  '          If RuntimeInformation.IsOSPlatform(OSPlatform.Windows) Then
  '            Beep()
  '          End If
  '          ed.MErr = 1                       'Set error flag
  '        End If


  '        '----- Handle Extended (2 Char) key codes
  '      ElseIf QPLen%(ky) = 2 Then

  '        Select Case ASCII%(Right(ky, 1))    'Get ASCII value of right 1

  '            '----- HELP key pressed
  '          Case 59
  '            'Make array for underlying Scr
  '            sz = ArraySize%(2, 80 - 57 - 1, 12, 80)
  '            ReDim HelpScr(sz)
  '            'Save underlying screen
  '            Call MScrnSave(2, 80 - 57 - 1, 12, 80, HelpScr)

  '            ReDim Hlp$(35) 'AS STRING * 57
  '            'Load array with Help text
  '            Call LoadHelp(Hlp$)

  '            LOCATE(2, 80 - 57 - 1, 0)       'Locate the cursor

  '            Call HideCursor()                'Print the help screen
  '            Call APrint0(Hlp$, 1, 10, 1, 57, 112)
  '            Call ShowCursor()
  '            'Paint Shadows
  '            Call MPaintBox(3, 79, 11, 80, 8)
  '            Call MPaintBox(12, 80 - 57 + 1, 12, 80, 8)
  '            LOCATE(10, 78)
  '            If Not mouseThere <> 0 AndAlso Not blocksThere <> 0 Then
  '              Call MQPrint("�", 79)
  '            Else
  '              Call MQPrint(Chr(18), 240)
  '            End If

  '            LOCATE(3, 80 - 57 - 1)          'Locate the cursor
  '            I = 2
  '            Do                             'Wait for a key press
  '              Temp = PeekBuf%()

  '              Select Case Temp            'Get key but don't remove it
  '                Case -72, -73            'Up arrow or PgUp
  '                  Select Case I         'Set pointer based on last one
  '                    Case 27
  '                      If blocksThere <> 0 Then
  '                        I = 19
  '                      Else
  '                        I = 2
  '                      End If
  '                    Case 19
  '                      I = 11
  '                    Case 11
  '                      I = 2
  '                    Case 2
  '                      If mouseThere <> 0 Then
  '                        I = 27
  '                      ElseIf blocksThere <> 0 Then
  '                        I = 11
  '                      End If
  '                  End Select
  '                Case -80, -81            'Down arrow or PgDn
  '                  Select Case I         'Set pointer based on last one
  '                    Case 2
  '                      If blocksThere <> 0 Then
  '                        I = 11
  '                      ElseIf mouseThere <> 0 Then
  '                        I = 27
  '                      End If
  '                    Case 11
  '                      If mouseThere <> 0 Then
  '                        I = 19
  '                      Else
  '                        I = 2
  '                      End If
  '                    Case 19
  '                      I = 27
  '                    Case 27
  '                      I = 2
  '                  End Select

  '                Case 27                  'Escape key
  '                  tempS = INKEY$()        'Take key out of buffer
  '                  Exit Do               'Bail out
  '                Case 0
  '                Case Else                'Bail out if any other key
  '                  Exit Do
  '              End Select

  '              If Temp <> 0 Then
  '                tempS = INKEY$()           'Now extract key from buffer
  '                Call HideCursor()          'Display new page of help
  '                Call APrint0(Hlp$, I, 8, 1, 56, 112)
  '                Call ShowCursor()
  '              End If
  '              'Bail out if mouse button pressed
  '              Call ButtonPress(1, 0, ed.Presses, X, y)
  '            Loop Until ed.Presses <> 0
  '            'Restore the screen
  '            Call MScrnRest(2, 80 - 57 - 1, 12, 80, HelpScr)
  '            Erase HelpScr, Hlp$            'Clean up memory


  '            '----- Cursor UP
  '          Case 72
  '            If PeekBuf%() = -72 Then
  '              ed.CurLine -= 2
  '              ky = INKEY$()
  '            Else
  '              ed.CurLine -= 1
  '            End If
  '            CursorUp(array, ed)                 'Go do it

  '            '----- Cursor DOWN
  '          Case 80
  '            If PeekBuf%() = -80 Then
  '              ed.CurLine += 2
  '              ky = INKEY$()
  '            Else
  '              ed.CurLine += 1
  '            End If
  '            CursorDown(array, ed)               'Go do it

  '            '----- Cursor LEFT
  '          Case 75
  '            If ed.CurCol > 1 Then
  '              If PeekBuf%() = -75 Then
  '                ed.CurCol -= 2
  '                ky = INKEY$()
  '              Else
  '                ed.CurCol -= 1
  '              End If
  '              CursorLeft(array, ed)
  '              'Are we in word wrap mode?
  '            ElseIf ed.Wrap <> 0 AndAlso ed.CurLine > 1 Then
  '              ed.CurCol = ed.Wrap         'Set column to right margin
  '              ed.LC = MaxInt%(ed.Wrap - ed.Wide + 1, 1)
  '              ed.CurLine -= 1
  '              CursorUp(array, ed)              'Let "CursorUp" handle it
  '              reprint = -1
  '              colBlock = False
  '            End If

  '            '----- Cursor RIGHT
  '          Case 77
  '            If ed.CurCol < 256 Then
  '              If PeekBuf%() = -77 Then
  '                ed.CurCol += 2
  '                ky = INKEY$()
  '              Else
  '                ed.CurCol += 1
  '              End If
  '              CursorRight(array, ed)
  '              'If at right margin
  '              If ed.Wrap <> 0 AndAlso ed.CurCol > ed.Wrap Then
  '                ed.CurCol = 1            'Set current column to 1
  '                ed.LC = 1                'Set window column to 1
  '                ed.CurLine += 1
  '                CursorDown(array, ed)         'Let "CursorDown" move down
  '                '  a line
  '                reprint = -1
  '                colBlock = False
  '              End If
  '            End If

  '            '----- Ctrl LEFT - move one word left
  '          Case 115
  '            Do
  '              If ed.CurCol = 1 And ed.CurLine > 1 Then
  '                ed.CurLine -= 1
  '                ed.CurCol = MaxInt%(QPLen%(array(ed.CurLine)), 1)
  '                colBlock = False
  '              End If

  '              X = -1
  '              Do While ed.CurCol > 1
  '                ed.CurCol -= 1
  '                If X <> 0 Then
  '                  If InStr(SkipTbl$, Mid(array(ed.CurLine), ed.CurCol, 1)) = 0 Then X = 0
  '                Else
  '                  If InStr(SkipTbl$, Mid(array(ed.CurLine), ed.CurCol, 1)) <> 0 Then
  '                    ed.CurCol += 1
  '                    Exit Do
  '                  End If
  '                End If
  '              Loop
  '            Loop While ed.CurLine > 1 AndAlso InStr(SkipTbl$, Mid(array(ed.CurLine), ed.CurCol, 1)) <> 0

  '            WindowLimits(ed)             'If off left of window, adjust

  '            '----- Ctrl RIGHT - move one word right
  '          Case 116
  '            Do
  '              X = -1
  '              Do While ed.CurCol <= QPLen%(array(ed.CurLine))
  '                ed.CurCol += 1 'Increment column position
  '                If X <> 0 Then
  '                  If InStr(SkipTbl$, Mid(array(ed.CurLine), ed.CurCol, 1)) <> 0 Then X = 0
  '                Else
  '                  If InStr(SkipTbl$, Mid(array(ed.CurLine), ed.CurCol, 1)) = 0 Then Exit Do
  '                End If
  '              Loop
  '              'If past end of line,
  '              If ed.CurCol > QPLen%(array(ed.CurLine)) And ed.CurLine < ed.LCount Then
  '                ed.CurCol = 1            '  Set current column to 1
  '                ed.CurLine += 1 'Increment line number
  '                colBlock = False
  '              Else
  '                Exit Do
  '              End If
  '            Loop While InStr(SkipTbl$, Mid(array(ed.CurLine), ed.CurCol, 1)) <> 0
  '            'If off right of window, adjust
  '            WindowLimits(ed)

  '            '----- Cursor HOME
  '          Case 71                           'Starting at column 1
  '            X = Blanks%(array(ed.CurLine)) + 1

  '            If X = ed.CurCol Then X = 1
  '            ed.CurCol = X

  '            WindowLimits(ed)


  '            '----- Cursor END
  '          Case 79                           'Set current column to end +
  '            ed.CurCol = QPLen%(array(ed.CurLine)) + FnSpaces2Pad%(array(ed.CurLine)) + 1

  '            WindowLimits(ed)

  '            '----- Ctrl HOME
  '          Case 119                          'Make current line top of screen
  '            ed.CurLine -= (CSRLIN() - ed.TSRow)

  '            '----- Ctrl END
  '          Case 117                          'Make bottom current line
  '            ed.CurLine += ((ed.TSRow + ed.Rows - 1) - CSRLIN())

  '            '----- PgUp
  '          Case 73
  '            If ed.TL > 1 Then              'ignore if already at the top
  '              X = ed.TL                   'save Ed.TL for a moment
  '              ed.TL = MaxInt%(1, ed.TL - ed.Rows)
  '              X -= ed.TL               'calc dif. between new and old
  '              ed.CurLine -= X 'don't move cursor unless we have to
  '              reprint = -1                'Set flag to re-print window
  '            End If

  '            '----- PgDn
  '          Case 81                           'almost at end, work backwards
  '            If ed.TL > arrayEnd - ed.Rows * 2 Then
  '              X = ed.CurLine - ed.TL      '  from end of array
  '              ed.TL = arrayEnd - ed.Rows + 1
  '              ed.CurLine = ed.TL + X      'Calc new current line
  '              reprint = -1                'Set flag to re-print window
  '            Else
  '              ed.TL += ed.Rows     'calc top line of next page
  '              ed.CurLine += ed.Rows 'update current line
  '              reprint = -1                'Set flag to re-print window
  '            End If

  '            '----- Ctrl PgUp
  '          Case 132
  '            ed.CurCol = 1                  'Make column 1 current
  '            ed.CurLine = 1                 'ditto for the current line
  '            ed.TL = 1                      'Set current top of screen
  '            ed.LC = 1                      'Set current left of screen
  '            reprint = -1                   'Set flag to re-print window

  '            '----- Ctrl PgDn
  '          Case 118                          'we're already there - ignore
  '            If ed.TL <= arrayEnd - ed.Rows Then
  '              If ed.LCount > 0 Then       'empty file - go to the top
  '                'Cursor past the end, but see
  '                If ed.CurLine > ed.LCount Then '  exactly where we are
  '                  X = ed.CurLine - ed.LCount
  '                  'Last line showing on screen
  '                  If X < ed.Rows And ed.TL <= ed.LCount Then
  '                    '  make that the current line
  '                    ed.CurLine = ed.LCount
  '                    LOCATE(CSRLIN() - X)  '  and put the cursor there
  '                  End If                '  too
  '                End If

  '                ed.CurLine = ed.LCount   'Make the last line current
  '                X = ed.CurLine - ed.TL   'See where cursor is relative
  '                '  to end
  '                'Last line is on the screen,
  '                If X <= 0 Or X >= ed.Rows Then
  '                  '  don't re-display, just move
  '                  'establish top line to display
  '                  ed.TL = ed.CurLine - (ed.Rows - 1)
  '                  'Can't have negative line #
  '                  ed.TL = MaxInt%(ed.TL, 1)
  '                  reprint = -1          'Set flag to re-print window
  '                End If
  '                'Locate at end of line
  '                ed.CurCol = QPLen%(array(ed.CurLine)) + 1
  '                If ed.CurCol < ed.LC Then
  '                  ed.LC = MaxInt%(1, ed.CurCol - ed.Wide + 1)
  '                  reprint = -1
  '                ElseIf ed.CurCol > ed.LC + ed.Wide - 1 Then
  '                  ed.LC = ed.CurCol - ed.Wide + 1
  '                  reprint = -1
  '                End If

  '              End If
  '            End If

  '            '----- Togle INSERT mode
  '          Case 82
  '            Insrt *= -1             'toggle insert on and off
  '            CsrSize()                  'set cursor size accordingly

  '            '----- Delete
  '          Case 83
  '            If ed.CurCol > QPLen%(array(ed.CurLine)) Then
  '              array(ed.CurLine) = array(ed.CurLine) + Space(ed.CurCol - QPLen%(array(ed.CurLine)) - 1)

  '              'if not on last line concatenate the next line to this one
  '              If ed.CurLine < arrayEnd Then array(ed.CurLine) = array(ed.CurLine) + LTrim(array(ed.CurLine + 1))

  '              Temp = ed.CurLine + 1
  '              DeleteLine(array, ed)

  '              reprint = -1                'Set flag to re-print window
  '              If ed.Wrap <> 0 Then
  '                WrapLine = ed.CurLine
  '                WrapDown(array, ed)           'Wrap words down
  '              End If
  '            Else                           'Within line, delete current
  '              '  character
  '              array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1) + Mid(array(ed.CurLine), ed.CurCol + 1)
  '              'Put new text in line buffer
  '              REM LSet LineBuf$ = Mid(Array$(Ed.CurLine), Ed.LC)
  '              LOCATE(, ed.LSCol, 0)       'Print the line buffer
  '              Call MQPrint(LineBuf$, -1)
  '            End If
  '            If ed.Wrap <> 0 Then
  '              WrapLine = ed.CurLine
  '              WrapUp(array, ed)                'Wrap words up
  '            End If
  '            ed.Changed = True
  '            lineEd = -1


  '            '----- Back TAB
  '          Case 15                           'Calc spaces to next stop
  '            X = (((ed.CurCol - ed.HTab - 2) \ ed.HTab) + 1) * ed.HTab + 1
  '            X = MaxInt%(X, 1)
  '            'If in Insert mode and not at
  '            '  end of line,
  '            If Insrt = -1 And ed.CurCol <= QPLen%(array(ed.CurLine)) Then
  '              'drag line to the left
  '              array(ed.CurLine) = Left(array(ed.CurLine), X - 1) + Mid(array(ed.CurLine), ed.CurCol)
  '              If ed.Wrap <> 0 Then
  '                WrapLine = ed.CurLine
  '                WrapUp(array, ed)             'Wrap words up
  '              End If
  '              ed.Changed = True
  '            End If

  '            ed.CurCol = X                  'Adjust current column posit.

  '            If ed.CurCol < ed.LC Then      'If off left of window,
  '              ed.LC = ed.CurCol           '  adjust current window col
  '              reprint = -1                'Set flag to re-print window
  '            Else                           'Otherwise,
  '              LOCATE(, ed.LSCol, 0) 'locate at left margin
  '              'Put new text in line buffer
  '              REM LSet LineBuf$ = Mid(Array$(Ed.CurLine), Ed.LC)
  '              Call MQPrint(LineBuf$, ed.AColor)  'Print the line buffer
  '            End If

  '          Case Else                         'Ignor all other keys
  '            ed.UnKnownKey = True

  '        End Select

  '      End If

  '    End If

  '    If lineEd <> 0 AndAlso ed.CurLine <> lineWas Then
  '      array(lineWas) = RTrim(array(lineWas))
  '      lineEd = 0
  '    End If

  '    '----- Re-display the window if flag set or block marking complete
  '    If reprint <> 0 OrElse (ed.WasMarked AndAlso markBlock = 0) Then
  '      LOCATE(ed.TSRow, ed.LSCol, 0)                   'APrint the screen
  '      Call HideCursor()                                             'Turn Mouse cursor off
  '      Call APrint0(array.ToArray,
  '                   ed.TL,
  '                   ed.Rows,
  '                   ed.LC,
  '                   ed.Wide,
  '                   ed.AColor)
  '      Call ShowCursor()                                             'Turn mouse cursor back on
  '      reprint = 0                                                   'Turn Flag off
  '      If markBlock = 0 Then ed.WasMarked = False                    'Turn hiliting off
  '    End If

  '    '----- If we have a window frame
  '    If ed.Frame Then
  '      'If line number has changed,
  '      If ed.CurLine <> LastCurLine OrElse reprint <> 0 Then
  '        DisplayLineNumber(ed)                                       '  display line # on status
  '        LastCurLine = ed.CurLine                                    'Save line number
  '      End If
  '      'If column number has changed,
  '      If ed.CurCol <> LastCurCol OrElse reprint <> 0 Then
  '        DisplayColNumber(ed)                                        '  display col. # on status
  '        LastCurCol = ed.CurCol                                      'Save column number
  '      End If
  '      If ed.LC <> lastC Then                                        'If window column has changed,
  '        LOCATE(ed.TSRow - 1, ed.LSCol, 0)
  '        Call MQPrint(Mid(Ruler(1), ed.LC + 1, ed.Wide), 112)
  '        lastC = ed.LC                                               'Save window column
  '      End If
  '    End If

  '    '----- Position the cursor
  '    LOCATE(ed.TSRow + ed.CurLine - ed.TL, ed.LSCol + (ed.CurCol - ed.LC), 1)

  '    '********************************************************************
  '    '* Block Operations code starts here.  If you don't want to use
  '    '* block operations, un-remark the lines above and delete the
  '    '* code from here to the remark - "End of Block Operations".
  '    '********************************************************************
  '    blocksThere = -1

  '    '----- If in block marking mode, paint the marked block
  '    If markBlock <> 0 AndAlso QPLen%(ky) <> 0 Then

  '      '----- If block marking key was just pressed
  '      If frstBlkKey <> 0 Then
  '        colBlock = False                      'Init flag to sentence mode
  '        'If still on same line, reset
  '        '  mode for column block
  '        If ed.CurLine = blkRow Then colBlock = True
  '        frstBlkKey = 0                    'Reset first key flag
  '        Bl(0).PB = 60                     'Init history variables
  '        Bl(1).PB = 80
  '        Bl(2).PB = 0
  '        Bl(3).PB = 0
  '      End If

  '      Bl(0).B = MinInt%(ed.CurLine, blkRow) 'Set top of block
  '      Bl(2).B = MaxInt%(ed.CurLine, blkRow) 'Set bottom of block

  '      '----- If in COLUMN BLOCK mode,
  '      If colBlock Then
  '        '----- Set left and right columns
  '        Bl(1).B = MinInt%(ed.CurCol, blkCol)
  '        Bl(3).B = MaxInt%(ed.CurCol - 1, blkCol)
  '        '----- Set coordinates to paint
  '        Bl(0).P = MaxInt%(Bl(0).B, ed.TL) - ed.TL + ed.TSRow
  '        Bl(1).P = MaxInt%(Bl(1).B, ed.LC) - ed.LC + ed.LSCol
  '        Bl(2).P = MinInt%(Bl(2).B, ed.TL + ed.Rows - 1) - ed.TL + ed.TSRow
  '        Bl(3).P = MaxInt%(MinInt%(Bl(3).B, ed.LC + ed.Wide - 1) - ed.LC + ed.LSCol, ed.LSCol)
  '        '----- Get previous paint coordinates
  '        Bl(0).PP = MaxInt%(Bl(0).PB, ed.TL) - ed.TL + ed.TSRow
  '        Bl(1).PP = MaxInt%(Bl(1).PB, ed.LC) - ed.LC + ed.LSCol
  '        Bl(2).PP = MinInt%(Bl(2).PB, ed.TL + ed.Rows - 1) - ed.TL + ed.TSRow
  '        Bl(3).PP = MinInt%(Bl(3).PB, ed.LC + ed.Wide - 1) - ed.LC + ed.LSCol
  '        '----- Un-paint old part of block
  '        If Bl(3).PB >= ed.LC And Bl(2).PB >= ed.TL Then
  '          If Bl(0).PB < Bl(0).B Then MPaintBox(Bl(0).PP, MinInt%(Bl(1).PP, Bl(1).P), Bl(0).P - 1, MaxInt%(Bl(3).PP, Bl(3).P), ed.AColor)
  '          If Bl(2).PB > Bl(2).B Then MPaintBox(Bl(2).P + 1, MinInt%(Bl(1).PP, Bl(1).P), Bl(2).PP, MaxInt%(Bl(3).PP, Bl(3).P), ed.AColor)
  '          If Bl(1).PB < Bl(1).B Then MPaintBox(MinInt%(Bl(0).P, Bl(0).PP), Bl(1).PP, MaxInt%(Bl(2).P, Bl(2).PP), Bl(1).P, ed.AColor)
  '          If Bl(3).PB > Bl(3).B Then MPaintBox(MinInt%(Bl(0).P, Bl(0).PP), Bl(3).P, MaxInt%(Bl(2).P, Bl(2).PP), Bl(3).PP, ed.AColor)
  '        End If
  '        '----- Paint the block
  '        If Bl(3).B >= ed.LC And Bl(2).B >= ed.TL Then
  '          Call MPaintBox(Bl(0).P, Bl(1).P, Bl(2).P, Bl(3).P, blkClr)
  '        End If

  '        '----- If in SENTENCE BLOCK mode
  '      Else
  '        '----- Set left and right columns
  '        If Bl(0).B < blkRow Then
  '          Bl(1).B = ed.CurCol
  '          Bl(3).B = blkCol
  '        Else
  '          Bl(1).B = blkCol
  '          Bl(3).B = ed.CurCol - 1
  '        End If
  '        '----- Set coordinates to paint
  '        Bl(0).P = MaxInt%(Bl(0).B, ed.TL) - ed.TL + ed.TSRow
  '        Bl(1).P = MaxInt%(Bl(1).B, ed.LC) - ed.LC + ed.LSCol
  '        Bl(2).P = MinInt%(Bl(2).B - ed.TL + ed.TSRow, ed.TSRow + ed.Rows - 1)
  '        Bl(3).P = MaxInt%(ed.LSCol, MinInt%(Bl(3).B, ed.LC + ed.Wide - 1) - ed.LC + ed.LSCol)
  '        '----- Get previous paint coordinates
  '        Bl(0).PP = MaxInt%(Bl(0).PB, ed.TL) - ed.TL + ed.TSRow
  '        Bl(1).PP = MaxInt%(Bl(1).PB, ed.LC) - ed.LC + ed.LSCol
  '        Bl(2).PP = MinInt%(Bl(2).PB - ed.TL + ed.TSRow, ed.TSRow + ed.Rows - 1)
  '        Bl(3).PP = MaxInt%(ed.LSCol, MinInt%(Bl(3).PB, ed.LC + ed.Wide - 1) - ed.LC + ed.LSCol)
  '        '----- Un-paint old part of block
  '        If Bl(2).B < Bl(2).PB Then MPaintBox(Bl(2).P, ed.LSCol, Bl(2).PP, ed.LSCol + ed.Wide - 1, ed.AColor)
  '        If Bl(0).B > Bl(0).PB Then MPaintBox(Bl(0).PP, ed.LSCol, Bl(0).P, ed.LSCol + ed.Wide - 1, ed.AColor)
  '        '----- Paint the new block
  '        If ed.CurLine <> blkRow Then              'If more than one line
  '          '----- Paint top line if on screen
  '          If Bl(0).B >= ed.TL Then
  '            If Bl(1).B >= Bl(1).PB Then MPaintBox(Bl(0).P, ed.LSCol, Bl(0).P, MaxInt%(Bl(1).P - 1, ed.LSCol), ed.AColor)
  '            Call MPaintBox(Bl(0).P, Bl(1).P, Bl(0).P, ed.LSCol + ed.Wide - 1, blkClr)
  '          End If
  '          '----- Paint middle lines if on screen
  '          For N = Bl(0).B + 1 To Bl(2).B - 1
  '            If N >= ed.TL And N < ed.TL + ed.Rows Then
  '              Bl(0).P = N - ed.TL + ed.TSRow
  '              Call MPaintBox(Bl(0).P, ed.LSCol, Bl(0).P, ed.LSCol + ed.Wide - 1, blkClr)
  '            End If
  '          Next
  '          '----- Paint bottom line if on screen
  '          If Bl(2).B < ed.TL + ed.Rows Then
  '            If Bl(3).B <= Bl(3).PB Then MPaintBox(Bl(2).P, Bl(3).P, Bl(2).P, ed.LSCol + ed.Wide - 1, ed.AColor)
  '            If Bl(3).B >= ed.LC Then MPaintBox(Bl(2).P, ed.LSCol, Bl(2).P, Bl(3).P, blkClr)
  '          End If
  '        Else                                      'Only one line
  '          Call MPaintBox(Bl(2).P, ed.LSCol, Bl(2).P, ed.LSCol + ed.Wide - 1, ed.AColor)
  '          lpCol = MaxInt%(MinInt%(blkCol, ed.CurCol), ed.LC) - ed.LC + ed.LSCol
  '          rpCol = MaxInt%(MinInt%(MaxInt%(blkCol, ed.CurCol - 1), ed.LC + ed.Wide - 1) - ed.LC + ed.LSCol, ed.LSCol)
  '          Call MPaintBox(Bl(2).P, lpCol, Bl(2).P, rpCol, blkClr)
  '        End If
  '      End If
  '      '----- Save bottom corner coordinates
  '      Bl(0).PB = Bl(0).B
  '      Bl(1).PB = Bl(1).B
  '      Bl(2).PB = Bl(2).B
  '      Bl(3).PB = Bl(3).B
  '    End If

  '    '----- If just displaying the window, bail out
  '    If action = 1 OrElse action = 2 OrElse action = 4 Then
  '      ky = ""
  '      action = 3                                      'Reset Action code to 3 (idle)
  '      Exit Do
  '    End If

  '    '----- Check for a key press or mouse action
  '    Do

  '      ky = INKEY$()                          'Get key press

  '      'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM
  '      'MM   If you hate rodents, exterminate the code from here to    MM
  '      'MM   the next set of "MMM" comments.                           MM
  '      'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
  '      '----- Get Mouse Coords. and Botton Info.
  '      mouseThere = -1                      'Mouse code is here

  '      'Check for button presses
  '      Call ButtonPress(1, Temp, ed.Presses, X, y)

  '      '----- If User pressed a button
  '      If ed.Presses <> 0 OrElse Temp <> 0 Then
  '        If ed.Presses = 0 Then GetCursor(X, y, Temp)
  '        ed.MRow = (y \ nPixLines) + 1     'Convert X, Y to Row and Col.
  '        ed.MCol = (X \ 8) + 1

  '        '----- If Button just pressed
  '        If ed.Presses <> 0 Then

  '          wasPressed = -1                'Set flag for "let go" proc.
  '          delay = 0

  '          '----- Are we within the ROW limit of the TEXT?
  '          If ed.MRow >= ed.TSRow And ed.MRow < ed.TSRow + ed.Rows Then

  '            '----- Are we within the COLUMN limit of the TEXT?
  '            If ed.MCol >= ed.LSCol And ed.MCol < ed.LSCol + ed.Wide Then
  '              mouseAct = 1                            'Flag for TEXT operations
  '              'Limit mouse movement to window
  '              Call MouseTrap(ed.TSRow + CInt(ed.Frame), ed.LSCol + CInt(ed.Frame), ed.TSRow + ed.Rows + CInt(Not ed.Frame), ed.LSCol + ed.Wide + CInt(Not ed.Frame))
  '              'Set the cursor row and column
  '              ed.CurLine = ed.MRow - ed.TSRow + ed.TL ' to the mouse location
  '              ed.CurCol = ed.MCol - ed.LSCol + ed.LC

  '              ky = MOUSE_KEY$                         'Set dummy key so we fall out
  '              '                                         of key loop
  '              If Not markBlock <> 0 Then              'If it's a new block
  '                blkRow = ed.CurLine                   'Save upper left coordinates
  '                blkCol = ed.CurCol
  '                saveT = ed.TL                         'Save window coordinates
  '                saveC = ed.LC
  '              Else
  '                ky = MOUSE_KEY_2
  '              End If
  '              ed.Presses = 0                          'Clear presses

  '              '----- Are we on the VERTICAL scroll bar?
  '            ElseIf ed.MCol = ed.LSCol + ed.Wide AndAlso ed.Frame Then
  '              'On a Scroll Arrow?
  '              If ed.MRow = ed.TSRow Or ed.MRow = ed.TSRow + ed.Rows - 1 Then
  '                'Limit mouse movement to the
  '                '  Scroll Arrow
  '                Call MouseTrap(ed.MRow, ed.MCol, ed.MRow, ed.MCol)
  '                mouseAct = 7
  '                '----- Are we on the scroll pointer?
  '              ElseIf ed.MRow = OldVrtPtr + ed.TSRow + 1 And ed.LCount > ed.Rows Then
  '                mouseAct = 2          'Flag for vertical scroll pointer
  '                'Limit mouse to scroll bar
  '                Call MouseTrap(ed.TSRow + 1, ed.LSCol + ed.Wide, ed.TSRow + ed.Rows - 2, ed.LSCol + ed.Wide)
  '              End If
  '              ed.Presses = 0           'Clear the press
  '            ElseIf ed.MCol = ed.LSCol - 1 AndAlso ed.Frame Then
  '              mouseAct = 8             'On left hand border, move window
  '              Call MouseTrap(ed.MRow - ed.TSRow + 2 + tMarg, 1, ed.MRow + ScrRows - (ed.TSRow + ed.Rows) - bMarg, 79 - ed.Wide)
  '              ed.Presses = 0              'Clear the press
  '            End If

  '            '----- Are we on the HORIZONTAL scroll bar
  '          ElseIf ed.MRow = ed.TSRow + ed.Rows AndAlso ed.Frame Then
  '            If ed.MCol >= ed.LSCol - 1 And ed.MCol <= ed.LSCol + ed.Wide Then
  '              'Where on the line?
  '              Select Case ed.MCol
  '                        '----- On a scroll arrow?
  '                Case ed.LSCol + 8, ed.LSCol + ed.Wide - 1
  '                  'Limit mouse movement to the
  '                  '  Scroll Arrow
  '                  Call MouseTrap(ed.MRow, ed.MCol, ed.MRow, ed.MCol)

  '                  mouseAct = 7
  '                        '----- On the scroll pointer?
  '                Case ed.LSCol + 9 + OldHorPtr
  '                  'Set flag for horizontal
  '                  mouseAct = 3       '   scroll pointer
  '                  'Limit mouse to scroll bar
  '                  Call MouseTrap(ed.TSRow + ed.Rows, ed.LSCol + 9, ed.TSRow + ed.Rows, ed.LSCol + ed.Wide - 2)
  '                        '----- On Right Corner of Frame?
  '                Case ed.LSCol + ed.Wide
  '                  mouseAct = 4       'Flag for lower right corner
  '                  'Limit movement to screen
  '                  '  margins
  '                  Call MouseTrap(tMarg + 1, 1, ScrRows - bMarg, 80)

  '                Case Else
  '              End Select
  '              ed.Presses = 0           'Clear the mouse press
  '            End If

  '            '----- Are we on the TOP line of the Frame
  '          ElseIf ed.MRow = ed.TSRow - 1 AndAlso ed.Frame Then
  '            If ed.MCol >= ed.LSCol - 1 AndAlso ed.MCol <= ed.LSCol + ed.Wide Then
  '              'On the TOP LEFT corner?
  '              If ed.MCol = ed.LSCol - 1 Then
  '                Call MouseTrap(tMarg + 1, 1, ScrRows - bMarg, 80)
  '                mouseAct = 5          'Flag for upper left corner

  '                '----- Are we on the RIGHT MARGIN marker?
  '              ElseIf ed.MCol = ed.LSCol + ed.Wrap - ed.LC And ed.Wrap >= ed.LC And ed.Wrap < ed.LC + ed.Wide Then
  '                mouseAct = 6          'Flag for margin marker
  '                'Limit mouse to ruler line
  '                Call MouseTrap(ed.TSRow - 1, ed.LSCol, ed.TSRow - 1, ed.LSCol + ed.Wide - 1)

  '              ElseIf ed.MCol < ed.LSCol + ed.Wide Then
  '                mouseAct = 8          'move window
  '                Call MouseTrap(tMarg + 1, ed.MCol - ed.LSCol + 2, ed.MRow + ScrRows - (ed.TSRow + ed.Rows) - bMarg, ed.MCol + (80 - ed.Wide - ed.LSCol))

  '              Else
  '                mouseAct = 9
  '              End If
  '              ed.Presses = 0           'Clear the mouse press
  '            End If
  '          End If

  '          '----- Button is DOWN but not just pressed
  '        Else
  '          '----- DRAG actions
  '          Select Case mouseAct

  '                  '----- BLOCK marking operations
  '            Case 1
  '              '----- Has the mouse been moved?
  '              If ed.MRow <> lmRow Or ed.MCol <> lmCol Then
  '                If Not markBlock <> 0 Then 'Beginning of block?

  '                  frstBlkKey = -1    'Set first key flag
  '                  markBlock = -1     'Set block marking flags
  '                  ed.WasMarked = True
  '                End If
  '                'Set cursors new location
  '                ed.CurLine = MinInt%(ed.TL + ed.Rows - 1, MaxInt%(ed.TL, ed.MRow - ed.TSRow + ed.TL))
  '                ed.CurCol = MinInt%(ed.LC + ed.Wide - 1, MaxInt%(ed.LC, ed.MCol - ed.LSCol + ed.LC))
  '                ky = MOUSE_KEY$        'Make dummy key so we fall out
  '              End If

  '              '----- On TOP of frame?  Scroll UP
  '              If ed.MRow = ed.TSRow - 1 Then
  '                'Check timer
  '                If Timer() > lmTime + 0.05 Or Timer() < lmTime Then
  '                  ed.CurLine = ed.TL - 1 'Set cursor to top of window
  '                  CursorUp(array, ed)     'Move cursor up and scroll
  '                  ky = MOUSE_KEY$     'Make dummy key
  '                  lmTime = CSng(Timer())    'Reset timer for delay
  '                End If
  '              End If

  '              '----- On BOTTOM of frame?  Scroll DOWN
  '              If ed.MRow = ed.TSRow + ed.Rows Then
  '                'Check timer
  '                If Timer() > lmTime + 0.05 Or Timer() < lmTime Then
  '                  ed.CurLine = ed.TL + ed.Rows
  '                  CursorDown(array, ed)   'Move cursor down and scroll
  '                  ky = MOUSE_KEY$     'Make dummy key
  '                  lmTime = CSng(Timer())    'Reset timer
  '                End If
  '              End If

  '              '----- On RIGHT side of frame?  Scroll right
  '              If ed.MCol = ed.LSCol + ed.Wide Then
  '                'Check timer
  '                If Timer() > lmTime + 0.05 Or Timer() < lmTime Then
  '                  ed.CurCol = ed.LC + ed.Wide
  '                  CursorRight(array, ed)
  '                  ky = MOUSE_KEY$     'Make dummy key
  '                  lmTime = CSng(Timer())    'Reset timer
  '                End If
  '              End If

  '              '----- On LEFT side of frame?  Scroll left
  '              If ed.MCol = ed.LSCol - 1 Then
  '                'Cursor to left of window
  '                'Check timer
  '                If Timer() > lmTime + 0.05 Or Timer() < lmTime Then
  '                  ed.CurCol = ed.LC - 1      'Decrement current column
  '                  CursorLeft(array, ed)
  '                  ky = MOUSE_KEY$     'Make dummy key
  '                  lmTime = CSng(Timer())    'Reset timer
  '                End If
  '              End If

  '                  '----- Vertical Scroll Pointer operations
  '            Case 2
  '              If ed.MRow <> lmRow Then         'If the mouse moved
  '                'Calc. % of bar
  '                'Set line to %
  '                ed.CurLine = CInt(Int(((ed.MRow - ed.TSRow - 1&) * ed.LCount) \ (ed.Rows - 3)) + 1)
  '                ed.CurLine = MinInt%(ed.CurLine, ed.LCount)
  '                If ed.MRow > lmRow Then       'Set top of window
  '                  ed.TL = MaxInt%(MinInt%(MaxInt%(ed.TL, ed.CurLine), ed.LCount - ed.Rows + 2), 1)
  '                Else
  '                  ed.TL = MaxInt%(MinInt%(ed.TL, ed.CurLine - ed.Rows + 1), 1)
  '                End If
  '                ky = MOUSE_KEY$                'Make dummy key
  '                reprint = -1                  'Set flag to reprint
  '              End If                           '  window

  '                  '----- Horizontal Scroll Pointer operations
  '            Case 3
  '              If ed.MCol <> lmCol Then         'If the mouse moved
  '                If ed.Wrap <> 0 Then               'If word wrap is on,
  '                  X = ed.Wrap + 1            '  use right margin
  '                Else                          '  as text width
  '                  X = 256                    '  otherwise use 256
  '                End If
  '                'Set column to %
  '                ed.CurCol = Int(((ed.MCol - ed.LSCol - 9) * X) \ (ed.Wide - 10)) + 1
  '                If ed.MCol > lmCol Then       'Set left column
  '                  ed.LC = MaxInt%(MinInt%(MaxInt%(ed.LC, ed.CurCol), X - ed.Wide), 1)
  '                Else
  '                  ed.LC = MaxInt%(MinInt%(ed.LC, ed.CurCol - ed.Wide + 1), 1)
  '                End If
  '                ky = MOUSE_KEY$                'Make dummy key
  '                reprint = -1                  'Set flag to reprint
  '              End If                           '  window

  '                  '----- Lower Right Corner operations (ReSize window)
  '            Case 4                      'If the mouse has moved
  '              If ed.MRow <> lmRow Or ed.MCol <> lmCol Then
  '                'Calc new location and size
  '                ed.Wide = MaxInt%(ed.MCol - ed.LSCol, 11)
  '                ed.LSCol = MaxInt%(ed.MCol - ed.Wide, 2)

  '                ed.Rows = MaxInt%(ed.MRow - ed.TSRow, 3)
  '                ed.TSRow = MaxInt%(ed.MRow - ed.Rows, tMarg + 2)

  '                InitWindow(ed)      'Let subroutine redraw window
  '                ky = MOUSE_KEY$        'Make dummy key
  '              End If

  '                  '----- Upper Left Corner operations (ReSize window)
  '            Case 5                      'If the mouse has moved
  '              If ed.MRow <> lmRow Or ed.MCol <> lmCol Then
  '                'Calc new location and size
  '                WC(3, 0) = ed.LSCol + ed.Wide
  '                WC(2, 0) = ed.TSRow + ed.Rows

  '                ed.LSCol = MinInt%(ed.MCol + 1, 80 - 11)
  '                ed.Wide = MaxInt%(WC(3, 0) - ed.LSCol, 11)

  '                ed.TSRow = MinInt%(ed.MRow + 1, ScrRows - bMarg - 3)
  '                ed.Rows = MaxInt%(WC(2, 0) - ed.TSRow, 3)

  '                InitWindow(ed)      'Let subroutine redraw window
  '                ky = MOUSE_KEY$        'Make dummy key
  '              End If

  '                  '----- Left boarder operation (Move window)
  '            Case 8                      'If the mouse has moved
  '              If ed.MRow <> lmRow Or ed.MCol <> lmCol Then
  '                'Calc new location and size
  '                ed.LSCol = ed.LSCol + ed.MCol - lmCol
  '                ed.TSRow = ed.TSRow + ed.MRow - lmRow

  '                InitWindow(ed)      'Let subroutine redraw window
  '                ky = MOUSE_KEY$        'Make dummy key
  '              End If

  '                  '----- Zoom
  '            Case 9
  '              ed.LSCol = 2
  '              ed.Wide = 78

  '              ed.TSRow = tMarg + 2
  '              ed.Rows = ScrRows - bMarg - ed.TSRow

  '              InitWindow(ed)      'Let subroutine redraw window
  '              ky = MOUSE_KEY$        'Make dummy key

  '                  '----- Move right margin operations
  '            Case 6
  '              If ed.MCol <> lmCol Then 'If the mouse has moved
  '                Ruler(1) = Ruler(0)   'Reset the ruler$ to erase
  '                '  the old marker
  '                MID$(Ruler(1), 2) = Chr(16)
  '                'Calc the new right margin
  '                ed.Wrap = MaxInt%(ed.MCol - ed.LSCol + ed.LC, 10)
  '                'Put margin icon in Ruler$
  '                MID$(Ruler(1), ed.Wrap + 1) = Chr(17)
  '                LOCATE(ed.TSRow - 1, ed.LSCol, 0)         '  re-display ruler line
  '                Call MQPrint(Mid(Ruler(1), ed.LC + 1, ed.Wide), 112) 'part to print

  '                WrapLine = 1          'Start wrapping at line 1
  '                WrapAll(array, ed)         'Go wrap all text
  '                ed.CurCol = MinInt%(ed.CurCol, ed.Wrap)

  '                ky = MOUSE_KEY$        'Make dummy key
  '                reprint = -1          'Set flag to redisplay text
  '                'Set "Changed" flag
  '                If ed.LCount <> 0 Then ed.Changed = True
  '              End If

  '              '----- Scroll bar operations (except scroll pointer)
  '            Case Else

  '              '----- Wait for delay
  '              If (Timer() > lmTime + delay OrElse Timer() < lmTime) AndAlso ed.Frame Then
  '                If mouseAct = 7 Then
  '                  delay = 0.3
  '                  mouseAct = 0
  '                Else
  '                  delay = 0.05
  '                End If
  '                lmTime = CSng(Timer())

  '                '----- Vertical scroll bar operations
  '                If ed.MCol = ed.LSCol + ed.Wide And ed.MRow >= ed.TSRow And ed.MRow < ed.TSRow + ed.Rows Then
  '                  scrVrtPtr = VrtPtr + ed.TSRow + 1
  '                  Select Case ed.MRow

  '                              '----- On Up Arrow icon
  '                    Case Is = ed.TSRow
  '                      If ed.TL > 1 Then
  '                        ed.TL -= 1
  '                        ed.CurLine = MinInt%(ed.CurLine, ed.TL + ed.Rows - 1)
  '                        ky = MOUSE_KEY$
  '                        reprint = -1
  '                      End If

  '                              '----- On Down Arrow icon
  '                    Case Is = ed.TSRow + ed.Rows - 1
  '                      If ed.TL < arrayEnd - ed.Rows + 1 Then
  '                        ed.TL += 1
  '                        ed.CurLine = MaxInt%(ed.CurLine, ed.TL)
  '                        ky = MOUSE_KEY$
  '                        reprint = -1
  '                      End If

  '                              '----- Above Pointer
  '                    Case Is < scrVrtPtr
  '                      ky = ZERO + Chr(73)

  '                              '----- Below Pointer
  '                    Case Is > scrVrtPtr
  '                      ky = ZERO + Chr(81)

  '                    Case Else
  '                  End Select
  '                End If

  '                '----- Horizontal scroll bar operations
  '                If ed.MRow = ed.TSRow + ed.Rows And ed.MCol >= ed.LSCol And ed.MCol < ed.LSCol + ed.Wide Then
  '                  scrHorPtr = ed.LSCol + 9 + HorPtr
  '                  Select Case ed.MCol

  '                              '----- On Left Arrow icon
  '                    Case Is = ed.LSCol + 8
  '                      If ed.LC > 1 Then
  '                        ed.LC -= 1
  '                        ed.CurCol = MinInt%(ed.CurCol, ed.LC + ed.Wide - 1)
  '                        ky = MOUSE_KEY$
  '                        reprint = -1
  '                      End If

  '                              '----- On Right Arrow icon
  '                    Case Is = ed.LSCol + ed.Wide - 1
  '                      If ed.LC < 255 - ed.Wide + 1 Then
  '                        ed.LC += 1
  '                        ed.CurCol = MaxInt%(ed.CurCol, ed.LC)
  '                        ky = MOUSE_KEY$
  '                        reprint = -1
  '                      End If

  '                              '----- Left of Pointer
  '                    Case Is < scrHorPtr
  '                      mTime -= 0.05!
  '                      ed.LC = MaxInt%(ed.LC - ed.Wide, 1)
  '                      ed.CurCol = MaxInt%(ed.CurCol - ed.Wide, 1)
  '                      ky = MOUSE_KEY$
  '                      reprint = -1

  '                              '----- Right of Pointer
  '                    Case Is > scrHorPtr
  '                      mTime -= 0.05!
  '                      If ed.Wrap <> 0 Then
  '                        Temp = MaxInt%(1, ed.Wrap - ed.Wide)
  '                      Else
  '                        Temp = 256 - ed.Wide
  '                      End If
  '                      ed.LC = MinInt%(ed.LC + ed.Wide, Temp)
  '                      ed.CurCol = MinInt%(ed.CurCol + ed.Wide, Temp)
  '                      ky = MOUSE_KEY$
  '                      reprint = -1

  '                    Case Else
  '                  End Select
  '                End If

  '              End If

  '          End Select
  '        End If

  '        lmRow = ed.MRow           'Remember were we were so we
  '        lmCol = ed.MCol           '  can detect movement.

  '        '----- No Mouse activity
  '      ElseIf wasPressed <> 0 Then
  '        mouseAct = 0

  '        '----- Un-restrict Mouse cursor if button let go
  '        Call MouseTrap(1, 1, ScrRows, 80)

  '        wasPressed = 0
  '      End If

  '      'MMMMMMMMMMMMMMMMMMMMMM End of Mouse Code MMMMMMMMMMMMMMMMMMMMMMMM

  '    Loop Until QPLen%(ky) <> 0 OrElse action <> 0

  '    '----- If they pressed a shift cursor key,
  '    If ShiftKey%() <> 0 AndAlso
  '       ((QPLen%(ky) = 1 AndAlso InStr(NumPad$, ky) <> 0) OrElse
  '        (QPLen%(ky) = 2 AndAlso InStr(NumPad2$, Right(ky, 1)) <> 0)) Then

  '      If Not markBlock <> 0 Then        'If it's a new block
  '        frstBlkKey = -1           'Set first key flag
  '        blkRow = ed.CurLine       'Save upper left coordinates
  '        blkCol = ed.CurCol
  '        saveT = ed.TL             'Save window coordinates
  '        saveC = ed.LC
  '        markBlock = -1            'Set block marking flag
  '        ed.WasMarked = True
  '      End If

  '      '----- Reset the key codes to their un-shifted states for
  '      '      normal key processing
  '      Select Case ASCII%(ky)
  '        Case 49                           '1/End Key
  '          ky = ZERO + Chr(79)
  '        Case 50                           '2/Down Key
  '          ky = ZERO + Chr(80)
  '        Case 51                           '3/PgDn Key
  '          ky = ZERO + Chr(81)
  '        Case 52                           '4/Left Key
  '          ky = ZERO + Chr(75)
  '        Case 54                           '6/Right Key
  '          ky = ZERO + Chr(77)
  '        Case 55                           '7/Home Key
  '          ky = ZERO + Chr(71)
  '        Case 56                           '8/Up Key
  '          ky = ZERO + Chr(72)
  '        Case 57                           '9/PgUp Key
  '          ky = ZERO + Chr(73)
  '        Case Else
  '      End Select

  '      '----- If a block is marked and they didn't press a shifted cursor
  '      '----- key or they pressed "Ctrl Y", capture the Block to the
  '      '----- Clipboard array
  '    ElseIf markBlock <> 0 OrElse
  '           ky = ChrW(25) OrElse
  '           ed.DelBlock Then

  '      If ky = ChrW(25) Then                'If they pressed "Ctrl Y",
  '        Bl(0).B = ed.CurLine               '  save current line coords.
  '        Bl(1).B = 1
  '        Bl(2).B = ed.CurLine
  '        Bl(3).B = QPLen(array(ed.CurLine))
  '        colBlock = True                     'Not a column block.
  '      End If

  '      'If key not Escape or F1-Help
  '      ok = ed.DelBlock OrElse
  '           QPLen(ky) <> 0
  '      ok = ok AndAlso
  '           ky <> Chr(27) AndAlso
  '           ky <> ZERO + Chr(59) AndAlso
  '           ky <> MOUSE_KEY$

  '      If ok Then

  '        ed.UlCRow = Bl(0).B               'Save top and bottom block
  '        ed.UlCCol = Bl(1).B               '  coordinates.
  '        ed.BrCRow = Bl(2).B
  '        ed.BrCCol = Bl(3).B
  '        ed.CBlock = colBlock              'Save block type (mode) flag

  '        numRows = ed.BrCRow - ed.UlCRow + 1

  '        If ed.CBlock Then                 'Is this a column block?
  '          bBytes = 0                    'Init. bytes needed for block
  '          'Get width of block
  '          numCols = ed.BrCCol - ed.UlCCol + 1
  '          For N = Bl(0).B To Bl(2).B     'Calc. bytes needed for block
  '            bBytes += QPLen%(Mid(array(N), Bl(1).B, numCols))
  '          Next
  '          '----- See if we have enough far memory
  '          free = 32768 'FRE(-1)
  '          If ((CLng(numRows) * numCols)) + 50000 > free Or CLng(numRows) * numCols > 65536 Then
  '            ed.MErr = 1                 'Set error flag
  '            ed.UlCRow = 0               'Erase block marking flags
  '            markBlock = 0
  '            ky = ""                     'Erase key so nothing more
  '          Else                           '  happens
  '            'Save the block to far memory
  '            ReDim clipBd((numRows * numCols) \ 2 + 1)
  '            'CALL MidStrSave(VARPTR(Array$(Ed.UlCRow)), NumRows, Ed.UlCCol, NumCols, ClipBd(0))
  '            Call MidStrSave(array.ToArray, ed.UlCRow, numRows, ed.UlCCol, numCols, clipBd, 0)
  '          End If
  '        Else                              'Sentence block
  '          'Save top and bottom lines
  '          clipBd1 = Mid(array(ed.UlCRow), ed.UlCCol)
  '          clipBd2 = Mid(array(ed.BrCRow), 1, ed.BrCCol)

  '          numRows -= 2          'Calc number of rows
  '          'Init. bytes needed for block
  '          bBytes = QPLen%(clipBd1) + QPLen%(clipBd2)

  '          If numRows > 0 Then            'Calc bytes needed by rest of
  '            For N = Bl(0).B To Bl(2).B  '  block
  '              bBytes += QPLen%(array(N))
  '            Next
  '            '----- See if we have enough far memory
  '            Temp& = CInt(StringSize&(array.ToArray, ed.UlCRow + 1, numRows))
  '            If Temp& + 40000 > 32768 Then 'FRE(-1) THEN
  '              If RuntimeInformation.IsOSPlatform(OSPlatform.Windows) Then
  '                Beep()
  '              End If
  '              ed.MErr = 1              'Set error flag
  '              ed.UlCRow = 0            'Erase block marking flags
  '              markBlock = 0
  '              ky = ""                  'Erase key so nothing more
  '            Else                        '  happens
  '              'Save the block to far memory
  '              ReDim clipBd(Temp& \ 2 + (Temp& Mod 2))
  '              Call StringSave(array.ToArray, ed.UlCRow + 1, clipBd, 0, numRows)
  '            End If
  '          Else
  '            ReDim clipBd(0)
  '          End If
  '        End If

  '        '----- Delete block if they pressed "Shift Del" or just "Del"
  '        If ed.DelBlock OrElse
  '           (ShiftKey%() <> 0 AndAlso ky = ".") OrElse
  '           ky = ZERO + ChrW(83) Then
  '          ed.DelBlock = False                'If it's a column block,
  '          If ed.BrCRow = ed.UlCRow Then
  '            array(ed.UlCRow) = Left(array(ed.UlCRow), ed.UlCCol - 1) + Mid(array(ed.BrCRow), ed.BrCCol + 1)
  '            array(ed.UlCRow) = array(ed.UlCRow) + Space(FnSpaces2Pad%(array(ed.BrCRow)))
  '            If arrayEnd > ed.UlCRow Then       'EW 7-26-93
  '              array(ed.UlCRow) = array(ed.UlCRow) + array(ed.UlCRow + 1)
  '            End If
  '            Temp = ed.UlCRow + 1
  '            DeleteLine(array, ed)
  '          ElseIf ed.CBlock Then
  '            '  delete range of each row
  '            For N = ed.UlCRow To ed.BrCRow
  '              array(N) = Left(array(N), ed.UlCCol - 1) + Mid(array(N), ed.BrCCol + 1)
  '              array(N) = RTrim(array(N))
  '            Next
  '          Else                           'Sentence block
  '            array(ed.UlCRow) = Left(array(ed.UlCRow), ed.UlCCol - 1) + Mid(array(ed.BrCRow), ed.BrCCol + 1)
  '            'Delete range of each row
  '            For N = ed.UlCRow + 1 To ed.BrCRow
  '              Temp = ed.UlCRow + 1
  '              DeleteLine(array, ed)
  '            Next
  '          End If
  '          ed.LCount = FindLast%(array.ToArray, arrayEnd, arrayEnd)
  '          ed.CurLine = ed.UlCRow         'Re-establish current cursor
  '          ed.CurCol = ed.UlCCol          '  and window coordinates.
  '          ed.TL = MinInt%(saveT, ed.CurLine)
  '          ed.LC = MinInt%(saveC, ed.CurCol)
  '          If ed.Wrap <> 0 Then                'If word wrap is on, then
  '            WrapLine = ed.CurLine
  '            WrapUp(array, ed)                '  and then up
  '            WrapDown(array, ed)              '  wrap words down
  '          End If
  '          ky = ZERO + ONE              'Clear key press
  '          ed.Changed = True
  '        End If
  '        reprint = -1                      'Set to reprint window

  '      ElseIf ky = Chr(27) Then            'User pressed Esc so clear it
  '        ky = ZERO + ONE
  '      End If

  '      'If User didn't press help,
  '      '  clear block marking flag
  '      If ky <> ZERO + Chr(59) AndAlso
  '         ky <> MOUSE_KEY$ AndAlso
  '         QPLen%(ky) <> 0 Then markBlock = 0

  '      '----- Shift Insert (Insert Clipboard contents)
  '    ElseIf ed.InsBlock OrElse
  '           (ShiftKey%() <> 0 AndAlso
  '            (ky = "0" OrElse ky = ZERO + Chr(82))) Then

  '      ed.InsBlock = False
  '      X& = 32768 'FRE("")                         'If a block has been marked
  '      '  and there is enough string
  '      '  memory
  '      If ed.UlCRow <> 0 AndAlso bBytes < X& - 1280 Then

  '        If ed.CBlock Then                 ' If its a column block,
  '          tempS = Space(numCols)
  '          If numRows > 1 Then
  '            For N = 1 To numRows        'Do each line of clipboard

  '              Temp = ed.CurLine + N - 1 'Calc text array element
  '              If Temp < arrayEnd Then
  '                'If it would be inside a line,
  '                '  insert a line
  '                ok = False
  '                If ed.CurCol <= QPLen%(array(Temp)) Then
  '                  If Blanks%(array(Temp)) < ed.CurCol + numCols - 1 Then
  '                    tempS = ""
  '                    InsertLine(array, ed)
  '                    tempS = Space(numCols)
  '                  Else
  '                    ok = True
  '                  End If
  '                End If

  '                Call MidStrRest(tempS, N, clipBd, 0)

  '                If Not Null%(tempS) <> 0 Then
  '                  If ok Then
  '                    MID$(array(Temp), ed.CurCol) = tempS
  '                  Else
  '                    'Pad with spaces to the left
  '                    array(Temp) = array(Temp) + Space(ed.CurCol - QPLen%(array(Temp)) - 1) + tempS
  '                    array(Temp) = RTrim(array(Temp))
  '                  End If
  '                End If
  '                'Update line count
  '                ed.LCount = MaxInt%(ed.LCount, Temp)
  '              End If
  '            Next
  '            'Update number of lines
  '            ed.LCount = FindLast%(array.ToArray, arrayEnd, arrayEnd)

  '          Else                           'Restore one line of text
  '            If ed.CurCol > QPLen%(array(ed.CurLine)) Then array(ed.CurLine) = array(ed.CurLine) + Space(ed.CurCol - QPLen%(array(ed.CurLine)) - 1)
  '            Call MidStrRest(tempS, 1, clipBd, 0)
  '            array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1) + tempS + Mid(array(ed.CurLine), ed.CurCol)
  '            array(ed.CurLine) = RTrim(array(ed.CurLine))
  '            'Wrap words down
  '            ed.LCount = MaxInt%(ed.LCount, ed.CurLine)
  '            If ed.Wrap <> 0 Then
  '              WrapLine = ed.CurLine
  '              WrapDown(array, ed)
  '            End If
  '          End If
  '          tempS = ""

  '        Else                              'Sentence block mode

  '          clipBytes = UBound(clipBd) * 2 + CInt((clipBd(UBound(clipBd)) < 256))
  '          If ed.Wrap <> 0 Then                'If in word wrap mode,
  '            'Compose top line of block
  '            rightSide = Mid(array(ed.CurLine), ed.CurCol)
  '            array(ed.CurLine) = Left(array(ed.CurLine), ed.CurCol - 1)

  '            Temp = ed.CurLine           'Save current line #

  '            For N = 0 To numRows        'Compose middle lines of block
  '              If N <> 0 Then
  '                'Make string to hold a line
  '                tempS = Space(StrLength(clipBd, 0, clipBytes, N))
  '                'Get the next string
  '                Get1Str(tempS, clipBd, 0, N)
  '              Else
  '                tempS = clipBd1
  '              End If

  '              tempS += Space(FnSpaces2Pad%(tempS))
  '              'If the string is nul,
  '              If QPLen%(tempS) = 0 Or Left(tempS, 1) = " " Then
  '                '  insert a new line
  '                Temp = MinInt%(Temp + 1, arrayEnd)
  '                InsertLine(array, ed)
  '                If QPLen%(tempS) = 0 Then
  '                  Temp = MinInt%(Temp + 1, arrayEnd)
  '                  InsertLine(array, ed)
  '                End If
  '              Else                    'Insert text in current line
  '                array(Temp) = array(Temp) + Space(FnSpaces2Pad%(tempS))
  '                array(Temp) = array(Temp) + tempS
  '              End If
  '            Next                        'Insert last line
  '            array(Temp) = RTrim(array(Temp) + clipBd2 + rightSide)
  '            'Update last line number
  '            ed.LCount = FindLast%(array.ToArray, arrayEnd, arrayEnd)
  '            tempS = ""                  'Erase the restore buffer

  '            WrapLine = ed.CurLine
  '            WrapAll(array, ed)               'Wrap words down

  '          Else                           'Not in word wrap mode


  '            For N = 1 To numRows + 2    'For each row of the block
  '              Temp = ed.CurLine + N - 1 'Calc line number of text
  '              If Temp <= arrayEnd Then  'Don't go off the end of array

  '                tempS = ""           'Init buffer string
  '                If N = 1 Then        'If it's the first line, use
  '                  '  the "ClipBd1$"
  '                  tempS = Space(ed.UlCCol - 1) + clipBd1
  '                  'If it's the last line use
  '                ElseIf N = numRows + 2 Then '  "ClipBd2$"
  '                  If ed.BrCCol > 0 Then tempS = clipBd2

  '                Else                 'For the body of the block,
  '                  '  get lines from far mem.
  '                  'TempS = SPACE$(StrLength%(ClipBd(0), ClipBytes&, N - 1))
  '                  tempS = Space(StrLength%(clipBd, 0, clipBytes, N - 1))
  '                  Call Get1Str(tempS, clipBd, 0, N - 1)
  '                End If
  '                tempS = RTrim(tempS)
  '                InsertLine(array, ed)
  '              End If
  '            Next
  '            tempS = ""                  'Erase the restore buffer
  '            'Update last line number
  '            ed.LCount = FindLast%(array.ToArray, arrayEnd, arrayEnd)

  '          End If
  '        End If
  '        reprint = -1                      'Set flag to re-print window
  '        ed.Changed = True                   'Text has been changed

  '      ElseIf bBytes >= X& - 1280 Then     'Wasn't enough string memory
  '        If RuntimeInformation.IsOSPlatform(OSPlatform.Windows) Then
  '          Beep()
  '        End If
  '        ed.MErr = 1                       'Set error flag
  '      End If
  '      ky = ZERO + ONE                    'Clear key press
  '    End If

  '    '********************************************************************
  '    '* End of Block Operations
  '    '********************************************************************

  '  Loop While action = 0                       'Go back for another key if not in polling mode.

  '  ed.InsStat = Insrt                           'Show the caller the Insert state

  'End Sub          'That's all folks.  And you thought editing was easy?

  '****************************************************************************
  '*********************   Start of Subroutine code   *************************
  '****************************************************************************

  '----- Adjust the size depending on monitor type, and insert mode

  Private Sub CsrSize()
    ' The below has been modified to match behavior in EDIT.COM (MS-DOS).
    If m_ed.Insert Then                          'If in insert mode,
      'LOCATE , , , BScan \ 2, BScan            '  make large size cursor
      LOCATE(, , , BScan - 2, BScan)            '  use 2 scan line cursor.
    Else                                        'Overtype mode,
      'LOCATE , , , BScan - 1, BScan            '  use 2 scan line cursor.
      LOCATE(, , , 0, BScan)                     '  make large size cursor
    End If
  End Sub

  '----- Display the current line number on the status line
  'Private Sub DisplayLineNumber(ByRef ed As EditInfo)
  '  LOCATE(, , 0)                                'Turn the cursor off
  '  REM RSet CurLine$ = Mid(Str(Ed.CurLine), 2)   'Put number in temp string
  '  Call QPrintRC(CurLine$, ed.TSRow + ed.Rows, ed.LSCol, 63)

  '  '----- Update the vertical scroll bar pointer
  '  If ed.LCount >= ed.Rows Then                'Calc the Vertical position %
  '    VrtPtr = MinInt%(ed.CurLine * (ed.Rows - 3) \ ed.LCount, ed.Rows - 3)
  '  Else
  '    VrtPtr = 0
  '  End If
  '  'Erase old pointer
  '  Call QPrintRC(Chr(176), ed.TSRow + OldVrtPtr + 1, ed.LSCol + ed.Wide, 112)
  '  'Print the new one
  '  Call QPrintRC(Chr(8), ed.TSRow + VrtPtr + 1, ed.LSCol + ed.Wide, 112)
  '  OldVrtPtr = VrtPtr                          'Save old pointer for later
  'End Sub

  '----- Display the current column number on the status line
  'Private Sub DisplayColNumber(ByRef ed As EditInfo)
  '  LOCATE(, , 0)                                'Turn the cursor off
  '  REM RSet CurCol$ = Mid(Str(Ed.CurCol), 2)     'Put number in temp string
  '  Call QPrintRC(CurCol$, ed.TSRow + ed.Rows, ed.LSCol + 5, 63)

  '  '----- Update the horizontal scroll bar pointer
  '  If ed.Wrap <> 0 Then                             'Calc pointer position
  '    HorPtr = ed.Wrap + 1
  '  Else
  '    HorPtr = 256
  '  End If
  '  HorPtr = MinInt%(ed.CurCol * (ed.Wide - 10) \ HorPtr, ed.Wide - 11)

  '  Call QPrintRC(Chr(176), ed.TSRow + ed.Rows, ed.LSCol + 9 + OldHorPtr, 112)
  '  Call QPrintRC(Chr(8), ed.TSRow + ed.Rows, ed.LSCol + 9 + HorPtr, 112)
  '  OldHorPtr = HorPtr                          'Save pointer for next pass

  'End Sub

  Private Sub WindowLimits()
    If m_ed.CurCol >= m_ed.LC + m_ed.Wide Then
      m_ed.LC = m_ed.CurCol - m_ed.Wide + 1
      reprint = True
    ElseIf m_ed.CurCol < m_ed.LC Then
      m_ed.LC = MaxInt%(1, m_ed.CurCol - m_ed.Wide)
      reprint = True
    End If
    If m_ed.CurLine >= m_ed.TL + m_ed.Rows Then
      m_ed.TL = m_ed.CurLine - m_ed.Rows + 1
      reprint = True
    ElseIf m_ed.CurLine < m_ed.TL Then
      m_ed.TL = m_ed.CurLine
      reprint = True
    End If
  End Sub

  '----- Move the cursor down a line
  Private Sub CursorDown(array As List(Of String), ByRef ed As EditInfo)
    If ed.CurLine >= arrayEnd Then ed.CurLine = arrayEnd
    'if we're at the bottom of the screen
    Dim temp = (ed.CurLine - ed.TL + 1) - ed.Rows
    If temp > 0 Then
      ed.TL += temp          '  show the top line being one higher,
      X = ed.TSRow + ed.Rows - 1
      Call HideCursor()               'Scroll the window up
      Call ScrollU(ed.TSRow, ed.LSCol, X, ed.LSCol + ed.Wide - 1, temp, -1)
      For N = 1 To temp
        Call QPrintRC(Mid(array(ed.CurLine - N + 1), ed.LC, ed.Wide), X - N + 1, ed.LSCol, -1)
      Next
      Call ShowCursor()
    End If
  End Sub

  '----- Move the cursor up a line
  Private Sub CursorUp(array As List(Of String), ByRef ed As EditInfo)
    If ed.CurLine < 1 Then ed.CurLine = 1 'ignore if already on first line

    Dim temp = ed.TL - ed.CurLine
    If temp > 0 Then                    'if we're at the top of the screen
      ed.TL -= temp             '  show the top line being one less,
      Call HideCursor()                  'Scroll the window Down
      ScrollD(ed.TSRow, ed.LSCol, ed.TSRow + ed.Rows - 1, ed.LSCol + ed.Wide - 1, temp, -1)
      For N = temp To 1 Step -1
        Call QPrintRC(Mid(array(ed.CurLine + N - 1), ed.LC, ed.Wide), ed.TSRow + N - 1, ed.LSCol, ed.AColor)
      Next
      Call ShowCursor()
    End If
  End Sub

  '----- Move cursor left
  Private Sub CursorLeft(array As List(Of String), ByRef ed As EditInfo)
    If ed.CurCol < 1 Then ed.CurCol = 1
    Dim temp = ed.LC - ed.CurCol
    If temp > 0 Then                            'If off the left of screen
      ed.LC -= temp                     'Decrement window column
      Call HideCursor()
      Call ScrollR(ed.TSRow, ed.LSCol, ed.TSRow + ed.Rows - 1, ed.LSCol + ed.Wide - 1, temp, -1)
      LOCATE(ed.TSRow, ed.LSCol)
      Call APrint0(array.ToArray, ed.TL, ed.Rows, ed.LC, temp, -1)
      Call ShowCursor()
    End If
  End Sub

  '----- Move cursor right
  Private Sub CursorRight(array As List(Of String), ByRef ed As EditInfo)
    If ed.CurCol > 255 Then ed.CurCol = 255
    'If off right of window,
    Dim temp = (ed.CurCol - ed.LC + 1) - ed.Wide
    If temp > 0 Then
      ed.LC += temp                     '  increment window column
      Call HideCursor()
      Call ScrollL(ed.TSRow, ed.LSCol, ed.TSRow + ed.Rows - 1, ed.LSCol + ed.Wide - 1, temp, -1)
      LOCATE(ed.TSRow, ed.LSCol + ed.Wide - temp)
      Call APrint0(array.ToArray, ed.TL, ed.Rows, ed.LC + ed.Wide - temp, temp, -1)
      Call ShowCursor()
    End If
  End Sub

  ''----- Wrap all text to current right margin
  'Private Sub WrapAll(array As List(Of String), ByRef ed As EditInfo, wrapLine As Integer)
  '  Do Until wrapLine > ed.LCount               'Stop at last used element
  '    If QPLen%(array(wrapLine)) <> 0 Then        'If it's not a blank line,
  '      WrapUp(array, ed, wrapLine)                         '  then up.
  '      WrapDown(array, ed, wrapLine)                       '  wrap paragraph down and
  '      wrapLine = Lin + 1                   'Look at line past paragraph
  '    Else                                    '  "Lin" is set in "WrapUp"
  '      wrapLine += 1              'Line was blank, look at next
  '    End If
  '  Loop
  'End Sub

  ''----- Wrap words down to form a paragraph
  'Private Sub WrapDown(array As List(Of String), ByRef ed As EditInfo, wrapLine As Integer)
  '  Lin = wrapLine                               'Make temp copy of line #
  '  LinLen = QPLen%(array(Lin))                'Get the length of cur. line
  '  P = InStr(array(Lin), par$)                'Look for a paragraph marker
  '  'Do lines that are too long
  '  Do While LinLen > ed.Wrap

  '    If P > 0 And P <= ed.Wrap + 1 Then       'If marker is in range,
  '      B = P                                 '  set cutoff to marker
  '      array(Lin) = Left(array(Lin), P - 1) + Mid(array(Lin), P + 1)
  '    Else                                     'Starting at the right margin,
  '      'look backwards for a blank
  '      I = QInstrB%(ed.Wrap + 1, array(Lin), " ")
  '      If I = 0 Then I = ed.Wrap + 1         'If no blanks, chop it off

  '      B = Blanks%(Mid(array(Lin), I)) + I 'Now look fwd. for non-blank
  '    End If


  '    If B <= LinLen Then                      'If we didn't go past the end,
  '      Lin += 1                         '  increment the line number
  '      If Lin > arrayEnd Then                'Past end?  Bail out
  '        Lin -= 1
  '        ed.MErr = 2
  '        Exit Do
  '      End If

  '      LinLen = QPLen%(array(Lin))          'Get the length of new line
  '      'If this is a blank line or
  '      '  starts with a space
  '      If LinLen = 0 OrElse Blanks%(array(Lin)) <> 0 OrElse ASCII%(array(Lin)) = 20 Then
  '        Dim temp = Lin                         'Insert remainder of last line
  '        tempS = Mid(array(Lin - 1), B)   '  into a new line
  '        InsertLine(array, ed, temp)
  '      Else                                  'Add remainder of prev. line
  '        '  to this line
  '        array(Lin) = Mid(array(Lin - 1), B) + Space(FnSpaces2Pad%(array(Lin - 1))) + array(Lin)
  '      End If
  '      'If we just wrapped current
  '      If Lin - 1 = ed.CurLine Then          '  line and the cursor was
  '        If ed.CurCol >= B Then             '  within the wrapped words,
  '          ed.CurCol = ed.CurCol - B + 1   '  move the cursor to the new
  '          ed.LC = ((ed.CurCol \ ed.Wide) * ed.Wide) + 1 '  line at the end of the
  '          ed.CurLine = MinInt%(Lin, arrayEnd)          '  wrapped words
  '          ed.TL = MaxInt%(ed.TL, ed.CurLine - ed.Rows + 1)
  '        End If
  '      End If

  '      LinLen = QPLen%(array(Lin))          'Get length of the new line
  '      'Trim the prev. line
  '      array(Lin - 1) = RTrim(Left(array(Lin - 1), B - 1))
  '      reprint = True                          'Set flag to re-print window

  '    Else                                     'Words weren't found past wrap
  '      Lin += 1                         'Increment the line number
  '      If Lin > arrayEnd Then
  '        Lin -= 1
  '        ed.MErr = 2
  '        Exit Do                            'Past end?  Bail out
  '      End If
  '      LinLen = QPLen%(array(Lin))          'Get its length
  '    End If

  '    P = InStr(array(Lin), par$)             'Look for a paragraph marker

  '  Loop                                        'Process the new line

  '  If P <> 0 Then
  '    array(Lin) = Left(array(Lin), P - 1) + Mid(array(Lin), P + 1)
  '  End If

  'End Sub

  ''----- Wrap words Up to re-form a paragraph
  'Private Sub WrapUp(array As List(Of String), ByRef ed As EditInfo, wrapLine As Integer)

  '  Lin = wrapLine                              'Make temp copy of line #
  '  '----- If the line isn't nul and current line isn't past the end and the
  '  '----- current line doen't have a paragraph marker on the end.
  '  Do Until Null%(array(Lin)) <> 0 OrElse Lin >= ed.LCount

  '    P = InStr(array(Lin + 1), par$)
  '    If P <> 0 Then
  '      array(Lin + 1) = Left(array(Lin + 1), P - 1) + Mid(array(Lin + 1), P + 1)
  '    End If

  '    LinLen = QPLen%(array(Lin + 1))        'Get the length of next line
  '    'If it has Par marker or is
  '    '  blank, bail out
  '    If Blanks%(array(Lin + 1)) <> 0 Then Exit Do

  '    Dim temp = FnSpaces2Pad%(array(Lin))

  '    If Lin = wrapLine And ed.Wrap > WrapWas Then
  '      Extra = WrapWas - QPLen%(array(Lin)) - Temp + 1
  '      GetWordPos(array)
  '      If I <> 0 Then                            'Words were found to fill so,
  '        If InStr(Left(array(Lin + 1), I - 1), " ") <> 0 Then
  '          Exit Do
  '        End If
  '      End If
  '    End If

  '    Extra = ed.Wrap - QPLen%(array(Lin)) - Temp + 1

  '    GetWordPos(array)

  '    If I <> 0 Then

  '      If Lin = wrapLine And ed.Wrap <= WrapWas Then
  '        If InStr(Left(array(Lin + 1), I - 1), " ") <> 0 Then ' OR I = LinLen THEN
  '          Exit Do
  '        End If
  '      End If
  '      'Add words from next line
  '      array(Lin) = array(Lin) + Space(Temp) + Left(array(Lin + 1), I)
  '      'Now delete them from next
  '      array(Lin + 1) = LTrim(Mid(array(Lin + 1), I + 1))
  '      reprint = True                         'Set "RePrint" flag

  '      If Len(array(Lin + 1)) = 0 Then     'If nothing is left of next
  '        '  line, delete it.
  '        temp = Lin + 1
  '        DeleteLine(array, ed, temp)
  '        Lin -= 1
  '        If Lin + 1 > ed.LCount Then Exit Do 'Off the end, bail out
  '      End If
  '    End If

  '    Lin += 1
  '  Loop

  'End Sub

  'Private Sub GetWordPos(array As List(Of String))
  '  If Extra > 0 Then                           'If there is extra space,
  '    If Extra >= LinLen Then
  '      I = LinLen
  '    Else
  '      B = Extra
  '      If Mid(array(Lin + 1), B, 1) <> " " Then
  '        B = QInstrB%(B, array(Lin + 1), " ")
  '      End If

  '      For I = B To 1 Step -1
  '        If Mid(array(Lin + 1), I, 1) <> " " Then Exit For
  '      Next
  '    End If
  '  Else
  '    I = 0
  '  End If
  'End Sub

  '----- Insert an element into the text array at "Temp"
  Private Sub InsertLine(array As List(Of String), ByRef ed As EditInfo, temp As Integer, tempS As String)
    If temp - 1 <= ed.LCount Then
      ed.LCount = MinInt%(ed.LCount + 1, arrayEnd)
      Call InsertStr(array.ToArray, temp - 2, tempS, ed.LCount - temp)
    Else
      ed.LCount = temp
      array(temp - 1) = tempS
    End If
  End Sub

  '----- Delete an element from the text array
  Private Shared Sub DeleteLine(array As List(Of String), ByRef ed As EditInfo, temp As Integer)
    If temp <= ed.LCount Then
      If temp < ed.LCount Then
        Call DeleteStr(array.ToArray, temp - 1, ed.LCount - temp)
      End If
      array(ed.LCount) = ""
      ed.LCount -= 1                'Update last line number
    End If
  End Sub

  '----- Resize and Draw the window frame
  Private Sub InitWindow()

    ' Calculate the bounds of the window
    WC(0, 0) = m_ed.TSRow                 'Top line of window
    WC(1, 0) = m_ed.LSCol                 'Left margin of window
    WC(2, 0) = WC(0, 0) + m_ed.Rows - 1   'Bottom line of window
    WC(3, 0) = WC(1, 0) + m_ed.Wide - 1   'Right margin of window

    'Save new bounds for next time
    WC(0, 1) = WC(0, 0)
    WC(1, 1) = WC(1, 0)
    WC(2, 1) = WC(2, 0)
    WC(3, 1) = WC(3, 0)

    'Temp buffer used for printing
    LineBuf$ = Space(m_ed.Wide)

    'Set flag to print the window
    reprint = True

    'Make sure the cursor stays in the window
    m_ed.CurLine = MinInt%(m_ed.CurLine, m_ed.TL + m_ed.Rows - 1)
    m_ed.CurCol = MinInt%(m_ed.CurCol, m_ed.LC + m_ed.Wide - 1)

    'Init scroll pointers
    OldVrtPtr = 0
    OldHorPtr = 0

  End Sub

  ''----- Restore the screen and free up memory
  'Private Sub CleanUp()

  '  Call MScrnRest(1, 1, ScrRows, 80, ScrBuf) 'Restore the screen
  '  Erase ScrBuf, Ruler                         'Clean up memory
  '  LineBuf = ""
  '  LOCATE(, , , BScan - 1, BScan)

  'End Sub

  Private Shared Function FnSpaces2Pad(text As String) As Integer
    Static Temp As Integer
    Select Case ASCII%(Right(text, 1))
      Case 46, 33, 63          '".", "!", "?"
        Temp = 2
      Case 32, -1              'Space
        Temp = 0
      Case Else
        Temp = 1
    End Select
    FnSpaces2Pad% = Temp
  End Function

  Private Sub LoadHelp(Hlp$())
    Throw New NotImplementedException
  End Sub

#Region "QPP"

  Private Sub Get1Str(v1 As String, v2() As Integer, v3 As Integer, v4 As Integer)
    Throw New NotImplementedException
  End Sub

  Private Function StrLength(array() As Integer, element As Integer, numBytes As Long, strNumber As Integer) As Integer
    Throw New NotImplementedException
  End Function

  Private Sub MidStrRest(v1 As String, v2 As Integer, v3() As Integer, v4 As Integer)
    Throw New NotImplementedException
  End Sub

  Private Function ShiftKey() As Integer
    Return 0
  End Function

  Private Sub StringSave(Array$(), v1 As Integer, v2() As Integer, v3 As Integer, v4 As Integer)
    Throw New NotImplementedException
  End Sub

  Private Sub MidStrSave(Array$(), v1 As Integer, v2 As Integer, v3 As Integer, v4 As Integer, v5() As Integer, v6 As Integer)
    Throw New NotImplementedException
  End Sub

  Private Sub MouseTrap(ulRow As Integer, ulCol As Integer, lrrow As Integer, lrcol As Integer)
    Throw New NotImplementedException
  End Sub

  Private Function PeekBuf() As Integer
    Throw New NotImplementedException
  End Function

  Private Sub Pause(v1 As Integer)
    Throw New NotImplementedException
  End Sub

#End Region

End Class
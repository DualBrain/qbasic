Imports QB.Video
Imports VbPixelGameEngine

Public Class DocumentPanel
  Inherits PgeX
  Implements IContext

  Public Class Location
    Public Sub New()
    End Sub
    Public Sub New(row As Integer, column As Integer)
      Me.Row = row : Me.Column = column
    End Sub
    Public Row As Integer
    Public Column As Integer
  End Class

  'Private Structure BlkCoords                    ' Structure for Block coordinates
  '  Dim B As Integer                             ' Block coordinates
  '  Dim PB As Integer                            ' Previous block coordinates
  '  Dim P As Integer                             ' Paint coordinates
  '  Dim PP As Integer                            ' Previous paint coordinates
  'End Structure

  'Private Structure EditInfo

  'Public Rows As Integer             ' Text Window Rows
  'Public Wide As Integer             ' Text Window Columns
  'Public Wrap As Integer             ' Right Margin for Word Wrap (0 for No Wrap)
  'Public AColor As Integer           ' Window Color
  'Public Frame As Boolean            ' Display Frame Flag

  'Public LSCol As Integer            ' Left Screen Column of editable window
  'Public LC As Integer               ' Left Text Column
  'Public CurCol As Integer           ' Current text column
  'Public TSRow As Integer            ' Top Screen Row of editable window
  'Public TL As Integer               ' Top Text Line
  'Public CurLine As Integer          ' Current text Line

  '' Contains the "highlight"
  'Public UlCRow As Integer           ' Upper Left Block Row
  'Public UlCCol As Integer           ' Upper Left Block Column
  'Public BrCRow As Integer           ' Lower Right Block Row
  'Public BrCCol As Integer           ' Lower Right Block Column
  'Public CBlock As Boolean           ' Column Block Flag

  'Public WasMarked As Boolean        ' Flag that something has been marked
  'Public DelBlock As Boolean         ' Flag to Delete highlighted block
  'Public InsBlock As Boolean         ' Flag to Paste Buffer contents
  'Public Text2Paste As Boolean       ' Flag that something is in the Paste buffer
  'Public CopyBlock As Boolean        ' Flag to paste the buffer now

  'Public Presses As Integer          ' Mouse presses.                Read Only!
  'Public MRow As Integer             ' Mouse Screen Row.             Read Only!
  'Public MCol As Integer             ' Mouse Screen Column.          Read Only!

  'Public UnKnownKey As Boolean       ' Flag, True if key pressed but not handled
  'Public Insert As Boolean          ' Insert State (False = Overtype, True = Insert)
  'Public LCount As Integer           ' Number of text lines.         Read Only!
  'Public MErr As Integer             ' Error flag  (1 = Out of memory, 2 = Too many lines)

  'End Structure

  'Private Const NumPad As String = "12346789"              ' Shifted arrow keys
  'Private Const NumPad2 As String = "stOPQKMGHK"           ' Unshifted arrow keys
  'Private Const SkipTbl As String = " ^*()[]-=+,.<>/\"     ' Word delimiters
  'Private Const ZERO As String = ChrW(0)
  'Private Const ONE As String = ChrW(1)

  Public Property BlockTopLeft As Location
  Public Property BlockBottomRight As Location

  Public Property EditorTop As Integer = 2
  Public Property EditorLeft As Integer = 1
  Public Property EditorWidth As Integer = 80
  Public Property EditorHeight As Integer = 21

  Public Property Title As String = "Untitled"
  Public Property Text As String
    Get
      Dim result = ""
      For Each line In m_document
        result &= line & vbCrLf
      Next
      Return result
    End Get
    Set(value As String)
      m_document.Clear()
      Dim lines = value.Split(vbLf)
      For Each line In lines
        m_document.Add(line)
      Next
    End Set
  End Property

  Public Property WindowColor As Integer
  Public Property TabSize As Integer = 2
  Public Property TextRows As Integer
  Public Property TextColumns As Integer
  Public Property LeftScreenColumn As Integer
  Public Property LeftTextColumn As Integer = 1
  Public Property TopScreenRow As Integer = 2
  Public Property TopTextLine As Integer = 1
  Public Property CurrentColumn As Integer = 1
  Public Property CurrentLine As Integer = 1
  Public Property LineCount As Integer

  Public Property Insert As Boolean = True
  Public Property ScrollBars As Boolean = True
  Public Property Focused As Boolean = True
  Public Property Visible As Boolean = True

  Public Property Changed As Boolean

  Private ReadOnly m_document As New List(Of String)

  Public ReadOnly Property CursorRow As Integer Implements IContext.CursorRow
    Get
      Return TopScreenRow + CurrentLine - TopTextLine
    End Get
  End Property

  Public ReadOnly Property CursorCol As Integer Implements IContext.CursorCol
    Get
      Return LeftScreenColumn + (CurrentColumn - LeftTextColumn)
    End Get
  End Property

  Public ReadOnly Property DocumentRow As Integer
    Get
      Return CurrentLine
    End Get
  End Property

  Public ReadOnly Property DocumentCol As Integer
    Get
      Return CurrentColumn
    End Get
  End Property

  Public Sub New()

    TextRows = EditorHeight - 2
    TextColumns = EditorWidth - 2
    Insert = True
    TabSize = 2
    WindowColor = OneColor(8, 1)
    TabSize = MaxInt(TabSize, 1)
    Clear()

    Dim BScan = 12
    LOCATE(, , 0, BScan - 2, BScan)

    WindowLimits()

    Dim arrayEnd = m_document.Count - 1
    LineCount = FindLast(m_document.ToArray, arrayEnd, arrayEnd)

    CsrSize()

  End Sub

  Public Sub Render() Implements IContext.Render

    If Not Visible Then Exit Sub

    'Dim ScrRows = 25

    TextRows = EditorHeight - 2
    TextColumns = EditorWidth - 2

    TopScreenRow = EditorTop + 1 'CSRLIN()       'Calc the top line of window
    LeftScreenColumn = EditorLeft + 1 'POS(0)       'Calc Left margin of window
    'Both are to inside of frame
    'TextRows = MinInt(MaxInt(TextRows, 3), ScrRows - EditorTop + 1)
    'TextColumns = MinInt(MaxInt(TextColumns, 11), 80 - EditorLeft + 1)

    If TopTextLine < 1 Then TopTextLine = 1             'Top of window row number
    If LeftTextColumn < 1 Then LeftTextColumn = 1             'Set window column to 1
    If CurrentLine < 1 Then CurrentLine = 1   'make top line the cursor col.
    If CurrentColumn < 1 Then CurrentColumn = 1     'establish cursor column at 1

    'Make sure the cursor stays in the window
    'CurrentLine = MinInt(CurrentLine, TopTextLine + TextRows - 1)
    'CurrentColumn = MinInt(CurrentColumn, LeftTextColumn + TextColumns - 1)

    Dim lrRow = EditorTop + EditorHeight - 1
    Dim lrCol = EditorLeft + EditorWidth - 1

    ' Box
    Box0(EditorTop, EditorLeft, EditorTop + EditorHeight - 1, lrCol, 1, OneColor(8, 1))

    If EditorTop > 2 Then
      HLine(EditorTop, EditorLeft, lrCol, 1, OneColor(8, 1))
    End If

    ' Content Area
    ClearScr0(EditorTop + 1, EditorLeft + 1, lrRow - 1, lrCol - 1, OneColor(8, 1))

    LOCATE(TopScreenRow, LeftScreenColumn, 0)
    Call HideCursor()
    Call APrint0(m_document.ToArray,
                 TopTextLine - 1,
                 TextRows,
                 LeftTextColumn,
                 TextColumns,
                 WindowColor)
    LOCATE(TopScreenRow + CurrentLine - TopTextLine, LeftScreenColumn + (CurrentColumn - LeftTextColumn), 1)
    Call ShowCursor()

    ' Expand/Collapse Tool
    QPrintRC(ChrW(180), EditorTop, lrCol - 4, OneColor(8, 1))
    QPrintRC(ChrW(24), EditorTop, lrCol - 3, OneColor(1, 8))
    QPrintRC(ChrW(195), EditorTop, lrCol - 2, OneColor(8, 1))

    ' Scrollbars
    If ScrollBars Then
      'TODO: Determine current position within the scrollbars...
      If EditorHeight > 5 Then VScrollBar(EditorTop + 1, lrCol, lrRow - 2, 1)
      If EditorHeight > 3 Then HScrollBar(lrRow - 1, EditorLeft + 1, lrCol - 1, 1)
    End If

    ' Title
    Dim title = $" {Me.Title} "
    Dim titleOffset = (EditorWidth - title.Length) \ 2
    QPrintRC(title, EditorTop, EditorLeft + titleOffset, OneColor(1, 8))

  End Sub

  Public Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys
    Editor(keys, capsLock, ctrl, alt, shift)
    Return True
  End Function

  Public Sub Clear()
    m_document.Clear()
    m_document.Add("")
    CurrentLine = 1
    CurrentColumn = 1
  End Sub

  Private Sub Editor(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean)

    ' Handle keyboard/mouse
    If keys IsNot Nothing Then
      For Each key In keys
        If ctrl AndAlso Not alt AndAlso Not shift Then
          Select Case key
            Case ConsoleKey.A ' Word Left (WordStar)
            Case ConsoleKey.C : CursorPageDownAction() ' Page down (WordStar)
            Case ConsoleKey.D : CursorRightAction() ' Cursor right (WordStar)
            Case ConsoleKey.E : CursorUpAction() ' Cursor up (WordStar)
            Case ConsoleKey.F ' Word Right (WordStar)
            Case ConsoleKey.G ' Delete selected text / Delete one character at the cursor (WordStar)
            Case ConsoleKey.H : BackspaceAction() ' Delete one character to the left of the cursor (WordStar)
            Case ConsoleKey.J : CursorNextLineAction() ' Cursor to beginning of next line (WordStar)
            Case ConsoleKey.K '...
              'm_isK = True
            Case ConsoleKey.L ' Repeat find for same text
            Case ConsoleKey.N : InsertLineAboveAction() ' Insert Line above
            Case ConsoleKey.P ' Insert special characters...
              'm_isCtrlP = True
            Case ConsoleKey.Q '... 
              'm_isQ = True
            Case ConsoleKey.R : CursorPageUpAction() ' Page up (WordStar)
            Case ConsoleKey.S : CursorLeftAction() ' Cursor left (WordStar)
            Case ConsoleKey.T ' Delete the rest of the word the cursor is on (WordStar)
            Case ConsoleKey.V ' Switch between insert and overstrike modes (WordStar)
            Case ConsoleKey.X : CursorDownAction() ' Cursor dn (WordStar)
            Case ConsoleKey.Y ' Cut current line
            Case ConsoleKey.Enter : CursorNextLineAction() ' Cursor to beginning of next line (skipping spaces)
            Case ConsoleKey.Insert ' Copy to Clipboard
            Case ConsoleKey.LeftArrow ' Word Left
            Case ConsoleKey.RightArrow ' Word Right
            Case ConsoleKey.UpArrow, ConsoleKey.W : ScrollUpAction() ' Scroll up, Scroll up (WordStar)
            Case ConsoleKey.DownArrow, ConsoleKey.Z : ScrollDownAction() ' Scroll dn, Scroll dn (WordStar)
            Case ConsoleKey.PageUp ' Left one window
            Case ConsoleKey.PageDown ' Right one window
            Case ConsoleKey.Home : DocumentHomeAction()
            Case ConsoleKey.End : DocumentEndAction()
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

              If Insert Then
                If CurrentColumn <= m_document(CurrentLine - 1).Length Then
                  Dim leftSide = If(CurrentColumn > 0, m_document(CurrentLine - 1).Substring(0, CurrentColumn - 1), "")
                  Dim rightSide = If(CurrentColumn <= m_document(CurrentLine - 1).Length, m_document(CurrentLine - 1).Substring(CurrentColumn - 1), "")
                  m_document(CurrentLine - 1) = leftSide & GetChar(key, capsLock, shift) & rightSide
                Else
                  m_document(CurrentLine - 1) &= GetChar(key, capsLock, shift)
                End If
                CurrentColumn += 1
                Changed = True
              Else
                Stop
              End If

            Case ConsoleKey.Tab : TabAction()
            Case ConsoleKey.LeftArrow : CursorLeftAction()
            Case ConsoleKey.RightArrow : CursorRightAction()
            Case ConsoleKey.UpArrow : CursorUpAction()
            Case ConsoleKey.DownArrow : CursorDownAction()
            Case ConsoleKey.Home : CursorHomeAction()
            Case ConsoleKey.End : CursorEndOfLineAction()
            Case ConsoleKey.PageUp : CursorPageUpAction()
            Case ConsoleKey.PageDown : CursorPageDownAction()
            Case ConsoleKey.Insert : ToggleInsertAction()
            Case ConsoleKey.Backspace : BackspaceAction()
            Case ConsoleKey.Delete : DeleteAction()
            Case ConsoleKey.Enter : EnterAction()
            Case ConsoleKey.Escape
            Case Else
          End Select
        End If
      Next
    End If

  End Sub

#Region "Actions"

  Private Sub BackspaceAction()
    If CurrentColumn = 1 Then
      If CurrentLine > 1 AndAlso CurrentLine <= m_document.Count Then
        Dim l = m_document(CurrentLine - 2).Length
        m_document(CurrentLine - 2) = m_document(CurrentLine - 2) + LTrim(m_document(CurrentLine - 1))
        m_document.RemoveAt(CurrentLine - 1)
        CurrentLine -= 1 : CurrentColumn = l + 1
        Changed = True
      End If
    Else
      If CurrentColumn <= m_document(CurrentLine - 1).Length + 1 Then
        Dim leftSide = If(CurrentColumn > 1, m_document(CurrentLine - 1).Substring(0, CurrentColumn - 2), "")
        Dim rightSide = If(CurrentColumn <= m_document(CurrentLine - 1).Length, m_document(CurrentLine - 1).Substring(CurrentColumn - 1), "")
        m_document(CurrentLine - 1) = leftSide & rightSide
        Changed = True
      End If
      CurrentColumn -= 1
    End If
  End Sub

  Private Sub CursorDownAction()
    CurrentLine += 1 : CursorDown()
  End Sub

  Private Sub CursorEndOfLineAction()
    If CurrentLine <= m_document.Count Then
      CurrentColumn = m_document(CurrentLine - 1).Length + 1
    Else
      CurrentColumn = 1
    End If
  End Sub

  Private Sub CursorHomeAction()
    CurrentColumn = 1
  End Sub

  Private Sub CursorLeftAction()
    CurrentColumn -= 1 : CursorLeft()
  End Sub

  Private Sub CursorNextLineAction()
    CurrentLine += 1 : CursorDown()
    Dim offset = 0
    If CurrentLine < m_document.Count Then
      For Each ch In m_document(CurrentLine - 1)
        If ch = " "c Then offset += 1 Else Exit For
      Next
    End If
    CurrentColumn = 1 + offset
  End Sub

  Private Sub CursorPageDownAction()
    If TopTextLine <= (m_document.Count - 1) - (TextRows - 1) Then
      TopTextLine += TextRows - 1                    'calc top line of next page
      CurrentLine += TextRows - 1               'update current line
    End If
  End Sub

  Private Sub CursorPageUpAction()
    If TopTextLine > 1 Then                           'ignore if already at the top
      Dim x = TopTextLine                             'save Ed.TL for a moment
      TopTextLine = MaxInt(1, TopTextLine - TextRows)
      x -= TopTextLine                                'calc dif. between new and old
      CurrentLine -= x                           'don't move cursor unless we have to
    End If
  End Sub

  Private Sub CursorRightAction()
    CurrentColumn += 1 : CursorRight()
  End Sub

  Private Sub CursorUpAction()
    CurrentLine -= 1 : CursorUp()
  End Sub

  Private Sub DocumentEndAction()
    Dim pages = m_document.Count \ TextRows
    TopTextLine = pages * TextRows
    CurrentLine = m_document.Count : CurrentColumn = 1
  End Sub

  Private Sub DocumentHomeAction()
    CurrentLine = 1 : CurrentColumn = 1 : TopTextLine = 1
  End Sub

  Private Sub DeleteAction()
    If CurrentColumn > m_document(CurrentLine - 1).Length Then
      If CurrentLine < m_document.Count Then
        m_document(CurrentLine - 1) = m_document(CurrentLine - 1) + LTrim(m_document(CurrentLine))
        m_document.RemoveAt(CurrentLine)
        Changed = True
      End If
    Else
      m_document(CurrentLine - 1) = Left(m_document(CurrentLine - 1), CurrentColumn - 1) + Mid(m_document(CurrentLine - 1), CurrentColumn + 1)
      Changed = True
    End If
  End Sub

  Private Sub EnterAction()
    If CurrentLine >= m_document.Count Then
      m_document.Add("")
    Else
      m_document.Insert(If(CurrentColumn = 1, CurrentLine - 1, CurrentLine), "")
    End If
    If CurrentColumn > 1 AndAlso CurrentColumn <= m_document(CurrentLine - 1).Length Then
      m_document(CurrentLine) = m_document(CurrentLine - 1).Substring(CurrentColumn - 1)
      m_document(CurrentLine - 1) = m_document(CurrentLine - 1).Substring(0, CurrentColumn - 1)
    End If
    CurrentLine += 1 : CurrentColumn = 1
    Changed = True
  End Sub

  Private Sub InsertLineAboveAction()
    If CurrentLine >= m_document.Count Then
      m_document.Add("")
    Else
      m_document.Insert(If(CurrentColumn = 1, CurrentLine - 1, CurrentLine), "")
    End If
    If CurrentColumn > 1 AndAlso CurrentColumn <= m_document(CurrentLine - 1).Length Then
      m_document(CurrentLine) = m_document(CurrentLine - 1).Substring(CurrentColumn - 1)
      m_document(CurrentLine - 1) = m_document(CurrentLine - 1).Substring(0, CurrentColumn - 1)
    End If
    '---- Pretty much identical to Enter (in insert mode), but don't move the cursor... 
    'CurLine += 1 : CurCol = 1
    Changed = True
  End Sub

  Private Sub ScrollDownAction()
    If CurrentLine < m_document.Count Then TopTextLine += 1
    If CurrentLine < TopTextLine Then CurrentLine = TopTextLine
  End Sub

  Private Sub ScrollUpAction()
    If TopTextLine > 1 Then TopTextLine -= 1
    If CurrentLine > (TopTextLine - 1) + TextRows - 1 Then CurrentLine = (TopTextLine - 1) + TextRows - 1
  End Sub

  Private Sub TabAction()
    If CurrentColumn <= m_document(CurrentLine - 1).Length Then
      Dim leftSide = If(CurrentColumn > 0, m_document(CurrentLine - 1).Substring(0, CurrentColumn - 1), "")
      Dim rightSide = If(CurrentColumn <= m_document(CurrentLine - 1).Length, m_document(CurrentLine - 1).Substring(CurrentColumn - 1), "")
      m_document(CurrentLine - 1) = leftSide & Space(TabSize) & rightSide
      Changed = True
    End If
    CurrentColumn += TabSize
  End Sub

  Private Sub ToggleInsertAction()
    Insert = Not Insert
    CsrSize()
  End Sub

#End Region

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

  Private Sub CsrSize()
    Dim BScan = 12
    ' The below has been modified to match behavior in EDIT.COM (MS-DOS).
    If Insert Then                          'If in insert mode,
      'LOCATE , , , BScan \ 2, BScan            '  make large size cursor
      LOCATE(, , , BScan - 2, BScan)            '  use 2 scan line cursor.
    Else                                        'Overtype mode,
      'LOCATE , , , BScan - 1, BScan            '  use 2 scan line cursor.
      LOCATE(, , , 0, BScan)                     '  make large size cursor
    End If
  End Sub

  Private Sub WindowLimits()
    If CurrentColumn >= LeftTextColumn + TextColumns Then
      LeftTextColumn = CurrentColumn - TextColumns + 1
    ElseIf CurrentColumn < LeftTextColumn Then
      LeftTextColumn = MaxInt(1, CurrentColumn - TextColumns)
    End If
    If CurrentLine >= TopTextLine + TextRows Then
      TopTextLine = CurrentLine - TextRows + 1
    ElseIf CurrentLine < TopTextLine Then
      TopTextLine = CurrentLine
    End If
  End Sub

  Private Sub CursorDown()
    If CurrentLine > m_document.Count - 1 Then CurrentLine = m_document.Count
    Dim temp = (CurrentLine - TopTextLine + 1) - (TextRows - 1)
    If temp > 0 Then                          'if we're at the bottom of the screen
      TopTextLine += temp                         '  show the top line being one higher,
    End If
  End Sub

  Private Sub CursorUp()
    If CurrentLine < 1 Then CurrentLine = 1 'ignore if already on first line
    Dim temp = TopTextLine - CurrentLine
    If temp > 0 Then                          'if we're at the top of the screen
      TopTextLine -= temp                         '  show the top line being one less,
    End If
  End Sub

  Private Sub CursorLeft()
    If CurrentColumn < 1 Then CurrentColumn = 1
    Dim temp = LeftTextColumn - CurrentColumn
    If temp > 0 Then                          'If off the left of screen
      LeftTextColumn -= temp                         'Decrement window column
    End If
  End Sub

  Private Sub CursorRight()
    If CurrentColumn > 255 Then CurrentColumn = 255
    Dim temp = (CurrentColumn - LeftTextColumn + 1) - TextColumns
    If temp > 0 Then                          'If off right of window,
      LeftTextColumn += temp                         'increment window column
    End If
  End Sub

  'Private Shared Function FnSpaces2Pad(text As String) As Integer
  '  Select Case ASCII(Right(text, 1))
  '    Case 46, 33, 63          '".", "!", "?"
  '      Return 2
  '    Case 32, -1              'Space
  '      Return 0
  '    Case Else
  '      Return 1
  '  End Select
  'End Function

End Class
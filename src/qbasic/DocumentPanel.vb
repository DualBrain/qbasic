Imports System.Reflection.Metadata
Imports Basic.Input
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

    TextRows = EditorHeight - 2
    TextColumns = EditorWidth - 2

    TopScreenRow = EditorTop + 1 'Calc the top line of window
    LeftScreenColumn = EditorLeft + 1 'Calc Left margin of window

    If TopTextLine < 1 Then TopTextLine = 1             'Top of window row number
    If LeftTextColumn < 1 Then LeftTextColumn = 1       'Set window column to 1
    If CurrentLine < 1 Then CurrentLine = 1             'make top line the cursor col.
    If CurrentColumn < 1 Then CurrentColumn = 1         'establish cursor column at 1

    'Make sure the cursor stays in the window
    If CurrentColumn < LeftTextColumn Then LeftTextColumn = CurrentColumn
    If CurrentColumn > (TextColumns - 1) Then LeftTextColumn = CurrentColumn - (TextColumns - 1)

    Dim lrRow = EditorTop + EditorHeight - 1
    Dim lrCol = EditorLeft + EditorWidth - 1

    ' Box (the whole region)
    Box0(EditorTop, EditorLeft, EditorTop + EditorHeight - 1, lrCol, 1, OneColor(8, 1))

    ' Draw horizontal bottom line; to connect regions
    If EditorTop > 2 Then
      HLine(EditorTop, EditorLeft, lrCol, 1, OneColor(8, 1))
    End If

    ' Content Area
    ClearScr0(EditorTop + 1, EditorLeft + 1, lrRow - 1, lrCol - 1, OneColor(8, 1))

    ' Draw the current portion of (visible) text.
    LOCATE(TopScreenRow, LeftScreenColumn, 0)
    Call APrint0(m_document.ToArray,
                 TopTextLine - 1,
                 TextRows,
                 LeftTextColumn,
                 TextColumns,
                 WindowColor)
    LOCATE(TopScreenRow + CurrentLine - TopTextLine, LeftScreenColumn + (CurrentColumn - LeftTextColumn), 1)

    ' Highlight "selection"
    If BlockTopLeft IsNot Nothing Then
      Dim tr = BlockTopLeft.Row
      Dim lc = BlockTopLeft.Column
      Dim br = BlockBottomRight.Row
      Dim rc = BlockBottomRight.Column
      If br - tr = 0 Then
        PaintBox0(TopScreenRow + tr - TopTextLine, LeftScreenColumn + lc - LeftTextColumn, TopScreenRow + tr - TopTextLine, LeftScreenColumn + rc - LeftTextColumn, OneColor(1, 8))
      Else
        For r = tr To br
          PaintBox0(TopScreenRow + r - TopTextLine, LeftScreenColumn + 1 - LeftTextColumn, TopScreenRow + r - TopTextLine, LeftScreenColumn + EditorWidth - 2 - LeftTextColumn, OneColor(1, 8))
        Next
      End If
    End If

    ' Expand/Collapse Tool
    QPrintRC(ChrW(180), EditorTop, lrCol - 4, OneColor(8, 1))
    If EditorHeight = 24 Then
      QPrintRC(ChrW(18), EditorTop, lrCol - 3, OneColor(1, 8))
    Else
      QPrintRC(ChrW(24), EditorTop, lrCol - 3, OneColor(1, 8))
    End If
    QPrintRC(ChrW(195), EditorTop, lrCol - 2, OneColor(8, 1))

    If ScrollBars Then
      If EditorHeight > 5 Then
        ' Determine vertical percent
        Dim percent = CurrentLine / m_document.Count
        If CurrentLine = 1 Then percent = 0
        If percent > 1 Then percent = 1
        ' Draw scroll bar
        VScrollBar(percent, EditorTop + 1, lrCol, lrRow - 2, 1)
      End If
      If EditorHeight > 3 Then
        ' Determine maximum width of text across all lines
        Dim maxCols = 1
        For Each line In m_document
          If line.Length > maxCols Then maxCols = line.Length
        Next
        ' Determine horizontal percent
        Dim percent = CurrentColumn / maxCols
        If CurrentColumn = 1 Then percent = 0
        If percent > 1 Then percent = 1
        ' Draw scroll bar
        HScrollBar(percent, lrRow - 1, EditorLeft + 1, lrCol - 1, 1)
      End If
    End If

    ' Document Title
    Dim title = $" {Me.Title} "
    Dim titleOffset = (EditorWidth - title.Length) \ 2
    QPrintRC(title, EditorTop, EditorLeft + titleOffset, OneColor(1, 8))

  End Sub

  Public Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys

    ' Handle keyboard/mouse
    If keys IsNot Nothing Then
      For Each key In keys
        If ctrl AndAlso Not alt Then ' shift can be in either state
          Select Case key
            Case ConsoleKey.A : WordLeftAction(shift) ' Word Left (WordStar)
            Case ConsoleKey.C : CursorPageDownAction(shift) ' Page down (WordStar)
            Case ConsoleKey.D : CursorRightAction(shift) ' Cursor right (WordStar)
            Case ConsoleKey.E : CursorUpAction(shift) ' Cursor up (WordStar)
            Case ConsoleKey.F : WordRightAction(shift) ' Word Right (WordStar)
            Case ConsoleKey.G ' Delete selected text / Delete one character at the cursor (WordStar)
            Case ConsoleKey.H : BackspaceAction() ' Delete one character to the left of the cursor (WordStar)
            Case ConsoleKey.J : CursorNextLineAction(shift) ' Cursor to beginning of next line (WordStar)
            Case ConsoleKey.K '...
              'm_isK = True
            Case ConsoleKey.L ' Repeat find for same text
            Case ConsoleKey.N : InsertLineAboveAction() ' Insert Line above
            Case ConsoleKey.P ' Insert special characters...
              'm_isCtrlP = True
            Case ConsoleKey.Q '... 
              'm_isQ = True
            Case ConsoleKey.R : CursorPageUpAction(shift) ' Page up (WordStar)
            Case ConsoleKey.S : CursorLeftAction(shift) ' Cursor left (WordStar)
            Case ConsoleKey.T ' Delete the rest of the word the cursor is on (WordStar)
            Case ConsoleKey.V ' Switch between insert and overstrike modes (WordStar)
            Case ConsoleKey.X : CursorDownAction(shift) ' Cursor dn (WordStar)
            Case ConsoleKey.Y ' Cut current line
            Case ConsoleKey.Enter : CursorNextLineAction(shift) ' Cursor to beginning of next line (skipping spaces)
            Case ConsoleKey.Insert ' Copy to Clipboard
            Case ConsoleKey.LeftArrow : WordLeftAction(shift) ' Word Left
            Case ConsoleKey.RightArrow : WordRightAction(shift) ' Word Right
            Case ConsoleKey.UpArrow, ConsoleKey.W : ScrollUpAction(shift) ' Scroll up, Scroll up (WordStar)
            Case ConsoleKey.DownArrow, ConsoleKey.Z : ScrollDownAction(shift) ' Scroll dn, Scroll dn (WordStar)
            Case ConsoleKey.PageUp ' Left one window
            Case ConsoleKey.PageDown ' Right one window
            Case ConsoleKey.Home : DocumentHomeAction(shift)
            Case ConsoleKey.End : DocumentEndAction(shift)
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
              Else
                If CurrentColumn <= m_document(CurrentLine - 1).Length Then
                  Dim leftSide = If(CurrentColumn > 0, m_document(CurrentLine - 1).Substring(0, CurrentColumn - 1), "")
                  Dim rightSide = If(CurrentColumn < m_document(CurrentLine - 1).Length, m_document(CurrentLine - 1).Substring(CurrentColumn), "")
                  m_document(CurrentLine - 1) = leftSide & GetChar(key, capsLock, shift) & rightSide
                Else
                  m_document(CurrentLine - 1) &= GetChar(key, capsLock, shift)
                End If
              End If
              CurrentColumn += 1
              Changed = True

            Case ConsoleKey.Tab : TabAction(shift) ' Insert x spaces ("tab") or Delete leading spaces from selected lines
            Case ConsoleKey.LeftArrow : CursorLeftAction(shift) ' Cursor left or Select Character/lines
            Case ConsoleKey.RightArrow : CursorRightAction(shift) ' Cursor right or Select Character/lines
            Case ConsoleKey.UpArrow : CursorUpAction(shift) ' Cursor up or Select Character/lines
            Case ConsoleKey.DownArrow : CursorDownAction(shift) ' Cursor down or Select Character/lines
            Case ConsoleKey.Home : CursorHomeAction(shift)
            Case ConsoleKey.End : CursorEndOfLineAction(shift)
            Case ConsoleKey.PageUp : CursorPageUpAction(shift) ' Cursor page up or Select Screen up
            Case ConsoleKey.PageDown : CursorPageDownAction(shift) ' Cursor page down or Select Screen down
            Case ConsoleKey.Insert : InsertAction(shift) ' Toggle insert mode or Insert From Clipboard
            Case ConsoleKey.Backspace : BackspaceAction()
            Case ConsoleKey.Delete : DeleteAction(shift) ' Delete at cursor or Cut selected text
            Case ConsoleKey.Enter : EnterAction()
            Case ConsoleKey.Escape : EscapeAction()
            Case Else
          End Select
        End If
      Next
    End If

    Return True

  End Function

  Public Sub Clear()
    Title = "Untitled"
    m_document.Clear()
    m_document.Add("")
    CurrentLine = 1
    CurrentColumn = 1
    ClearBlock()
  End Sub

  Private Sub ClearBlock()
    BlockTopLeft = Nothing
    BlockBottomRight = Nothing
  End Sub


#Region "Actions"

  Private Sub WordLeftAction(shift As Boolean)
    Dim r = CurrentLine
    Dim c = CurrentColumn
    Dim flagged = False
    If c > m_document(r - 1).Length Then
      c = m_document(r - 1).Length
    End If
    Do
      c -= 1
      If c < 1 Then
        r -= 1 : If r < 1 Then r = 1 : Exit Do
        c = m_document(r - 1).Length
      End If
      Dim ch = m_document(r - 1)(c - 1)
      Select Case ch
        Case " "c,
             ":"c, ";"c, ","c, "("c, ")"c,
             "+"c, "-"c, "/"c, "\"c, "*"c,
             ChrW(34), "'"c,
             "="c, ">"c, "<"c,
             ChrW(10), ChrW(13)
          If flagged Then
            c += 1
            If c > m_document(r - 1).Length Then
              c = 1
              r += 1
            End If
            Exit Do
          End If
          'flagged = True
        Case Else
          flagged = True
      End Select
    Loop
    If shift Then
      If BlockTopLeft Is Nothing Then
        BlockTopLeft = New Location(r, c)
        BlockBottomRight = New Location(CurrentLine, CurrentColumn - 1)
        CurrentLine = r : CursorDown()
        CurrentColumn = c : CursorRight()
      Else
        Dim prevLine = CurrentLine
        Dim prevColumn = CurrentColumn
        CurrentLine = r : CursorDown()
        CurrentColumn = c : CursorRight()
        If prevLine = BlockBottomRight.Row AndAlso
           prevColumn > BlockTopLeft.Column AndAlso
           c < BlockBottomRight.Column Then
          ' Shrink
          'BlockBottomRight.Row = CurrentLine
          BlockBottomRight.Column = CurrentColumn - 1
        ElseIf prevLine > BlockTopLeft.Row AndAlso r <= BlockBottomRight.Row Then
          ' Shrink
          BlockBottomRight.Row = CurrentLine
          BlockBottomRight.Column = CurrentColumn - 1
        Else
          ' Expand
          If CurrentLine < BlockTopLeft.Row Then
            BlockTopLeft.Row = CurrentLine
          End If
          If CurrentColumn < BlockTopLeft.Column Then
            BlockTopLeft.Column = CurrentColumn
          End If
        End If
      End If
    Else
      ClearBlock()
      CurrentLine = r : CursorUp()
      CurrentColumn = c : CursorLeft()
    End If
  End Sub

  Private Sub WordRightAction(shift As Boolean)
    Dim r = CurrentLine
    Dim c = CurrentColumn
    Dim flagged = False
    Do
      c += 1
      Do
        If r > m_document.Count Then Exit Do
        If c > m_document(r - 1).Length Then
          If shift AndAlso c = m_document(r - 1).Length + 1 Then
            Exit Do
          Else
            r += 1 : c = 1 : If Not shift Then flagged = True
          End If
        Else
          Exit Do
        End If
      Loop
      If r > m_document.Count - 1 AndAlso
         c > m_document(r - 1).Length Then
        c -= 1
        Exit Do
      End If
      ' Using CHRW(254) as an virtual end of line "char"
      Dim ch = If(c > m_document(r - 1).Length, ChrW(254), m_document(r - 1)(c - 1))
      Select Case ch
        Case " "c,
             ":"c, ";"c, ","c, "("c, ")"c,
             "+"c, "-"c, "/"c, "\"c, "*"c,
             ChrW(34), "'"c,
             "="c, ">"c, "<"c,
             ChrW(10), ChrW(13),
             ChrW(254)
          If shift AndAlso flagged Then
            Exit Do
          ElseIf Not shift Then
            flagged = True
          End If
        Case Else
          If shift AndAlso Not flagged Then
            flagged = True
          ElseIf Not shift AndAlso flagged Then
            c -= 1 : If c < 0 Then c = 0
            Exit Do
          End If
      End Select
    Loop
    If shift Then
      If BlockTopLeft Is Nothing Then
        BlockTopLeft = New Location(CurrentLine, CurrentColumn)
        BlockBottomRight = New Location(r, c - 1)
        CurrentLine = r : CursorDown()
        CurrentColumn = c : CursorRight()
      Else
        Dim prevLine = CurrentLine
        Dim prevColumn = CurrentColumn
        CurrentLine = r : CursorDown()
        CurrentColumn = c : CursorRight()
        If prevLine = BlockTopLeft.Row AndAlso
           prevColumn < BlockBottomRight.Column AndAlso
           c > BlockTopLeft.Column Then
          ' Shrink
          'BlockTopLeft.Row = CurrentLine
          BlockTopLeft.Column = CurrentColumn '- 1
        ElseIf prevLine < BlockBottomRight.Row AndAlso r >= BlockTopLeft.Row Then
          ' Shrink
          BlockTopLeft.Row = CurrentLine
          BlockTopLeft.Column = CurrentColumn - 1
        Else
          ' Grow
          If CurrentLine > BlockBottomRight.Row Then
            BlockBottomRight.Row = CurrentLine
          End If
          If CurrentColumn > BlockBottomRight.Column Then
            BlockBottomRight.Column = CurrentColumn - 1
          End If
        End If
      End If
    Else
      ClearBlock()
      CurrentLine = r : CursorDown()
      CurrentColumn = c + 1 : CursorRight()
    End If
  End Sub

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
    ClearBlock()
  End Sub

  Private Sub CursorHomeAction(shift As Boolean)
    Dim l = 1
    ' Determine how many spaces at the beginning of the line...
    If m_document(CurrentLine - 1).Length > 0 Then
      For index = 0 To m_document(CurrentLine - 1).Length - 1
        Select Case m_document(CurrentLine - 1)(index)
          Case " "c
          Case Else
            l = index + 1 : Exit For
        End Select
      Next
    End If
    If shift Then
      If BlockTopLeft Is Nothing Then
        If CurrentColumn - 1 > 1 Then
          BlockBottomRight = New Location(CurrentLine, CurrentColumn - 1)
          CurrentColumn = l : CursorLeft()
          BlockTopLeft = New Location(CurrentLine, CurrentColumn)
        End If
      Else
        ' Move the cursor
        Dim prev = CurrentColumn
        CurrentColumn = l : CursorLeft()
        ' Adjust the selection
        If prev > CurrentColumn Then
          BlockBottomRight.Column = CurrentColumn - 1
        ElseIf prev > BlockTopLeft.Column Then
          ' Shift selection to right
          BlockBottomRight.Column = BlockTopLeft.Column - 1
          BlockTopLeft.Column = CurrentColumn
        ElseIf CurrentColumn < BlockBottomRight.Column Then
          ' Grow to the left
          BlockTopLeft.Column = CurrentColumn
        Else
          ' Expand to the right...
          BlockBottomRight.Column = CurrentColumn - 1
        End If
        ' If result of select would invert, clear
        If BlockBottomRight.Column < BlockTopLeft.Column AndAlso
         BlockTopLeft.Row = BlockBottomRight.Row Then
          ClearBlock()
        End If
      End If
    Else
      ClearBlock()
      CurrentColumn = l
    End If
  End Sub

  Private Sub CursorEndOfLineAction(shift As Boolean)
    Dim l = 1
    ' Determine how many spaces at the beginning of the line...
    If m_document(CurrentLine - 1).Length > 0 Then
      For index = m_document(CurrentLine - 1).Length - 1 To 0 Step -1
        Select Case m_document(CurrentLine - 1)(index)
          Case " "c
          Case Else
            l = index + 1 : Exit For
        End Select
      Next
    End If
    If shift Then
      If BlockTopLeft Is Nothing Then
        If CurrentColumn - 1 < l Then
          BlockTopLeft = New Location(CurrentLine, CurrentColumn)
          CurrentColumn = l : CursorLeft()
          BlockBottomRight = New Location(CurrentLine, CurrentColumn - 1)
        End If
      Else
        Dim prev = CurrentColumn
        ' Move the cursor
        CurrentColumn = l : CursorLeft()
        ' Adjust the selection
        If prev <= BlockBottomRight.Column Then
          ' Shift selection to the right...
          BlockTopLeft.Column = BlockBottomRight.Column + 1
          BlockBottomRight.Column = CurrentColumn - 1
        Else
          ' Expand to the right...
          BlockBottomRight.Column = CurrentColumn - 1
        End If
        ' If result of select would invert, clear
        If BlockBottomRight.Column < BlockTopLeft.Column AndAlso
           BlockTopLeft.Row = BlockBottomRight.Row Then
          ClearBlock()
        End If
      End If
    Else
      'If CurrentLine <= m_document.Count Then
      '  CurrentColumn = m_document(CurrentLine - 1).Length + 1
      'Else
      CurrentColumn = l '1
      'End If
      ClearBlock()
    End If
  End Sub

  Private Sub CursorNextLineAction(shift As Boolean)
    If shift Then
      Stop
    Else
      CurrentLine += 1 : CursorDown()
      Dim offset = 0
      If CurrentLine < m_document.Count Then
        For Each ch In m_document(CurrentLine - 1)
          If ch = " "c Then offset += 1 Else Exit For
        Next
      End If
      CurrentColumn = 1 + offset
      ClearBlock()
    End If
  End Sub

  Private Sub CursorPageDownAction(shift As Boolean)
    If shift Then
      Stop
    Else
      If TopTextLine <= (m_document.Count - 1) - (TextRows - 1) Then
        TopTextLine += TextRows - 1                    'calc top line of next page
        CurrentLine += TextRows - 1               'update current line
      End If
      ClearBlock()
    End If
  End Sub

  Private Sub CursorPageUpAction(shift As Boolean)
    If shift Then
      Stop
    Else
      If TopTextLine > 1 Then                           'ignore if already at the top
        Dim x = TopTextLine                             'save Ed.TL for a moment
        TopTextLine = MaxInt(1, TopTextLine - TextRows)
        x -= TopTextLine                                'calc dif. between new and old
        CurrentLine -= x                           'don't move cursor unless we have to
      End If
      ClearBlock()
    End If
  End Sub

  Private Sub CursorLeftAction(shift As Boolean)
    If shift Then
      If BlockTopLeft Is Nothing Then
        CurrentColumn -= 1 : CursorLeft()
        BlockBottomRight = New Location(CurrentLine, CurrentColumn)
        BlockTopLeft = New Location(CurrentLine, CurrentColumn)
      Else
        ' Move the cursor
        CurrentColumn -= 1 : CursorLeft()
        ' Adjust the selection
        If CurrentColumn < BlockTopLeft.Column Then
          ' Grow to the left...
          BlockTopLeft.Column = CurrentColumn
        Else
          ' Shrink from the right...
          BlockBottomRight.Column = CurrentColumn - 1
        End If
        ' If result of select would invert, clear
        If BlockBottomRight.Column < BlockTopLeft.Column AndAlso
           BlockTopLeft.Row = BlockBottomRight.Row Then
          ClearBlock()
        End If
      End If
    Else
      ClearBlock()
      CurrentColumn -= 1 : CursorLeft()
    End If
  End Sub

  Private Sub CursorRightAction(shift As Boolean)
    If shift Then
      If BlockTopLeft Is Nothing Then
        BlockTopLeft = New Location(CurrentLine, CurrentColumn)
        BlockBottomRight = New Location(CurrentLine, CurrentColumn)
        CurrentColumn += 1 : CursorRight()
      Else
        ' If result of select would leave just one character, clear
        If CurrentColumn = BlockTopLeft.Column AndAlso
           BlockTopLeft.Column = BlockBottomRight.Column AndAlso
           BlockTopLeft.Row = BlockBottomRight.Row Then
          ClearBlock()
        Else
          ' Adjust the selection
          If CurrentColumn > BlockBottomRight.Column Then
            ' Expand right side
            BlockBottomRight.Column = CurrentColumn
          Else
            ' Shrink left side
            BlockTopLeft.Column = CurrentColumn + 1
          End If
        End If
        ' Move the cursor
        CurrentColumn += 1 : CursorRight()
      End If
    Else
      ClearBlock()
      CurrentColumn += 1 : CursorRight()
    End If
  End Sub

  Private Sub CursorUpAction(shift As Boolean)
    If shift Then
      If BlockTopLeft Is Nothing Then
        BlockBottomRight = New Location(CurrentLine, CurrentColumn)
        CurrentLine -= 1 : CursorUp()
        BlockTopLeft = New Location(CurrentLine, CurrentColumn)
      Else
        CurrentLine -= 1 : CursorUp()
        If CurrentLine >= BlockTopLeft.Row Then
          BlockBottomRight.Row = CurrentLine
        Else
          BlockTopLeft.Row = CurrentLine
        End If
        If CurrentLine = BlockTopLeft.Row AndAlso
           BlockTopLeft.Row = BlockBottomRight.Row AndAlso
           BlockTopLeft.Column = BlockBottomRight.Column Then
          ClearBlock()
        End If
      End If
    Else
      ClearBlock()
      CurrentLine -= 1 : CursorUp()
    End If
  End Sub

  Private Sub CursorDownAction(shift As Boolean)
    If shift Then
      If BlockTopLeft Is Nothing Then
        BlockTopLeft = New Location(CurrentLine, CurrentColumn)
        CurrentLine += 1 : CursorDown()
        BlockBottomRight = New Location(CurrentLine, CurrentColumn)
      Else
        CurrentLine += 1 : CursorDown()
        If CurrentLine <= BlockBottomRight.Row Then
          BlockTopLeft.Row = CurrentLine
        Else
          BlockBottomRight.Row = CurrentLine
        End If
        If CurrentLine = BlockTopLeft.Row AndAlso
           BlockTopLeft.Row = BlockBottomRight.Row AndAlso
           BlockTopLeft.Column = BlockBottomRight.Column Then
          ClearBlock()
        End If
      End If
    Else
      ClearBlock()
      CurrentLine += 1 : CursorDown()
    End If
  End Sub

  Private Sub DocumentEndAction(shift As Boolean)
    If shift Then
      Stop
    Else
      Dim pages = m_document.Count \ TextRows
      TopTextLine = pages * TextRows
      CurrentLine = m_document.Count : CurrentColumn = 1
      ClearBlock()
    End If
  End Sub

  Private Sub DocumentHomeAction(shift As Boolean)
    If shift Then
      Stop
    Else
      CurrentLine = 1 : CurrentColumn = 1 : TopTextLine = 1
      ClearBlock()
    End If
  End Sub

  Private Sub DeleteAction(shift As Boolean)
    If shift Then
      Stop
    Else
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
      ClearBlock()
    End If
  End Sub

  Private Sub EscapeAction()
    ClearBlock()
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

  Friend Sub InsertAction(shift As Boolean)
    If shift Then
      Stop
    Else
      ToggleInsertMode()
    End If
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
    ClearBlock()
  End Sub

  Private Sub ScrollDownAction(shift As Boolean)
    If CurrentLine < m_document.Count Then TopTextLine += 1
    If CurrentLine < TopTextLine Then CurrentLine = TopTextLine
    If Not shift Then ClearBlock()
  End Sub

  Private Sub ScrollUpAction(shift As Boolean)
    If TopTextLine > 1 Then TopTextLine -= 1
    If CurrentLine > (TopTextLine - 1) + TextRows - 1 Then CurrentLine = (TopTextLine - 1) + TextRows - 1
    If Not shift Then ClearBlock()
  End Sub

  Private Sub TabAction(shift As Boolean)
    Dim tr = CurrentLine
    Dim br = CurrentLine
    If BlockTopLeft IsNot Nothing Then
      tr = BlockTopLeft.Row
      br = BlockBottomRight.Row
    End If
    If shift Then
      Dim remove = 255
      For index = tr To br
        For i = 0 To m_document(index - 1).Length - 1
          Select Case m_document(index - 1)(i)
            Case " "c
            Case Else
              If i < remove Then remove = i : Exit For
          End Select
        Next
      Next
      If remove > 0 Then
        For index = tr To br
          If m_document(index - 1).Length - 1 > 2 Then
            m_document(index - 1) = m_document(index - 1).Substring(2)
          End If
        Next
      End If
    Else
      If br - tr > 0 Then
        For index = tr To br
          If m_document(index - 1).Length > 0 Then
            m_document(index - 1) = "  " & m_document(index - 1)
            Changed = True
          End If
        Next
      Else
        If CurrentColumn <= m_document(CurrentLine - 1).Length Then
          Dim leftSide = If(CurrentColumn > 0, m_document(CurrentLine - 1).Substring(0, CurrentColumn - 1), "")
          Dim rightSide = If(CurrentColumn <= m_document(CurrentLine - 1).Length, m_document(CurrentLine - 1).Substring(CurrentColumn - 1), "")
          m_document(CurrentLine - 1) = leftSide & Space(TabSize) & rightSide
          Changed = True
        End If
        CurrentColumn += TabSize
      End If
    End If
  End Sub

#End Region

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
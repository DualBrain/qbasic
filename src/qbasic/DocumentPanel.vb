Imports Basic.Input
Imports QBLib.Video
Imports VbPixelGameEngine

Public Class DocumentPanel
  Inherits PgeX
  Implements IContext

  Public Event SelectionChanged()

  Public Class Location
    Public Sub New()
    End Sub
    Public Sub New(row As Integer, column As Integer)
      Me.Row = row : Me.Column = column
    End Sub
    Public Row As Integer
    Public Column As Integer
  End Class

  'Private Const NumPad As String = "12346789"              ' Shifted arrow keys
  'Private Const NumPad2 As String = "stOPQKMGHK"           ' Unshifted arrow keys

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
        result &= line & vbLf
      Next
      Return result
    End Get
    Set(value As String)
      Clear()
      value = value.Replace(vbCrLf, vbLf)
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

  Private ReadOnly m_copyBuffer As New List(Of String)

  Public ReadOnly Property CanCutSelection As Boolean
    Get
      Return BlockTopLeft IsNot Nothing
    End Get
  End Property

  Public ReadOnly Property CanCopySelection As Boolean
    Get
      Return BlockTopLeft IsNot Nothing
    End Get
  End Property

  Public ReadOnly Property CanDeleteSelection As Boolean
    Get
      Return BlockTopLeft IsNot Nothing
    End Get
  End Property

  Public ReadOnly Property CanPaste As Boolean
    Get
      Return m_copyBuffer.Count > 0
    End Get
  End Property

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
    If CurrentColumn > LeftTextColumn + (TextColumns - 1) Then LeftTextColumn = CurrentColumn - (TextColumns - 1)

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
        Dim ulRow1 = TopScreenRow + (tr - TopTextLine)
        Dim ulCol1 = LeftScreenColumn + lc - LeftTextColumn
        If ulCol1 < LeftScreenColumn Then ulCol1 = LeftScreenColumn
        Dim lrRow1 = TopScreenRow + (tr - TopTextLine)
        Dim lrCol1 = LeftScreenColumn + rc - LeftTextColumn
        If lrCol1 > LeftScreenColumn + TextColumns Then lrCol1 = LeftScreenColumn + TextColumns
        PaintBox0(ulRow1,
                  ulCol1,
                  lrRow1,
                  lrCol1, OneColor(1, 8))
      Else
        For r = tr To br
          If r - TopTextLine < 0 Then Continue For
          If r - TopTextLine > TextRows Then Continue For
          Dim ulRow1 = TopScreenRow + (r - TopTextLine)
          Dim ulCol1 = LeftScreenColumn
          Dim lrRow1 = TopScreenRow + (r - TopTextLine)
          Dim lrCol1 = LeftScreenColumn + TextColumns - 1
          PaintBox0(ulRow1, ulCol1, lrRow1, lrCol1, OneColor(1, 8))
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
            Case ConsoleKey.G : DeleteAction(False) ' Delete selected text / Delete one character at the cursor (WordStar)
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
            Case ConsoleKey.T : DeleteRestOfWordAction() ' Delete the rest of the word the cursor is on (WordStar)
            Case ConsoleKey.V : InsertAction(False) ' Switch between insert and overstrike modes (WordStar)
            Case ConsoleKey.X : CursorDownAction(shift) ' Cursor dn (WordStar)
            Case ConsoleKey.Y : CutCurrentLineAction() ' Cut current line
            Case ConsoleKey.Enter : CursorNextLineAction(shift) ' Cursor to beginning of next line (skipping spaces)
            Case ConsoleKey.Insert : CopyAction() ' Copy to Clipboard
            Case ConsoleKey.LeftArrow : WordLeftAction(shift) ' Word Left
            Case ConsoleKey.RightArrow : WordRightAction(shift) ' Word Right
            Case ConsoleKey.UpArrow, ConsoleKey.W : ScrollUpAction(shift) ' Scroll up, Scroll up (WordStar)
            Case ConsoleKey.DownArrow, ConsoleKey.Z : ScrollDownAction(shift) ' Scroll dn, Scroll dn (WordStar)
            Case ConsoleKey.PageUp : ScrollWindowLeftAction(shift) ' Left one window
            Case ConsoleKey.PageDown : ScrollWindowRightAction(shift) ' Right one window
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
              If BlockTopLeft IsNot Nothing Then RemoveBlock() 'False)
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
    TopTextLine = 1
    LeftTextColumn = 1
    CurrentLine = 1
    CurrentColumn = 1
    ClearSelection()
  End Sub

  Public Sub CopySelection()
    CopyAction()
  End Sub

  Public Sub Paste()
    InsertAction(True)
  End Sub

  Public Sub CutSelection()
    DeleteAction(False)
  End Sub

  Public Sub DeleteSelection()
    DeleteAction(False)
  End Sub

  Public Sub ClearSelection()
    BlockTopLeft = Nothing
    BlockBottomRight = Nothing
    RaiseEvent SelectionChanged()
  End Sub

  Private Sub CopyBlock()
    If BlockTopLeft IsNot Nothing Then
      Dim tr = BlockTopLeft.Row
      Dim lc = BlockTopLeft.Column
      Dim br = BlockBottomRight.Row
      Dim rc = BlockBottomRight.Column
      m_copyBuffer.Clear()
      If br - tr = 0 Then
        If lc = 1 AndAlso rc = 32768 Then
          m_copyBuffer.Add(m_document(tr - 1) & vbLf)
        Else
          Dim middle = m_document(tr - 1).Substring(lc - 1, rc - lc + 1)
          m_copyBuffer.Add(middle)
        End If
      Else
        For r = tr To br
          m_copyBuffer.Add(m_document(r - 1) & vbLf)
        Next
      End If
      RaiseEvent SelectionChanged()
    End If
  End Sub

  Private Sub RemoveBlock()
    If BlockTopLeft IsNot Nothing Then
      Dim tr = BlockTopLeft.Row
      Dim lc = BlockTopLeft.Column
      Dim br = BlockBottomRight.Row
      Dim rc = BlockBottomRight.Column
      If br - tr = 0 Then
        If lc = 1 AndAlso rc = 32768 Then
          m_document.RemoveAt(tr - 1)
        Else
          Dim leftSide = If(lc > 0, m_document(tr - 1).Substring(0, lc - 1), "")
          Dim middle = m_document(tr - 1).Substring(lc - 1, rc - lc + 1)
          Dim rightSide = If(rc < m_document(tr - 1).Length, m_document(tr - 1).Substring(rc), "")
          m_document(tr - 1) = leftSide & rightSide
          CurrentColumn = lc : CursorLeft()
        End If
      Else
        m_document.RemoveRange(tr - 1, br - tr + 1)
        CurrentLine = tr : CursorUp()
      End If
      Changed = True
      ClearSelection()
    End If
  End Sub

  Private Sub HandleSelection(curRow As Integer, curCol As Integer, newRow As Integer, newCol As Integer)

    Dim magicColumn = 32768

    If BlockTopLeft IsNot Nothing Then

      Dim topRow = BlockTopLeft.Row
      Dim topCol = BlockTopLeft.Column
      Dim botRow = BlockBottomRight.Row
      Dim botCol = BlockBottomRight.Column

      Debug.Write($"({topRow},{topCol})-({botRow},{botCol})")

      '' Is the new position completely contained within the current selection...
      'If newRow >= topRow AndAlso newRow <= botRow AndAlso newCol >= topCol AndAlso newCol <= botCol Then
      '  ' No selection change...
      'Else

      If (curRow = topRow AndAlso curCol = topCol) OrElse curRow < topRow Then

        Debug.Write(" UL")

        ' cursor was at the top, left

        If newRow < topRow Then
          ' Expand
          topRow = newRow : topCol = newCol
        ElseIf newRow > botRow Then
          Debug.Write(" FLIP1")
          topCol = botCol + 1
          botCol = newCol - 1
          botRow = newRow
        ElseIf newRow > topRow Then
          ' Shrink
          topRow = newRow
          If newRow = botRow Then
            If newCol <= botCol Then
              topCol = newCol
            Else
              Debug.Write(" FLIP2")
              topCol = botCol + 1
              botCol = newCol - 1
            End If
          End If
        Else ' If nr = btr Then
          ' Adjust columns?
          If topRow = botRow AndAlso newCol > botCol Then
            ' Flip
            Debug.Write(" FLIP3")
            topCol = botCol + 1
            botCol = newCol - 1
          Else
            topCol = newCol
          End If
        End If

      ElseIf (curRow = botRow AndAlso curCol - 1 = botCol) OrElse curRow > botRow Then

        ' cursor was at the bottom, right
        Debug.Write(" BR")

        If newRow > botRow Then
          ' Expand
          botRow = newRow : botCol = newCol - 1
        ElseIf newRow < topRow Then
          Debug.Write(" FLIP1")
          botCol = topCol - 1
          topCol = newCol
          topRow = newRow
        ElseIf newRow < botRow Then
          ' Shrink
          botRow = newRow
          If newRow = topRow Then
            If newCol >= topCol Then
              botCol = newCol - 1
            Else
              Debug.Write(" FLIP2")
              botCol = topCol - 1
              topCol = newCol
            End If
          End If
        Else ' If nr = btr Then
          ' Adjust columns?
          If botRow = topRow AndAlso newCol < topCol Then 'AndAlso curCol > topCol Then
            ' Flip
            Debug.Write(" FLIP3")
            botCol = topCol - 1
            topCol = newCol
            'ElseIf newCol < topCol Then
            '  ' Shrink (to left)
            '  topCol = newCol
          Else
            ' Expand (to left)
            botCol = newCol - 1
          End If
        End If

      Else

        '?????
        Stop

      End If

      If topCol = 0 Then topRow += 1 : topCol = magicColumn
      If botCol = 0 Then botRow -= 1 : botCol = magicColumn

      If (topRow = botRow AndAlso topCol = botCol) OrElse
         botRow < topRow OrElse
         topCol > magicColumn OrElse
         botCol > magicColumn Then
        Debug.WriteLine($" ()-()")
        BlockTopLeft = Nothing
        BlockBottomRight = Nothing
      Else
        Debug.WriteLine($" ({topRow},{topCol})-({botRow},{botCol})")
        BlockTopLeft.Row = topRow
        BlockTopLeft.Column = topCol
        BlockBottomRight.Row = botRow
        BlockBottomRight.Column = botCol
      End If

      'End If

    Else

      Dim topRow As Integer
      Dim topCol As Integer
      Dim botRow As Integer
      Dim botCol As Integer

      If curRow * 80 + curCol < newRow * 80 + newCol Then
        topRow = curRow : topCol = curCol
        botRow = newRow : botCol = newCol - 1
      Else
        topRow = newRow : topCol = newCol
        botRow = curRow : botCol = curCol - 1
      End If

      If topCol = 0 Then topRow += 1 : topCol = magicColumn
      If botCol = 0 Then botRow -= 1 : botCol = magicColumn

      BlockTopLeft = New Location(topRow, topCol)
      BlockBottomRight = New Location(botRow, botCol)

      Debug.WriteLine($"({topRow},{topCol})-({botRow},{botCol})")

    End If

    RaiseEvent SelectionChanged()

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
    ClearSelection()
  End Sub

#Region "Cursor Movement and Block Selection"

  Private Sub CursorHomeAction(shift As Boolean)
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
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
    CurrentColumn = l
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub CursorEndOfLineAction(shift As Boolean)
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
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
    CurrentColumn = l
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub CursorNextLineAction(shift As Boolean)
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
    CurrentLine += 1 : CursorDown()
    Dim offset = 0
    If CurrentLine < m_document.Count Then
      For Each ch In m_document(CurrentLine - 1)
        If ch = " "c Then offset += 1 Else Exit For
      Next
    End If
    CurrentColumn = 1 + offset
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub CursorPageDownAction(shift As Boolean)
    Dim prevLine = CurrentLine
    Dim prevColumn = CurrentColumn
    If TopTextLine <= (m_document.Count - 1) - (TextRows - 1) Then
      TopTextLine += TextRows - 1               'calc top line of next page
      CurrentLine += TextRows - 1               'update current line
    Else
      Return
    End If
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub CursorPageUpAction(shift As Boolean)
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
    If TopTextLine > 1 Then                           'ignore if already at the top
      Dim x = TopTextLine                             'save Ed.TL for a moment
      TopTextLine = MaxInt(1, TopTextLine - TextRows)
      x -= TopTextLine                                'calc dif. between new and old
      CurrentLine -= x                                'don't move cursor unless we have to
    End If
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub CursorLeftAction(shift As Boolean)
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
    CurrentColumn -= 1 : CursorLeft()
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub CursorRightAction(shift As Boolean)
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
    CurrentColumn += 1 : CursorRight()
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub CursorUpAction(shift As Boolean)
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
    CurrentLine -= 1 : CursorUp()
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub CursorDownAction(shift As Boolean)
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
    CurrentLine += 1 : CursorDown()
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

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
             ":"c, ";"c, ","c, "."c, "("c, ")"c, "["c, "]"c, "^"c,
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
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
    CurrentLine = r : CursorDown()
    CurrentColumn = c : CursorRight()
    If shift Then
      'HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
      If BlockTopLeft Is Nothing Then
        BlockTopLeft = New Location(r, c)
        BlockBottomRight = New Location(prevLine, prevColumn - 1)
      Else
        If prevLine = BlockTopLeft.Row AndAlso prevColumn > BlockTopLeft.Column AndAlso c < BlockTopLeft.Column Then
          ' Flip
          BlockBottomRight.Row = BlockTopLeft.Row
          BlockBottomRight.Column = BlockTopLeft.Column - 1
          BlockTopLeft.Row = r
          BlockTopLeft.Column = c
        ElseIf prevLine = BlockBottomRight.Row AndAlso prevColumn > BlockTopLeft.Column AndAlso c < BlockBottomRight.Column Then
          ' Shrink
          BlockBottomRight.Column = CurrentColumn - 1
        ElseIf prevLine > BlockTopLeft.Row AndAlso r <= BlockBottomRight.Row Then
          ' Shrink
          BlockBottomRight.Row = CurrentLine
          BlockBottomRight.Column = CurrentColumn - 1
        Else
          ' Expand
          If CurrentLine < BlockTopLeft.Row Then BlockTopLeft.Row = CurrentLine
          If CurrentColumn < BlockTopLeft.Column Then BlockTopLeft.Column = CurrentColumn
        End If
      End If
      RaiseEvent SelectionChanged()
    Else
      ClearSelection()
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
             ":"c, ";"c, ","c, "."c, "("c, ")"c, "["c, "]"c, "^"c,
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
      Dim prevLine = CurrentLine, prevColumn = CurrentColumn
      CurrentLine = r : CursorDown()
      CurrentColumn = c : CursorRight()
      'HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
      If BlockTopLeft Is Nothing Then
        BlockTopLeft = New Location(prevLine, prevColumn)
        BlockBottomRight = New Location(r, c - 1)
      Else
        If prevLine = BlockBottomRight.Row AndAlso prevColumn < BlockBottomRight.Column AndAlso c > BlockBottomRight.Column Then
          ' Flip
          BlockTopLeft.Row = BlockBottomRight.Row
          BlockTopLeft.Column = BlockBottomRight.Column + 1
          BlockBottomRight.Row = r
          BlockBottomRight.Column = c
        ElseIf prevLine = BlockTopLeft.Row AndAlso prevColumn < BlockBottomRight.Column AndAlso c > BlockTopLeft.Column Then
          ' Shrink
          BlockTopLeft.Column = CurrentColumn '- 1
        ElseIf prevLine < BlockBottomRight.Row AndAlso r >= BlockTopLeft.Row Then
          ' Shrink
          BlockTopLeft.Row = CurrentLine
          BlockTopLeft.Column = CurrentColumn - 1
        Else
          ' Grow
          If CurrentLine > BlockBottomRight.Row Then BlockBottomRight.Row = CurrentLine
          If CurrentColumn > BlockBottomRight.Column Then BlockBottomRight.Column = CurrentColumn - 1
        End If
      End If
      RaiseEvent SelectionChanged()
    Else
      ClearSelection()
      CurrentLine = r : CursorDown()
      CurrentColumn = c + 1 : CursorRight()
    End If
  End Sub

  Private Sub DocumentEndAction(shift As Boolean)
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
    Dim pages = m_document.Count \ TextRows
    TopTextLine = pages * TextRows
    CurrentLine = m_document.Count : CurrentColumn = 1
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub DocumentHomeAction(shift As Boolean)
    If CurrentLine = 1 AndAlso CurrentColumn = 1 Then Return
    Dim prevLine = CurrentLine, prevColumn = CurrentColumn
    CurrentLine = 1 : CurrentColumn = 1 : TopTextLine = 1
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

#End Region

  Private Sub EscapeAction()
    ClearSelection()
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

  Private Sub CutCurrentLineAction()
    ClearSelection()
    If CurrentLine < m_document.Count Then
      'TODO: The original would "mark" the line before doing the following.
      '      Not sure if I want to recreate this as don't know if this was
      '      done on purpose or as part of a more generic cut routine...
      '      thus a visual artifact of the approach.
      m_copyBuffer.Clear()
      m_copyBuffer.Add(m_document(CurrentLine - 1) & vbLf)
      m_document.RemoveAt(CurrentLine - 1)
      Changed = True
      RaiseEvent SelectionChanged()
    End If
  End Sub

  Private Sub CopyAction()
    CopyBlock()
  End Sub

  Private Sub DeleteAction(shift As Boolean)
    If shift AndAlso BlockTopLeft IsNot Nothing Then
      CopyBlock()
      RemoveBlock() 'True)
    Else
      If BlockTopLeft IsNot Nothing Then
        RemoveBlock() 'False)
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
        ClearSelection()
      End If
    End If
  End Sub

  Private Sub DeleteRestOfWordAction()
    If CurrentLine > m_document.Count Then Return
    If CurrentColumn > m_document(CurrentLine - 1).Length Then
      If CurrentLine = m_document.Count Then Return
      m_document(CurrentLine - 1) &= m_document(CurrentLine)
      m_document.RemoveAt(CurrentLine)
    Else
      Dim ch = m_document(CurrentLine - 1)(CurrentColumn - 1)
      Dim spc = ch = " "c
      Dim c = CurrentColumn + 1
      Do
        If c > m_document(CurrentLine - 1).Length Then c -= 1 : Exit Do
        If (spc AndAlso m_document(CurrentLine - 1)(c - 1) <> " "c) OrElse
           (Not spc AndAlso m_document(CurrentLine - 1)(c - 1) = " "c) Then
          c -= 1 : Exit Do
        Else
          c += 1
        End If
      Loop
      Dim leftSide = If(CurrentColumn - 1 > 0, m_document(CurrentLine - 1).Substring(0, CurrentColumn - 1), "")
      Dim rightSide = If(c < m_document(CurrentLine - 1).Length, m_document(CurrentLine - 1).Substring(c), "")
      m_document(CurrentLine - 1) = leftSide & rightSide
    End If
    Changed = True
    ClearSelection()
  End Sub

  Friend Sub InsertAction(shift As Boolean)
    If shift Then
      If m_copyBuffer.Count > 0 Then
        If BlockTopLeft IsNot Nothing Then RemoveBlock() 'False)
        Dim r = CurrentLine
        Dim c = CurrentColumn
        If m_copyBuffer(0).EndsWith(vbLf) Then
          ' insert the line(s) above (removing the vbLf from the end)
          For index = m_copyBuffer.Count - 1 To 0 Step -1
            m_document.Insert(r - 1, m_copyBuffer(index).Substring(0, m_copyBuffer(index).Length - 1))
          Next
          Changed = True
        Else
          ' paste copy buffer at cursor location.
          Dim leftSide = If(c > 0, m_document(r - 1).Substring(0, c - 1), "")
          Dim rightSide = If(c < m_document(r - 1).Length, m_document(r - 1).Substring(c - 1), "")
          m_document(r - 1) = leftSide & m_copyBuffer(0) & rightSide
          Changed = True
        End If
      End If
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
    ClearSelection()
  End Sub

  Private Sub ScrollDownAction(shift As Boolean)
    If CurrentLine < m_document.Count Then TopTextLine += 1
    If CurrentLine < TopTextLine Then CurrentLine = TopTextLine
    If Not shift Then ClearSelection()
  End Sub

  Private Sub ScrollUpAction(shift As Boolean)
    If TopTextLine > 1 Then TopTextLine -= 1
    If CurrentLine > (TopTextLine - 1) + TextRows - 1 Then CurrentLine = (TopTextLine - 1) + TextRows - 1
    If Not shift Then ClearSelection()
  End Sub

  Private Sub ScrollWindowLeftAction(shift As Boolean)
    Dim prevColumn = CurrentColumn, prevLine = CurrentLine
    LeftTextColumn -= 78
    If LeftTextColumn < 1 Then LeftTextColumn = 1
    If CurrentColumn - 78 > 0 Then CurrentColumn -= 78
    CursorLeft()
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
  End Sub

  Private Sub ScrollWindowRightAction(shift As Boolean)
    'TODO: Should this be limited? It is in the original,
    'but not really sure exactly *how* it is limited (but it certainly is).
    Dim prevColumn = CurrentColumn, prevLine = CurrentLine
    LeftTextColumn += 78
    CurrentColumn += 78
    CursorRight()
    If shift Then
      HandleSelection(prevLine, prevColumn, CurrentLine, CurrentColumn)
    Else
      ClearSelection()
    End If
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
        ClearSelection()
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
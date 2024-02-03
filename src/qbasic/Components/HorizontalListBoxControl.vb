Public Class HorizontalListBoxControl
  Inherits Control

  Public Property Items As New List(Of String)

  Private m_visibleIndex As Integer = 0
  Public Property SelectedIndex As Integer = -1

  Public Event SelectedIndexChanged As EventHandler(Of EventArgs)

  Public ReadOnly Property SelectedItem As String
    Get
      If SelectedIndex > -1 AndAlso SelectedIndex < Items.Count Then
        Return Items(SelectedIndex)
      Else
        Return Nothing
      End If
    End Get
  End Property

  Public Sub New(parent As Control)
    Me.Parent = parent
    CursorVisible = True
    Clear()
  End Sub

  Public Sub Clear()
    m_cursorRow = Location.Row
    m_cursorCol = Location.Col
    Items.Clear()
    SelectedIndex = -1
    m_visibleIndex = 0
  End Sub

  Public Overrides Sub OnKeyPress(e As KeyPressEventArgs)

    Dim h = Size.Rows - 2
    Dim cOffset = 12
    For Each item In Items
      If item.Length > cOffset Then cOffset = item.Length
    Next
    Dim visibleColumns = Size.Cols / cOffset
    Dim c = Size.Cols \ cOffset
    c = If(visibleColumns > c * 1.1, c + 1, c)
    Dim col = SelectedIndex \ cOffset
    Dim visibleMax = m_visibleIndex + (c * h) - 1

    Select Case e.Key
      Case ConsoleKey.Home
        m_visibleIndex = 0 : SelectedIndex = 0 : e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.End
        m_visibleIndex = ((Items.Count \ h) - (Size.Cols \ cOffset)) * h
        SelectedIndex = Items.Count - 1
        e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.LeftArrow
        If SelectedIndex - h > 0 Then
          SelectedIndex -= h
          If SelectedIndex >= m_visibleIndex AndAlso SelectedIndex <= visibleMax Then
          Else
            m_visibleIndex -= h
          End If
        Else
          m_visibleIndex = 0
          SelectedIndex = 0
        End If
        e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.RightArrow
        If SelectedIndex = -1 Then SelectedIndex = 0
        If SelectedIndex + h < Items.Count Then SelectedIndex += h Else SelectedIndex = Items.Count - 1
        If SelectedIndex >= m_visibleIndex AndAlso SelectedIndex <= visibleMax Then
        Else
          m_visibleIndex += h
        End If
        e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.UpArrow
        If SelectedIndex > 0 Then SelectedIndex -= 1
        If SelectedIndex >= m_visibleIndex AndAlso SelectedIndex <= visibleMax Then
        Else
          m_visibleIndex -= h
        End If
        e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.DownArrow
        If SelectedIndex < Items.Count - 1 Then SelectedIndex += 1
        If SelectedIndex >= m_visibleIndex AndAlso SelectedIndex <= visibleMax Then
        Else
          m_visibleIndex += h
        End If
        e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case Else
    End Select

  End Sub

  Public Overrides Sub OnDraw()

    Dim top = If(Parent?.Location.Row, 1) + Location.Row - 1
    Dim left = If(Parent?.Location.Col, 1) + Location.Col - 1

    Box0(top, left, top + Size.Rows - 1, left + Size.Cols - 1, 1, OneColor(Foreground, Background))
    Dim index = 0
    Dim col = 0
    Dim cOffset = 0
    For Each item In Items
      If item.Length > cOffset Then cOffset = item.Length
    Next
    Dim ulRow = top + 1
    Dim ulCol = left + 2
    Dim lrRow = top + Size.Rows - 1
    Dim lrCol = left + Size.Cols - 1
    Do
      If ulCol + (col * (cOffset + 2)) > lrCol Then Exit Do
      If (col * 8) + index > Items.Count - 1 Then Exit Do
      Dim item = ""
      If (col * 8) + index + m_visibleIndex < Items.Count Then
        item = $" {Items((col * 8) + index + m_visibleIndex).PadRight(cOffset)} "
      End If
      If ulCol - 1 + (col * (cOffset + 2)) + item.Length > lrCol Then
        ' need to shorten...
        Dim max = lrCol - (ulCol - 1 + (col * (cOffset + 2)))
        item = item.Substring(0, max)
      End If
      If (col * 8) + index = SelectedIndex - m_visibleIndex Then
        QPrintRC(item, ulRow + index, ulCol + (col * (cOffset + 2)) - 1, OneColor(Background, Foreground))
        m_cursorRow = ulRow + index
        m_cursorCol = ulCol + (col * (cOffset + 2))
      Else
        QPrintRC(item, ulRow + index, ulCol + (col * (cOffset + 2)) - 1, OneColor(Foreground, Background))
      End If
      index += 1
      If index > 7 Then col += 1 : index = 0
    Loop

    If SelectedIndex = -1 Then
      m_cursorRow = ulRow
      m_cursorCol = ulCol
    End If

    HScrollBar(lrRow, ulCol - 1, lrCol - 1, Foreground)

  End Sub

End Class
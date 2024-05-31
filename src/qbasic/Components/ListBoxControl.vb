Public Class ListboxControl
  Inherits Control

  Public Property Items As New List(Of String)

  Private m_visibleIndex As Integer = 0
  Public Property SelectedIndex As Integer = -1

  Public Event SelectedIndexChanged As EventHandler(Of EventArgs)

  Public ReadOnly Property SelectedItem As String
    Get
      If SelectedIndex > -1 Then
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
    Dim visibleMax = m_visibleIndex + h - 1

    Select Case e.Key
      Case ConsoleKey.Home
        m_visibleIndex = 0 : SelectedIndex = 0 : e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.End
        m_visibleIndex = Items.Count - h : If m_visibleIndex < 0 Then m_visibleIndex = 0
        SelectedIndex = Items.Count - 1
        e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.UpArrow, ConsoleKey.LeftArrow
        If SelectedIndex > 0 Then SelectedIndex -= 1
        If SelectedIndex >= m_visibleIndex AndAlso SelectedIndex <= visibleMax Then
        Else
          m_visibleIndex -= 1
        End If
        e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.DownArrow, ConsoleKey.RightArrow
        If SelectedIndex < Items.Count - 1 Then SelectedIndex += 1
        If SelectedIndex >= m_visibleIndex AndAlso SelectedIndex <= visibleMax Then
        Else
          m_visibleIndex += 1
        End If
        e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.PageUp
        If m_visibleIndex - h > 0 Then
          m_visibleIndex -= h
          SelectedIndex = m_visibleIndex + h - 1
        Else
          m_visibleIndex = 0
          SelectedIndex = h - 1
          If SelectedIndex > Items.Count - 1 Then SelectedIndex = Items.Count - 1
        End If
        e.Handled = True
        RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
      Case ConsoleKey.PageDown
        If m_visibleIndex + h < Items.Count - 1 Then
          m_visibleIndex += h
          If m_visibleIndex > Items.Count - 1 - h Then
            m_visibleIndex = Items.Count - 1 - h
          End If
          SelectedIndex = m_visibleIndex
          e.Handled = True
          RaiseEvent SelectedIndexChanged(Me, EventArgs.Empty)
        End If
      Case Else
    End Select

  End Sub

  Public Overrides Sub OnDraw()

    Dim top = If(Parent?.Location.Row, 1) + Location.Row - 1
    Dim left = If(Parent?.Location.Col, 1) + Location.Col - 1

    Box0(top, left, top + Size.Rows - 1, left + Size.Cols - 1, 1, OneColor(ForeColor, BackColor))

    Dim ulRow = top + 1
    Dim ulCol = left + 2
    Dim lrRow = top + Size.Rows - 1
    Dim lrCol = left + Size.Cols - 1
    Dim w = Size.Cols - 4
    Dim h = Size.Rows - 2

    Dim index = m_visibleIndex '0

    Do
      Dim txt = Items(index).Substring(0, MinInt(Items(index).Length, w))
      If index = SelectedIndex Then
        QPrintRC($" {txt.PadRight(w)} ", ulRow + index - m_visibleIndex, ulCol - 1, OneColor(BackColor, ForeColor))
        m_cursorRow = ulRow + index - m_visibleIndex
        m_cursorCol = ulCol
      Else
        QPrintRC(txt, ulRow + index - m_visibleIndex, ulCol, OneColor(ForeColor, BackColor))
      End If
      index += 1
      If index > m_visibleIndex + h - 1 OrElse index > Items.Count - 1 Then Exit Do
    Loop

    VScrollBar(0, ulRow, lrCol, lrRow - 1, ForeColor)

    If SelectedIndex = -1 Then
      m_cursorRow = ulRow
      m_cursorCol = ulCol
    End If

  End Sub

End Class
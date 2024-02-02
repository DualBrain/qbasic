Public Class Form
  Inherits Control

  ''' <summary>
  ''' Caption of the Form.
  ''' </summary>
  ''' <returns></returns>
  Public Property Text As String

  ''' <summary>
  ''' Contains the controls on the Form surface.
  ''' </summary>
  ''' <returns></returns>
  Public Property Controls As New List(Of Control)

  ''' <summary>
  ''' DialogResult will be set to this value if Enter is pressed.
  ''' </summary>
  ''' <returns></returns>
  Public Property AcceptAction As DialogResult = DialogResult.None
  ''' <summary>
  ''' DialogResult will be set to this value if Esc is pressed.
  ''' </summary>
  ''' <returns></returns>
  Public Property CancelAction As DialogResult = DialogResult.None
  ''' <summary>
  ''' DialogResult can be used to determine how the Form was closed.
  ''' </summary>
  ''' <returns></returns>
  Public Property DialogResult As DialogResult = DialogResult.None

  Public Sub New(text As String,
                 location As Location,
                 size As Size)
    Me.Text = text
    Me.Location = location
    Me.Size = size
    Visible = True
    Focused = True
  End Sub

  Public Overrides Sub OnKeyPress(e As KeyPressEventArgs)
    For Each control In Controls
      If control.Visible AndAlso control.Focused Then
        control.OnKeyPress(e)
        Exit For
      End If
    Next
    If Not e.Handled Then
      Select Case e.Key
        Case ConsoleKey.Tab
          Dim selectedIndex = 0
          For index = 0 To Controls.Count - 1
            If Controls(index).Focused Then
              selectedIndex = index : Exit For
            End If
          Next
          Controls(selectedIndex).Focused = False
          If e.Shift Then
            Do
              selectedIndex -= 1
              If selectedIndex < 0 Then selectedIndex = Controls.Count - 1
              If Controls(selectedIndex).Visible Then
                Controls(selectedIndex).Focused = True
                Exit Do
              End If
            Loop
          Else
            Do
              selectedIndex += 1
              If selectedIndex > Controls.Count - 1 Then selectedIndex = 0
              If Controls(selectedIndex).Visible Then
                Controls(selectedIndex).Focused = True
                Exit Do
              End If
            Loop
          End If
          e.Handled = True
          'Case ConsoleKey.Enter
          '  If AcceptAction <> DialogResult.None Then
          '    DialogResult = AcceptAction
          '    e.Handled = True
          '  End If
          'Case ConsoleKey.Escape
          '  If CancelAction <> DialogResult.None Then
          '    DialogResult = CancelAction
          '    e.Handled = True
          '  End If
        Case Else
      End Select
    End If
  End Sub

  Public Overrides Sub OnDraw()

    Dim lrRow = Location.Row + Size.Rows - 1
    Dim lrCol = Location.Col + Size.Cols - 1

    PaintBox0(Location.Row + 1, Location.Col + 2, lrRow + 1, lrCol + 2, OneColor(7, 0))
    ClearScr0(Location.Row, Location.Col, lrRow, lrCol, OneColor(Foreground, Background))
    Box0(Location.Row, Location.Col, lrRow, lrCol, 1, OneColor(Foreground, Background))

    If Not String.IsNullOrEmpty(Text) Then
      Dim text = $" {Me.Text} "
      Dim textOffset = CInt((Size.Cols - text.Length) / 2)
      QPrintRC(text, Location.Row, Location.Col + textOffset, OneColor(Foreground, Background))
    End If

    Dim btnHasFocus = False
    For Each control In Controls
      If TypeOf control Is ButtonControl Then
        CType(control, ButtonControl).Highlight = False
        If control.Focused Then btnHasFocus = True
      End If
    Next
    If Not btnHasFocus Then
      For Each control In Controls
        If TypeOf control Is ButtonControl AndAlso CType(control, ButtonControl).Default Then CType(control, ButtonControl).Highlight = True : Exit For
      Next
    End If

    For Each control In Controls
      control.OnDraw()
    Next

    For index = 0 To Controls.Count - 1
      If Controls(index).Focused Then
        m_cursorRow = Controls(index).CursorRow
        m_cursorCol = Controls(index).CursorCol
        Exit For
      End If
    Next

  End Sub

End Class
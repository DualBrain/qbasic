Public Class PrintDialog
  Inherits Form
  Implements IContext

  Public Shadows ReadOnly Property CursorRow As Integer Implements IContext.CursorRow
    Get
      Return MyBase.CursorRow
    End Get
  End Property

  Public Shadows ReadOnly Property CursorCol As Integer Implements IContext.CursorCol
    Get
      Return MyBase.CursorCol
    End Get
  End Property

  Private WithEvents SelectedText As New RadioButtonControl(Me)
  Private WithEvents CurrentWindow As New RadioButtonControl(Me)
  Private WithEvents EntireProgram As New RadioButtonControl(Me)

  Private WithEvents OkButton As New ButtonControl(Me)
  Private WithEvents CancelButton As New ButtonControl(Me)
  Private WithEvents HelpButton As New ButtonControl(Me)

  Sub New()
    MyBase.New("Print", New Size(9, 34))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    SelectedText.Location = New Location(3, 8)
    SelectedText.TabOrder = 0
    SelectedText.Text = "Selected Text Only"
    Controls.Add(SelectedText)

    CurrentWindow.Location = New Location(4, 8)
    CurrentWindow.TabOrder = 0
    CurrentWindow.Text = "Current Window"
    Controls.Add(CurrentWindow)

    EntireProgram.Location = New Location(5, 8)
    EntireProgram.TabOrder = 0
    EntireProgram.Text = "Entire Program"
    EntireProgram.Checked = True
    EntireProgram.Focused = True
    Controls.Add(EntireProgram)

    OkButton.Text = "< OK >"
    OkButton.Default = True
    OkButton.Location = New Location(8, 4)
    OkButton.Focused = False
    OkButton.TabStop = True
    OkButton.TabOrder = 1
    Controls.Add(OkButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Default = False
    CancelButton.Location = New Location(8, 12)
    CancelButton.Focused = False
    CancelButton.TabStop = True
    CancelButton.TabOrder = 2
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Default = False
    HelpButton.Location = New Location(8, 24)
    HelpButton.Focused = False
    HelpButton.TabStop = True
    HelpButton.TabOrder = 3
    Controls.Add(HelpButton)

  End Sub

  Public Sub Render() Implements IContext.Render
    OnDraw()
    HLine(Location.Row + 6, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(Foreground, Background))
  End Sub

  Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys

    If keys?.Count > 0 Then
      For Each key In keys
        Dim e = New KeyPressEventArgs(key, capsLock, ctrl, alt, shift)
        OnKeyPress(e)
        If Not e.Handled Then
          Select Case e.Key
            Case ConsoleKey.Enter
            Case ConsoleKey.Escape : Return False
            Case Else
          End Select
        End If
      Next
    End If

    Return True

  End Function

End Class
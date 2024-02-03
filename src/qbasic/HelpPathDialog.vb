Public Class HelpPathDialog
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

  Private WithEvents PathTextbox As New TextboxControl(Me)

  Private WithEvents OkButton As New ButtonControl(Me)
  Private WithEvents CancelButton As New ButtonControl(Me)
  Private WithEvents HelpButton As New ButtonControl(Me)

  Sub New()
    MyBase.New("Help Path", New Location(8, 13), New Size(10, 55))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    PathTextbox.Location = New Location(6, 4)
    PathTextbox.Size = New Size(1, 42)
    PathTextbox.TabOrder = 0
    PathTextbox.Text = ""
    PathTextbox.Focused = True
    Controls.Add(PathTextbox)

    OkButton.Text = "< OK >"
    OkButton.Default = True
    OkButton.Location = New Location(9, 10)
    OkButton.TabOrder = 1
    Controls.Add(OkButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Location = New Location(9, 25)
    CancelButton.TabOrder = 2
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Location = New Location(9, 44)
    HelpButton.TabOrder = 3
    Controls.Add(HelpButton)

  End Sub

  Public Sub Render() Implements IContext.Render
    OnDraw()
    QPrintRC("Location (path) of QBASIC.HLP file:", Location.Row + 2, Location.Col + 10, OneColor(Foreground, Background))
    Box0(Location.Row + 4, Location.Col + 2, Location.Row + 6, Location.Col + Size.Cols - 3, 1, OneColor(Foreground, Background))
    HLine(Location.Row + 7, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(Foreground, Background))
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
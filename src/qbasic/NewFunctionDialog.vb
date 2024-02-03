Public Class NewFunctionDialog
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

  Private WithEvents NameTextbox As New TextBoxControl(Me)

  Private WithEvents OkButton As New ButtonControl(Me)
  Private WithEvents CancelButton As New ButtonControl(Me)
  Private WithEvents HelpButton As New ButtonControl(Me)

  Sub New()
    MyBase.New("New FUNCTION", New Location(9, 22), New Size(8, 38))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    NameTextbox.Location = New Location(3, 10)
    NameTextbox.Size = New Size(1, 26)
    NameTextbox.TabOrder = 0
    NameTextbox.Text = ""
    NameTextbox.Focused = True
    Controls.Add(NameTextbox)

    OkButton.Text = "< OK >"
    OkButton.Default = True
    OkButton.Location = New Location(7, 4)
    OkButton.Focused = False
    OkButton.TabStop = True
    OkButton.TabOrder = 1
    Controls.Add(OkButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Default = False
    CancelButton.Location = New Location(7, 14)
    CancelButton.Focused = False
    CancelButton.TabStop = True
    CancelButton.TabOrder = 2
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Default = False
    HelpButton.Location = New Location(7, 28)
    HelpButton.Focused = False
    HelpButton.TabStop = True
    HelpButton.TabOrder = 3
    Controls.Add(HelpButton)

  End Sub

  Public Sub Render() Implements IContext.Render
    OnDraw()
    QPrintRC("Name:", Location.Row + 2, Location.Col + 2, OneColor(Foreground, Background))
    Box0(Location.Row + 1, Location.Col + 8, Location.Row + 3, Location.Col + 35, 1, OneColor(Foreground, Background))
    HLine(Location.Row + 5, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(Foreground, Background))
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
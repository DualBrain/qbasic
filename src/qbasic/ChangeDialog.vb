Public Class ChangeDialog
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

  Private WithEvents FindTextbox As New TextboxControl(Me)
  Private WithEvents ChangeTextbox As New TextboxControl(Me)

  Private WithEvents MatchCaseCheckbox As New CheckboxControl(Me)
  Private WithEvents WholeWordCheckbox As New CheckboxControl(Me)

  Private WithEvents FindVerifyButton As New ButtonControl(Me)
  Private WithEvents ChangeAllButton As New ButtonControl(Me)
  Private WithEvents CancelButton As New ButtonControl(Me)
  Private WithEvents HelpButton As New ButtonControl(Me)

  Sub New()
    MyBase.New("Change", New Location(6, 11), New Size(14, 59))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    FindTextbox.Location = New Location(3, 15)
    FindTextbox.Size = New Size(1, 41)
    FindTextbox.TabOrder = 0
    FindTextbox.Text = ""
    FindTextbox.Focused = True
    Controls.Add(FindTextbox)

    ChangeTextbox.Location = New Location(6, 15)
    ChangeTextbox.Size = New Size(1, 41)
    ChangeTextbox.TabOrder = 1
    ChangeTextbox.Text = ""
    Controls.Add(ChangeTextbox)

    MatchCaseCheckbox.Text = "Match Upper/Lowercase"
    MatchCaseCheckbox.Location = New Location(10, 6)
    MatchCaseCheckbox.TabOrder = 2
    Controls.Add(MatchCaseCheckbox)

    WholeWordCheckbox.Text = "Whole Word"
    WholeWordCheckbox.Location = New Location(10, 40)
    WholeWordCheckbox.TabOrder = 3
    Controls.Add(WholeWordCheckbox)

    FindVerifyButton.Text = "< Find and Verify >"
    FindVerifyButton.Default = True
    FindVerifyButton.Location = New Location(13, 3)
    FindVerifyButton.TabOrder = 4
    Controls.Add(FindVerifyButton)

    ChangeAllButton.Text = "< Change All >"
    ChangeAllButton.Default = True
    ChangeAllButton.Location = New Location(13, 23)
    ChangeAllButton.TabOrder = 5
    Controls.Add(ChangeAllButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Location = New Location(13, 38)
    CancelButton.TabOrder = 6
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Location = New Location(13, 49)
    HelpButton.TabOrder = 7
    Controls.Add(HelpButton)

  End Sub

  Public Sub Render() Implements IContext.Render
    OnDraw()
    QPrintRC("Find What:", Location.Row + 2, Location.Col + 2, OneColor(Foreground, Background))
    QPrintRC("Change To:", Location.Row + 5, Location.Col + 2, OneColor(Foreground, Background))
    Box0(Location.Row + 1, Location.Col + 13, Location.Row + 3, Location.Col + 56, 1, OneColor(Foreground, Background))
    Box0(Location.Row + 4, Location.Col + 13, Location.Row + 6, Location.Col + 56, 1, OneColor(Foreground, Background))
    HLine(Location.Row + 11, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(Foreground, Background))
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
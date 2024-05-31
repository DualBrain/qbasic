Public Class FindDialog
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

  Private WithEvents FindTextbox As New TextBoxControl(Me)

  Private WithEvents MatchCaseCheckbox As New CheckboxControl(Me)
  Private WithEvents WholeWordCheckbox As New CheckboxControl(Me)

  Private WithEvents OkButton As New ButtonControl(Me)
  Private WithEvents CancelButton As New ButtonControl(Me)
  Private WithEvents HelpButton As New ButtonControl(Me)

  Sub New()
    MyBase.New("Find", New Location(8, 10), New Size(11, 60))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    FindTextbox.Location = New Location(3, 15)
    FindTextbox.Size = New Size(1, 42)
    FindTextbox.TabIndex = 0
    FindTextbox.Text = ""
    FindTextbox.Focused = True
    Controls.Add(FindTextbox)

    MatchCaseCheckbox.Text = "Match Upper/Lowercase"
    MatchCaseCheckbox.Location = New Location(7, 6)
    MatchCaseCheckbox.TabIndex = 1
    Controls.Add(MatchCaseCheckbox)

    WholeWordCheckbox.Text = "Whole Word"
    WholeWordCheckbox.Location = New Location(7, 40)
    WholeWordCheckbox.TabIndex = 2
    Controls.Add(WholeWordCheckbox)

    OkButton.Text = "< OK >"
    OkButton.Default = True
    OkButton.Location = New Location(10, 10)
    OkButton.TabIndex = 3
    Controls.Add(OkButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Location = New Location(10, 25)
    CancelButton.TabIndex = 4
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Location = New Location(10, 44)
    HelpButton.TabIndex = 5
    Controls.Add(HelpButton)

  End Sub

  Public Sub Render() Implements IContext.Render
    OnDraw()
    QPrintRC("Find What:", Location.Row + 2, Location.Col + 2, OneColor(ForeColor, BackColor))
    Box0(Location.Row + 1, Location.Col + 13, Location.Row + 3, Location.Col + 57, 1, OneColor(ForeColor, BackColor))
    HLine(Location.Row + 8, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(ForeColor, BackColor))
  End Sub

  Public Function ProcessKeys(keys As List(Of ConsoleKey),
                              capsLock As Boolean,
                              ctrl As Boolean,
                              alt As Boolean,
                              shift As Boolean,
                              mButton As Boolean,
                              mRow As Integer,
                              mCol As Integer) As Boolean Implements IContext.ProcessKeys

    If mButton Then
      For Each control In Controls
        If control.MouseHit(mRow, mCol) Then
          If Not control.Focused Then
            For index = 0 To Controls.Count - 1
              If Controls(index).Focused Then Controls(index).Focused = False : Exit For
            Next
            control.Focused = True
          End If
          'DialogResult = If(Controls(0).Focused, DialogResult.Ok, DialogResult.Cancel)
          'Return False
          Exit For
        End If
      Next
    End If

    If keys?.Count > 0 Then
      For Each key In keys
        Dim e = New KeyPressEventArgs(key, capsLock, ctrl, alt, shift)
        OnKeyPress(e)
        If Not e.Handled Then
          Select Case e.Key
            Case ConsoleKey.Enter
            Case ConsoleKey.Escape : DialogResult = CancelAction : Return False
            Case Else
          End Select
        End If
      Next
    End If

    Return True

  End Function

End Class
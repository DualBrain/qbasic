Public Class NewSubDialog
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
    MyBase.New("New SUB", New Location(9, 22), New Size(8, 38))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    NameTextbox.Location = New Location(3, 10)
    NameTextbox.Size = New Size(1, 26)
    NameTextbox.TabIndex = 0
    NameTextbox.Text = ""
    NameTextbox.Focused = True
    Controls.Add(NameTextbox)

    OkButton.Text = "< OK >"
    OkButton.Default = True
    OkButton.Location = New Location(7, 4)
    OkButton.Focused = False
    OkButton.TabStop = True
    OkButton.TabIndex = 1
    Controls.Add(OkButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Default = False
    CancelButton.Location = New Location(7, 14)
    CancelButton.Focused = False
    CancelButton.TabStop = True
    CancelButton.TabIndex = 2
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Default = False
    HelpButton.Location = New Location(7, 28)
    HelpButton.Focused = False
    HelpButton.TabStop = True
    HelpButton.TabIndex = 3
    Controls.Add(HelpButton)

  End Sub

  Public Sub Render() Implements IContext.Render
    OnDraw()
    QPrintRC("Name:", Location.Row + 2, Location.Col + 2, OneColor(ForeColor, BackColor))
    Box0(Location.Row + 1, Location.Col + 8, Location.Row + 3, Location.Col + 35, 1, OneColor(ForeColor, BackColor))
    HLine(Location.Row + 5, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(ForeColor, BackColor))
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
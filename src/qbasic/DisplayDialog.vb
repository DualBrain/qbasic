Public Class DisplayDialog
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

  Private WithEvents NormalText As New RadioButtonControl(Me)
  Private WithEvents CurrentStatement As New RadioButtonControl(Me)
  Private WithEvents BreakpointLines As New RadioButtonControl(Me)

  Private WithEvents ForegroundListbox As New ListboxControl(Me)
  Private WithEvents BackgroundListbox As New ListboxControl(Me)

  Private WithEvents ScrollBarsCheckbox As New CheckboxControl(Me)
  Private WithEvents TabStopsTextbox As New TextboxControl(Me)

  Private WithEvents OkButton As New ButtonControl(Me)
  Private WithEvents CancelButton As New ButtonControl(Me)
  Private WithEvents HelpButton As New ButtonControl(Me)

  Sub New()
    MyBase.New("Display", New Location(2, 11), New Size(22, 60))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    NormalText.Location = New Location(5, 5)
    NormalText.TabOrder = 0
    NormalText.Text = "1."
    NormalText.Checked = True
    NormalText.Focused = True
    Controls.Add(NormalText)

    CurrentStatement.Location = New Location(7, 5)
    CurrentStatement.TabOrder = 0
    CurrentStatement.Text = "2."
    Controls.Add(CurrentStatement)

    BreakpointLines.Location = New Location(9, 5)
    BreakpointLines.TabOrder = 0
    BreakpointLines.Text = "3."
    Controls.Add(BreakpointLines)

    ForegroundListbox.Location = New Location(4, 32)
    ForegroundListbox.Size = New Size(10, 11)
    ForegroundListbox.TabOrder = 1
    Controls.Add(ForegroundListbox)

    BackgroundListbox.Location = New Location(4, 45)
    BackgroundListbox.Size = New Size(10, 11)
    BackgroundListbox.TabOrder = 2
    Controls.Add(BackgroundListbox)

    ScrollBarsCheckbox.Location = New Location(18, 7)
    ScrollBarsCheckbox.Text = "Scroll Bars"
    ScrollBarsCheckbox.Checked = True
    ScrollBarsCheckbox.TabOrder = 3
    Controls.Add(ScrollBarsCheckbox)

    TabStopsTextbox.Location = New Location(18, 51)
    TabStopsTextbox.Size = New Size(1, 2)
    TabStopsTextbox.Text = "2"
    TabStopsTextbox.TabOrder = 4
    Controls.Add(TabStopsTextbox)

    OkButton.Text = "< OK >"
    OkButton.Default = True
    OkButton.Location = New Location(21, 11)
    OkButton.TabOrder = 5
    Controls.Add(OkButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Location = New Location(21, 25)
    CancelButton.TabOrder = 6
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Location = New Location(21, 41)
    HelpButton.TabOrder = 7
    Controls.Add(HelpButton)

    ForegroundListbox.Items.Add("Black")
    ForegroundListbox.Items.Add("Blue")
    ForegroundListbox.Items.Add("Green")
    ForegroundListbox.Items.Add("Cyan")
    ForegroundListbox.Items.Add("Red")
    ForegroundListbox.Items.Add("Magenta")
    ForegroundListbox.Items.Add("Brown")
    ForegroundListbox.Items.Add("White")
    ForegroundListbox.Items.Add("Gray")
    ForegroundListbox.Items.Add("BrBlue")
    ForegroundListbox.Items.Add("BrGreen")
    ForegroundListbox.Items.Add("BrCyan")
    ForegroundListbox.Items.Add("BrRed")
    ForegroundListbox.Items.Add("Pink")
    ForegroundListbox.Items.Add("Yellow")
    ForegroundListbox.Items.Add("BrWhite")

    BackgroundListbox.Items.Add("Black")
    BackgroundListbox.Items.Add("Blue")
    BackgroundListbox.Items.Add("Green")
    BackgroundListbox.Items.Add("Cyan")
    BackgroundListbox.Items.Add("Red")
    BackgroundListbox.Items.Add("Magenta")
    BackgroundListbox.Items.Add("Brown")
    BackgroundListbox.Items.Add("White")
    BackgroundListbox.Items.Add("Gray")
    BackgroundListbox.Items.Add("BrBlue")
    BackgroundListbox.Items.Add("BrGreen")
    BackgroundListbox.Items.Add("BrCyan")
    BackgroundListbox.Items.Add("BrRed")
    BackgroundListbox.Items.Add("Pink")
    BackgroundListbox.Items.Add("Yellow")
    BackgroundListbox.Items.Add("BrWhite")

  End Sub

  Public Sub Render() Implements IContext.Render
    OnDraw()
    Box0(Location.Row + 1, Location.Col + 2, Location.Row + 14, Location.Col + Size.Cols - 3, 1, OneColor(Foreground, Background))
    QPrintRC(" Colors ", Location.Row + 1, Location.Col + 27, OneColor(Foreground, Background))
    QPrintRC(" Normal Text       ", Location.Row + 4, Location.Col + 11, OneColor(8, 1))
    QPrintRC(" Current Statement ", Location.Row + 6, Location.Col + 11, OneColor(15, 1))
    QPrintRC(" Breakpoint Lines  ", Location.Row + 8, Location.Col + 11, OneColor(8, 4))
    QPrintRC("Foreground", Location.Row + 2, Location.Col + 32, OneColor(Foreground, Background))
    QPrintRC("Background", Location.Row + 2, Location.Col + 45, OneColor(Foreground, Background))
    Box0(Location.Row + 16, Location.Col + 2, Location.Row + 18, Location.Col + Size.Cols - 3, 1, OneColor(Foreground, Background))
    QPrintRC(" Display Options ", Location.Row + 16, Location.Col + 21, OneColor(Foreground, Background))
    QPrintRC("Tab Stops:", Location.Row + 17, Location.Col + 39, OneColor(Foreground, Background))
    HLine(Location.Row + 19, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(Foreground, Background))
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
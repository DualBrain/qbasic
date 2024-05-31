Public Class SavePrompt
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

  Private WithEvents Button1 As New ButtonControl(Me)
  Private WithEvents Button2 As New ButtonControl(Me)
  Private WithEvents Button3 As New ButtonControl(Me)
  Private WithEvents Button4 As New ButtonControl(Me)

  Sub New()
    MyBase.New("", New Location(10, 18), New Size(7, 46))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    Button1.Text = "< Yes >"
    Button1.Default = True
    Button1.Focused = True
    Button1.Location = New Location(6, 5)
    Button1.TabIndex = 0
    Controls.Add(Button1)

    Button2.Text = "<  No  >"
    Button2.Location = New Location(6, 14)
    Button2.TabIndex = 1
    Controls.Add(Button2)

    Button3.Text = "<Cancel>"
    Button3.Location = New Location(6, 25)
    Button3.TabIndex = 2
    Controls.Add(Button3)

    Button4.Text = "< Help >"
    Button4.Location = New Location(6, 36)
    Button4.TabIndex = 3
    Controls.Add(Button4)

  End Sub

  Public Sub Render() Implements IContext.Render

    OnDraw()

    Dim welcomeText = "Loaded file is not saved. Save it now?"
    Dim textOffset = (Size.Cols - welcomeText.Length) \ 2
    QPrintRC(welcomeText, Location.Row + 2, Location.Col + textOffset, OneColor(0, 8))

    HLine(Location.Row + 4, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(ForeColor, BackColor))

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
          If Controls(0).Focused Then DialogResult = DialogResult.Yes
          If Controls(1).Focused Then DialogResult = DialogResult.No
          If Controls(2).Focused Then DialogResult = DialogResult.Cancel
          If Controls(3).Focused Then DialogResult = DialogResult.Help
          Return False
        End If
      Next
    End If

    If keys?.Count > 0 Then
      For Each key In keys
        Dim e = New KeyPressEventArgs(key, capsLock, ctrl, alt, shift)
        OnKeyPress(e)
        If Not e.Handled Then
          Select Case key
            Case ConsoleKey.Y : DialogResult = DialogResult.Yes : Return False ' Yes
            Case ConsoleKey.N : DialogResult = DialogResult.No : Return False ' No
            Case ConsoleKey.Escape : DialogResult = DialogResult.Cancel : Return False ' Cancel - NOTE: C doesn't appear to be valid in the original?
            Case ConsoleKey.H : DialogResult = DialogResult.Help : Return False ' Help
            Case ConsoleKey.F1 : DialogResult = DialogResult.Help : Return False ' Help (needs special handling, was m_selected = -1)
            Case ConsoleKey.Enter, ConsoleKey.Spacebar
              If Controls(0).Focused Then DialogResult = DialogResult.Yes
              If Controls(1).Focused Then DialogResult = DialogResult.No
              If Controls(2).Focused Then DialogResult = DialogResult.Cancel
              If Controls(3).Focused Then DialogResult = DialogResult.Help
              Return False ' Current selected button...
            Case Else
          End Select
        End If
      Next
    End If

    Return True

  End Function

End Class
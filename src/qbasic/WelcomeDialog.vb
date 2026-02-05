Public Class WelcomeDialog
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

  Private ReadOnly m_copyright As String = DetermineCopyright()

  Sub New()
    MyBase.New("", New Location(6, 10), New Size(11, 60))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    Button1.Text = "< Press Enter to see the Survival Guide >"
    Button1.Default = True
    Button1.Focused = True
    Button1.Location = New Location(8, 10)
    Button1.TabIndex = 0
    Controls.Add(Button1)

    Button2.Text = "< Press ESC to clear this dialog box >"
    Button2.Location = New Location(10, 12)
    Button2.TabIndex = 1
    Controls.Add(Button2)

  End Sub

  Public Sub Render() Implements IContext.Render

    OnDraw()

    Dim welcomeText = "Welcome to Community QBasic"
    Dim textOffset = (Size.Cols - welcomeText.Length) \ 2
    QPrintRC(welcomeText, Location.Row + 2, Location.Col + textOffset, OneColor(0, 8))

    Dim copyrightText = $"Copyright (C) {m_copyright}, 1964-2026"
    textOffset = (Size.Cols - copyrightText.Length) \ 2
    QPrintRC(copyrightText, Location.Row + 4, Location.Col + textOffset, OneColor(0, 8))

    Dim reservedText = "All rights reserved."
    textOffset = (Size.Cols - reservedText.Length) \ 2
    QPrintRC(reservedText, Location.Row + 5, Location.Col + textOffset, OneColor(0, 8))

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
          DialogResult = If(Controls(0).Focused, DialogResult.Ok, DialogResult.Cancel)
          Return False
        End If
      Next
    End If

    If keys?.Count > 0 Then
      For Each key In keys
        Dim e = New KeyPressEventArgs(key, capsLock, ctrl, alt, shift)
        OnKeyPress(e)
        If Not e.Handled Then
          Select Case e.Key
            Case ConsoleKey.Enter, ConsoleKey.Spacebar
              DialogResult = If(Controls(0).Focused, DialogResult.Ok, DialogResult.Cancel) : Return False
            Case ConsoleKey.Escape : DialogResult = CancelAction : Return False
            Case Else
          End Select
        End If
      Next
    End If

    Return True

  End Function

End Class
Public Class AboutDialog
  Inherits Form
  Implements IContext

  Private WithEvents Button1 As New ButtonControl(Me)

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

  Sub New()
    MyBase.New("", New Location(9, 14), New Size(9, 51))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    Button1.Text = "<  OK  >"
    Button1.Location = New Location(8, 22)
    Button1.Default = True
    Button1.Focused = True
    Button1.TabIndex = 0
    Controls.Add(Button1)

  End Sub

  Sub Render() Implements IContext.Render

    OnDraw()

    Dim title = "Community QBasic"
    Dim titleOffset = (Size.Cols - title.Length) \ 2
    QPrintRC(title, Location.Row + 2, Location.Col + titleOffset, OneColor(0, 8))
    Dim version = "Version 1.0"
    Dim versionOffset = (Size.Cols - version.Length) \ 2
    QPrintRC(version, Location.Row + 3, Location.Col + versionOffset, OneColor(0, 8))
    Dim copyright = "Copyright (C) Dartmouth Didn't, 1964-2026"
    Dim copyrightOffset = (Size.Cols - copyright.Length) \ 2
    QPrintRC(copyright, Location.Row + 4, Location.Col + copyrightOffset, OneColor(0, 8))

    HLine(Location.Row + 6, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(0, 8))

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
          DialogResult = DialogResult.Ok
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
              DialogResult = DialogResult.Ok : Return False
            Case ConsoleKey.Escape : DialogResult = CancelAction : Return False
            Case Else
          End Select
        End If
      Next
    End If

    Return True

  End Function

End Class
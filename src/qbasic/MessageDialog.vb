Public Class MessageDialog
  Inherits Form
  Implements IContext

  Private ReadOnly m_text As String

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

  Private WithEvents OkButton As New ButtonControl(Me)

  Sub New(text As String)
    MyBase.New("", New Location(8, 14), New Size(6, 52))
    m_text = text
    Dim lines = m_text.Split(vbLf)
    Dim needed = lines.Length + 5
    Dim row = (25 - needed) \ 2
    Location = New Location(row, 14)
    Size = New Size(needed + 1, 52)

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    OkButton.Text = "<  OK  >"
    OkButton.Location = New Location(Size.Rows - 1, 23)
    OkButton.Default = True
    OkButton.Focused = True
    OkButton.TabIndex = 0
    Controls.Add(OkButton)

  End Sub

  Public Sub Render() Implements IContext.Render

    OnDraw()

    Dim textOffset = (Size.Cols - m_text.Length) \ 2
    QPrintRC(m_text, Location.Row + 2, Location.Col + textOffset, OneColor(ForeColor, BackColor))

    HLine(Location.Row + Size.Rows - 3, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(ForeColor, BackColor))

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
        Select Case key
          Case ConsoleKey.Escape : DialogResult = CancelAction : Return False
          Case ConsoleKey.Enter, ConsoleKey.Spacebar : DialogResult = DialogResult.Ok : Return False
          Case ConsoleKey.F1
          Case ConsoleKey.Tab
          Case ConsoleKey.LeftArrow, ConsoleKey.RightArrow, ConsoleKey.UpArrow, ConsoleKey.DownArrow
          Case Else
        End Select
      Next
    End If

    Return True

  End Function

End Class
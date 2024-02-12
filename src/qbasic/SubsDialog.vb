Public Class SubsDialog
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

  Private WithEvents SubsHorizontalListBox As New HorizontalListboxControl(Me, 20)
  Private WithEvents EditInActiveButton As New ButtonControl(Me)
  Private WithEvents DeleteButton As New ButtonControl(Me)
  Private WithEvents CancelButton As New ButtonControl(Me)
  Private WithEvents HelpButton As New ButtonControl(Me)

  Sub New(name As String, subs As List(Of String))
    MyBase.New("SUBs", New Location(3, 4), New Size(18, 74))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    SubsHorizontalListBox.Location = New Location(4, 3)
    SubsHorizontalListBox.Size = New Size(11, 70)
    SubsHorizontalListBox.TabOrder = 0
    SubsHorizontalListBox.Focused = True
    SubsHorizontalListBox.Foreground = 0
    SubsHorizontalListBox.Background = 8
    Controls.Add(SubsHorizontalListBox)

    EditInActiveButton.Text = "< Edit in Active >"
    EditInActiveButton.Default = True
    EditInActiveButton.Location = New Location(17, 7)
    EditInActiveButton.Focused = False
    EditInActiveButton.TabStop = True
    EditInActiveButton.TabOrder = 1
    Controls.Add(EditInActiveButton)

    DeleteButton.Text = "< Delete >"
    DeleteButton.Default = False
    DeleteButton.Location = New Location(17, 30)
    DeleteButton.Focused = False
    DeleteButton.TabStop = True
    DeleteButton.TabOrder = 2
    Controls.Add(DeleteButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Default = False
    CancelButton.Location = New Location(17, 46)
    CancelButton.Focused = False
    CancelButton.TabStop = True
    CancelButton.TabOrder = 3
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Default = False
    HelpButton.Location = New Location(17, 61)
    HelpButton.Focused = False
    HelpButton.TabStop = True
    HelpButton.TabOrder = 4
    Controls.Add(HelpButton)

    SubsHorizontalListBox.Items.Add(name)
    'SubsHorizontalListBox.Items.AddRange(subs)
    For Each entry In subs
      SubsHorizontalListBox.Items.Add($"  {entry}")
    Next

  End Sub

  Public Sub Render() Implements IContext.Render

    OnDraw()

    Dim filename = "Untitled"

    QPrintRC("Choose program item to edit", Location.Row + 2, Location.Col + 2, OneColor(Foreground, Background))
    QPrintRC($"{filename} is the Main Module", Location.Row + 14, Location.Col + 2, OneColor(Foreground, Background))
    HLine(Location.Row + Size.Rows - 3, Location.Col, Location.Col + Size.Cols - 1, 1, OneColor(Foreground, Background))

  End Sub

  Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys

    'If m_selected = 0 Then
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
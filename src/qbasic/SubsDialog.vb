Public Class SubsDialog
  Inherits Form
  Implements IContext

  Private ReadOnly m_name As String

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

    m_name = name

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    SubsHorizontalListBox.Location = New Location(4, 3)
    SubsHorizontalListBox.Size = New Size(11, 70)
    SubsHorizontalListBox.TabIndex = 0
    SubsHorizontalListBox.Focused = True
    SubsHorizontalListBox.ForeColor = 0
    SubsHorizontalListBox.BackColor = 8
    Controls.Add(SubsHorizontalListBox)

    EditInActiveButton.Text = "< Edit in Active >"
    EditInActiveButton.Default = True
    EditInActiveButton.Location = New Location(17, 7)
    EditInActiveButton.Focused = False
    EditInActiveButton.TabStop = True
    EditInActiveButton.TabIndex = 1
    Controls.Add(EditInActiveButton)

    DeleteButton.Text = "< Delete >"
    DeleteButton.Default = False
    DeleteButton.Location = New Location(17, 30)
    DeleteButton.Focused = False
    DeleteButton.TabStop = True
    DeleteButton.TabIndex = 2
    Controls.Add(DeleteButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Default = False
    CancelButton.Location = New Location(17, 46)
    CancelButton.Focused = False
    CancelButton.TabStop = True
    CancelButton.TabIndex = 3
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Default = False
    HelpButton.Location = New Location(17, 61)
    HelpButton.Focused = False
    HelpButton.TabStop = True
    HelpButton.TabIndex = 4
    Controls.Add(HelpButton)

    SubsHorizontalListBox.Items.Add(m_name)
    'SubsHorizontalListBox.Items.AddRange(subs)
    For Each entry In subs
      SubsHorizontalListBox.Items.Add($"  {entry}")
    Next

  End Sub

  Public Sub Render() Implements IContext.Render

    OnDraw()

    QPrintRC("Choose program item to edit", Location.Row + 2, Location.Col + 2, OneColor(ForeColor, BackColor))
    QPrintRC($"{m_name} is the Main Module", Location.Row + 14, Location.Col + 2, OneColor(ForeColor, BackColor))
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
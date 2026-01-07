Imports System.Runtime.InteropServices
Imports System.Runtime.InteropServices.RuntimeInformation

Public Class OpenDialog
  Inherits Form
  Implements IContext

  Private ReadOnly m_initialPath As String
  Private m_folder As String
  Private ReadOnly m_filespec As String
  Private m_searchPattern As String = "*.BAS"

  Public ReadOnly Property Path As String
    Get
      Return IO.Path.Combine(m_folder, FileTextBox.Text)
    End Get
  End Property

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

  Private ReadOnly FileTextBox As New TextBoxControl(Me)
  Private WithEvents FileHorizontalListBox As New HorizontalListBoxControl(Me)
  Private WithEvents FolderListBox As New ListBoxControl(Me)

  Private WithEvents OkButton As New ButtonControl(Me)
  Private WithEvents CancelButton As New ButtonControl(Me)
  Private WithEvents HelpButton As New ButtonControl(Me)

  Sub New(path As String)
    MyBase.New("Open", New Location(3, 7), New Size(20, 67))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    m_initialPath = path
    If m_initialPath Is Nothing Then
      If IsOSPlatform(OSPlatform.Windows) Then
        m_initialPath = IO.Path.Combine(IO.Path.GetDirectoryName(Reflection.Assembly.GetEntryAssembly().Location), "*.BAS")
      Else
        m_initialPath = IO.Path.Combine(IO.Path.GetDirectoryName(Reflection.Assembly.GetEntryAssembly().Location), "*.*")
      End If
    End If

    m_folder = IO.Path.GetDirectoryName(m_initialPath)
    m_filespec = IO.Path.GetFileName(m_initialPath)
    If m_filespec.Contains("*"c) Then m_searchPattern = m_filespec
    If IO.Directory.Exists(m_folder) Then
      For Each file In IO.Directory.GetFiles(m_folder, m_filespec)
        FileHorizontalListBox.Items.Add(IO.Path.GetFileName(file))
      Next
      If m_folder <> IO.Path.GetPathRoot(m_folder) Then FolderListBox.Items.Add("..")
      For Each folder In IO.Directory.GetDirectories(m_folder)
        FolderListBox.Items.Add(IO.Path.GetFileName(folder))
      Next
    End If

    FileTextBox.Text = m_filespec
    FileTextBox.Location = New Location(3, 16)
    FileTextBox.Size = New Size(1, 50)
    FileTextBox.TabStop = True
    FileTextBox.TabIndex = 0
    FileTextBox.Visible = True
    FileTextBox.Focused = True
    FileTextBox.ForeColor = 0
    FileTextBox.BackColor = 8
    FileTextBox.SelectAll()
    Controls.Add(FileTextBox)

    FileHorizontalListBox.Location = New Location(7, 2)
    FileHorizontalListBox.Size = New Size(10, 45)
    FileHorizontalListBox.TabStop = True
    FileHorizontalListBox.TabIndex = 1
    FileHorizontalListBox.Visible = True
    FileHorizontalListBox.Focused = False
    FileHorizontalListBox.ForeColor = 0
    FileHorizontalListBox.BackColor = 8
    Controls.Add(FileHorizontalListBox)

    FolderListBox.Location = New Location(7, 49)
    FolderListBox.Size = New Size(10, 16)
    FolderListBox.TabStop = True
    FolderListBox.TabIndex = 2
    FolderListBox.Visible = True
    FolderListBox.Focused = False
    FolderListBox.ForeColor = 0
    FolderListBox.BackColor = 8
    Controls.Add(FolderListBox)

    OkButton.Text = "< OK >"
    OkButton.Default = True
    OkButton.Location = New Location(19, 12)
    OkButton.Focused = False
    OkButton.TabStop = True
    OkButton.TabIndex = 3
    Controls.Add(OkButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Default = False
    CancelButton.Location = New Location(19, 28)
    CancelButton.Focused = False
    CancelButton.TabStop = True
    CancelButton.TabIndex = 4
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Default = False
    HelpButton.Location = New Location(19, 48)
    HelpButton.Focused = False
    HelpButton.TabStop = True
    HelpButton.TabIndex = 5
    Controls.Add(HelpButton)

  End Sub

  Public Sub Render() Implements IContext.Render

    OnDraw()

    Dim ulRow = 3, ulCol = 7, lrRow = 22, lrCol = 73
    Dim w = lrCol - ulCol

    QPrintRC("File Name:", ulRow + 2, ulCol + 2, OneColor(0, 8))
    Box0(ulRow + 1, ulCol + 13, ulRow + 3, lrCol - 2, 1, OneColor(0, 8))

    QPrintRC(m_folder, ulRow + 4, ulCol + 2, OneColor(0, 8))
    QPrintRC("Files", ulRow + 5, ulCol + 22, OneColor(0, 8))
    QPrintRC("Dirs/Drives", ulRow + 5, lrCol - 15, OneColor(0, 8))

    HLine(lrRow - 2, ulCol, lrCol, 1, OneColor(0, 8))

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

    'If m_selected = 0 Then
    If keys?.Count > 0 Then
      For Each key In keys
        Dim e = New KeyPressEventArgs(key, capsLock, ctrl, alt, shift)
        OnKeyPress(e)
        If Not e.Handled Then
          Select Case e.Key
            Case ConsoleKey.Enter
              If FileTextBox.Focused Then
                If String.IsNullOrWhiteSpace(FileTextBox.Text) Then
                  ' show a message stating:
                  '    Must specify name
                  ' with OK and Help buttons
                ElseIf FileTextBox.Text.Contains("*"c) Then
                  If FileTextBox.Text = "*" Then FileTextBox.Text = "*.BAS"
                  m_searchPattern = FileTextBox.Text
                  If IO.Directory.Exists(m_folder) Then
                    FileHorizontalListBox.Clear()
                    For Each file In IO.Directory.GetFiles(m_folder, m_searchPattern)
                      FileHorizontalListBox.Items.Add(IO.Path.GetFileName(file))
                    Next
                  End If
                ElseIf FileTextBox.Text = ".." Then
                  m_folder = IO.Path.GetDirectoryName(m_folder)
                  FileHorizontalListBox.Clear()
                  For Each file In IO.Directory.GetFiles(m_folder, m_searchPattern)
                    FileHorizontalListBox.Items.Add(IO.Path.GetFileName(file))
                  Next
                  FolderListBox.Clear()
                  If m_folder <> IO.Path.GetPathRoot(m_folder) Then FolderListBox.Items.Add("..")
                  For Each folder In IO.Directory.GetDirectories(m_folder)
                    FolderListBox.Items.Add(IO.Path.GetFileName(folder))
                  Next
                  FileTextBox.Text = m_searchPattern
                Else
                  If IO.Directory.Exists(IO.Path.Combine(m_folder, FileTextBox.Text)) Then
                    m_folder = IO.Path.Combine(m_folder, FileTextBox.Text)
                    FileHorizontalListBox.Clear()
                    For Each file In IO.Directory.GetFiles(m_folder, m_searchPattern)
                      FileHorizontalListBox.Items.Add(IO.Path.GetFileName(file))
                    Next
                    FolderListBox.Clear()
                    If m_folder <> IO.Path.GetPathRoot(m_folder) Then FolderListBox.Items.Add("..")
                    For Each folder In IO.Directory.GetDirectories(m_folder)
                      FolderListBox.Items.Add(IO.Path.GetFileName(folder))
                    Next
                    FileTextBox.Text = m_searchPattern
                  Else
                    DialogResult = AcceptAction
                    Return False
                  End If
                End If
              ElseIf FileHorizontalListBox.Focused Then
                DialogResult = AcceptAction
                Return False
              ElseIf FolderListBox.Focused Then
                FileTextBox.Text = m_searchPattern
                If FolderListBox.SelectedItem = ".." Then
                  m_folder = IO.Path.GetDirectoryName(m_folder)
                Else
                  m_folder = IO.Path.Combine(m_folder, FolderListBox.SelectedItem)
                End If
                If IO.Directory.Exists(m_folder) Then
                  FileHorizontalListBox.Clear()
                  For Each file In IO.Directory.GetFiles(m_folder, m_searchPattern)
                    FileHorizontalListBox.Items.Add(IO.Path.GetFileName(file))
                  Next
                  FolderListBox.Clear()
                  If m_folder <> IO.Path.GetPathRoot(m_folder) Then FolderListBox.Items.Add("..")
                  For Each folder In IO.Directory.GetDirectories(m_folder)
                    FolderListBox.Items.Add(IO.Path.GetFileName(folder))
                  Next
                End If
                FolderListBox.Focused = False
                FileTextBox.Focused = True
              End If
            Case ConsoleKey.Escape : DialogResult = CancelAction : Return False
            Case Else
          End Select
        End If
      Next
    End If

    Return True

  End Function

  Private Sub FileHorizontalListBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles FileHorizontalListBox.SelectedIndexChanged
    FileTextBox.Text = If(FileHorizontalListBox.SelectedItem, m_searchPattern)
  End Sub

End Class
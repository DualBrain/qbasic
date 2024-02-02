Imports System.Runtime.InteropServices.RuntimeInformation
Imports System.Runtime.InteropServices.OSPlatform
Imports System.Reflection

Public Class OpenDialog
  Inherits Form
  Implements IContext

  'Private ReadOnly m_caption As String = "Open"
  Private ReadOnly m_initialPath As String
  Private m_folder As String
  Private m_filespec As String
  Private m_searchPattern As String = "*.BAS"

  Private ReadOnly m_ulRow As Integer
  Private ReadOnly m_ulCol As Integer
  Private ReadOnly m_lrRow As Integer
  Private ReadOnly m_lrCol As Integer

  Public ReadOnly Property Path As String
    Get
      Return IO.Path.Combine(m_folder, FileTextBox.Text)
    End Get
  End Property

  'Private m_cursorRow As Integer = 1
  'Private m_cursorCol As Integer = 1

  'Private m_folders As New List(Of String)
  'Private m_files As New List(Of String)

  'Private m_folderTopIndex As Integer
  'Private m_fileTopIndex As Integer

  'Private m_textOffset As Integer
  'Private m_textCursor As Integer

  'Private m_textSelected As Boolean
  'Private m_textSelectBegin As Integer
  'Private m_textSelectEnd As Integer

  'Private m_fileIndex As Integer
  'Private m_folderIndex As Integer

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

  Private ReadOnly FileTextBox As New TextBoxControl
  Private WithEvents FileHorizontalListBox As New HorizontalListBoxControl
  Private WithEvents FolderListBox As New ListBoxControl

  Private WithEvents OkButton As New ButtonControl
  Private WithEvents CancelButton As New ButtonControl
  Private WithEvents HelpButton As New ButtonControl

  Sub New(path As String, Optional selected As Integer = 0)
    MyBase.New("Open", New Location(3, 7), New Size(20, 67))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    m_initialPath = path
    If m_initialPath Is Nothing Then
      If IsOSPlatform(Windows) Then
        m_initialPath = IO.Path.Combine(IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "*.BAS")
      Else
        m_initialPath = IO.Path.Combine(IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "*.*")
      End If
    End If

    m_folder = IO.Path.GetDirectoryName(m_initialPath)
    m_filespec = IO.Path.GetFileName(m_initialPath)
    If m_filespec.Contains("*"c) Then m_searchPattern = m_filespec
    If IO.Directory.Exists(m_folder) Then
      For Each file In IO.Directory.GetFiles(m_folder, m_filespec)
        FileHorizontalListBox.Items.Add(IO.Path.GetFileName(file))
      Next
      FolderListBox.Items.Add("..")
      For Each folder In IO.Directory.GetDirectories(m_folder)
        FolderListBox.Items.Add(IO.Path.GetFileName(folder))
      Next
    End If

    'm_selected = selected

    Dim needed = 19

    m_ulRow = (25 - needed) \ 2
    m_ulCol = 7
    m_lrRow = m_ulRow + needed
    m_lrCol = 73

    FileTextBox.Text = m_filespec
    FileTextBox.Location = New Location(m_ulRow + 2, m_ulCol + 14)
    FileTextBox.Size = New Size(1, 50)
    FileTextBox.TabStop = True
    FileTextBox.TabOrder = 0
    FileTextBox.Visible = True
    FileTextBox.Focused = True
    FileTextBox.Foreground = 0
    FileTextBox.Background = 8
    FileTextBox.SelectAll()
    Controls.Add(FileTextBox)

    'Box0(m_ulRow + 6, m_ulCol + 3, m_lrRow - 4, m_lrCol - 20, 1, OneColor(0, 8))

    FileHorizontalListBox.Location = New Location(m_ulRow + 6, m_ulCol + 2)
    FileHorizontalListBox.Size = New Size(10, 45)
    FileHorizontalListBox.TabStop = True
    FileHorizontalListBox.TabOrder = 1
    FileHorizontalListBox.Visible = True
    FileHorizontalListBox.Focused = False
    FileHorizontalListBox.Foreground = 0
    FileHorizontalListBox.Background = 8
    Controls.Add(FileHorizontalListBox)

    'Box0(m_ulRow + 6, m_lrCol - 17, m_lrRow - 4, m_lrCol - 2, 1, OneColor(0, 8))

    FolderListBox.Location = New Location(m_ulRow + 6, m_lrCol - 17)
    FolderListBox.Size = New Size(10, 16)
    FolderListBox.TabStop = True
    FolderListBox.TabOrder = 2
    FolderListBox.Visible = True
    FolderListBox.Focused = False
    FolderListBox.Foreground = 0
    FolderListBox.Background = 8
    Controls.Add(FolderListBox)

    'Dim okOffset = 12
    'Dim cancelOffset = 28
    'Dim helpOffset = 48

    'Common.Button("< OK >", m_lrRow - 1, m_ulCol + okOffset, True)
    'Common.Button("< Cancel >", m_lrRow - 1, m_ulCol + cancelOffset, False)
    'Common.Button("< Help >", m_lrRow - 1, m_ulCol + helpOffset, False)

    OkButton.Text = "< OK >"
    OkButton.Default = True
    OkButton.Location = New Location(m_lrRow - 1, m_ulCol + 12)
    OkButton.Focused = False
    OkButton.TabStop = True
    OkButton.TabOrder = 3
    Controls.Add(OkButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Default = False
    CancelButton.Location = New Location(m_lrRow - 1, m_ulCol + 28)
    CancelButton.Focused = False
    CancelButton.TabStop = True
    CancelButton.TabOrder = 4
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Default = False
    HelpButton.Location = New Location(m_lrRow - 1, m_ulCol + 48)
    HelpButton.Focused = False
    HelpButton.TabStop = True
    HelpButton.TabOrder = 5
    Controls.Add(HelpButton)

  End Sub

  'Private m_selected As Integer

  Public ReadOnly Property Selected As Integer
    Get
      Return 0
    End Get
  End Property

  Public Sub Render() Implements IContext.Render

    Me.OnDraw()

    Dim w = m_lrCol - m_ulCol

    'PaintBox0(m_ulRow + 1, m_ulCol + 2, m_lrRow + 1, m_lrCol + 2, OneColor(7, 0))
    'ClearScr0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, OneColor(0, 8))
    'Box0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, 1, OneColor(0, 8))

    'Dim text = $" {m_caption} "
    'Dim textOffset = CInt((w - text.Length) / 2)
    'QPrintRC(text, m_ulRow, m_ulCol + textOffset, OneColor(0, 8))

    QPrintRC("File Name:", m_ulRow + 2, m_ulCol + 2, OneColor(0, 8))
    Box0(m_ulRow + 1, m_ulCol + 13, m_ulRow + 3, m_lrCol - 2, 1, OneColor(0, 8))

    'For Each control In m_controls
    '  control.OnDraw()
    'Next

    'QPrintRC(m_filespec, m_ulRow + 2, m_ulCol + 14, OneColor(0, 8))
    'If m_textSelected Then
    '  PaintBox0(m_ulRow + 2, m_ulCol + 14 + m_textSelectBegin, m_ulRow + 2, m_ulCol + 14 + m_textSelectEnd, OneColor(8, 0))
    'End If

    QPrintRC(m_folder, m_ulRow + 4, m_ulCol + 2, OneColor(0, 8))
    QPrintRC("Files", m_ulRow + 5, m_ulCol + 22, OneColor(0, 8))
    QPrintRC("Dirs/Drives", m_ulRow + 5, m_lrCol - 15, OneColor(0, 8))
    'Box0(m_ulRow + 6, m_ulCol + 3, m_lrRow - 4, m_lrCol - 20, 1, OneColor(0, 8))
    'Dim index = 0
    'Dim col = 0
    'Dim cOffset = 0
    'For Each file In m_files
    '  If file.Length > cOffset Then cOffset = file.Length
    'Next
    'Do
    '  If m_ulCol + 5 + (col * (cOffset + 2)) > m_lrCol - 21 Then Exit Do
    '  If (col * 8) + index > m_files.Count - 1 Then Exit Do
    '  Dim filename = m_files((col * 8) + index)
    '  If m_ulCol + 5 + (col * (cOffset + 2)) + filename.Length > m_lrCol - 21 Then
    '    ' need to shorten...
    '    Dim max = m_lrCol - 21 - (m_ulCol + 5 + (col * (cOffset + 2)))
    '    filename = filename.Substring(0, max)
    '  End If
    '  QPrintRC(filename, m_ulRow + 7 + index, m_ulCol + 5 + (col * (cOffset + 2)), OneColor(0, 8))
    '  index += 1
    '  If index > 7 Then col += 1 : index = 0
    'Loop

    'HScrollBar(m_lrRow - 4, m_ulCol + 4, m_lrCol - 21, 0)
    'Box0(m_ulRow + 6, m_lrCol - 17, m_lrRow - 4, m_lrCol - 2, 1, OneColor(0, 8))
    'For i = 0 To m_folders.Count - 1
    '  QPrintRC(m_folders(i).Substring(0, MinInt(m_folders(i).Length, 12)), m_ulRow + 7 + i, m_lrCol - 15, OneColor(0, 8))
    'Next

    'VScrollBar(m_ulRow + 7, m_lrCol - 2, m_lrRow - 5, 0)

    HLine(m_lrRow - 2, m_ulCol, m_lrCol, 1, OneColor(0, 8))

    'Dim okOffset = 12, okCursorOffset = 2
    'Dim cancelOffset = 28, cancelCursorOffset = 2
    'Dim helpOffset = 48, helpCursorOffset = 2

    'Common.Button("< OK >", m_lrRow - 1, m_ulCol + okOffset, True)
    'Common.Button("< Cancel >", m_lrRow - 1, m_ulCol + cancelOffset, False)
    'Common.Button("< Help >", m_lrRow - 1, m_ulCol + helpOffset, False)

    'Select Case m_selected
    '  Case 0
    '    'm_cursorRow = m_ulRow + 2 : m_cursorCol = m_ulCol + 14 + m_textCursor
    '    m_cursorRow = m_filespecTextBox.CursorRow
    '    m_cursorCol = m_filespecTextBox.CursorCol
    '  Case 1
    '    'm_cursorRow = m_ulRow + 7 + (m_fileIndex Mod 8) : m_cursorCol = m_ulCol + 5 + ((m_fileIndex \ 8) * (cOffset + 2))
    '    m_cursorRow = m_filesHListBox.CursorRow
    '    m_cursorCol = m_filesHListBox.CursorCol
    '  Case 2 : m_cursorRow = m_ulRow + 7 + m_folderIndex : m_cursorCol = m_lrCol - 15
    '  Case 3 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + okOffset + okCursorOffset
    '  Case 4 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + cancelOffset + cancelCursorOffset
    '  Case 5 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + helpOffset + helpCursorOffset
    '  Case Else
    '    m_selected = 0
    'End Select

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
              If FileTextBox.Focused Then
                If FileTextBox.Text.Contains("*"c) Then
                  m_searchPattern = FileTextBox.Text
                  If IO.Directory.Exists(m_folder) Then
                    FileHorizontalListBox.Clear()
                    For Each file In IO.Directory.GetFiles(m_folder, m_searchPattern)
                      FileHorizontalListBox.Items.Add(IO.Path.GetFileName(file))
                    Next
                  End If
                Else
                  DialogResult = AcceptAction
                  Return False
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
            Case ConsoleKey.Escape : Return False
            Case Else
          End Select
        End If
      Next
    End If
    Return True
    'End If

    'If keys?.Count > 0 Then
    '  For Each key In keys
    '    Select Case key
    '      Case ConsoleKey.Escape : m_selected = 1 : Return False
    '      Case ConsoleKey.F1 : m_selected = -2 : Return False ' Help
    '      Case ConsoleKey.Tab ' Change selected button...
    '        If shift Then
    '          m_selected -= 1 : If m_selected < 0 Then m_selected = 5
    '        Else
    '          m_selected += 1 : If m_selected > 5 Then m_selected = 0
    '        End If
    '      Case ConsoleKey.Enter
    '        Select Case m_selected
    '          'Case 0 ' if a filespec, reload file list; otherwise, if a filename, load the file
    '          'Case 1 ' load the selected file
    '          '  m_selected = 3 : Return False
    '          Case 2 ' traverse to the selected directly, reload file list
    '          Case Else
    '            Return False ' Current selected button...
    '        End Select
    '      Case ConsoleKey.Spacebar
    '        Select Case m_selected
    '          Case 0, 1, 2
    '          Case Else
    '            Return False ' Current selected button...
    '        End Select
    '      Case ConsoleKey.LeftArrow
    '        'Select Case m_selected
    '        '  Case 1 : If m_fileIndex - 8 > 0 Then m_fileIndex -= 8 Else m_fileIndex = 0
    '        '  Case Else
    '        'End Select
    '      Case ConsoleKey.RightArrow
    '        'Select Case m_selected
    '        '  Case 1 : If m_fileIndex + 8 < m_files.Count Then m_fileIndex += 8 Else m_fileIndex = m_files.Count - 1
    '        '  Case Else
    '        'End Select
    '      Case ConsoleKey.UpArrow
    '        Select Case m_selected
    '          'Case 1 : If m_fileIndex > 0 Then m_fileIndex -= 1
    '          Case 2 : If m_folderIndex > 0 Then m_folderIndex -= 8 Else m_folderIndex = 0
    '          Case Else
    '        End Select
    '      Case ConsoleKey.DownArrow
    '        Select Case m_selected
    '          'Case 1 : If m_fileIndex < m_files.Count Then m_fileIndex += 1
    '          Case 2 : If m_folderIndex < m_folders.Count Then m_folderIndex += 1
    '          Case Else
    '        End Select
    '      Case Else
    '    End Select
    '  Next
    'End If

    Return True

  End Function

  Private Sub FileHorizontalListBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles FileHorizontalListBox.SelectedIndexChanged
    FileTextBox.Text = If(FileHorizontalListBox.SelectedItem, m_searchPattern)
  End Sub

End Class
Imports System.Runtime.InteropServices.RuntimeInformation
Imports System.Runtime.InteropServices.OSPlatform
Imports System.Reflection
Imports System.IO.Enumeration

Public Class SaveAsDialog
  Inherits Form
  Implements IContext

  Private ReadOnly m_initialPath As String
  Private m_folder As String
  Private m_filename As String = ""

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
  Private WithEvents FolderListBox As New ListBoxControl(Me)

  Private WithEvents OkButton As New ButtonControl(Me)
  Private WithEvents CancelButton As New ButtonControl(Me)
  Private WithEvents HelpButton As New ButtonControl(Me)

  Sub New(path As String, filename As String)
    MyBase.New("Save As", New Location(4, 20), New Size(18, 42))

    AcceptAction = DialogResult.Ok
    CancelAction = DialogResult.Cancel

    m_initialPath = path
    If m_initialPath Is Nothing Then
      If IsOSPlatform(Windows) Then
        m_initialPath = IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
      Else
        m_initialPath = IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
      End If
    End If

    m_folder = IO.Path.GetDirectoryName(m_initialPath)
    If IO.Directory.Exists(m_folder) Then
      If m_folder <> IO.Path.GetPathRoot(m_folder) Then FolderListBox.Items.Add("..")
      For Each folder In IO.Directory.GetDirectories(m_folder)
        FolderListBox.Items.Add(IO.Path.GetFileName(folder))
      Next
    End If

    m_filename = filename
    FileTextBox.Text = m_filename
    FileTextBox.Location = New Location(3, 15)
    FileTextBox.Size = New Size(1, 25)
    FileTextBox.TabStop = True
    FileTextBox.TabOrder = 0
    FileTextBox.Visible = True
    FileTextBox.Focused = True
    FileTextBox.Foreground = 0
    FileTextBox.Background = 8
    FileTextBox.SelectAll()
    Controls.Add(FileTextBox)

    FolderListBox.Location = New Location(7, 15)
    FolderListBox.Size = New Size(9, 16)
    FolderListBox.TabStop = True
    FolderListBox.TabOrder = 2
    FolderListBox.Visible = True
    FolderListBox.Focused = False
    FolderListBox.Foreground = 0
    FolderListBox.Background = 8
    Controls.Add(FolderListBox)

    OkButton.Text = "< OK >"
    OkButton.Default = True
    OkButton.Location = New Location(17, 8)
    OkButton.Focused = False
    OkButton.TabStop = True
    OkButton.TabOrder = 3
    Controls.Add(OkButton)

    CancelButton.Text = "< Cancel >"
    CancelButton.Default = False
    CancelButton.Location = New Location(17, 18)
    CancelButton.Focused = False
    CancelButton.TabStop = True
    CancelButton.TabOrder = 4
    Controls.Add(CancelButton)

    HelpButton.Text = "< Help >"
    HelpButton.Default = False
    HelpButton.Location = New Location(17, 32)
    HelpButton.Focused = False
    HelpButton.TabStop = True
    HelpButton.TabOrder = 5
    Controls.Add(HelpButton)

  End Sub

  Public Sub Render() Implements IContext.Render

    OnDraw()

    Dim ulRow = Location.Row
    Dim ulCol = Location.Col
    Dim lrRow = Location.Row + Size.Rows - 1
    Dim lrCol = Location.Col + Size.Cols - 1
    Dim w = lrCol - ulCol

    QPrintRC("File Name:", ulRow + 2, ulCol + 2, OneColor(Foreground, Background))
    Box0(ulRow + 1, ulCol + 13, ulRow + 3, lrCol - 2, 1, OneColor(Foreground, Background))

    QPrintRC(m_folder, ulRow + 4, ulCol + 2, OneColor(Foreground, Background))
    QPrintRC("Dirs/Drives", ulRow + 5, ulCol + 16, OneColor(Foreground, Background))

    HLine(lrRow - 2, ulCol, lrCol, 1, OneColor(Foreground, Background))

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
                Dim txt = FileTextBox.Text
                If String.IsNullOrWhiteSpace(txt) Then
                  ' show a message stating:
                  '    Must specify name
                  ' with OK and Help buttons
                Else
                  Dim slash = txt.LastIndexOf("/"c)
                  Dim whack = txt.LastIndexOf("\"c)
                  Dim ps = If(slash > whack, slash, whack)
                  If ps > -1 Then
                    'TODO: there's a path in the textbox...
                  ElseIf txt = ".." Then
                    'TODO: change up one directory (if possible)
                  ElseIf txt.Contains("*"c) Then
                    ' do nothing...
                  Else
                    DialogResult = AcceptAction
                    Return False
                  End If
                End If
              ElseIf FolderListBox.Focused Then
                If FolderListBox.SelectedItem = ".." Then
                  m_folder = IO.Path.GetDirectoryName(m_folder)
                Else
                  m_folder = IO.Path.Combine(m_folder, FolderListBox.SelectedItem)
                End If
                If IO.Directory.Exists(m_folder) Then
                  FolderListBox.Clear()
                  If m_folder <> IO.Path.GetPathRoot(m_folder) Then FolderListBox.Items.Add("..")
                  For Each folder In IO.Directory.GetDirectories(m_folder)
                    FolderListBox.Items.Add(IO.Path.GetFileName(folder))
                  Next
                End If
                FolderListBox.Focused = False
                Dim txt = FileTextBox.Text
                Dim slash = txt.LastIndexOf("/"c)
                Dim whack = txt.LastIndexOf("\"c)
                Dim split = If(slash > whack, slash, whack)
                Dim rightSide = If(split < txt.Length - 2, txt.Substring(split + 1), "")
                FileTextBox.Text = $"{rightSide}"
                FileTextBox.Focused = True
              End If
            Case ConsoleKey.Escape : Return False
            Case Else
          End Select
        End If
      Next
    End If

    Return True

  End Function

  Private Sub FolderListBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles FolderListBox.SelectedIndexChanged
    Dim txt = FileTextBox.Text
    If String.IsNullOrWhiteSpace(txt) Then txt = "*.BAS"
    If txt.Contains("/"c) OrElse txt.Contains("\"c) Then
      Dim slash = txt.LastIndexOf("/"c)
      Dim whack = txt.LastIndexOf("\"c)
      Dim split = If(slash > whack, slash, whack)
      Dim rightSide = If(split < txt.Length - 1, txt.Substring(split), "")
      FileTextBox.Text = $"{FolderListBox.SelectedItem}{rightSide}"
    Else
      If IsOSPlatform(Windows) Then
        FileTextBox.Text = $"{FolderListBox.SelectedItem}\{txt}"
      Else
        FileTextBox.Text = $"{FolderListBox.SelectedItem}/{txt}"
      End If
    End If
  End Sub

End Class
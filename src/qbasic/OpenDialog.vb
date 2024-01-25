Imports VbPixelGameEngine
Imports System.Runtime.InteropServices.RuntimeInformation
Imports System.Runtime.InteropServices.OSPlatform
Imports System.Reflection

Public Class OpenDialog
  Inherits PgeX
  Implements IContext

  Private ReadOnly m_caption As String = "Open"
  Private ReadOnly m_initialPath As String
  Private m_folder As String
  Private m_filespec As String

  Private ReadOnly m_ulRow As Integer
  Private ReadOnly m_ulCol As Integer
  Private ReadOnly m_lrRow As Integer
  Private ReadOnly m_lrCol As Integer

  Private m_cursorRow As Integer
  Private m_cursorCol As Integer

  Private m_folders As New List(Of String)
  Private m_files As New List(Of String)

  Private m_folderTopIndex As Integer
  Private m_fileTopIndex As Integer

  Private m_textIndex As Integer
  Private m_fileIndex As Integer
  Private m_folderIndex As Integer

  Public ReadOnly Property CursorRow As Integer Implements IContext.CursorRow
    Get
      Return m_cursorRow
    End Get
  End Property

  Public ReadOnly Property CursorCol As Integer Implements IContext.CursorCol
    Get
      Return m_cursorCol
    End Get
  End Property

  Sub New(path As String, Optional selected As Integer = 0)

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
    m_textIndex = m_filespec.Length
    If IO.Directory.Exists(m_folder) Then
      For Each file In IO.Directory.GetFiles(m_folder, m_filespec)
        m_files.Add(IO.Path.GetFileName(file))
      Next
      m_folders.Add("..")
      For Each folder In IO.Directory.GetDirectories(m_folder)
        m_folders.Add(IO.Path.GetFileName(folder))
      Next
    End If

    m_selected = selected

    Dim needed = 19

    m_ulRow = (25 - needed) \ 2
    m_ulCol = 7
    m_lrRow = m_ulRow + needed
    m_lrCol = 73

  End Sub

  Private m_selected As Integer

  Public ReadOnly Property Selected As Integer
    Get
      Return m_selected
    End Get
  End Property

  Public Sub Render() Implements IContext.Render

    Dim w = m_lrCol - m_ulCol

    PaintBox0(m_ulRow + 1, m_ulCol + 2, m_lrRow + 1, m_lrCol + 2, OneColor(7, 0))
    ClearScr0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, OneColor(0, 8))
    Box0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, 1, OneColor(0, 8))

    Dim text = $" {m_caption} "
    Dim textOffset = CInt((w - text.Length) / 2)
    QPrintRC(text, m_ulRow, m_ulCol + textOffset, OneColor(0, 8))

    QPrintRC("File Name:", m_ulRow + 2, m_ulCol + 2, OneColor(0, 8))
    Box0(m_ulRow + 1, m_ulCol + 13, m_ulRow + 3, m_lrCol - 2, 1, OneColor(0, 8))
    QPrintRC(m_filespec, m_ulRow + 2, m_ulCol + 14, OneColor(8, 0))
    QPrintRC(m_folder, m_ulRow + 4, m_ulCol + 2, OneColor(0, 8))
    QPrintRC("Files", m_ulRow + 5, m_ulCol + 22, OneColor(0, 8))
    QPrintRC("Dirs/Drives", m_ulRow + 5, m_lrCol - 15, OneColor(0, 8))
    Box0(m_ulRow + 6, m_ulCol + 3, m_lrRow - 4, m_lrCol - 20, 1, OneColor(0, 8))
    Dim index = 0
    Dim col = 0
    Dim cOffset = 0
    For Each file In m_files
      If file.Length > cOffset Then cOffset = file.Length
    Next
    Do
      If m_ulCol + 5 + (col * (cOffset + 2)) > m_lrCol - 21 Then Exit Do
      If (col * 8) + index > m_files.Count - 1 Then Exit Do
      Dim filename = m_files((col * 8) + index)
      If m_ulCol + 5 + (col * (cOffset + 2)) + filename.Length > m_lrCol - 21 Then
        ' need to shorten...
        Dim max = m_lrCol - 21 - (m_ulCol + 5 + (col * (cOffset + 2)))
        filename = filename.Substring(0, max)
      End If
      QPrintRC(filename, m_ulRow + 7 + index, m_ulCol + 5 + (col * (cOffset + 2)), OneColor(0, 8))
      index += 1
      If index > 7 Then col += 1 : index = 0
    Loop

    HScrollBar(m_lrRow - 4, m_ulCol + 4, m_lrCol - 21, 0)
    Box0(m_ulRow + 6, m_lrCol - 17, m_lrRow - 4, m_lrCol - 2, 1, OneColor(0, 8))
    For i = 0 To m_folders.Count - 1
      QPrintRC(m_folders(i).Substring(0, MinInt(m_folders(i).Length, 12)), m_ulRow + 7 + i, m_lrCol - 15, OneColor(0, 8))
    Next

    VScrollBar(m_ulRow + 7, m_lrCol - 2, m_lrRow - 5, 0)

    HLine(m_lrRow - 2, m_ulCol, m_lrCol, 1, OneColor(0, 8))

    Dim okOffset = 12, okCursorOffset = 2
    Dim cancelOffset = 28, cancelCursorOffset = 2
    Dim helpOffset = 48, helpCursorOffset = 2

    Button("< OK >", m_lrRow - 1, m_ulCol + okOffset, m_selected <= 3)
    Button("< Cancel >", m_lrRow - 1, m_ulCol + cancelOffset, m_selected = 4)
    Button("< Help >", m_lrRow - 1, m_ulCol + helpOffset, m_selected = 5)

    Select Case m_selected
      Case 0 : m_cursorRow = m_ulRow + 2 : m_cursorCol = m_ulCol + 14 + m_textIndex
      Case 1 : m_cursorRow = m_ulRow + 7 + (m_fileIndex Mod 8) : m_cursorCol = m_ulCol + 5 + ((m_fileIndex \ 8) * (cOffset + 2))
      Case 2 : m_cursorRow = m_ulRow + 7 + m_folderIndex : m_cursorCol = m_lrCol - 15
      Case 3 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + okOffset + okCursorOffset
      Case 4 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + cancelOffset + cancelCursorOffset
      Case 5 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + helpOffset + helpCursorOffset
      Case Else
        m_selected = 0
    End Select

  End Sub

  Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys

    If keys?.Count > 0 Then
      For Each key In keys
        Select Case key
          Case ConsoleKey.Escape : m_selected = 1 : Return False
          Case ConsoleKey.F1 : m_selected = -2 : Return False ' Help
          Case ConsoleKey.Tab ' Change selected button...
            If shift Then
              m_selected -= 1 : If m_selected < 0 Then m_selected = 5
            Else
              m_selected += 1 : If m_selected > 5 Then m_selected = 0
            End If
          Case ConsoleKey.Enter
            Select Case m_selected
              Case 0 ' if a filespec, reload file list; otherwise, if a filename, load the file
              Case 1 ' load the selected file
                m_selected = 3 : Return False
              Case 2 ' traverse to the selected directly, reload file list
              Case Else
                Return False ' Current selected button...
            End Select
          Case ConsoleKey.Spacebar
            Select Case m_selected
              Case 0, 1, 2
              Case Else
                Return False ' Current selected button...
            End Select
          Case ConsoleKey.LeftArrow
            Select Case m_selected
              Case 0 : If m_textIndex > 0 Then m_textIndex -= 1
              Case 1 : If m_fileIndex - 8 > 0 Then m_fileIndex -= 8 Else m_fileIndex = 0
              Case Else
            End Select
          Case ConsoleKey.RightArrow
            Select Case m_selected
              Case 0 : If m_textIndex < m_filespec.Length Then m_textIndex += 1
              Case 1 : If m_fileIndex + 8 < m_files.Count Then m_fileIndex += 8 Else m_fileIndex = m_files.Count - 1
              Case Else
            End Select
          Case ConsoleKey.UpArrow
            Select Case m_selected
              Case 1 : If m_fileIndex > 0 Then m_fileIndex -= 1
              Case 2 : If m_folderIndex > 0 Then m_folderIndex -= 8 Else m_folderIndex = 0
              Case Else
            End Select
          Case ConsoleKey.DownArrow
            Select Case m_selected
              Case 1 : If m_fileIndex < m_files.Count Then m_fileIndex += 1
              Case 2 : If m_folderIndex < m_files.Count Then m_folderIndex += 1
              Case Else
            End Select
          Case Else
        End Select
      Next
    End If

    Return True

  End Function

End Class
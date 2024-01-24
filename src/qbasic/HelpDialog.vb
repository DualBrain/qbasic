Imports QB.Video
Imports VbPixelGameEngine

Public Class HelpDialog
  Inherits PgeX
  Implements IContext

  Private ReadOnly m_caption As String
  Private ReadOnly m_body As String

  Private ReadOnly m_ulRow As Integer
  Private ReadOnly m_ulCol As Integer
  Private ReadOnly m_lrRow As Integer
  Private ReadOnly m_lrCol As Integer

  Sub New(caption As String, body As String)

    m_caption = caption
    m_body = body

    Dim lines = m_body.Split(vbLf)
    Dim needed = lines.Length + 5

    m_ulRow = (25 - needed) \ 2
    m_ulCol = 7
    m_lrRow = m_ulRow + needed
    m_lrCol = 74

  End Sub

  Public Sub Render() Implements IContext.Render

    Dim w = m_lrCol - m_ulCol

    PaintBox0(m_ulRow + 1, m_ulCol + 2, m_lrRow + 1, m_lrCol + 2, OneColor(7, 0))
    ClearScr0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, OneColor(0, 8))
    Box0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, 1, OneColor(0, 8))

    Dim caption = $" HELP: {m_caption} "
    Dim textOffset = CInt((w - caption.Length) / 2)
    QPrintRC(caption, m_ulRow, m_ulCol + textOffset, OneColor(0, 8))

    Box0(m_ulRow + 1, m_ulCol + 2, m_lrRow - 3, m_lrCol - 2, 1, OneColor(0, 8))

    'Dim textOffset = CInt((w - m_text.Length) / 2)
    'QPrintRC(m_text, m_ulRow + 2, m_ulCol + textOffset, OneColor(0, 8))

    Dim lines = m_body.Split(vbLf)
    For index = 0 To lines.Count - 1
      QPrintRC(lines(index), m_ulRow + 2 + index, m_ulCol + 4, OneColor(0, 8))
    Next

    HLine(m_lrRow - 2, m_ulCol, m_lrCol, 1, OneColor(0, 8))

    Dim btnOffset = (w - 8) \ 2
    Button("<  OK  >", m_lrRow - 1, m_ulCol + btnOffset, True)

  End Sub

  Public Shared Sub Button(text As String, row As Integer, col As Integer, Optional selected As Boolean = False)
    QPrintRC(text, row, col, OneColor(0, 8))
    If selected Then
      QPrintRC("<", row, col, OneColor(15, 8))
      QPrintRC(">", row, col + text.Length - 1, OneColor(15, 8))
    End If
  End Sub

  Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys

    If keys?.Count > 0 Then
      For Each key In keys
        Select Case key
          Case ConsoleKey.Escape
            Return False
          Case ConsoleKey.Enter, ConsoleKey.Spacebar
            Return False
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
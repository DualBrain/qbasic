Imports QB.Video
Imports VbPixelGameEngine

Public Class MessageDialog
  Inherits PgeX
  Implements IContext

  Private ReadOnly m_text As String

  Private ReadOnly m_ulRow As Integer
  Private ReadOnly m_ulCol As Integer
  Private ReadOnly m_lrRow As Integer
  Private ReadOnly m_lrCol As Integer

  Public ReadOnly Property CursorRow As Integer Implements IContext.CursorRow
  Public ReadOnly Property CursorCol As Integer Implements IContext.CursorCol

  Sub New(text As String)

    m_text = text

    Dim lines = m_text.Split(vbLf)
    Dim needed = lines.Length + 5

    m_ulRow = (25 - needed) \ 2
    m_ulCol = 14
    m_lrRow = m_ulRow + needed
    m_lrCol = 66

    Me.CursorRow = m_lrRow - 1
    Dim w = m_lrCol - m_ulCol
    Dim btnOffset = (w - 8) \ 2
    Me.CursorCol = m_ulCol + btnOffset + 3

  End Sub

  Public Sub Render() Implements IContext.Render

    Dim w = m_lrCol - m_ulCol

    PaintBox0(m_ulRow + 1, m_ulCol + 2, m_lrRow + 1, m_lrCol + 2, OneColor(7, 0))
    ClearScr0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, OneColor(0, 8))
    Box0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, 1, OneColor(0, 8))

    Dim textOffset = (w - m_text.Length) \ 2
    QPrintRC(m_text, m_ulRow + 2, m_ulCol + textOffset, OneColor(0, 8))

    HLine(m_lrRow - 2, m_ulCol, m_lrCol, 1, OneColor(0, 8))
    Dim btnOffset = (w - 8) \ 2
    QPrintRC("<", m_lrRow - 1, m_ulCol + btnOffset, OneColor(15, 8))
    QPrintRC("OK", m_lrRow - 1, m_ulCol + btnOffset + 3, OneColor(0, 8))
    QPrintRC(">", m_lrRow - 1, m_ulCol + btnOffset + 7, OneColor(15, 8))

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

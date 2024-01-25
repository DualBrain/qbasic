Imports VbPixelGameEngine
Imports QB.Video

Public Class AboutDialog
  Inherits PgeX
  Implements IContext

  Private ReadOnly m_ulRow As Integer = 9
  Private ReadOnly m_ulCol As Integer = 14
  Private ReadOnly m_lrRow As Integer = 17
  Private ReadOnly m_lrCol As Integer = 66

  Public ReadOnly Property CursorRow As Integer Implements IContext.CursorRow
  Public ReadOnly Property CursorCol As Integer Implements IContext.CursorCol

  Sub New()

    Me.CursorRow = m_lrRow - 1
    Dim w = m_lrCol - m_ulCol
    Dim btnOffset = (w - 8) \ 2
    Me.CursorCol = m_ulCol + btnOffset + 3

  End Sub

  Sub Render() Implements IContext.Render

    Dim w = m_lrCol - m_ulCol

    PaintBox0(m_ulRow + 1, m_ulCol + 2, m_lrRow + 1, m_lrCol + 2, OneColor(7, 0))
    ClearScr0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, OneColor(0, 8))
    Box0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, 1, OneColor(0, 8))

    Dim title = "Community QBasic"
    Dim titleOffset = (w - title.Length) \ 2
    QPrintRC(title, m_ulRow + 2, m_ulCol + titleOffset, OneColor(0, 8))
    Dim version = "Version 1.0"
    Dim versionOffset = (w - version.Length) \ 2
    QPrintRC(version, m_ulRow + 3, m_ulCol + versionOffset, OneColor(0, 8))
    Dim copyright = "Copyright (C) Dartmouth Didn't, 1964-2024"
    Dim copyrightOffset = (w - copyright.Length) \ 2
    QPrintRC(copyright, m_ulRow + 4, m_ulCol + copyrightOffset, OneColor(0, 8))
    HLine(m_ulRow + 6, m_ulCol, m_lrCol, 1, OneColor(0, 8))
    Dim btnOffset = (w - 8) \ 2
    QPrintRC("<", m_ulRow + 7, m_ulCol + btnOffset, OneColor(15, 8))
    QPrintRC("OK", m_ulRow + 7, m_ulCol + btnOffset + 3, OneColor(0, 8))
    QPrintRC(">", m_ulRow + 7, m_ulCol + btnOffset + 7, OneColor(15, 8))

    LOCATE(m_ulRow + 7, m_ulCol + btnOffset + 3)

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

Imports VbPixelGameEngine

Public Class WelcomeDialog
  Inherits PgeX
  Implements IContext

  Private ReadOnly m_ulRow As Integer
  Private ReadOnly m_ulCol As Integer
  Private ReadOnly m_lrRow As Integer
  Private ReadOnly m_lrCol As Integer

  Private m_cursorRow As Integer = 1
  Private m_cursorCol As Integer = 1

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

  Private m_selected As Integer

  Public ReadOnly Property Selected As Integer
    Get
      Return m_selected
    End Get
  End Property

  Sub New()

    m_ulRow = 6
    m_ulCol = 14
    m_lrRow = 16
    m_lrCol = 66

    'm_cursorRow = m_lrRow - 1
    'Dim w = m_lrCol - m_ulCol
    'Dim btnOffset = (w - 8) \ 2
    'm_cursorCol = m_ulCol + btnOffset + 3

  End Sub

  Public Sub Render() Implements IContext.Render

    Dim w = m_lrCol - m_ulCol

    PaintBox0(m_ulRow + 1, m_ulCol + 2, m_lrRow + 1, m_lrCol + 2, OneColor(7, 0))
    ClearScr0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, OneColor(0, 8))
    Box0(m_ulRow, m_ulCol, m_lrRow, m_lrCol, 1, OneColor(0, 8))

    Dim welcomeText = "Welcome to Community QBasic"
    Dim textOffset = (w - welcomeText.Length) \ 2
    QPrintRC(welcomeText, m_ulRow + 2, m_ulCol + textOffset, OneColor(0, 8))

    Dim copyrightText = "Copyright (C) Dartmouth Didn't, 1964-2024"
    textOffset = (w - copyrightText.Length) \ 2
    QPrintRC(copyrightText, m_ulRow + 4, m_ulCol + textOffset, OneColor(0, 8))

    Dim reservedText = "All rights reserved."
    textOffset = (w - reservedText.Length) \ 2
    QPrintRC(reservedText, m_ulRow + 5, m_ulCol + textOffset, OneColor(0, 8))

    Dim survivalGuide = "< Press Enter to see the Survival Guide >"
    Dim offset1 = (w - survivalGuide.Length) \ 2
    Button(survivalGuide, m_lrRow - 3, m_ulCol + offset1, True)
    HLine(m_lrRow - 2, m_ulCol, m_lrCol, 1, OneColor(0, 8))
    Dim clearDialog = "< Press ESC to clear this dialog box >"
    Dim offset2 = (w - clearDialog.Length) \ 2
    Button(clearDialog, m_lrRow - 1, m_ulCol + offset2, False)

    Select Case m_selected
      Case 0 : m_cursorRow = m_lrRow - 3 : m_cursorCol = m_ulCol + offset1 + 2
      Case 1 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + offset2 + 2
      Case Else
        m_selected = 0
    End Select

  End Sub

  Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys

    If keys?.Count > 0 Then
      For Each key In keys
        Select Case key
          Case ConsoleKey.Tab
            If shift Then
              m_selected -= 1 : If m_selected < 0 Then m_selected = 1
            Else
              m_selected += 1 : If m_selected > 1 Then m_selected = 0
            End If
          Case ConsoleKey.Escape
            m_selected = 1 : Return False
          Case ConsoleKey.Enter, ConsoleKey.Spacebar
            Return False
          Case ConsoleKey.F1
          Case ConsoleKey.LeftArrow, ConsoleKey.RightArrow, ConsoleKey.UpArrow, ConsoleKey.DownArrow
          Case Else
        End Select
      Next
    End If

    Return True

  End Function

End Class
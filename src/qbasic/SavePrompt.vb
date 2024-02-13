Imports QBLib.Video
Imports VbPixelGameEngine

Public Class SavePrompt
  Inherits PgeX
  Implements IContext

  Private ReadOnly m_text As String

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

  Sub New(Optional selected As Integer = 0)

    m_selected = selected

    m_text = "Loaded file is not saved. Save it now?"

    Dim needed = 6

    m_ulRow = (25 - needed) \ 2
    m_ulCol = 18
    m_lrRow = m_ulRow + needed
    m_lrCol = 63

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

    Dim textOffset = CInt((w - m_text.Length) / 2)
    QPrintRC(m_text, m_ulRow + 2, m_ulCol + textOffset, OneColor(0, 8))

    HLine(m_lrRow - 2, m_ulCol, m_lrCol, 1, OneColor(0, 8))

    Dim yesOffset = 3, yesCursorOffset = 2
    Dim noOffset = 13, noCursorOffset = 3
    Dim cancelOffset = 24, cancelCursorOffset = 1
    Dim helpOffset = 35, helpCursorOffset = 2

    Button("< Yes >", m_lrRow - 1, m_ulCol + yesOffset, m_selected = 0)
    Button("<  No  >", m_lrRow - 1, m_ulCol + noOffset, m_selected = 1)
    Button("<Cancel>", m_lrRow - 1, m_ulCol + cancelOffset, m_selected = 2)
    Button("< Help >", m_lrRow - 1, m_ulCol + helpOffset, m_selected = 3)

    Select Case m_selected
      Case 0 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + yesOffset + yesCursorOffset
      Case 1 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + noOffset + noCursorOffset
      Case 2 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + cancelOffset + cancelCursorOffset
      Case 3 : m_cursorRow = m_lrRow - 1 : m_cursorCol = m_ulCol + helpOffset + helpCursorOffset
      Case Else
        m_selected = 0
    End Select

  End Sub

  Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys

    If keys?.Count > 0 Then
      For Each key In keys
        Select Case key
          Case ConsoleKey.Y : m_selected = 0 : Return False ' Yes
          Case ConsoleKey.N : m_selected = 1 : Return False ' No
          Case ConsoleKey.Escape : m_selected = 2 : Return False ' Cancel - NOTE: C doesn't appear to be valid in the original?
          Case ConsoleKey.H : m_selected = 3 : Return False ' Help
          Case ConsoleKey.F1 : m_selected = -3 : Return False ' Help
          Case ConsoleKey.Enter, ConsoleKey.Spacebar : Return False ' Current selected button...
          Case ConsoleKey.Tab ' Change selected button...
            If shift Then
              m_selected -= 1 : If m_selected < 0 Then m_selected = 3
            Else
              m_selected += 1 : If m_selected > 3 Then m_selected = 0
            End If
          Case ConsoleKey.LeftArrow, ConsoleKey.RightArrow, ConsoleKey.UpArrow, ConsoleKey.DownArrow
          Case Else
        End Select
      Next
    End If

    Return True

  End Function

End Class
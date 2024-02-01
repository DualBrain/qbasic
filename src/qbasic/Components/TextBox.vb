Public Class TextBox
  Inherits Control

  Public Property Text As String

  Private m_cursorOffset As Integer = 0
  Private m_textOffset As Integer = 0

  Private m_selected As Boolean
  Private m_selectionStart As Integer
  Private m_selectionEnd As Integer

  Public Sub New()
    CursorVisible = True
    m_cursorRow = Location.Row
    m_cursorCol = Location.Col + m_cursorOffset
  End Sub

  Public Sub SelectAll()
    m_selected = True
    m_selectionStart = 0
    m_selectionEnd = Text.Length - 1
    m_cursorOffset = Text.Length
  End Sub

  Public Overrides Sub OnKeyPress(e As KeyPressEventArgs)

    If Not Visible OrElse Not Focused Then Return

    If e.Control AndAlso Not e.Alt Then
    ElseIf Not e.Control AndAlso e.Alt Then
    ElseIf e.Control AndAlso e.Alt Then
    Else
      Select Case e.Key
        Case ConsoleKey.Home
          If e.Shift Then
            If m_selected Then
              If m_selectionStart = 0 Then
              ElseIf m_cursorOffset < m_selectionStart Then
                m_selectionEnd = 0
              ElseIf m_cursorOffset >= m_selectionStart Then
                m_selectionEnd = m_selectionStart - 1
                m_selectionStart = 0
              End If
              If m_selectionEnd < m_selectionStart Then QB.Core.SWAP(m_selectionStart, m_selectionEnd)
            ElseIf m_cursorOffset > 0 Then
              m_selectionStart = 0 : m_selectionEnd = m_cursorOffset - 1 : m_selected = True
            End If
          Else
            m_selected = False
          End If
          m_cursorOffset = 0
          m_textOffset = 0
          e.Handled = True
        Case ConsoleKey.End
          If e.Shift Then
            If m_selected Then
              If m_selectionEnd = Text.Length - 1 Then
              ElseIf m_cursorOffset > m_selectionEnd Then
                m_selectionStart = Text.Length - 1
              ElseIf m_cursorOffset <= m_selectionStart Then
                m_selectionStart = m_selectionEnd + 1
                m_selectionEnd = Text.Length - 1
              End If
              If m_selectionEnd < m_selectionStart Then QB.Core.SWAP(m_selectionStart, m_selectionEnd)
            Else
              m_selectionStart = m_cursorOffset : m_selectionEnd = Text.Length - 1 : m_selected = True
            End If
          Else
            m_selected = False
          End If
          m_cursorOffset = Text.Length
          If m_textOffset + m_cursorOffset > Size.Cols - 1 Then
            m_textOffset = Text.Length - Size.Cols
            m_cursorOffset = Size.Cols - 1
          End If
          e.Handled = True
        Case ConsoleKey.LeftArrow
          m_cursorOffset -= 1
          If m_cursorOffset < 0 Then
            m_cursorOffset = 0
            If m_textOffset > 0 Then m_textOffset -= 1
          End If
          If e.Shift Then
            If m_selected Then
              If m_cursorOffset < m_selectionStart Then
                m_selectionStart -= 1
              ElseIf m_cursorOffset = m_selectionEnd Then
                m_selectionEnd -= 1
              End If
              If m_selectionEnd < m_selectionStart Then m_selected = False
            Else
              m_selectionStart = m_cursorOffset : m_selectionEnd = m_cursorOffset : m_selected = True
            End If
          Else
            m_selected = False
          End If
          e.Handled = True
        Case ConsoleKey.RightArrow
          m_cursorOffset += 1
          If m_cursorOffset > Size.Cols - 1 Then
            m_textOffset += 1 : m_cursorOffset = Size.Cols - 1
          End If
          If e.Shift Then
            If m_selected Then
              If m_cursorOffset - 1 = m_selectionEnd Then
                m_selected = False
              ElseIf m_cursorOffset > m_selectionEnd Then
                m_selectionEnd += 1
              ElseIf m_cursorOffset > m_selectionStart Then
                m_selectionStart += 1
              End If
              If m_selectionEnd < m_selectionStart Then m_selected = False
            Else
              m_selectionStart = m_cursorOffset - 1 : m_selectionEnd = m_cursorOffset - 1 : m_selected = True
            End If
          Else
            m_selected = False
          End If
          e.Handled = True
        Case ConsoleKey.Delete
          Text = Left(Text, m_cursorOffset) + Mid(Text, m_cursorOffset + 2)
          e.Handled = True
        Case ConsoleKey.Backspace
          If m_cursorOffset > 0 Then
            If m_cursorOffset <= Text?.Length Then
              Dim leftSide = If(m_cursorOffset > 1, Text?.Substring(0, m_cursorOffset - 1), "")
              Dim rightSide = If(m_cursorOffset <= Text?.Length, Text?.Substring(m_cursorOffset), "")
              Text = leftSide & rightSide
            End If
            m_cursorOffset -= 1
          End If
          e.Handled = True
        Case ConsoleKey.A To ConsoleKey.Z,
             ConsoleKey.Spacebar,
             ConsoleKey.D0 To ConsoleKey.D9,
             ConsoleKey.Multiply, ConsoleKey.Divide, ConsoleKey.Add, ConsoleKey.Subtract,
             ConsoleKey.Oem1, ConsoleKey.Oem2, ConsoleKey.Oem3, ConsoleKey.Oem4, ConsoleKey.Oem5, ConsoleKey.Oem6, ConsoleKey.Oem7,
             ConsoleKey.OemPlus, ConsoleKey.OemMinus,
             ConsoleKey.OemComma, ConsoleKey.OemPeriod
          If Insert Then
            If m_cursorOffset <= Text?.Length Then
              Dim leftSide = If(m_cursorOffset > 0, Text.Substring(0, m_cursorOffset), "")
              Dim rightSide = If(m_cursorOffset <= Text.Length, Text.Substring(m_cursorOffset), "")
              Text = leftSide & GetChar(e.Key, e.CapsLock, e.Shift) & rightSide
            Else
              Text &= GetChar(e.Key, e.CapsLock, e.Shift)
            End If
          Else
            If m_cursorOffset <= Text?.Length Then
              Dim leftSide = If(m_cursorOffset > 0, Text.Substring(0, m_cursorOffset), "")
              Dim rightSide = If(m_cursorOffset < Text.Length, Text.Substring(m_cursorOffset + 1), "")
              Text = leftSide & GetChar(e.Key, e.CapsLock, e.Shift) & rightSide
            Else
              Text &= GetChar(e.Key, e.CapsLock, e.Shift)
            End If
          End If
          m_cursorOffset += 1
        Case Else
      End Select
    End If

  End Sub

  Public Overrides Sub OnDraw()

    m_cursorRow = Location.Row
    m_cursorCol = Location.Col + m_cursorOffset

    If Not Visible Then Return

    Dim c = Size.Cols
    Dim txt = Text
    If m_textOffset < txt?.Length Then
      If c > txt.Length - m_textOffset Then c = txt.Length - m_textOffset
      txt = Text.Substring(m_textOffset, c)
    Else
      txt = ""
    End If

    QPrintRC(txt?.PadRight(Size.Cols), Location.Row, Location.Col, OneColor(Foreground, Background))
    If m_selected Then
      PaintBox0(Location.Row, Location.Col + m_selectionStart, Location.Row, Location.Col + m_selectionEnd, OneColor(Background, Foreground))
    End If

  End Sub

End Class

Public Class TextBoxControl
  Inherits Control

  Public Property Text As String
    Get
      Return m_text
    End Get
    Set(value As String)
      m_text = value
      SelectAll()
    End Set
  End Property

  Private m_text As String

  Private m_cursorOffset As Integer = 0
  Private m_textOffset As Integer = 0

  Private m_selected As Boolean
  Private m_selectionStart As Integer
  Private m_selectionEnd As Integer

  Public Sub New(parent As Control)
    Me.Parent = parent
    CursorVisible = True
    m_cursorRow = Location.Row
    m_cursorCol = Location.Col + m_cursorOffset
  End Sub

  Public Sub SelectAll()
    m_selected = True
    m_selectionStart = 0
    m_selectionEnd = m_text.Length - 1
    m_cursorOffset = m_text.Length
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
              If m_selectionEnd = m_text.Length - 1 Then
              ElseIf m_cursorOffset > m_selectionEnd Then
                m_selectionStart = m_text.Length - 1
              ElseIf m_cursorOffset <= m_selectionStart Then
                m_selectionStart = m_selectionEnd + 1
                m_selectionEnd = m_text.Length - 1
              End If
              If m_selectionEnd < m_selectionStart Then QB.Core.SWAP(m_selectionStart, m_selectionEnd)
            Else
              m_selectionStart = m_cursorOffset : m_selectionEnd = m_text.Length - 1 : m_selected = True
            End If
          Else
            m_selected = False
          End If
          m_cursorOffset = m_text.Length
          If m_textOffset + m_cursorOffset > Size.Cols - 1 Then
            m_textOffset = m_text.Length - Size.Cols
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
          m_text = Left(m_text, m_cursorOffset) + Mid(m_text, m_cursorOffset + 2)
          m_selected = False
          e.Handled = True
        Case ConsoleKey.Backspace
          If m_cursorOffset > 0 Then
            If m_cursorOffset <= m_text?.Length Then
              Dim leftSide = If(m_cursorOffset > 1, m_text?.Substring(0, m_cursorOffset - 1), "")
              Dim rightSide = If(m_cursorOffset <= m_text?.Length, m_text?.Substring(m_cursorOffset), "")
              m_text = leftSide & rightSide
            End If
            m_cursorOffset -= 1
          End If
          m_selected = False
          e.Handled = True
        Case ConsoleKey.A To ConsoleKey.Z,
             ConsoleKey.Spacebar,
             ConsoleKey.D0 To ConsoleKey.D9,
             ConsoleKey.Multiply, ConsoleKey.Divide, ConsoleKey.Add, ConsoleKey.Subtract,
             ConsoleKey.Oem1, ConsoleKey.Oem2, ConsoleKey.Oem3, ConsoleKey.Oem4, ConsoleKey.Oem5, ConsoleKey.Oem6, ConsoleKey.Oem7,
             ConsoleKey.OemPlus, ConsoleKey.OemMinus,
             ConsoleKey.OemComma, ConsoleKey.OemPeriod
          If m_selected Then
            ' need to remove the selection...
            Dim leftSide = If(m_selectionStart > 0, m_text.Substring(0, m_selectionStart), "")
            Dim rightSide = If(m_selectionEnd + 1 < m_text.Length, m_text.Substring(m_selectionEnd + 1), "")
            m_text = leftSide & rightSide
            m_cursorOffset = m_selectionStart
            m_selected = False
          End If
          If Insert Then
            If m_cursorOffset <= m_text?.Length Then
              Dim leftSide = If(m_cursorOffset > 0, m_text.Substring(0, m_cursorOffset), "")
              Dim rightSide = If(m_cursorOffset <= m_text.Length, m_text.Substring(m_cursorOffset), "")
              m_text = leftSide & GetChar(e.Key, e.CapsLock, e.Shift) & rightSide
            Else
              m_text &= GetChar(e.Key, e.CapsLock, e.Shift)
            End If
          Else
            If m_cursorOffset <= m_text?.Length Then
              Dim leftSide = If(m_cursorOffset > 0, m_text.Substring(0, m_cursorOffset), "")
              Dim rightSide = If(m_cursorOffset < m_text.Length, m_text.Substring(m_cursorOffset + 1), "")
              m_text = leftSide & GetChar(e.Key, e.CapsLock, e.Shift) & rightSide
            Else
              m_text &= GetChar(e.Key, e.CapsLock, e.Shift)
            End If
          End If
          m_selected = False
          m_cursorOffset += 1
        Case Else
      End Select
    End If

  End Sub

  Public Overrides Sub OnDraw()

    Dim top = If(Parent?.Location.Row, 1) + Location.Row - 1
    Dim left = If(Parent?.Location.Col, 1) + Location.Col - 1

    m_cursorRow = top
    m_cursorCol = left + m_cursorOffset

    If Not Visible Then Return

    Dim c = Size.Cols
    Dim txt = m_text
    If m_textOffset < txt?.Length Then
      If c > txt.Length - m_textOffset Then c = txt.Length - m_textOffset
      txt = m_text.Substring(m_textOffset, c)
    Else
      txt = ""
    End If

    QPrintRC(txt?.PadRight(Size.Cols), top, left, OneColor(Foreground, Background))
    If Focused AndAlso m_selected Then
      PaintBox0(top, left + m_selectionStart, top, left + m_selectionEnd, OneColor(Background, Foreground))
    End If

  End Sub

End Class

Public Class ButtonControl
  Inherits Control

  Public Property Text As String
    Get
      Return m_text
    End Get
    Set(value As String)
      m_text = value
      If m_text IsNot Nothing Then
        For index = 0 To m_text.Length - 1
          Select Case m_text(index)
            Case "<"c, " "c
            Case Else
              m_cursorOffset = index : Exit For
          End Select
        Next
        Size = New Size(1, m_text.Length)
      Else
        m_cursorOffset = 0
        Size = New Size(1, 1)
      End If
    End Set
  End Property

  Public Property [Default] As Boolean
  Friend Property Highlight As Boolean

  Private m_text As String

  Private m_cursorOffset As Integer = 0

  Public Sub New()
    CursorVisible = True
    m_cursorRow = Location.Row
    m_cursorCol = Location.Col + m_cursorOffset
  End Sub

  Public Overrides Sub OnKeyPress(e As KeyPressEventArgs)

    If Not Visible OrElse Not Focused Then Return

    If e.Control AndAlso Not e.Alt Then
    ElseIf Not e.Control AndAlso e.Alt Then
    ElseIf e.Control AndAlso e.Alt Then
    Else
    End If

  End Sub

  Public Overrides Sub OnDraw()

    m_cursorRow = Location.Row
    m_cursorCol = Location.Col + m_cursorOffset

    If Not Visible Then Return

    Common.Button(m_text, Location.Row, Location.Col, Focused OrElse Highlight)

  End Sub

End Class
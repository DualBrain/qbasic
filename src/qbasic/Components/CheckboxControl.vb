Public Class CheckboxControl
  Inherits Control

  Public Property Text As String
    Get
      Return m_text
    End Get
    Set(value As String)
      m_text = value
      If m_text IsNot Nothing Then
        Size = New Size(1, m_text.Length + 4)
      Else
        Size = New Size(1, 3)
      End If
    End Set
  End Property
  ' Location
  ' Size
  Public Property Checked As Boolean = False
  Public Event CheckedChanged As EventHandler(Of EventArgs)

  Private m_text As String

  Public Sub New(parent As Control)
    Me.Parent = parent
    CursorVisible = True
    m_cursorRow = Location.Row
    m_cursorCol = Location.Col + 1
  End Sub

  Public Overrides Sub OnKeyPress(e As KeyPressEventArgs)

    If Not Visible OrElse Not Focused Then Return

    If e.Control AndAlso Not e.Alt Then
    ElseIf Not e.Control AndAlso e.Alt Then
    ElseIf e.Control AndAlso e.Alt Then
    Else
      Select Case e.Key
        Case ConsoleKey.Spacebar
          Checked = Not Checked
          e.Handled = True
        Case Else
      End Select
    End If

  End Sub

  Public Overrides Sub OnDraw()

    Dim top = If(Parent?.Location.Row, 1) + Location.Row - 1
    Dim left = If(Parent?.Location.Col, 1) + Location.Col - 1

    m_cursorRow = top
    m_cursorCol = left + 1

    If Not Visible Then Return

    QPrintRC($"[ ] {Text}", top, left, OneColor(Foreground, Background))
    If Checked Then
      QPrintRC("X", top, left + 1, OneColor(Foreground, Background))
    End If

  End Sub

End Class
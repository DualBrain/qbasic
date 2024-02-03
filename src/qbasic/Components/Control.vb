Public MustInherit Class Control

  Public Property Location As New Location(1, 1)
  Public Property Size As New Size(1, 1)

  Public Property Foreground As Integer = 0
  Public Property Background As Integer = 8

  Public Property TabOrder As Integer

  Public Property TabStop As Boolean = True
  Public Property Visible As Boolean = True
  Public Property Focused As Boolean

  Public Property Parent As Control

  Public Property CursorVisible As Boolean
  Protected m_cursorRow As Integer
  Public ReadOnly Property CursorRow As Integer
    Get
      Return m_cursorRow
    End Get
  End Property
  Protected m_cursorCol As Integer
  Public ReadOnly Property CursorCol As Integer
    Get
      Return m_cursorCol
    End Get
  End Property

  'Cursor Size?

  Public MustOverride Sub OnKeyPress(e As KeyPressEventArgs)

  Public MustOverride Sub OnDraw()

End Class

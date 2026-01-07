Public Interface IContext

  ReadOnly Property CursorRow As Integer
  ReadOnly Property CursorCol As Integer

  Sub Render()

  Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean, mButton As Boolean, mRow As Integer, mCol As Integer) As Boolean

End Interface
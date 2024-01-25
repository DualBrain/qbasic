Imports VbPixelGameEngine

Public Class StatusPanel
  Inherits PgeX
  Implements IContext

  Public Property Text As String
  Public Property Dialog As Boolean
  Public Property DocumentRow As Integer
  Public Property DocumentCol As Integer

  Public ReadOnly Property CursorRow As Integer Implements IContext.CursorRow
  Public ReadOnly Property CursorCol As Integer Implements IContext.CursorCol

  Public Sub Render() Implements IContext.Render
    If Dialog Then
      QPrintRC($" {Text}".PadRight(80), 25, 1, OneColor(15, 3))
    Else
      QPrintRC($" {Text}".PadRight(80), 25, 1, OneColor(15, 3))
      If Text.Length <= 62 Then
        QPrintRC($"{ChrW(179)}       {DocumentRow:00000}:{DocumentCol:000} ", 25, 63, OneColor(0, 3))
      End If
    End If
  End Sub

  Public Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys
    Return True
  End Function

End Class
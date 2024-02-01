Public Class KeyPressEventArgs
  Inherits EventArgs

  Public ReadOnly Property Key As ConsoleKey

  Public ReadOnly Property CapsLock As Boolean
  Public ReadOnly Property Control As Boolean
  Public ReadOnly Property Alt As Boolean
  Public ReadOnly Property Shift As Boolean

  Public Property Handled As Boolean = False

  Public Sub New(key As ConsoleKey, capsLock As Boolean, control As Boolean, alt As Boolean, shift As Boolean)
    Me.Key = key : Me.CapsLock = capsLock : Me.Control = control : Me.Alt = alt : Me.Shift = shift
  End Sub

End Class

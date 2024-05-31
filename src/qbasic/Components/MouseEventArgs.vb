Public Class MouseEventArgs
  Inherits EventArgs

  Public ReadOnly Property Button1 As Boolean
  Public ReadOnly Property Button2 As Boolean
  Public ReadOnly Property Button3 As Boolean
  Public ReadOnly Property Button4 As Boolean

  Public ReadOnly Property Row As Integer
  Public ReadOnly Property Column As Integer

  Public Property Handled As Boolean = False

  Public Sub New(button1 As Boolean, button2 As Boolean, button3 As Boolean, button4 As Boolean, row As Integer, column As Integer)
    Me.Button1 = button1
    Me.Button2 = button2
    Me.Button3 = button3
    Me.Button4 = button4
    Me.Row = row
    Me.Column = column
  End Sub

End Class
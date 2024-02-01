Public Class Location

  Public Property Row As Integer
  Public Property Col As Integer

  Public Sub New()
  End Sub

  Public Sub New(row As Integer, col As Integer)
    Me.Row = row : Me.Col = col
  End Sub

End Class

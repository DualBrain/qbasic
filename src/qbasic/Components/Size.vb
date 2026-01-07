Public Class Size

  Public Property Rows As Integer
  Public Property Cols As Integer

  Public Sub New()
  End Sub

  Public Sub New(rows As Integer, cols As Integer)
    Me.Rows = rows : Me.Cols = cols
  End Sub

End Class
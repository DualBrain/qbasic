Public Class Rectangle

  Public Property Top As Integer
  Public Property Left As Integer
  Public Property Right As Integer
  Public Property Bottom As Integer

  Public ReadOnly Property Width As Integer
    Get
      Return Right - Left
    End Get
  End Property
  Public ReadOnly Property Height As Integer
    Get
      Return Bottom - Height
    End Get
  End Property

  Public Sub New()
  End Sub

  Public Sub New(top As Integer, left As Integer, bottom As Integer, right As Integer)
    Me.Top = top : Me.Left = left : Me.Bottom = bottom : Me.Right = right
  End Sub

End Class

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundScreenStatement
    Inherits BoundStatement

    Public Sub New(mode As BoundExpression,
                   colorBurst As BoundExpression,
                   aPage As BoundExpression,
                   vPage As BoundExpression,
                   [erase] As BoundExpression)
      Me.Mode = mode
      Me.ColorBurst = colorBurst
      Me.APage = aPage
      Me.VPage = vPage
      Me.Erase = [erase]
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ScreenStatement
    Public ReadOnly Property Mode As BoundExpression
    Public ReadOnly Property ColorBurst As BoundExpression
    Public ReadOnly Property APage As BoundExpression
    Public ReadOnly Property VPage As BoundExpression
    Public ReadOnly Property [Erase] As BoundExpression

  End Class

End Namespace
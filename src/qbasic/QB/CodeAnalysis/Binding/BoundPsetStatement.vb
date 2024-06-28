Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPsetStatement
    Inherits BoundStatement

    Public Sub New([step] As Boolean,
                   x As BoundExpression,
                   y As BoundExpression,
                   color As BoundExpression)
      Me.Step = [step]
      Me.X = x
      Me.Y = y
      Me.Color = color
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PsetStatement
    Public ReadOnly Property [Step] As Boolean
    Public ReadOnly Property X As BoundExpression
    Public ReadOnly Property Y As BoundExpression
    Public ReadOnly Property Color As BoundExpression

  End Class

End Namespace
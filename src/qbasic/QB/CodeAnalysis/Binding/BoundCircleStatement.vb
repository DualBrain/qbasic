Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundCircleStatement
    Inherits BoundStatement

    Public Sub New([step] As Boolean, x As BoundExpression, y As BoundExpression, radius As BoundExpression, color As BoundExpression, start As BoundExpression, [end] As BoundExpression, aspect As BoundExpression)
      Me.Step = [step]
      Me.X = x
      Me.Y = y
      Me.Radius = radius
      Me.Color = color
      Me.Start = start
      Me.End = [end]
      Me.Aspect = aspect
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CircleStatement
    Public ReadOnly Property [Step] As Boolean
    Public ReadOnly Property X As BoundExpression
    Public ReadOnly Property Y As BoundExpression
    Public ReadOnly Property Radius As BoundExpression
    Public ReadOnly Property Color As BoundExpression
    Public ReadOnly Property Start As BoundExpression
    Public ReadOnly Property [End] As BoundExpression
    Public ReadOnly Property Aspect As BoundExpression

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnComGosubStatement
    Inherits BoundStatement

    Public Sub New(channel As BoundExpression, target As BoundExpression)
      Me.Channel = channel
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnComGosubStatement
    Public ReadOnly Property Channel As BoundExpression
    Public ReadOnly Property Target As BoundExpression

  End Class

End Namespace
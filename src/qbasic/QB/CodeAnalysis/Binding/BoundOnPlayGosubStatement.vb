Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnPlayGosubStatement
    Inherits BoundStatement

    Public Sub New(queueSize As BoundExpression, target As BoundExpression)
      Me.QueueSize = queueSize
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnPlayGosubStatement
    Public ReadOnly Property QueueSize As BoundExpression
    Public ReadOnly Property Target As BoundExpression

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnStrigGosubStatement
    Inherits BoundStatement

    Public Sub New(triggerNumber As BoundExpression, target As BoundExpression)
      Me.TriggerNumber = triggerNumber
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnStrigGosubStatement
    Public ReadOnly Property TriggerNumber As BoundExpression
    Public ReadOnly Property Target As BoundExpression

  End Class

End Namespace
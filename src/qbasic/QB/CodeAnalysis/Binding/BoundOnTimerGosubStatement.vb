Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnTimerGosubStatement
    Inherits BoundStatement

    Public Sub New(interval As BoundExpression, target As BoundExpression)
      Me.Interval = interval
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnTimerGosubStatement
    Public ReadOnly Property Interval As BoundExpression
    Public ReadOnly Property Target As BoundExpression

  End Class

End Namespace
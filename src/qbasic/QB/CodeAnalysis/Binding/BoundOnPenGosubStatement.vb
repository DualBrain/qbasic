Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnPenGosubStatement
    Inherits BoundStatement

    Public Sub New(target As BoundExpression)
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnPenGosubStatement
    Public ReadOnly Property Target As BoundExpression

  End Class

End Namespace
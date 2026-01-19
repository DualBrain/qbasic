Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnErrorGotoStatement
    Inherits BoundStatement

    Public Sub New(target As BoundExpression)
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnErrorGotoStatement
    Public ReadOnly Property Target As BoundExpression

  End Class

End Namespace
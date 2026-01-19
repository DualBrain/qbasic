Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundErrorStatement
    Inherits BoundStatement

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ErrorStatement
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
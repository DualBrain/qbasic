Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLsetStatement
    Inherits BoundStatement

    Public Sub New(variable As VariableSymbol, expression As BoundExpression)
      Me.Variable = variable
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LsetStatement
    Public ReadOnly Property Variable As VariableSymbol
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
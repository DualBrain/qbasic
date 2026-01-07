Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundArrayAccessExpression
    Inherits BoundExpression

    Public Sub New(variable As VariableSymbol, index As BoundExpression)
      Me.Variable = variable
      Me.Index = index
    End Sub

    Public Overrides ReadOnly Property Type As TypeSymbol
      Get
        Return Variable.Type
      End Get
    End Property
    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ArrayAccessExpression
    Public ReadOnly Property Variable As VariableSymbol
    Public ReadOnly Property Index As BoundExpression

  End Class

End Namespace
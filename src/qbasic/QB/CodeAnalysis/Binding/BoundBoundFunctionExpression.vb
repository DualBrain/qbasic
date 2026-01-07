Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBoundFunctionExpression
    Inherits BoundExpression

    Public Sub New(arrayVariable As VariableSymbol, dimension As BoundExpression, isLbound As Boolean)
      Me.ArrayVariable = arrayVariable
      Me.Dimension = dimension
      Me.IsLbound = isLbound
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BoundFunctionExpression
    Public Overrides ReadOnly Property Type As TypeSymbol = TypeSymbol.Integer
    Public ReadOnly Property ArrayVariable As VariableSymbol
    Public ReadOnly Property Dimension As BoundExpression
    Public ReadOnly Property IsLbound As Boolean

  End Class

End Namespace
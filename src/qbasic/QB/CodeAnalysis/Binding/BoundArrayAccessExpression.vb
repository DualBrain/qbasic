Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundArrayAccessExpression
    Inherits BoundExpression

    Public Sub New(variable As VariableSymbol, index As BoundExpression)
      Me.Variable = variable
      Me.Index = index
      Me.Indices = ImmutableArray.Create(index)
    End Sub

    Public Sub New(variable As VariableSymbol, indices As ImmutableArray(Of BoundExpression))
      Me.Variable = variable
      Me.Index = Nothing
      Me.Indices = indices
    End Sub

    Public Overrides ReadOnly Property Type As TypeSymbol
      Get
        Return Variable.Type
      End Get
    End Property
    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ArrayAccessExpression
    Public ReadOnly Property Variable As VariableSymbol
    Public ReadOnly Property Index As BoundExpression
    Public ReadOnly Property Indices As ImmutableArray(Of BoundExpression)

  End Class

End Namespace
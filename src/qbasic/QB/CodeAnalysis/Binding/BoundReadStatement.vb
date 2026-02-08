Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundReadStatement
    Inherits BoundStatement

    Public Sub New(expressions As ImmutableArray(Of BoundExpression))
      Me.Expressions = expressions
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ReadStatement
    Public ReadOnly Property Expressions As ImmutableArray(Of BoundExpression)

  End Class

End Namespace
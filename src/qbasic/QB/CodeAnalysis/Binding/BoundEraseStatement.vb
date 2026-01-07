Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend Class BoundEraseStatement
    Inherits BoundStatement

    Public Sub New(variables As ImmutableArray(Of BoundExpression))
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.EraseStatement
    Public ReadOnly Property Variables As ImmutableArray(Of BoundExpression)

  End Class

End Namespace
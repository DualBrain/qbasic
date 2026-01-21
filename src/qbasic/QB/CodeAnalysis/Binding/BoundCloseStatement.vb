Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundCloseStatement
    Inherits BoundStatement

    Public Sub New(fileNumbers As ImmutableArray(Of BoundExpression))
      Me.FileNumbers = fileNumbers
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CloseStatement
    Public ReadOnly Property FileNumbers As ImmutableArray(Of BoundExpression)

  End Class

End Namespace
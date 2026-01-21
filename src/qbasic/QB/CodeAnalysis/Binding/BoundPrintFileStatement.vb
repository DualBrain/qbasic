Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPrintFileStatement
    Inherits BoundStatement

    Public Sub New(fileNumber As BoundExpression, format As BoundExpression, nodes As ImmutableArray(Of BoundNode))
      Me.FileNumber = fileNumber
      Me.Format = format
      Me.Nodes = nodes
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PrintFileStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property Format As BoundExpression
    Public ReadOnly Property Nodes As ImmutableArray(Of BoundNode)

  End Class

End Namespace
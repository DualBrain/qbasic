Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPrintStatement
    Inherits BoundStatement

    Public Sub New(nodes As ImmutableArray(Of BoundNode), format As BoundExpression, suppressCr As Boolean)
      Me.Nodes = nodes
      Me.Format = format
      Me.SuppressCr = suppressCr
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PrintStatement
    Public ReadOnly Property Nodes As ImmutableArray(Of BoundNode)
    Public ReadOnly Property Format As BoundExpression
    Public ReadOnly Property SuppressCr As Boolean

  End Class

End Namespace
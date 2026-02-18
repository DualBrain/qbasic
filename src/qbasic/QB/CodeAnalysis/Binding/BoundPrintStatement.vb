Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPrintStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PrintStatementSyntax

    Public Sub New(nodes As ImmutableArray(Of BoundNode), format As BoundExpression, suppressCr As Boolean)
      Me.Nodes = nodes
      Me.Format = format
      Me.SuppressCr = suppressCr
    End Sub

    Public Sub New(syntax As PrintStatementSyntax, nodes As ImmutableArray(Of BoundNode), format As BoundExpression, suppressCr As Boolean)
      m_syntax = syntax
      Me.Nodes = nodes
      Me.Format = format
      Me.SuppressCr = suppressCr
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PrintStatement
    Public ReadOnly Property Nodes As ImmutableArray(Of BoundNode)
    Public ReadOnly Property Format As BoundExpression
    Public ReadOnly Property SuppressCr As Boolean

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
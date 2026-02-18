Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPrintFileStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PrintStatementSyntax

    Public Sub New(fileNumber As BoundExpression, format As BoundExpression, nodes As ImmutableArray(Of BoundNode))
      Me.FileNumber = fileNumber
      Me.Format = format
      Me.Nodes = nodes
    End Sub

    Public Sub New(syntax As PrintStatementSyntax, fileNumber As BoundExpression, format As BoundExpression, nodes As ImmutableArray(Of BoundNode))
      m_syntax = syntax
      Me.FileNumber = fileNumber
      Me.Format = format
      Me.Nodes = nodes
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PrintFileStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property Format As BoundExpression
    Public ReadOnly Property Nodes As ImmutableArray(Of BoundNode)

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

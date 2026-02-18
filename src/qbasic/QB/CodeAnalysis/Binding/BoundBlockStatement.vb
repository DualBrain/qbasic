Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBlockStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As BlockStatementSyntax

    Public Sub New(statements As ImmutableArray(Of BoundStatement))
      Me.Statements = statements
    End Sub

    Public Sub New(syntax As BlockStatementSyntax, statements As ImmutableArray(Of BoundStatement))
      m_syntax = syntax
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BlockStatement
    Public ReadOnly Property Statements As ImmutableArray(Of BoundStatement)

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

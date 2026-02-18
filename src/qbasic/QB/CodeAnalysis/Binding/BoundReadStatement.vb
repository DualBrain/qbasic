Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundReadStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As ReadStatementSyntax

    Public Sub New(expressions As ImmutableArray(Of BoundExpression))
      Me.Expressions = expressions
    End Sub

    Public Sub New(syntax As ReadStatementSyntax, expressions As ImmutableArray(Of BoundExpression))
      m_syntax = syntax
      Me.Expressions = expressions
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ReadStatement
    Public ReadOnly Property Expressions As ImmutableArray(Of BoundExpression)

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

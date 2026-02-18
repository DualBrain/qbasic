Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundExpressionStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As ExpressionStatementSyntax

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Sub New(syntax As ExpressionStatementSyntax, expression As BoundExpression)
      m_syntax = syntax
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ExpressionStatement
    Public ReadOnly Property Expression As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

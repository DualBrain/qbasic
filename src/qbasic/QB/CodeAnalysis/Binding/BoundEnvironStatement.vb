Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend Class BoundEnvironStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As EnvironStatementSyntax

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Sub New(syntax As EnvironStatementSyntax, expression As BoundExpression)
      m_syntax = syntax
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.EnvironStatement
    Public ReadOnly Property Expression As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
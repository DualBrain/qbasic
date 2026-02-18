Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundMkDirStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As MkDirStatementSyntax

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Sub New(syntax As MkDirStatementSyntax, expression As BoundExpression)
      m_syntax = syntax
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.MkDirStatement
    Public ReadOnly Property Expression As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

  Friend NotInheritable Class BoundKillStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As KillStatementSyntax

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Sub New(syntax As KillStatementSyntax, expression As BoundExpression)
      m_syntax = syntax
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.KillStatement
    Public ReadOnly Property Expression As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

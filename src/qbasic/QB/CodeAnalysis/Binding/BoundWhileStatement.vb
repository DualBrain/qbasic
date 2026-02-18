Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundWhileStatement
    Inherits BoundLoopStatement

    Private ReadOnly m_syntax As WhileStatementSyntax

    Public Sub New(expression As BoundExpression, statements As BoundStatement, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      Me.Expression = expression
      Me.Statements = statements
    End Sub

    Public Sub New(syntax As WhileStatementSyntax, expression As BoundExpression, statements As BoundStatement, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      m_syntax = syntax
      Me.Expression = expression
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.WhileStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property Statements As BoundStatement

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
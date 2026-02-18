Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDoWhileStatement
    Inherits BoundLoopStatement

    Private ReadOnly m_syntax As DoWhileStatementSyntax

    Public Sub New(statements As BoundStatement, expression As BoundExpression, atBeginning As Boolean, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      Me.Statements = statements
      Me.Expression = expression
      Me.AtBeginning = atBeginning
    End Sub

    Public Sub New(syntax As DoWhileStatementSyntax, statements As BoundStatement, expression As BoundExpression, atBeginning As Boolean, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      m_syntax = syntax
      Me.Statements = statements
      Me.Expression = expression
      Me.AtBeginning = atBeginning
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DoWhileStatement
    Public ReadOnly Property Statements As BoundStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property AtBeginning As Boolean

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

  Friend NotInheritable Class BoundDoUntilStatement
    Inherits BoundLoopStatement

    Private ReadOnly m_syntax As DoUntilStatementSyntax

    Public Sub New(statements As BoundStatement, expression As BoundExpression, atBeginning As Boolean, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      Me.Statements = statements
      Me.Expression = expression
      Me.AtBeginning = atBeginning
    End Sub

    Public Sub New(syntax As DoUntilStatementSyntax, statements As BoundStatement, expression As BoundExpression, atBeginning As Boolean, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      m_syntax = syntax
      Me.Statements = statements
      Me.Expression = expression
      Me.AtBeginning = atBeginning
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DoUntilStatement
    Public ReadOnly Property Statements As BoundStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property AtBeginning As Boolean

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
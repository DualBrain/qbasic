Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ClearStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   clearKeyword As SyntaxToken,
                   dummyExpression1 As ExpressionSyntax,
                   commaToken1 As SyntaxToken,
                   dummyExpression2 As ExpressionSyntax,
                   commaToken2 As SyntaxToken,
                   stackSpaceExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.ClearKeyword = clearKeyword
      Me.DummyExpression1 = dummyExpression1
      Me.CommaToken1 = commaToken1
      Me.DummyExpression2 = dummyExpression2
      Me.CommaToken2 = commaToken2
      Me.StackSpaceExpression = stackSpaceExpression
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.ClearStatement
    Public ReadOnly Property ClearKeyword As SyntaxToken
    Public ReadOnly Property DummyExpression1 As ExpressionSyntax
    Public ReadOnly Property CommaToken1 As SyntaxToken
    Public ReadOnly Property DummyExpression2 As ExpressionSyntax
    Public ReadOnly Property CommaToken2 As SyntaxToken
    Public ReadOnly Property StackSpaceExpression As ExpressionSyntax

  End Class

End Namespace
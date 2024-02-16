Namespace Global.QB.CodeAnalysis.Syntax

  Partial Friend Class ColorStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, colorKeyword As SyntaxToken, argumentExpression1 As ExpressionSyntax, commaToken1 As SyntaxToken, argumentExpression2 As ExpressionSyntax, commaToken2 As SyntaxToken, argumentExpression3 As ExpressionSyntax)
      MyBase.New(tree)
      Me.ColorKeyword = colorKeyword
      Me.ArgumentExpression1 = argumentExpression1
      Me.commaToken1 = commaToken1
      Me.ArgumentExpression2 = argumentExpression2
      Me.CommaToken2 = commaToken2
      Me.ArgumentExpression3 = argumentExpression3
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.ColorStatement
    Public ReadOnly Property ColorKeyword As SyntaxToken
    Public ReadOnly Property ArgumentExpression1 As ExpressionSyntax
    Public ReadOnly Property CommaToken1 As SyntaxToken
    Public ReadOnly Property ArgumentExpression2 As ExpressionSyntax
    Public ReadOnly Property CommaToken2 As SyntaxToken
    Public ReadOnly Property ArgumentExpression3 As ExpressionSyntax

  End Class

End Namespace
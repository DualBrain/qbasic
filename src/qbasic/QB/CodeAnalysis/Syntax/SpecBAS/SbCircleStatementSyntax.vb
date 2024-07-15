Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SbCircleStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   circleKeyword As SyntaxToken,
                   optionalInkStatement As InkStatementSyntax,
                   optionalSemicolonToken As SyntaxToken,
                   xExpression As ExpressionSyntax,
                   commaToken1 As SyntaxToken,
                   yExpression As ExpressionSyntax,
                   commaToken2 As SyntaxToken,
                   rExpression As ExpressionSyntax,
                   optionalFillKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.CircleKeyword = circleKeyword
      Me.OptionalInkStatement = optionalInkStatement
      Me.OptionalSemicolonToken = optionalSemicolonToken
      Me.XExpression = xExpression
      Me.CommaToken1 = commaToken1
      Me.YExpression = yExpression
      Me.CommaToken2 = commaToken2
      Me.RExpression = rExpression
      Me.OptionalFillKeyword = optionalFillKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbCircleStatement
    Public ReadOnly Property CircleKeyword As SyntaxToken
    Public ReadOnly Property OptionalInkStatement As InkStatementSyntax
    Public ReadOnly Property OptionalSemicolonToken As SyntaxToken
    Public ReadOnly Property XExpression As ExpressionSyntax
    Public ReadOnly Property CommaToken1 As SyntaxToken
    Public ReadOnly Property YExpression As ExpressionSyntax
    Public ReadOnly Property CommaToken2 As SyntaxToken
    Public ReadOnly Property RExpression As ExpressionSyntax
    Public ReadOnly Property OptionalFillKeyword As SyntaxToken

  End Class

End Namespace
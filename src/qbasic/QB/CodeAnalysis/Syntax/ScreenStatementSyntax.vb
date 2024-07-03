Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ScreenStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   screenKeyword As SyntaxToken,
                   optionalModeExpression As ExpressionSyntax,
                   optionalColorBurstCommaToken As SyntaxToken,
                   optionalColorBurstExpression As ExpressionSyntax,
                   optionalApageCommaToken As SyntaxToken,
                   optionalApageExpression As ExpressionSyntax,
                   optionalVpageCommaToken As SyntaxToken,
                   optionalVpageExpression As ExpressionSyntax,
                   optionalEraseCommaToken As SyntaxToken,
                   optionalEraseExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.ScreenKeyword = screenKeyword
      Me.OptionalModeExpression = optionalModeExpression
      Me.OptionalColorBurstCommaToken = optionalColorBurstCommaToken
      Me.OptionalColorBurstExpression = optionalColorBurstExpression
      Me.OptionalApageCommaToken = optionalApageCommaToken
      Me.OptionalApageExpression = optionalApageExpression
      Me.OptionalVpageCommaToken = optionalVpageCommaToken
      Me.OptionalVpageExpression = optionalVpageExpression
      Me.OptionalEraseCommaToken = optionalEraseCommaToken
      Me.OptionalEraseExpression = optionalEraseExpression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ScreenKeyword
    Public ReadOnly Property ScreenKeyword As SyntaxToken
    Public ReadOnly Property OptionalModeExpression As ExpressionSyntax
    Public ReadOnly Property OptionalColorBurstCommaToken As SyntaxToken
    Public ReadOnly Property OptionalColorBurstExpression As ExpressionSyntax
    Public ReadOnly Property OptionalApageCommaToken As SyntaxToken
    Public ReadOnly Property OptionalApageExpression As ExpressionSyntax
    Public ReadOnly Property OptionalVpageCommaToken As SyntaxToken
    Public ReadOnly Property OptionalVpageExpression As ExpressionSyntax
    Public ReadOnly Property OptionalEraseCommaToken As SyntaxToken
    Public ReadOnly Property OptionalEraseExpression As ExpressionSyntax

  End Class

End Namespace
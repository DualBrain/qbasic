Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class PresetStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   presetKeyword As SyntaxToken,
                   optionalStepKeyword As SyntaxToken,
                   openParenToken As SyntaxToken,
                   xExpression As ExpressionSyntax,
                   commaToken As SyntaxToken,
                   yExpression As ExpressionSyntax,
                   closeParenToken As SyntaxToken,
                   optionalCommaToken As SyntaxToken,
                   optionalColorExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.PresetKeyword = presetKeyword
      Me.OptionalStepKeyword = optionalStepKeyword
      Me.OpenParenToken = openParenToken
      Me.XExpression = xExpression
      Me.CommaToken = commaToken
      Me.YExpression = yExpression
      Me.CloseParenToken = closeParenToken
      Me.OptionalCommaToken = optionalCommaToken
      Me.OptionalColorExpression = optionalColorExpression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.PResetKeyword
    Public ReadOnly Property PresetKeyword As SyntaxToken
    Public ReadOnly Property OptionalStepKeyword As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property XExpression As ExpressionSyntax
    Public ReadOnly Property CommaToken As SyntaxToken
    Public ReadOnly Property YExpression As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property OptionalCommaToken As SyntaxToken
    Public ReadOnly Property OptionalColorExpression As ExpressionSyntax

  End Class

End Namespace
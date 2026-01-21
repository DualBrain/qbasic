Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OnKeyGosubStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, onKeyword As SyntaxToken, keyKeyword As SyntaxToken,
            openParen As SyntaxToken, keyNumber As ExpressionSyntax, closeParen As SyntaxToken,
            gosubKeyword As SyntaxToken, target As ExpressionSyntax)
      MyBase.New(tree)
      Me.OnKeyword = onKeyword
      Me.KeyKeyword = keyKeyword
      Me.OpenParenToken = openParen
      Me.KeyNumber = keyNumber
      Me.CloseParenToken = closeParen
      Me.GosubKeyword = gosubKeyword
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OnKeyGosubStatement
    Public ReadOnly Property OnKeyword As SyntaxToken
    Public ReadOnly Property KeyKeyword As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property KeyNumber As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property GosubKeyword As SyntaxToken
    Public ReadOnly Property Target As ExpressionSyntax

  End Class

End Namespace
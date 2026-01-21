Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OnComGosubStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, onKeyword As SyntaxToken, comKeyword As SyntaxToken,
            openParen As SyntaxToken, channel As ExpressionSyntax, closeParen As SyntaxToken,
            gosubKeyword As SyntaxToken, target As ExpressionSyntax)
      MyBase.New(tree)
      Me.OnKeyword = onKeyword
      Me.ComKeyword = comKeyword
      Me.OpenParenToken = openParen
      Me.Channel = channel
      Me.CloseParenToken = closeParen
      Me.GosubKeyword = gosubKeyword
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OnComGosubStatement
    Public ReadOnly Property OnKeyword As SyntaxToken
    Public ReadOnly Property ComKeyword As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Channel As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property GosubKeyword As SyntaxToken
    Public ReadOnly Property Target As ExpressionSyntax

  End Class

End Namespace
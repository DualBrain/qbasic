Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OnPlayGosubStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, onKeyword As SyntaxToken, playKeyword As SyntaxToken,
            openParen As SyntaxToken, queueSize As ExpressionSyntax, closeParen As SyntaxToken,
            gosubKeyword As SyntaxToken, target As ExpressionSyntax)
      MyBase.New(tree)
      Me.OnKeyword = onKeyword
      Me.PlayKeyword = playKeyword
      Me.OpenParenToken = openParen
      Me.QueueSize = queueSize
      Me.CloseParenToken = closeParen
      Me.GosubKeyword = gosubKeyword
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OnPlayGosubStatement
    Public ReadOnly Property OnKeyword As SyntaxToken
    Public ReadOnly Property PlayKeyword As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property QueueSize As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property GosubKeyword As SyntaxToken
    Public ReadOnly Property Target As ExpressionSyntax

  End Class

End Namespace
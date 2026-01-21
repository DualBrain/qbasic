Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class PlayEventStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, playKeyword As SyntaxToken, openParen As SyntaxToken, queueSize As ExpressionSyntax, closeParen As SyntaxToken, verb As SyntaxToken)
      MyBase.New(tree)
      Me.PlayKeyword = playKeyword
      Me.OpenParenToken = openParen
      Me.QueueSize = queueSize
      Me.CloseParenToken = closeParen
      Me.Verb = verb
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.PlayEventStatement
    Public ReadOnly Property PlayKeyword As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property QueueSize As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property Verb As SyntaxToken

  End Class

End Namespace
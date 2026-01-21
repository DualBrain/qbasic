Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ComStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, comKeyword As SyntaxToken, openParen As SyntaxToken, channel As ExpressionSyntax, closeParen As SyntaxToken, verb As SyntaxToken)
      MyBase.New(tree)
      Me.ComKeyword = comKeyword
      Me.OpenParenToken = openParen
      Me.Channel = channel
      Me.CloseParenToken = closeParen
      Me.Verb = verb
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ComStatement
    Public ReadOnly Property ComKeyword As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Channel As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property Verb As SyntaxToken

  End Class

End Namespace
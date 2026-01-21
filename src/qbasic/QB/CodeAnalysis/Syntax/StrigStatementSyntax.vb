Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class StrigStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, strigKeyword As SyntaxToken, openParen As SyntaxToken, triggerNumber As ExpressionSyntax, closeParen As SyntaxToken, verb As SyntaxToken)
      MyBase.New(tree)
      Me.StrigKeyword = strigKeyword
      Me.OpenParenToken = openParen
      Me.TriggerNumber = triggerNumber
      Me.CloseParenToken = closeParen
      Me.Verb = verb
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.StrigStatement
    Public ReadOnly Property StrigKeyword As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property TriggerNumber As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property Verb As SyntaxToken

  End Class

End Namespace
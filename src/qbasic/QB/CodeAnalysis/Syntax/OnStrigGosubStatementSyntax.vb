Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OnStrigGosubStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, onKeyword As SyntaxToken, strigKeyword As SyntaxToken,
            openParen As SyntaxToken, triggerNumber As ExpressionSyntax, closeParen As SyntaxToken,
            gosubKeyword As SyntaxToken, target As ExpressionSyntax)
      MyBase.New(tree)
      Me.OnKeyword = onKeyword
      Me.StrigKeyword = strigKeyword
      Me.OpenParenToken = openParen
      Me.TriggerNumber = triggerNumber
      Me.CloseParenToken = closeParen
      Me.GosubKeyword = gosubKeyword
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OnStrigGosubStatement
    Public ReadOnly Property OnKeyword As SyntaxToken
    Public ReadOnly Property StrigKeyword As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property TriggerNumber As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property GosubKeyword As SyntaxToken
    Public ReadOnly Property Target As ExpressionSyntax

  End Class

End Namespace
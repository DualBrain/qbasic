Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OnTimerGosubStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, onKeyword As SyntaxToken, timerKeyword As SyntaxToken,
            openParen As SyntaxToken, interval As ExpressionSyntax, closeParen As SyntaxToken,
            gosubKeyword As SyntaxToken, target As ExpressionSyntax)
      MyBase.New(tree)
      Me.OnKeyword = onKeyword
      Me.TimerKeyword = timerKeyword
      Me.OpenParenToken = openParen
      Me.Interval = interval
      Me.CloseParenToken = closeParen
      Me.GosubKeyword = gosubKeyword
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OnTimerGosubStatement
    Public ReadOnly Property OnKeyword As SyntaxToken
    Public ReadOnly Property TimerKeyword As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Interval As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property GosubKeyword As SyntaxToken
    Public ReadOnly Property Target As ExpressionSyntax

  End Class

End Namespace
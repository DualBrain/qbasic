Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OnErrorGotoStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, onKeyword As SyntaxToken, errorKeyword As SyntaxToken, gotoKeyword As SyntaxToken, target As ExpressionSyntax)
      MyBase.New(tree)
      Me.OnKeyword = onKeyword
      Me.ErrorKeyword = errorKeyword
      Me.GotoKeyword = gotoKeyword
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OnErrorGotoStatement
    Public ReadOnly Property OnKeyword As SyntaxToken
    Public ReadOnly Property ErrorKeyword As SyntaxToken
    Public ReadOnly Property GotoKeyword As SyntaxToken
    Public ReadOnly Property Target As ExpressionSyntax

  End Class

End Namespace
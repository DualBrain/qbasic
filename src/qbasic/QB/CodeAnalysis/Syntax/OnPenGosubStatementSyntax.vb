Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OnPenGosubStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, onKeyword As SyntaxToken, penKeyword As SyntaxToken,
            gosubKeyword As SyntaxToken, target As ExpressionSyntax)
      MyBase.New(tree)
      Me.OnKeyword = onKeyword
      Me.PenKeyword = penKeyword
      Me.GosubKeyword = gosubKeyword
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OnPenGosubStatement
    Public ReadOnly Property OnKeyword As SyntaxToken
    Public ReadOnly Property PenKeyword As SyntaxToken
    Public ReadOnly Property GosubKeyword As SyntaxToken
    Public ReadOnly Property Target As ExpressionSyntax

  End Class

End Namespace
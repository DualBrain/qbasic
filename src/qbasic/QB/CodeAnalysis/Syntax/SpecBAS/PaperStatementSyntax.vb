Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class PaperStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, paperKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.PaperKeyword = paperKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbPaperStatement
    Public ReadOnly Property PaperKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
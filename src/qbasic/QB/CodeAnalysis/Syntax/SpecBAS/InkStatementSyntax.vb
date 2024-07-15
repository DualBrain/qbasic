Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class InkStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, inkKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.InkKeyword = inkKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbInkStatement
    Public ReadOnly Property InkKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
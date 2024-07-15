Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class DrawStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, drawKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.DrawKeyword = drawKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DrawStatement
    Public ReadOnly Property DrawKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class OverStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, overKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.OverKeyword = overKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbOverStatement
    Public ReadOnly Property OverKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
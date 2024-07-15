Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class BorderStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, borderKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.BorderKeyword = borderKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbBorderStatement
    Public ReadOnly Property BorderKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
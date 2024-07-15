Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class TimeStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, timeKeyword As SyntaxToken, equal As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.TimeKeyword = timeKeyword
      Me.Equal = equal
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.TimeStatement
    Public ReadOnly Property TimeKeyword As SyntaxToken
    Public ReadOnly Property Equal As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
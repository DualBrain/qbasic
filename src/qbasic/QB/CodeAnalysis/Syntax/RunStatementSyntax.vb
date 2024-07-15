Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class RunStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, runKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.RunKeyword = runKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.RunStatement
    Public ReadOnly Property RunKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
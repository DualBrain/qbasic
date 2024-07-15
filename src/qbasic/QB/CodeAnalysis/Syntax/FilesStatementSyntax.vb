Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class FilesStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, filesKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.FilesKeyword = filesKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.FilesStatement
    Public ReadOnly Property FilesKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
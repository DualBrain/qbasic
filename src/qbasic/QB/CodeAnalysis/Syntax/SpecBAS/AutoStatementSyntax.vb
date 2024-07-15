Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class AutoStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, autoKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.AutoKeyword = autoKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbAutoStatement
    Public ReadOnly Property AutoKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
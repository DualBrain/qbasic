Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ErrorStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, errorKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.ErrorKeyword = errorKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ErrorStatement
    Public ReadOnly Property ErrorKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
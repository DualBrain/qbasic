Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class DateStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, dateKeyword As SyntaxToken, equalToken As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.DateKeyword = dateKeyword
      Me.EqualToken = equalToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DateStatement
    Public ReadOnly Property DateKeyword As SyntaxToken
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class EnvironStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, environKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.EnvironKeyword = environKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.EnvironStatement
    Public ReadOnly Property EnvironKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class CloseStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, closeKeyword As SyntaxToken, expressions As SeparatedSyntaxList(Of ExpressionSyntax))
      MyBase.New(tree)
      Me.CloseKeyword = closeKeyword
      Me.Expressions = expressions
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CloseStatement
    Public ReadOnly Property CloseKeyword As SyntaxToken
    Public ReadOnly Property Expressions As SeparatedSyntaxList(Of ExpressionSyntax)

  End Class

End Namespace
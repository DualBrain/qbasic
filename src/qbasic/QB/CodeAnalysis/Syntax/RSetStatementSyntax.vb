Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class RSetStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, rsetKeyword As SyntaxToken, identifier As IdentifierSyntax, equal As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.RsetKeyword = rsetKeyword
      Me.Identifier = identifier
      Me.Equal = equal
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.RsetStatement
    Public ReadOnly Property RsetKeyword As SyntaxToken
    Public ReadOnly Property Identifier As IdentifierSyntax
    Public ReadOnly Property Equal As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
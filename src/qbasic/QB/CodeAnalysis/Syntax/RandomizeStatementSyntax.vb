Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class RandomizeStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, randomizeKeyword As SyntaxToken, seed As ExpressionSyntax)
      MyBase.New(tree)
      Me.RandomizeKeyword = randomizeKeyword
      Me.Seed = seed
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.RandomizeStatement
    Public ReadOnly Property RandomizeKeyword As SyntaxToken
    Public ReadOnly Property Seed As ExpressionSyntax

  End Class

End Namespace
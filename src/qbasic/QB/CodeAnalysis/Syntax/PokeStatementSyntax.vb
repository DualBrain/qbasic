Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class PokeStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, pokeKeyword As SyntaxToken, offset As ExpressionSyntax, commaToken As SyntaxToken, value As ExpressionSyntax)
      MyBase.New(tree)
      Me.PokeKeyword = pokeKeyword
      Me.Offset = offset
      Me.CommaToken = commaToken
      Me.Value = value
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.PokeStatement
    Public ReadOnly Property PokeKeyword As SyntaxToken
    Public ReadOnly Property Offset As ExpressionSyntax
    Public ReadOnly Property CommaToken As SyntaxToken
    Public ReadOnly Property Value As ExpressionSyntax

  End Class

End Namespace
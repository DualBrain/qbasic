Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ChainStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, chainKeyword As SyntaxToken, filename As ExpressionSyntax, optionalCommaToken As SyntaxToken, optionalLine As ExpressionSyntax)
      MyBase.New(tree)
      Me.ChainKeyword = chainKeyword
      Me.Filename = filename
      Me.OptionalCommaToken = optionalCommaToken
      Me.OptionalLine = optionalLine
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ChainStatement
    Public ReadOnly Property ChainKeyword As SyntaxToken
    Public ReadOnly Property Filename As ExpressionSyntax
    Public ReadOnly Property OptionalCommaToken As SyntaxToken
    Public ReadOnly Property OptionalLine As ExpressionSyntax

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class DefSegStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, defKeyword As SyntaxToken, segKeyword As SyntaxToken, optionalEqualToken As SyntaxToken, optionalAddress As ExpressionSyntax)
      MyBase.New(tree)
      Me.DefKeyword = defKeyword
      Me.SegKeyword = segKeyword
      Me.OptionalEqualToken = optionalEqualToken
      Me.OptionalAddress = optionalAddress
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.DefSegStatement
    Public ReadOnly Property DefKeyword As SyntaxToken
    Public ReadOnly Property SegKeyword As SyntaxToken
    Public ReadOnly Property OptionalEqualToken As SyntaxToken
    Public ReadOnly Property OptionalAddress As ExpressionSyntax

  End Class

End Namespace
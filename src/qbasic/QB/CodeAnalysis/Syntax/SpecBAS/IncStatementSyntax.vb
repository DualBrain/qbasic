Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class IncStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, incKeyword As SyntaxToken, identifier As IdentifierSyntax, optionalComma1 As SyntaxToken, optionalAmount As ExpressionSyntax, optionalComma2 As SyntaxToken, optionalStart As ExpressionSyntax, optionalToKeyword As SyntaxToken, optionalEnd As ExpressionSyntax)
      MyBase.New(tree)
      Me.IncKeyword = incKeyword
      Me.Identifier = identifier
      Me.OptionalComma1 = optionalComma1
      Me.OptionalAmount = optionalAmount
      Me.OptionalComma2 = optionalComma2
      Me.OptionalStart = optionalStart
      Me.OptionalToKeyword = optionalToKeyword
      Me.OptionalEnd = optionalEnd
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbIncStatement
    Public ReadOnly Property IncKeyword As SyntaxToken
    Public ReadOnly Property Identifier As IdentifierSyntax
    Public ReadOnly Property OptionalComma1 As SyntaxToken
    Public ReadOnly Property OptionalAmount As ExpressionSyntax
    Public ReadOnly Property OptionalComma2 As SyntaxToken
    Public ReadOnly Property OptionalStart As ExpressionSyntax
    Public ReadOnly Property OptionalToKeyword As SyntaxToken
    Public ReadOnly Property OptionalEnd As ExpressionSyntax

  End Class

End Namespace
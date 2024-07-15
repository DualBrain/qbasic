Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class PutStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, putKeyword As SyntaxToken, optionalStepKeyword As SyntaxToken, openParen As SyntaxToken, x As ExpressionSyntax, comma1 As SyntaxToken, y As ExpressionSyntax, closeParen As SyntaxToken, comma2 As SyntaxToken, buffer As IdentifierSyntax, optionalComma3 As SyntaxToken, optionalDisplayVerb As SyntaxToken)
      MyBase.New(tree)
      Me.PutKeyword = putKeyword
      Me.OptionalStepKeyword = optionalStepKeyword
      Me.OpenParen = openParen
      Me.X = x
      Me.Comma1 = comma1
      Me.Y = y
      Me.CloseParen = closeParen
      Me.Comma2 = comma2
      Me.Buffer = buffer
      Me.OptionalComma3 = optionalComma3
      Me.OptionalDisplayVerb = optionalDisplayVerb
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.PutStatement
    Public ReadOnly Property PutKeyword As SyntaxToken
    Public ReadOnly Property OptionalStepKeyword As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property X As ExpressionSyntax
    Public ReadOnly Property Comma1 As SyntaxToken
    Public ReadOnly Property Y As ExpressionSyntax
    Public ReadOnly Property CloseParen As SyntaxToken
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property Buffer As IdentifierSyntax
    Public ReadOnly Property OptionalComma3 As SyntaxToken
    Public ReadOnly Property OptionalDisplayVerb As SyntaxToken
  End Class

  Friend Class PutFileStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, putKeyword As SyntaxToken, optionalPoundToken As SyntaxToken, fileNumber As ExpressionSyntax, optionalComma1 As SyntaxToken, optionalRecord As ExpressionSyntax, optionalComma2 As SyntaxToken, optionalVariable As IdentifierSyntax)
      MyBase.New(tree)
      Me.PutKeyword = putKeyword
      Me.OptionalPoundToken = optionalPoundToken
      Me.FileNumber = fileNumber
      Me.OptionalComma1 = optionalComma1
      Me.OptionalRecord = optionalRecord
      Me.OptionalComma2 = optionalComma2
      Me.OptionalVariable = optionalVariable
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.PutFileStatement
    Public ReadOnly Property PutKeyword As SyntaxToken
    Public ReadOnly Property OptionalPoundToken As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property OptionalComma1 As SyntaxToken
    Public ReadOnly Property OptionalRecord As ExpressionSyntax
    Public ReadOnly Property OptionalComma2 As SyntaxToken
    Public ReadOnly Property OptionalVariable As IdentifierSyntax
  End Class

End Namespace

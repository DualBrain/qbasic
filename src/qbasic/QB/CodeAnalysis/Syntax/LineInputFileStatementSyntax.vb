Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class LineInputFileStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, lineKeyword As SyntaxToken, inputKeyword As SyntaxToken, pound As SyntaxToken, fileNumber As ExpressionSyntax, comma As SyntaxToken, identifier As IdentifierSyntax)
      MyBase.New(tree)
      Me.LineKeyword = lineKeyword
      Me.InputKeyword = inputKeyword
      Me.Pound = pound
      Me.FileNumber = fileNumber
      Me.Comma = comma
      Me.Identifier = identifier
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LineInputFileStatement
    Public ReadOnly Property LineKeyword As SyntaxToken
    Public ReadOnly Property InputKeyword As SyntaxToken
    Public ReadOnly Property Pound As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Identifier As IdentifierSyntax

  End Class

End Namespace
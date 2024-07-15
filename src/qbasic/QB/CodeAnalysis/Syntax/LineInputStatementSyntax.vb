Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class LineInputStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, lineKeyword As SyntaxToken, inputKeyword As SyntaxToken, prompt As SyntaxToken, separator As SyntaxToken, identifier As IdentifierSyntax)
      MyBase.New(tree)
      Me.LineKeyword = lineKeyword
      Me.InputKeyword = inputKeyword
      Me.Prompt = prompt
      Me.Separator = separator
      Me.Identifier = identifier
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LineInputStatement
    Public ReadOnly Property LineKeyword As SyntaxToken
    Public ReadOnly Property InputKeyword As SyntaxToken
    Public ReadOnly Property Prompt As SyntaxToken
    Public ReadOnly Property Separator As SyntaxToken
    Public ReadOnly Property Identifier As IdentifierSyntax

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SubStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, subKeyword As SyntaxToken, identifier As SyntaxToken, openParenToken As SyntaxToken, parameters As SeparatedSyntaxList(Of ParameterSyntax), closeParenToken As SyntaxToken, staticKeyword As SyntaxToken, statements As BlockStatementSyntax, endKeyword As SyntaxToken, endSubKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.SubKeyword = subKeyword
      Me.Identifier = identifier
      Me.OpenParenToken = openParenToken
      Me.Parameters = parameters
      Me.CloseParenToken = closeParenToken
      Me.StaticKeyword = staticKeyword
      Me.Statements = statements
      Me.EndKeyword = endKeyword
      Me.EndSubKeyword = endSubKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SubStatement
    Public ReadOnly Property SubKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Parameters As SeparatedSyntaxList(Of ParameterSyntax)
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property StaticKeyword As SyntaxToken
    Public ReadOnly Property Statements As BlockStatementSyntax
    Public ReadOnly Property EndKeyword As SyntaxToken
    Public ReadOnly Property EndSubKeyword As SyntaxToken

  End Class

End Namespace
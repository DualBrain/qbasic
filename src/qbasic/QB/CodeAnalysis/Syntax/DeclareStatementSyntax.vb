Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class DeclareStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, declareKeyword As SyntaxToken, typeKeyword As SyntaxToken, identifier As SyntaxToken, openParen As SyntaxToken, parameters As SeparatedSyntaxList(Of ParameterSyntax), closeParen As SyntaxToken)
      MyBase.New(tree)
      Me.DeclareKeyword = declareKeyword
      Me.TypeKeyword = typeKeyword
      Me.Identifier = identifier
      Me.OpenParen = openParen
      Me.Parameters = parameters
      Me.CloseParen = closeParen
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DeclareStatement
    Public ReadOnly Property DeclareKeyword As SyntaxToken
    Public ReadOnly Property TypeKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property Parameters As SeparatedSyntaxList(Of ParameterSyntax)
    Public ReadOnly Property CloseParen As SyntaxToken

  End Class

End Namespace
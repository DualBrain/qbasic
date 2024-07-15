Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class TypeStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, typeKeyword As SyntaxToken, identifier As SyntaxToken, properties As List(Of ParameterSyntax), endKeyword As SyntaxToken, endTypeKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.TypeKeyword = typeKeyword
      Me.Identifier = identifier
      Me.Properties = properties
      Me.EndKeyword = endKeyword
      Me.EndTypeKeyword = endTypeKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind
    Public ReadOnly Property TypeKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property Properties As List(Of ParameterSyntax)
    Public ReadOnly Property EndKeyword As SyntaxToken
    Public ReadOnly Property EndTypeKeyword As SyntaxToken

  End Class

End Namespace
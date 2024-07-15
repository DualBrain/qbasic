Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class OutStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, outKeyword As SyntaxToken, port As ExpressionSyntax, comma As SyntaxToken, data As ExpressionSyntax)
      MyBase.New(tree)
      Me.OutKeyword = outKeyword
      Me.Port = port
      Me.Comma = comma
      Me.Data = data
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OutStatement
    Public ReadOnly Property OutKeyword As SyntaxToken
    Public ReadOnly Property Port As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Data As ExpressionSyntax

  End Class

End Namespace
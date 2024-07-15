Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class PlayStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, playKeyword As SyntaxToken, command As ExpressionSyntax)
      MyBase.New(tree)
      Me.PlayKeyword = playKeyword
      Me.Command = command
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.PlayStatement
    Public ReadOnly Property PlayKeyword As SyntaxToken
    Public ReadOnly Property Command As ExpressionSyntax

  End Class

End Namespace
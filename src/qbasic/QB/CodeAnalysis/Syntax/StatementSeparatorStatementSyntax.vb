Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class StatementSeparatorStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, colonToken As SyntaxToken)
      MyBase.New(tree)
      Me.ColonToken = colonToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.StatementSeparatorStatement
    Public ReadOnly Property ColonToken As SyntaxToken

  End Class

End Namespace
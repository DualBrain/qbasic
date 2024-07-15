Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class RemStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, remKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.RemKeyword = remKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.RemStatement
    Public ReadOnly Property RemKeyword As SyntaxToken

  End Class

End Namespace
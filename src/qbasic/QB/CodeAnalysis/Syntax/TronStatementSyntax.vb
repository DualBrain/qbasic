Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class TronStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, tronKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.TronKeyword = tronKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.TronStatement
    Public ReadOnly Property TronKeyword As SyntaxToken

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class TroffStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, troffKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.TroffKeyword = troffKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.TroffStatement
    Public ReadOnly Property TroffKeyword As SyntaxToken

  End Class

End Namespace
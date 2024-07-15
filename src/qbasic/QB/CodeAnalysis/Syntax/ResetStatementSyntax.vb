Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ResetStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, resetKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.ResetKeyword = resetKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ResetStatement
    Public ReadOnly Property ResetKeyword As SyntaxToken

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class BeepStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, beepKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.BeepKeyword = beepKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.BeepStatement
    Public ReadOnly Property BeepKeyword As SyntaxToken

  End Class

End Namespace
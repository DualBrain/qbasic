Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ProgStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, progKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.ProgKeyword = progKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbProgStatement
    Public ReadOnly Property ProgKeyword As SyntaxToken

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Public NotInheritable Class WendStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, wendKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.WendKeyword = wendKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WendStatement
    Public ReadOnly Property WendKeyword As SyntaxToken

  End Class

End Namespace
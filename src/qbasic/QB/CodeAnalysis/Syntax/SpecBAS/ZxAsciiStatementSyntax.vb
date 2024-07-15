Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ZxAsciiStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, zxAsciiKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.ZxAsciiKeyword = zxAsciiKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbZxAsciiStatement
    Public ReadOnly Property ZxAsciiKeyword As SyntaxToken

  End Class

End Namespace
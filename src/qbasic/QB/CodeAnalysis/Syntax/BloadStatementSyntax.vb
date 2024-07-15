Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class BloadStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, bloadKeyword As SyntaxToken, filespec As ExpressionSyntax, comma As SyntaxToken, offset As ExpressionSyntax)
      MyBase.New(tree)
      Me.BloadKeyword = bloadKeyword
      Me.Filespec = filespec
      Me.Comma = comma
      Me.Offset = offset
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.BloadStatement
    Public ReadOnly Property BloadKeyword As SyntaxToken
    Public ReadOnly Property Filespec As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Offset As ExpressionSyntax

  End Class

End Namespace
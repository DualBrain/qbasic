Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class BsaveStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, bsaveKeyword As SyntaxToken, filespec As ExpressionSyntax, comma1 As SyntaxToken, offset As ExpressionSyntax, comma2 As SyntaxToken, length As ExpressionSyntax)
      MyBase.New(tree)
      Me.BsaveKeyword = bsaveKeyword
      Me.Filespec = filespec
      Me.Comma1 = comma1
      Me.Offset = offset
      Me.Comma2 = comma2
      Me.Length = length
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.BsaveStatement
    Public ReadOnly Property BsaveKeyword As SyntaxToken
    Public ReadOnly Property Filespec As ExpressionSyntax
    Public ReadOnly Property Comma1 As SyntaxToken
    Public ReadOnly Property Offset As ExpressionSyntax
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property Length As ExpressionSyntax

  End Class

End Namespace
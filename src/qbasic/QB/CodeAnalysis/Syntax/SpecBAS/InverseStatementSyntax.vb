Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class InverseStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, inverseKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.InverseKeyword = inverseKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbInverseStatement
    Public ReadOnly Property InverseKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class AtStatementSyntax
    Inherits SyntaxNode

    Public Sub New(tree As SyntaxTree, atKeyword As SyntaxToken, rowExpression As ExpressionSyntax, commaToken As SyntaxToken, colExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.AtKeyword = atKeyword
      Me.RowExpression = rowExpression
      Me.CommaToken = commaToken
      Me.ColExpression = colExpression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbAtClause
    Public ReadOnly Property AtKeyword As SyntaxToken
    Public ReadOnly Property RowExpression As ExpressionSyntax
    Public ReadOnly Property CommaToken As SyntaxToken
    Public ReadOnly Property ColExpression As ExpressionSyntax

  End Class

End Namespace
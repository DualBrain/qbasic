Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class MoveStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, moveKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.MoveKeyword = moveKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbMoveStatement
    Public ReadOnly Property MoveKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
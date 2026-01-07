Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class AssignmentExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(tree As SyntaxTree, variable As ExpressionSyntax, equalToken As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.Variable = variable
      Me.EqualToken = equalToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.AssignmentExpression
    Public ReadOnly Property Variable As ExpressionSyntax
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
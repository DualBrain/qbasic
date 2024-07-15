Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class AssignmentExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(tree As SyntaxTree, identifier As IdentifierSyntax, equalToken As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.Identifier = identifier
      Me.EqualToken = equalToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.AssignmentExpression
    Public ReadOnly Property Identifier As IdentifierSyntax
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
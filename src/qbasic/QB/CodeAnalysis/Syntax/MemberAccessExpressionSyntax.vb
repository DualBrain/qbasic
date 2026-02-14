Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class MemberAccessExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(tree As SyntaxTree, expression As ExpressionSyntax, dotToken As SyntaxToken, identifier As SyntaxToken)
      MyBase.New(tree)
      Me.Expression = expression
      Me.DotToken = dotToken
      Me.Identifier = identifier
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.MemberAccessExpression
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property DotToken As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken

  End Class

End Namespace


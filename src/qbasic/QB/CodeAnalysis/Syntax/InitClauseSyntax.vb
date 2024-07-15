Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class InitClauseSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree,
            equalToken As SyntaxToken,
            initializer As List(Of SyntaxNode),
            optionalBaseKeyword As SyntaxToken,
            optionalBaseExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.EqualToken = equalToken
      Me.Initializer = initializer
      Me.OptionalBaseKeyword = optionalBaseKeyword
      Me.OptionalBaseExpression = optionalBaseExpression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.InitClause
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Initializer As List(Of SyntaxNode)
    Public ReadOnly Property OptionalBaseKeyword As SyntaxToken
    Public ReadOnly Property OptionalBaseExpression As ExpressionSyntax

  End Class

End Namespace
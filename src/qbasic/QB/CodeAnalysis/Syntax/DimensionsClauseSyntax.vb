Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class DimensionsClauseSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, openParenToken As SyntaxToken, dimensions As List(Of SyntaxNode), closeParen As SyntaxToken)
      MyBase.New(tree)
      Me.OpenParenToken = openParenToken
      Me.Dimensions = dimensions
      Me.CloseParen = closeParen
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DimensionsClause
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Dimensions As List(Of SyntaxNode)
    Public ReadOnly Property CloseParen As SyntaxToken

  End Class

  Public NotInheritable Class DimensionClauseSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, optionalLower As ExpressionSyntax, optionalToKeyword As SyntaxToken, upper As ExpressionSyntax)
      MyBase.New(tree)
      Me.OptionalLower = optionalLower
      Me.OptionalToKeyword = optionalToKeyword
      Me.Upper = upper
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DimensionClause
    Public ReadOnly Property OptionalLower As ExpressionSyntax
    Public ReadOnly Property OptionalToKeyword As SyntaxToken
    Public ReadOnly Property Upper As ExpressionSyntax

  End Class

  'Partial Public NotInheritable Class LowerSubscriptClauseSyntax
  '  Inherits SyntaxNode

  '  Sub New(tree As SyntaxTree, lower As ExpressionSyntax, toKeyword As SyntaxToken)
  '    MyBase.New(tree)
  '    Me.Lower = lower
  '    Me.ToKeyword = toKeyword
  '  End Sub

  '  Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LowerSubscriptClause
  '  Public ReadOnly Property Lower As ExpressionSyntax
  '  Public ReadOnly Property ToKeyword As SyntaxToken

  'End Class

End Namespace
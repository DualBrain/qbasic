Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class CircleStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, circleKeyword As SyntaxToken, optionalStepKeyword As SyntaxToken, openParen As SyntaxToken, x As ExpressionSyntax, comma1 As SyntaxToken, y As ExpressionSyntax, closeParen As SyntaxToken, comma2 As SyntaxToken, radius As ExpressionSyntax, optionalComma3 As SyntaxToken, optionalColor As ExpressionSyntax, optionalComma4 As SyntaxToken, optionalStart As ExpressionSyntax, optionalComma5 As SyntaxToken, optionalEnd As ExpressionSyntax, optionalComma6 As SyntaxToken, optionalAspect As ExpressionSyntax)
      MyBase.New(tree)
      Me.CircleKeyword = circleKeyword
      Me.OptionalStepKeyword = optionalStepKeyword
      Me.OpenParen = openParen
      Me.X = x
      Me.Comma1 = comma1
      Me.Y = y
      Me.CloseParen = closeParen
      Me.Comma2 = comma2
      Me.Radius = radius
      Me.OptionalComma3 = optionalComma3
      Me.OptionalColor = optionalColor
      Me.OptionalComma4 = optionalComma4
      Me.OptionalStart = optionalStart
      Me.OptionalComma5 = optionalComma5
      Me.OptionalEnd = optionalEnd
      Me.OptionalComma6 = optionalComma6
      Me.OptionalAspect = optionalAspect
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CircleStatement
    Public ReadOnly Property Tree As SyntaxTree
    Public ReadOnly Property CircleKeyword As SyntaxToken
    Public ReadOnly Property OptionalStepKeyword As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property X As ExpressionSyntax
    Public ReadOnly Property Comma1 As SyntaxToken
    Public ReadOnly Property Y As ExpressionSyntax
    Public ReadOnly Property CloseParen As SyntaxToken
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property Radius As ExpressionSyntax
    Public ReadOnly Property OptionalComma3 As SyntaxToken
    Public ReadOnly Property OptionalColor As ExpressionSyntax
    Public ReadOnly Property OptionalComma4 As SyntaxToken
    Public ReadOnly Property OptionalStart As ExpressionSyntax
    Public ReadOnly Property OptionalComma5 As SyntaxToken
    Public ReadOnly Property OptionalEnd As ExpressionSyntax
    Public ReadOnly Property OptionalComma6 As SyntaxToken
    Public ReadOnly Property OptionalAspect As ExpressionSyntax

  End Class

End Namespace
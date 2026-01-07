Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class PaintStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, paintKeyword As SyntaxToken, optionalStepKeyword As SyntaxToken, openParen As SyntaxToken, x As ExpressionSyntax, comma1 As SyntaxToken, y As ExpressionSyntax, closeParen As SyntaxToken, optionalComma2 As SyntaxToken, optionalColorOrTile As ExpressionSyntax, optionalComma3 As SyntaxToken, optionalBorderColor As ExpressionSyntax, optionalComma4 As SyntaxToken, optionalBackground As ExpressionSyntax)
      MyBase.New(tree)
      Me.PaintKeyword = paintKeyword
      Me.OptionalStepKeyword = optionalStepKeyword
      Me.OpenParen = openParen
      Me.X = x
      Me.Comma1 = comma1
      Me.Y = y
      Me.CloseParen = closeParen
      Me.OptionalComma2 = optionalComma2
      Me.OptionalColorOrTile = optionalColorOrTile
      Me.OptionalComma3 = optionalComma3
      Me.OptionalBorderColor = optionalBorderColor
      Me.OptionalComma4 = optionalComma4
      Me.OptionalBackground = optionalBackground
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.PaintStatement
    Public ReadOnly Property PaintKeyword As SyntaxToken
    Public ReadOnly Property OptionalStepKeyword As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property X As ExpressionSyntax
    Public ReadOnly Property Comma1 As SyntaxToken
    Public ReadOnly Property Y As ExpressionSyntax
    Public ReadOnly Property CloseParen As SyntaxToken
    Public ReadOnly Property OptionalComma2 As SyntaxToken
    Public ReadOnly Property OptionalColorOrTile As ExpressionSyntax
    Public ReadOnly Property OptionalComma3 As SyntaxToken
    Public ReadOnly Property OptionalBorderColor As ExpressionSyntax
    Public ReadOnly Property OptionalComma4 As SyntaxToken
    Public ReadOnly Property OptionalBackground As ExpressionSyntax

  End Class

End Namespace
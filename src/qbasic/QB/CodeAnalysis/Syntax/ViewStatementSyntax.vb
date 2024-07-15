Namespace Global.QB.CodeAnalysis.Syntax
  Friend Class ViewStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, viewKeyword As SyntaxToken, screenKeyword As SyntaxToken, openParen1 As SyntaxToken, x1 As ExpressionSyntax, comma1 As SyntaxToken, y1 As ExpressionSyntax, closeParen1 As SyntaxToken, dash As SyntaxToken, openParen2 As SyntaxToken, x2 As ExpressionSyntax, comma2 As SyntaxToken, y2 As ExpressionSyntax, closeParen2 As SyntaxToken, comma3 As SyntaxToken, color As ExpressionSyntax, comma4 As SyntaxToken, border As ExpressionSyntax)
      MyBase.New(tree)
      Me.ViewKeyword = viewKeyword
      Me.ScreenKeyword = screenKeyword
      Me.OpenParen1 = openParen1
      Me.X1 = x1
      Me.Comma1 = comma1
      Me.Y1 = y1
      Me.CloseParen1 = closeParen1
      Me.Dash = dash
      Me.OpenParen2 = openParen2
      Me.X2 = x2
      Me.Comma2 = comma2
      Me.Y2 = y2
      Me.CloseParen2 = closeParen2
      Me.Comma3 = comma3
      Me.Color = color
      Me.Comma4 = comma4
      Me.Border = border
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ViewStatement
    Public ReadOnly Property ViewKeyword As SyntaxToken
    Public ReadOnly Property ScreenKeyword As SyntaxToken
    Public ReadOnly Property OpenParen1 As SyntaxToken
    Public ReadOnly Property X1 As ExpressionSyntax
    Public ReadOnly Property Comma1 As SyntaxToken
    Public ReadOnly Property Y1 As ExpressionSyntax
    Public ReadOnly Property CloseParen1 As SyntaxToken
    Public ReadOnly Property Dash As SyntaxToken
    Public ReadOnly Property OpenParen2 As SyntaxToken
    Public ReadOnly Property X2 As ExpressionSyntax
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property Y2 As ExpressionSyntax
    Public ReadOnly Property CloseParen2 As SyntaxToken
    Public ReadOnly Property Comma3 As SyntaxToken
    Public ReadOnly Property Color As ExpressionSyntax
    Public ReadOnly Property Comma4 As SyntaxToken
    Public ReadOnly Property Border As ExpressionSyntax

  End Class

End Namespace
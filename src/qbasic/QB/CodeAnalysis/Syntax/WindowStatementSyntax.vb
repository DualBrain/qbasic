Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class WindowStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, windowKeyword As SyntaxToken, screenKeyword As SyntaxToken, openParen1 As SyntaxToken, x1 As ExpressionSyntax, comma1 As SyntaxToken, y1 As ExpressionSyntax, closeparen1 As SyntaxToken, dash As SyntaxToken, openParen2 As SyntaxToken, x2 As ExpressionSyntax, comma2 As SyntaxToken, y2 As ExpressionSyntax, closeparen2 As SyntaxToken)
      MyBase.New(tree)
      Me.WindowKeyword = windowKeyword
      Me.ScreenKeyword = screenKeyword
      Me.OpenParen1 = openParen1
      Me.X1 = x1
      Me.Comma1 = comma1
      Me.Y1 = y1
      Me.Closeparen1 = closeparen1
      Me.Dash = dash
      Me.OpenParen2 = openParen2
      Me.X2 = x2
      Me.Comma2 = comma2
      Me.Y2 = y2
      Me.Closeparen2 = closeparen2
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WindowStatement
    Public ReadOnly Property WindowKeyword As SyntaxToken
    Public ReadOnly Property ScreenKeyword As SyntaxToken
    Public ReadOnly Property OpenParen1 As SyntaxToken
    Public ReadOnly Property X1 As ExpressionSyntax
    Public ReadOnly Property Comma1 As SyntaxToken
    Public ReadOnly Property Y1 As ExpressionSyntax
    Public ReadOnly Property Closeparen1 As SyntaxToken
    Public ReadOnly Property Dash As SyntaxToken
    Public ReadOnly Property OpenParen2 As SyntaxToken
    Public ReadOnly Property X2 As ExpressionSyntax
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property Y2 As ExpressionSyntax
    Public ReadOnly Property Closeparen2 As SyntaxToken

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class LineStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, lineKeyword As SyntaxToken, optionalStep1 As SyntaxToken, optionalOpenParen1 As SyntaxToken, optionalX1 As ExpressionSyntax, optionalComma1 As SyntaxToken, optionalY1 As ExpressionSyntax, optionalCloseParen1 As SyntaxToken, dashToken As SyntaxToken, optionalStep2 As SyntaxToken, openParen2 As SyntaxToken, x2 As ExpressionSyntax, comma2 As SyntaxToken, y2 As ExpressionSyntax, closeParen2 As SyntaxToken, optionalComma3 As SyntaxToken, optionalAttribute As ExpressionSyntax, optionalComma4 As SyntaxToken, optionalMode As SyntaxToken, optionalComma5 As SyntaxToken, optionalStyle As ExpressionSyntax)
      MyBase.New(tree)
      Me.Tree = tree
      Me.LineKeyword = lineKeyword
      Me.OptionalStep1 = optionalStep1
      Me.OptionalOpenParen1 = optionalOpenParen1
      Me.OptionalX1 = optionalX1
      Me.OptionalComma1 = optionalComma1
      Me.OptionalY1 = optionalY1
      Me.OptionalCloseParen1 = optionalCloseParen1
      Me.DashToken = dashToken
      Me.OptionalStep2 = optionalStep2
      Me.OpenParen2 = openParen2
      Me.X2 = x2
      Me.Comma2 = comma2
      Me.Y2 = y2
      Me.CloseParen2 = closeParen2
      Me.OptionalComma3 = optionalComma3
      Me.OptionalAttribute = optionalAttribute
      Me.OptionalComma4 = optionalComma4
      Me.OptionalMode = optionalMode
      Me.OptionalComma5 = optionalComma5
      Me.OptionalStyle = optionalStyle
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LineStatement
    Public ReadOnly Property Tree As SyntaxTree
    Public ReadOnly Property LineKeyword As SyntaxToken
    Public ReadOnly Property OptionalStep1 As SyntaxToken
    Public ReadOnly Property OptionalOpenParen1 As SyntaxToken
    Public ReadOnly Property OptionalX1 As ExpressionSyntax
    Public ReadOnly Property OptionalComma1 As SyntaxToken
    Public ReadOnly Property OptionalY1 As ExpressionSyntax
    Public ReadOnly Property OptionalCloseParen1 As SyntaxToken
    Public ReadOnly Property DashToken As SyntaxToken
    Public ReadOnly Property OptionalStep2 As SyntaxToken
    Public ReadOnly Property OpenParen2 As SyntaxToken
    Public ReadOnly Property X2 As ExpressionSyntax
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property Y2 As ExpressionSyntax
    Public ReadOnly Property CloseParen2 As SyntaxToken
    Public ReadOnly Property OptionalComma3 As SyntaxToken
    Public ReadOnly Property OptionalAttribute As ExpressionSyntax
    Public ReadOnly Property OptionalComma4 As SyntaxToken
    Public ReadOnly Property OptionalMode As SyntaxToken
    Public ReadOnly Property OptionalComma5 As SyntaxToken
    Public ReadOnly Property OptionalStyle As ExpressionSyntax

  End Class

End Namespace
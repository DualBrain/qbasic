Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class StrigStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, strigKeyword As SyntaxToken, openParen As SyntaxToken, n As ExpressionSyntax, closeParen As SyntaxToken, verb As SyntaxToken)
      MyBase.New(tree)
      Me.StrigKeyword = strigKeyword
      Me.OpenParen = openParen
      Me.N = n
      Me.CloseParen = closeParen
      Me.Verb = verb
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.StrigStatement
    Public ReadOnly Property StrigKeyword As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property N As ExpressionSyntax
    Public ReadOnly Property CloseParen As SyntaxToken
    Public ReadOnly Property Verb As SyntaxToken

  End Class

End Namespace
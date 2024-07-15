Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class CallStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, callKeyword As SyntaxToken, identifier As SyntaxToken, openParen As SyntaxToken, expressions As SeparatedSyntaxList(Of ExpressionSyntax), closeParen As SyntaxToken)
      MyBase.New(tree)
      Me.CallKeyword = callKeyword
      Me.Identifier = identifier
      Me.OpenParen = openParen
      Me.Expressions = expressions
      Me.CloseParen = closeParen
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CallStatement
    Public ReadOnly Property CallKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property Expressions As SeparatedSyntaxList(Of ExpressionSyntax)
    Public ReadOnly Property CloseParen As SyntaxToken

  End Class

End Namespace
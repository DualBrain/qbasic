Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class IdentifierExpressionSyntax
    Inherits ExpressionSyntax

    Public Sub New(tree As SyntaxTree, identifier As IdentifierSyntax)
      MyBase.New(tree)
      Me.Identifier = identifier.Identifier
      OpenParen = identifier.OpenParen
      Arguments = identifier.Arguments
      CloseParen = identifier.CloseParen
    End Sub

    Public Sub New(tree As SyntaxTree, identifier As SyntaxToken)
      MyBase.New(tree)
      Me.Identifier = identifier
      Me.OpenParen = Nothing
      Me.Arguments = Nothing
      Me.CloseParen = Nothing
    End Sub

    Public Sub New(tree As SyntaxTree, identifier As SyntaxToken, openParen As SyntaxToken, arguments As SeparatedSyntaxList(Of ExpressionSyntax), closeParen As SyntaxToken)
      MyBase.New(tree)
      Me.Identifier = identifier
      Me.OpenParen = openParen
      Me.Arguments = arguments
      Me.CloseParen = closeParen
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.IdentifierSyntax
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property Arguments As SeparatedSyntaxList(Of ExpressionSyntax)
    Public ReadOnly Property CloseParen As SyntaxToken

  End Class

End Namespace

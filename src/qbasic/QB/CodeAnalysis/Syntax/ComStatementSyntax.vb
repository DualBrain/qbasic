Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ComStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, comKeyword As SyntaxToken, openParen As SyntaxToken, n As ExpressionSyntax, closeParent As SyntaxToken, verb As SyntaxNode)
      MyBase.New(tree)
      Me.ComKeyword = comKeyword
      Me.OpenParen = openParen
      Me.N = n
      Me.CloseParent = closeParent
      Me.Verb = verb
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ComStatement
    Public ReadOnly Property ComKeyword As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property N As ExpressionSyntax
    Public ReadOnly Property CloseParent As SyntaxToken
    Public ReadOnly Property Verb As SyntaxNode

  End Class

End Namespace
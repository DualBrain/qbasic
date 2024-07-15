Namespace Global.QB.CodeAnalysis.Syntax

  Public NotInheritable Class LetStatementSyntax
    Inherits StatementSyntax

    ' LET x = 10
    ' LET a$ = "test"
    ' LET v = 1.111

    Public Sub New(tree As SyntaxTree, letToken As SyntaxToken, identifiers As List(Of SyntaxNode), equalToken As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.LetToken = letToken
      Me.Identifiers = identifiers
      Me.EqualToken = equalToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LetStatement
    Public ReadOnly Property LetToken As SyntaxToken
    Public ReadOnly Property Identifiers As List(Of SyntaxNode)
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
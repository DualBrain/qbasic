Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SbWaitStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, waitKeyword As SyntaxToken, optionalScreenKeyword As SyntaxToken, optionalExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.WaitKeyword = waitKeyword
      Me.OptionalScreenKeyword = optionalScreenKeyword
      Me.OptionalExpression = optionalExpression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbWaitStatement
    Public ReadOnly Property WaitKeyword As SyntaxToken
    Public ReadOnly Property OptionalScreenKeyword As SyntaxToken
    Public ReadOnly Property OptionalExpression As ExpressionSyntax

  End Class

End Namespace
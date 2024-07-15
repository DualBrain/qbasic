Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OnGotoStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, onKeyword As SyntaxToken, expression As ExpressionSyntax, gotoKeyword As SyntaxToken, targets As List(Of SyntaxToken))
      MyBase.New(tree)
      Me.OnKeyword = onKeyword
      Me.Expression = expression
      Me.GotoKeyword = gotoKeyword
      Me.Targets = targets
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OnGotoStatement
    Public ReadOnly Property OnKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property GotoKeyword As SyntaxToken
    Public ReadOnly Property Targets As List(Of SyntaxToken)

  End Class

End Namespace
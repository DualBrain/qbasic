Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OnGosubStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, onKeyword As SyntaxToken, expression As ExpressionSyntax, gosubKeyword As SyntaxToken, targets As List(Of SyntaxToken))
      MyBase.New(tree)
      Me.OnKeyword = onKeyword
      Me.Expression = expression
      Me.GosubKeyword = gosubKeyword
      Me.Targets = targets
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OnGosubStatement
    Public ReadOnly Property OnKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property GosubKeyword As SyntaxToken
    Public ReadOnly Property Targets As List(Of SyntaxToken)

  End Class

End Namespace
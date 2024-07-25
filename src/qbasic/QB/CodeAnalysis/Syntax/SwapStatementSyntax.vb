Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SwapStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, swapKeyword As SyntaxToken, variable1 As NameExpressionSyntax, comma As SyntaxToken, variable2 As NameExpressionSyntax)
      MyBase.New(tree)
      Me.SwapKeyword = swapKeyword
      Me.Variable1 = variable1
      Me.Comma = comma
      Me.Variable2 = variable2
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SwapStatement
    Public ReadOnly Property SwapKeyword As SyntaxToken
    Public ReadOnly Property Variable1 As NameExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Variable2 As NameExpressionSyntax

  End Class

End Namespace
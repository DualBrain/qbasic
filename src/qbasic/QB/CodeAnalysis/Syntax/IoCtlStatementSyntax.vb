Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class IoCtlStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, ioctlKeyword As SyntaxToken, pound As SyntaxToken, fileNumber As ExpressionSyntax, comma As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.IoctlKeyword = ioctlKeyword
      Me.Pound = pound
      Me.FileNumber = fileNumber
      Me.Comma = comma
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.IoCtlStatement
    Public ReadOnly Property IoctlKeyword As SyntaxToken
    Public ReadOnly Property Pound As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
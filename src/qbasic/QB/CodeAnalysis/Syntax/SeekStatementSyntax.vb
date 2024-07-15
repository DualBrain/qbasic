Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SeekStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, seekKeyword As SyntaxToken, pound As SyntaxToken, fileNumber As ExpressionSyntax, comma As SyntaxToken, position As ExpressionSyntax)
      MyBase.New(tree)
      Me.SeekKeyword = seekKeyword
      Me.Pound = pound
      Me.FileNumber = fileNumber
      Me.Comma = comma
      Me.Position = position
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SeekStatement
    Public ReadOnly Property SeekKeyword As SyntaxToken
    Public ReadOnly Property Pound As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Position As ExpressionSyntax

  End Class

End Namespace
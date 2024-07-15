Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class LockStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, lockKeyword As SyntaxToken, pound As SyntaxToken, fileNumber As ExpressionSyntax, comma As SyntaxToken, record As ExpressionSyntax)
      MyBase.New(tree)
      Me.LockKeyword = lockKeyword
      Me.Pound = pound
      Me.FileNumber = fileNumber
      Me.Comma = comma
      Me.Record = record
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LockStatement
    Public ReadOnly Property LockKeyword As SyntaxToken
    Public ReadOnly Property Pound As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Record As ExpressionSyntax

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ShellStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, shellKeyword As SyntaxToken, commandString As ExpressionSyntax)
      MyBase.New(tree)
      Me.ShellKeyword = shellKeyword
      Me.CommandString = commandString
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ShellStatement
    Public ReadOnly Property ShellKeyword As SyntaxToken
    Public ReadOnly Property CommandString As ExpressionSyntax

  End Class

End Namespace
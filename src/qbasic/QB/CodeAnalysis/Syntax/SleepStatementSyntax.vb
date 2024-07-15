Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SleepStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, sleepKeyword As SyntaxToken, seconds As ExpressionSyntax)
      MyBase.New(tree)
      Me.SleepKeyword = sleepKeyword
      Me.Seconds = seconds
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SleepStatement
    Public ReadOnly Property SleepKeyword As SyntaxToken
    Public ReadOnly Property Seconds As ExpressionSyntax

  End Class

End Namespace
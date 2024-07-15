Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class TimerStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, timerKeyword As SyntaxToken, verb As SyntaxToken)
      MyBase.New(tree)
      Me.TimerKeyword = timerKeyword
      Me.Verb = verb
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.TimerStatement
    Public ReadOnly Property TimerKeyword As SyntaxToken
    Public ReadOnly Property Verb As SyntaxToken

  End Class

End Namespace
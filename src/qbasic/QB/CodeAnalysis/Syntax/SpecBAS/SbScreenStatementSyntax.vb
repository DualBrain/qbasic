Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SbScreenStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   screenKeyword As SyntaxToken,
                   actionKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.ScreenKeyword = screenKeyword
      Me.ActionKeyword = actionKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbScreenStatement
    Public ReadOnly Property ScreenKeyword As SyntaxToken
    Public ReadOnly Property ActionKeyword As SyntaxToken

  End Class

End Namespace
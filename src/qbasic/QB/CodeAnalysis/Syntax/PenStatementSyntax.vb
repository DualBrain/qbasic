Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class PenStatementSyntax
    Inherits StatementSyntax

    ' PEN ON
    ' PEN OFF
    ' PEN STOP

    Public Sub New(tree As SyntaxTree, penKeyword As SyntaxToken, verbKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.PenKeyword = penKeyword
      Me.VerbKeyword = verbKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.PenStatement
    Public ReadOnly Property PenKeyword As SyntaxToken
    Public ReadOnly Property VerbKeyword As SyntaxToken

  End Class

End Namespace
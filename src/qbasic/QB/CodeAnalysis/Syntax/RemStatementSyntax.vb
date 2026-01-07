Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class RemStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, remKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.RemKeyword = remKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.RemStatement
    Public ReadOnly Property RemKeyword As SyntaxToken

    Public ReadOnly Property Comment As String
      Get
        For Each trivia In RemKeyword.TrailingTrivia
          If trivia.Kind = SyntaxKind.SingleLineCommentTrivia Then
            Return trivia.Text.Trim()
          End If
        Next
        Return ""
      End Get
    End Property

  End Class

End Namespace
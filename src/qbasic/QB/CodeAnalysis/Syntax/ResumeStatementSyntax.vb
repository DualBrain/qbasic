Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ResumeStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, resumeKeyword As SyntaxToken, optionalLine As ExpressionSyntax)
      MyBase.New(tree)
      Me.ResumeKeyword = resumeKeyword
      Me.OptionalLine = optionalLine
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ResumeStatement
    Public ReadOnly Property ResumeKeyword As SyntaxToken
    Public ReadOnly Property OptionalLine As ExpressionSyntax

  End Class

  Friend Class ResumeNextStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, resumeKeyword As SyntaxToken, nextKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.ResumeKeyword = resumeKeyword
      Me.NextKeyword = nextKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ResumeNextStatement
    Public ReadOnly Property ResumeKeyword As SyntaxToken
    Public ReadOnly Property NextKeyword As SyntaxToken

  End Class

End Namespace

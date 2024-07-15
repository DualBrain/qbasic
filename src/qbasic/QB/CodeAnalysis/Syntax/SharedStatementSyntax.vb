Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SharedStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, sharedKeyword As SyntaxToken, variables As List(Of SyntaxNode))
      MyBase.New(tree)
      Me.SharedKeyword = sharedKeyword
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SharedStatement
    Public ReadOnly Property SharedKeyword As SyntaxToken
    Public ReadOnly Property Variables As List(Of SyntaxNode)

  End Class

End Namespace
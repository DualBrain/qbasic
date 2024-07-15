Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class EraseStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, eraseKeyword As SyntaxToken, variables As List(Of SyntaxNode))
      MyBase.New(tree)
      Me.EraseKeyword = eraseKeyword
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.EraseStatement
    Public ReadOnly Property EraseKeyword As SyntaxToken
    Public ReadOnly Property Variables As List(Of SyntaxNode)

  End Class

End Namespace
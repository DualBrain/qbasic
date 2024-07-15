Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class RedimStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, redimKeyword As SyntaxToken, optionalSharedKeyword As Object, variables As List(Of SyntaxNode))
      MyBase.New(tree)
      Me.RedimKeyword = redimKeyword
      Me.OptionalSharedKeyword = optionalSharedKeyword
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.RedimStatement
    Public ReadOnly Property RedimKeyword As SyntaxToken
    Public ReadOnly Property OptionalSharedKeyword As Object
    Public ReadOnly Property Variables As List(Of SyntaxNode)

  End Class

End Namespace
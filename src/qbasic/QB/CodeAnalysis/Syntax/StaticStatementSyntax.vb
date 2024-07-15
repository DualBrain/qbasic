Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class StaticStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, staticKeyword As Object, variables As List(Of SyntaxNode))
      MyBase.New(tree)
      Me.StaticKeyword = staticKeyword
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.StaticStatement
    Public ReadOnly Property StaticKeyword As Object
    Public ReadOnly Property Variables As List(Of SyntaxNode)

  End Class

End Namespace
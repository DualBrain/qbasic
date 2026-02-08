Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ReadStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, readKeyword As SyntaxToken, variables As SeparatedSyntaxList(Of ExpressionSyntax))
      MyBase.New(tree)
      Me.ReadKeyword = readKeyword
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ReadStatement
    Public ReadOnly Property ReadKeyword As SyntaxToken
    Public ReadOnly Property Variables As SeparatedSyntaxList(Of ExpressionSyntax)

  End Class

End Namespace
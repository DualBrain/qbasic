Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class CommonStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, commonKeyword As SyntaxToken, sharedKeyword As SyntaxToken, variables As ImmutableArray(Of SyntaxNode))
      MyBase.New(tree)
      Me.CommonKeyword = commonKeyword
      Me.SharedKeyword = sharedKeyword
      Me.variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CommonStatement
    Public ReadOnly Property CommonKeyword As SyntaxToken
    Public ReadOnly Property SharedKeyword As SyntaxToken
    Public ReadOnly Property Variables As ImmutableArray(Of SyntaxNode)

  End Class

End Namespace
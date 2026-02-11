Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class DataStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, dataKeyword As SyntaxToken, data As ImmutableArray(Of String))
      MyBase.New(tree)
      Me.DataKeyword = dataKeyword
      Me.Data = data
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DataStatement
    Public ReadOnly Property DataKeyword As SyntaxToken
    Public ReadOnly Property Data As ImmutableArray(Of String)

  End Class

End Namespace
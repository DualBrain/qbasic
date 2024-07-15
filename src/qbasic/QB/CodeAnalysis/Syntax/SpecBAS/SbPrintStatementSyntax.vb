Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SbPrintStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, printKeyword As SyntaxToken, nodes As ImmutableArray(Of SyntaxNode))
      MyBase.New(tree)
      Me.Tree = tree
      Me.PrintKeyword = printKeyword
      Me.Nodes = nodes
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.PrintStatement
    Public ReadOnly Property Tree As SyntaxTree
    Public ReadOnly Property PrintKeyword As SyntaxToken
    Public ReadOnly Property Nodes As ImmutableArray(Of SyntaxNode)

  End Class

End Namespace
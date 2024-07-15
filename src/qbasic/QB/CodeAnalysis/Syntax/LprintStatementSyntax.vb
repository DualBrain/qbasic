Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class LprintStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, lprintKeyword As SyntaxToken, usingKeyword As SyntaxToken, usingFormat As ExpressionSyntax, usingSemicolon As SyntaxToken, syntaxNodes As ImmutableArray(Of SyntaxNode))
      MyBase.New(tree)
      Me.LprintKeyword = lprintKeyword
      Me.UsingKeyword = usingKeyword
      Me.UsingFormat = usingFormat
      Me.UsingSemicolon = usingSemicolon
      Me.SyntaxNodes = syntaxNodes
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LprintStatement
    Public ReadOnly Property LprintKeyword As SyntaxToken
    Public ReadOnly Property UsingKeyword As SyntaxToken
    Public ReadOnly Property UsingFormat As ExpressionSyntax
    Public ReadOnly Property UsingSemicolon As SyntaxToken
    Public ReadOnly Property SyntaxNodes As ImmutableArray(Of SyntaxNode)

  End Class

End Namespace
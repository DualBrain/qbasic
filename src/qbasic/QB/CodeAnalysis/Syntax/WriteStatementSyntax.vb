Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class WriteStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, writeKeyword As SyntaxToken, pound As SyntaxToken, fileNumber As ExpressionSyntax, comma As SyntaxToken, expressions As ImmutableArray(Of SyntaxNode))
      MyBase.New(tree)
      Me.WriteKeyword = writeKeyword
      Me.Pound = pound
      Me.FileNumber = fileNumber
      Me.Comma = comma
      Me.Expressions = expressions
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WriteStatement
    Public ReadOnly Property WriteKeyword As SyntaxToken
    Public ReadOnly Property Pound As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Expressions As ImmutableArray(Of SyntaxNode)

  End Class

End Namespace
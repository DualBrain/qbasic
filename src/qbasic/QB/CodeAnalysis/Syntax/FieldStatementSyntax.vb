Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class FieldStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, fieldKeyword As SyntaxToken, poundToken As SyntaxToken, filenumber As ExpressionSyntax, comma As SyntaxToken, identifiers As ImmutableArray(Of SyntaxNode))
      MyBase.New(tree)
      Me.FieldKeyword = fieldKeyword
      Me.PoundToken = poundToken
      Me.Filenumber = filenumber
      Me.Comma = comma
      Me.Identifiers = identifiers
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.FieldStatement
    Public ReadOnly Property FieldKeyword As SyntaxToken
    Public ReadOnly Property PoundToken As SyntaxToken
    Public ReadOnly Property Filenumber As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Identifiers As ImmutableArray(Of SyntaxNode)

  End Class

End Namespace
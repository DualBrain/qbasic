Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class PrintStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   printKeyword As SyntaxToken,
                   pound As SyntaxToken,
                   fileNumber As ExpressionSyntax,
                   comma As SyntaxToken,
                   usingKeyword As SyntaxToken,
                   usingformat As ExpressionSyntax,
                   usingSemicolon As SyntaxToken,
                   nodes As ImmutableArray(Of SyntaxNode))
      MyBase.New(tree)
      Me.PrintKeyword = printKeyword
      Me.Pound = pound
      Me.FileNumber = fileNumber
      Me.Comma = comma
      Me.UsingKeyword = usingKeyword
      Me.Usingformat = usingformat
      Me.UsingSemicolon = usingSemicolon
      Me.Nodes = nodes
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.PrintStatement
    Public ReadOnly Property PrintKeyword() As SyntaxToken
    Public ReadOnly Property Pound As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property UsingKeyword As SyntaxToken
    Public ReadOnly Property Usingformat As ExpressionSyntax
    Public ReadOnly Property UsingSemicolon As SyntaxToken
    Public ReadOnly Property Nodes() As ImmutableArray(Of SyntaxNode)

  End Class

End Namespace
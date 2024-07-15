Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class OpenStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, openKeyword As SyntaxToken, file As ExpressionSyntax, forKeyword As SyntaxToken, modeKeyword As SyntaxToken, accessKeyword As SyntaxToken, access As ImmutableArray(Of SyntaxNode), lock As ImmutableArray(Of SyntaxNode), asKeyword As SyntaxToken, pound As SyntaxToken, fileNumber As ExpressionSyntax, lenKeyword As SyntaxToken, equal As SyntaxToken, recLen As ExpressionSyntax)
      MyBase.New(tree)
      Me.OpenKeyword = openKeyword
      Me.File = file
      Me.ForKeyword = forKeyword
      Me.ModeKeyword = modeKeyword
      Me.AccessKeyword = accessKeyword
      Me.Access = access
      Me.Lock = lock
      Me.AsKeyword = asKeyword
      Me.Pound = pound
      Me.FileNumber = fileNumber
      Me.LenKeyword = lenKeyword
      Me.Equal = equal
      Me.RecLen = recLen
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.OpenStatement
    Public ReadOnly Property OpenKeyword As SyntaxToken
    Public ReadOnly Property File As ExpressionSyntax
    Public ReadOnly Property ForKeyword As SyntaxToken
    Public ReadOnly Property ModeKeyword As SyntaxToken
    Public ReadOnly Property AccessKeyword As SyntaxToken
    Public ReadOnly Property Access As ImmutableArray(Of SyntaxNode)
    Public ReadOnly Property Lock As ImmutableArray(Of SyntaxNode)
    Public ReadOnly Property AsKeyword As SyntaxToken
    Public ReadOnly Property Pound As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property LenKeyword As SyntaxToken
    Public ReadOnly Property Equal As SyntaxToken
    Public ReadOnly Property RecLen As ExpressionSyntax
  End Class

End Namespace
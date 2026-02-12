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
      Me.IsShorthandForm = False
    End Sub

    Public Sub New(tree As SyntaxTree, openKeyword As SyntaxToken, mode As ExpressionSyntax, comma1 As SyntaxToken, pound1 As SyntaxToken, fileNumber1 As ExpressionSyntax, comma2 As SyntaxToken, filename As ExpressionSyntax, Optional recLen As ExpressionSyntax = Nothing)
      MyBase.New(tree)
      ' For shorthand form OPEN "mode",#filenum,"filename",len:
      ' - first expression (mode) is the mode string like "r"
      ' - third expression (filename) is the actual file path
      ' We swap them in the constructor to match BoundOpenStatement expectations
      Me.OpenKeyword = openKeyword
      Me.File = filename
      Me.ForKeyword = Nothing
      Me.ModeKeyword = Nothing
      Me.AccessKeyword = Nothing
      Me.Access = ImmutableArray(Of SyntaxNode).Empty
      Me.Lock = ImmutableArray(Of SyntaxNode).Empty
      Me.AsKeyword = Nothing
      Me.Pound = Nothing
      Me.FileNumber = Nothing
      Me.LenKeyword = Nothing
      Me.Equal = Nothing
      Me.RecLen = recLen
      ' Shorthand-specific properties
      Me.IsShorthandForm = True
      Me.Mode = mode
      Me.Comma1 = comma1
      Me.Pound1 = pound1
      Me.FileNumber1 = fileNumber1
      Me.Comma2 = comma2
      Me.Filename = filename
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
    Public ReadOnly Property IsShorthandForm As Boolean

    ' Shorthand form properties
    Public ReadOnly Property Mode As ExpressionSyntax
    Public ReadOnly Property Comma1 As SyntaxToken
    Public ReadOnly Property Pound1 As SyntaxToken
    Public ReadOnly Property FileNumber1 As ExpressionSyntax
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property Filename As ExpressionSyntax
  End Class

End Namespace
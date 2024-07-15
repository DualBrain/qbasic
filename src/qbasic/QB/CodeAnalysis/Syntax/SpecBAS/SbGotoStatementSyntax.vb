Namespace Global.QB.CodeAnalysis.Syntax

  Public NotInheritable Class SbGotoStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, goKeyword As SyntaxToken, toKeyword As SyntaxToken, targetToken As SyntaxToken)
      MyBase.New(tree)
      Me.GoKeyword = goKeyword
      Me.ToKeyword = toKeyword
      Me.TargetToken = targetToken
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.SbGotoStatement
    Public ReadOnly Property GoKeyword() As SyntaxToken
    Public ReadOnly Property ToKeyword() As SyntaxToken
    Public ReadOnly Property TargetToken() As SyntaxToken

  End Class

  Public NotInheritable Class SbGosubStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, goKeyword As SyntaxToken, subKeyword As SyntaxToken, identifierToken As SyntaxToken)
      MyBase.New(tree)
      Me.GoKeyword = goKeyword
      Me.SubKeyword = subKeyword
      Me.IdentifierToken = identifierToken
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.SbGosubStatement
    Public ReadOnly Property GoKeyword() As SyntaxToken
    Public ReadOnly Property SubKeyword() As SyntaxToken
    Public ReadOnly Property IdentifierToken() As SyntaxToken

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class KeyStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, keyKeyword As SyntaxToken, key As ExpressionSyntax, commaToken As SyntaxToken, label As ExpressionSyntax)
      MyBase.New(tree)
      Me.KeyKeyword = keyKeyword
      Me.Key = key
      Me.CommaToken = commaToken
      Me.Label = label
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.KeyStatement
    Public ReadOnly Property KeyKeyword As SyntaxToken
    Public ReadOnly Property Key As ExpressionSyntax
    Public ReadOnly Property CommaToken As SyntaxToken
    Public ReadOnly Property Label As ExpressionSyntax

  End Class

  Friend Class KeyListStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, keyKeyword As SyntaxToken, listKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.KeyKeyword = keyKeyword
      Me.ListKeyword = listKeyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.KeyListStatement
    Public ReadOnly Property KeyKeyword As SyntaxToken
    Public ReadOnly Property ListKeyword As SyntaxToken

  End Class

  Friend Class KeyOffStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, keyKeyword As SyntaxToken, offKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.KeyKeyword = keyKeyword
      Me.OffKeyword = offKeyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.KeyOffStatement
    Public ReadOnly Property KeyKeyword As SyntaxToken
    Public ReadOnly Property OffKeyword As SyntaxToken

  End Class

  Friend Class KeyOnStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, keyKeyword As SyntaxToken, onKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.KeyKeyword = keyKeyword
      Me.OnKeyword = onKeyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.KeyOnStatement
    Public ReadOnly Property KeyKeyword As SyntaxToken
    Public ReadOnly Property OnKeyword As SyntaxToken

  End Class

End Namespace
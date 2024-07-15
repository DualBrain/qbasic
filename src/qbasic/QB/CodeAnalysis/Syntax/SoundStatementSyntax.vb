Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SoundStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, soundKeyword As SyntaxToken, frequency As ExpressionSyntax, comma As SyntaxToken, duration As ExpressionSyntax)
      MyBase.New(tree)
      Me.SoundKeyword = soundKeyword
      Me.Frequency = frequency
      Me.Comma = comma
      Me.Duration = duration
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SoundStatement
    Public ReadOnly Property SoundKeyword As SyntaxToken
    Public ReadOnly Property Frequency As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Duration As ExpressionSyntax

  End Class

End Namespace
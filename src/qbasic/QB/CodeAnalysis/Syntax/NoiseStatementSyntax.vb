Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class NoiseStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, noiseKeyword As SyntaxToken, source As ExpressionSyntax, comma As SyntaxToken, volume As ExpressionSyntax, comma2 As SyntaxToken, duration As ExpressionSyntax)
      MyBase.New(tree)
      Me.NoiseKeyword = noiseKeyword
      Me.Source = source
      Me.Comma = comma
      Me.Volume = volume
      Me.Comma2 = comma2
      Me.Duration = duration
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.NoiseStatement
    Public ReadOnly Property NoiseKeyword As SyntaxToken
    Public ReadOnly Property Source As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property Volume As ExpressionSyntax
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property Duration As ExpressionSyntax

  End Class

End Namespace
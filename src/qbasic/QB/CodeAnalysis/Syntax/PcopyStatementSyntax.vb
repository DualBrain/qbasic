Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class PcopyStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, pcopyKeyword As SyntaxToken, sourcePage As ExpressionSyntax, comma As SyntaxToken, destinationPage As ExpressionSyntax)
      MyBase.New(tree)
      Me.PcopyKeyword = pcopyKeyword
      Me.SourcePage = sourcePage
      Me.Comma = comma
      Me.DestinationPage = destinationPage
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.PcopyStatement
    Public ReadOnly Property PcopyKeyword As SyntaxToken
    Public ReadOnly Property SourcePage As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken
    Public ReadOnly Property DestinationPage As ExpressionSyntax

  End Class

End Namespace
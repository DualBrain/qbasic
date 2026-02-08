Namespace Global.QB.CodeAnalysis.Syntax

  ''' <summary>
  ''' Represents a NEXT statement that can end one or more FOR loops.
  ''' Supports both standalone NEXT and NEXT with multiple identifiers.
  ''' </summary>
  Public NotInheritable Class NextStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   nextKeyword As SyntaxToken,
                   identifiers As SeparatedSyntaxList(Of SyntaxToken))
      MyBase.New(tree)
      Me.NextKeyword = nextKeyword
      Me.Identifiers = identifiers
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.NextStatement
    Public ReadOnly Property NextKeyword As SyntaxToken
    Public ReadOnly Property Identifiers As SeparatedSyntaxList(Of SyntaxToken)

  End Class

End Namespace
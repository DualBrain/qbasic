Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class DefTypeStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, keyword As SyntaxToken, nodes As List(Of SyntaxNode))
      MyBase.New(tree)
      Me.Keyword = keyword
      Me.Nodes = nodes
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DefTypeStatement
    Public ReadOnly Property Keyword As SyntaxToken
    Public ReadOnly Property Nodes As List(Of SyntaxNode)

  End Class

  Friend Class DefVarRangeClause
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, lower As SyntaxToken, optionalDashToken As SyntaxToken, optionalUpper As SyntaxToken)
      MyBase.New(tree)
      Me.Lower = lower
      Me.OptionalDashToken = optionalDashToken
      Me.OptionalUpper = optionalUpper
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DefVarRangeClause
    Public ReadOnly Property Lower As SyntaxToken
    Public ReadOnly Property OptionalDashToken As SyntaxToken
    Public ReadOnly Property OptionalUpper As SyntaxToken

  End Class

End Namespace
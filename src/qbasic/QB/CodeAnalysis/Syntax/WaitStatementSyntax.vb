Namespace Global.QB.CodeAnalysis.Syntax
  Friend Class WaitStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, waitKeyword As SyntaxToken, portNumber As ExpressionSyntax, comma1 As SyntaxToken, andExpression As ExpressionSyntax, comma2 As SyntaxToken, xorExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.WaitKeyword = waitKeyword
      Me.PortNumber = portNumber
      Me.Comma1 = comma1
      Me.AndExpression = andExpression
      Me.Comma2 = comma2
      Me.XorExpression = xorExpression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WaitStatement
    Public ReadOnly Property WaitKeyword As SyntaxToken
    Public ReadOnly Property PortNumber As ExpressionSyntax
    Public ReadOnly Property Comma1 As SyntaxToken
    Public ReadOnly Property AndExpression As ExpressionSyntax
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property XorExpression As ExpressionSyntax

  End Class

End Namespace
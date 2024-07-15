Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class ViewPrintStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, viewKeyword As SyntaxToken, printKeyword As SyntaxToken, topRow As ExpressionSyntax, toKeyword As SyntaxToken, bottomRow As ExpressionSyntax)
      MyBase.New(tree)
      Me.ViewKeyword = viewKeyword
      Me.PrintKeyword = printKeyword
      Me.TopRow = topRow
      Me.ToKeyword = toKeyword
      Me.BottomRow = bottomRow
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ViewPrintStatement
    Public ReadOnly Property ViewKeyword As SyntaxToken
    Public ReadOnly Property PrintKeyword As SyntaxToken
    Public ReadOnly Property TopRow As ExpressionSyntax
    Public ReadOnly Property ToKeyword As SyntaxToken
    Public ReadOnly Property BottomRow As ExpressionSyntax

  End Class

End Namespace
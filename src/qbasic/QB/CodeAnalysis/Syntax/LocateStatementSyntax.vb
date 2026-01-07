Namespace Global.QB.CodeAnalysis.Syntax

  Public NotInheritable Class LocateStatementSyntax
    Inherits StatementSyntax


    Public Sub New(tree As SyntaxTree, locateKeyword As SyntaxToken, row As ExpressionSyntax, commaToken1 As SyntaxToken, col As ExpressionSyntax, commaToken2 As SyntaxToken, visible As ExpressionSyntax, commaToken3 As SyntaxToken, scanStart As ExpressionSyntax, commaToken4 As SyntaxToken, scanStop As ExpressionSyntax)
      MyBase.New(tree)
      Me.LocateKeyword = locateKeyword
      Me.Row = row
      Me.CommaToken1 = commaToken1
      Me.Col = col
      Me.CommaToken2 = commaToken2
      Me.Visible = visible
      Me.CommaToken3 = commaToken3
      Me.ScanStart = scanStart
      Me.CommaToken4 = commaToken4
      Me.ScanStop = scanStop
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LocateStatement
    Public ReadOnly Property LocateKeyword As SyntaxToken
    Public ReadOnly Property Row As ExpressionSyntax
    Public ReadOnly Property CommaToken1 As SyntaxToken
    Public ReadOnly Property Col As ExpressionSyntax
    Public ReadOnly Property CommaToken2 As SyntaxToken
    Public ReadOnly Property Visible As ExpressionSyntax
    Public ReadOnly Property CommaToken3 As SyntaxToken
    Public ReadOnly Property ScanStart As ExpressionSyntax
    Public ReadOnly Property CommaToken4 As SyntaxToken
    Public ReadOnly Property ScanStop As ExpressionSyntax

  End Class

End Namespace
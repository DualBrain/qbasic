Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class WidthStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, widthKeyword As SyntaxToken, optionalColumns As ExpressionSyntax, optionalCommaToken As SyntaxToken, optionalLines As ExpressionSyntax)
      MyBase.New(tree)
      Me.WidthKeyword = widthKeyword
      Me.OptionalColumns = optionalColumns
      Me.OptionalCommaToken = optionalCommaToken
      Me.OptionalLines = optionalLines
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WidthStatement
    Public ReadOnly Property WidthKeyword As SyntaxToken
    Public ReadOnly Property OptionalColumns As ExpressionSyntax
    Public ReadOnly Property OptionalCommaToken As SyntaxToken
    Public ReadOnly Property OptionalLines As ExpressionSyntax

  End Class

  Friend Class WidthFileStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, widthKeyword As SyntaxToken, poundKeyword As SyntaxToken, fileNumber As ExpressionSyntax, commaToken As SyntaxToken, columns As ExpressionSyntax)
      MyBase.New(tree)
      Me.WidthKeyword = widthKeyword
      Me.PoundKeyword = poundKeyword
      Me.FileNumber = fileNumber
      Me.CommaToken = commaToken
      Me.Columns = columns
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WidthFileStatement
    Public ReadOnly Property WidthKeyword As SyntaxToken
    Public ReadOnly Property PoundKeyword As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property CommaToken As SyntaxToken
    Public ReadOnly Property Columns As ExpressionSyntax

  End Class

  Friend Class WidthLprintStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, widthKeyword As SyntaxToken, lprintKeyword As SyntaxToken, columns As ExpressionSyntax)
      MyBase.New(tree)
      Me.WidthKeyword = widthKeyword
      Me.LprintKeyword = lprintKeyword
      Me.Columns = columns
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WidthLprintStatement
    Public ReadOnly Property WidthKeyword As SyntaxToken
    Public ReadOnly Property LprintKeyword As SyntaxToken
    Public ReadOnly Property Columns As ExpressionSyntax

  End Class

End Namespace
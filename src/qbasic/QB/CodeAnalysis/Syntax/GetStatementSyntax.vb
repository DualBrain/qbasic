Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class GetStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, getKeyword As SyntaxToken, optionalStepKeyword1 As SyntaxToken, openParen1 As SyntaxToken, x1 As ExpressionSyntax, comma1 As SyntaxToken, y1 As ExpressionSyntax, closeParen1 As SyntaxToken, dashToken As SyntaxToken, optionalStepKeyword2 As SyntaxToken, openParen2 As SyntaxToken, x2 As ExpressionSyntax, comma2 As SyntaxToken, y2 As ExpressionSyntax, closeParen2 As SyntaxToken, comma3 As SyntaxToken, buffer As IdentifierSyntax)
      MyBase.New(tree)
      Me.GetKeyword = getKeyword
      Me.OptionalS = OptionalS
      Me.OpenParen1 = openParen1
      Me.X1 = x1
      Me.Comma1 = comma1
      Me.Y1 = y1
      Me.CloseParen1 = closeParen1
      Me.DashToken = dashToken
      Me.OptionalStepKeyword2 = optionalStepKeyword2
      Me.OpenParen2 = openParen2
      Me.X2 = x2
      Me.Comma2 = comma2
      Me.Y2 = y2
      Me.CloseParen2 = closeParen2
      Me.Comma3 = comma3
      Me.Buffer = buffer
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.GetStatement
    Public ReadOnly Property GetKeyword As SyntaxToken
    Public ReadOnly Property OptionalS As Object
    Public ReadOnly Property OpenParen1 As SyntaxToken
    Public ReadOnly Property X1 As ExpressionSyntax
    Public ReadOnly Property Comma1 As SyntaxToken
    Public ReadOnly Property Y1 As ExpressionSyntax
    Public ReadOnly Property CloseParen1 As SyntaxToken
    Public ReadOnly Property DashToken As SyntaxToken
    Public ReadOnly Property OptionalStepKeyword2 As SyntaxToken
    Public ReadOnly Property OpenParen2 As SyntaxToken
    Public ReadOnly Property X2 As ExpressionSyntax
    Public ReadOnly Property Comma2 As SyntaxToken
    Public ReadOnly Property Y2 As ExpressionSyntax
    Public ReadOnly Property CloseParen2 As SyntaxToken
    Public ReadOnly Property Comma3 As SyntaxToken
    Public ReadOnly Property Buffer As IdentifierSyntax

  End Class

  Friend Class GetFileStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, getKeyword As SyntaxToken, optionalPoundToken As SyntaxToken, fileNumber As ExpressionSyntax, optionalComma1 As SyntaxToken, optionalRecord As ExpressionSyntax, optionalComma2 As SyntaxToken, optionalVariable As SyntaxToken)
      MyBase.New(tree)
      Me.GetKeyword = getKeyword
      Me.OptionalPoundToken = optionalPoundToken
      Me.FileNumber = fileNumber
      Me.OptionalComma1 = optionalComma1
      Me.OptionalRecord = optionalRecord
      Me.OptionalComma2 = optionalComma2
      Me.OptionalVariable = optionalVariable
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.GetFileStatement
    Public ReadOnly Property GetKeyword As SyntaxToken
    Public ReadOnly Property OptionalPoundToken As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property OptionalComma1 As SyntaxToken
    Public ReadOnly Property OptionalRecord As ExpressionSyntax
    Public ReadOnly Property OptionalComma2 As SyntaxToken
    Public ReadOnly Property OptionalVariable As SyntaxToken

  End Class

End Namespace
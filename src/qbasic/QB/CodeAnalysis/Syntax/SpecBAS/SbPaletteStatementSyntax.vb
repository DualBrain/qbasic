Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class SbPaletteStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   paletteKeyword As SyntaxToken,
                   indexExpression As ExpressionSyntax,
                   commaToken1 As SyntaxToken,
                   rExpression As ExpressionSyntax,
                   optionalCommaToken2 As SyntaxToken,
                   optionalGExpression As ExpressionSyntax,
                   optionalCommaToken3 As SyntaxToken,
                   optionalBExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.PaletteKeyword = paletteKeyword
      Me.IndexExpression = indexExpression
      Me.CommaToken1 = commaToken1
      Me.RExpression = rExpression
      Me.OptionalCommaToken2 = optionalCommaToken2
      Me.OptionalGExpression = optionalGExpression
      Me.OptionalCommaToken3 = optionalCommaToken3
      Me.OptionalBExpression = optionalBExpression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbPaletteStatement
    Public ReadOnly Property PaletteKeyword As SyntaxToken
    Public ReadOnly Property IndexExpression As ExpressionSyntax
    Public ReadOnly Property CommaToken1 As SyntaxToken
    Public ReadOnly Property RExpression As ExpressionSyntax
    Public ReadOnly Property OptionalCommaToken2 As SyntaxToken
    Public ReadOnly Property OptionalGExpression As ExpressionSyntax
    Public ReadOnly Property OptionalCommaToken3 As SyntaxToken
    Public ReadOnly Property OptionalBExpression As ExpressionSyntax

  End Class

  Friend Class PaletteDefaultStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   paletteKeyword As SyntaxToken,
                   defaultKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.PaletteKeyword = paletteKeyword
      Me.DefaultKeyword = defaultKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbPaletteDefaultStatement
    Public ReadOnly Property PaletteKeyword As SyntaxToken
    Public ReadOnly Property DefaultKeyword As SyntaxToken

  End Class

  Friend Class PaletteHsvStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   paletteKeyword As SyntaxToken,
                   hsvKeyword As SyntaxToken,
                   indexExpression As ExpressionSyntax,
                   commaToken1 As SyntaxToken,
                   hExpression As ExpressionSyntax,
                   optionalCommaToken2 As SyntaxToken,
                   optionalSExpression As ExpressionSyntax,
                   optionalCommaToken3 As SyntaxToken,
                   optionalVExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.PaletteKeyword = paletteKeyword
      Me.HsvKeyword = hsvKeyword
      Me.IndexExpression = indexExpression
      Me.CommaToken1 = commaToken1
      Me.HExpression = hExpression
      Me.OptionalCommaToken2 = optionalCommaToken2
      Me.OptionalSExpression = optionalSExpression
      Me.OptionalCommaToken3 = optionalCommaToken3
      Me.OptionalVExpression = optionalVExpression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbPaletteHsvStatement
    Public ReadOnly Property PaletteKeyword As SyntaxToken
    Public ReadOnly Property HsvKeyword As SyntaxToken
    Public ReadOnly Property IndexExpression As ExpressionSyntax
    Public ReadOnly Property CommaToken1 As SyntaxToken
    Public ReadOnly Property HExpression As ExpressionSyntax
    Public ReadOnly Property OptionalCommaToken2 As SyntaxToken
    Public ReadOnly Property OptionalSExpression As ExpressionSyntax
    Public ReadOnly Property OptionalCommaToken3 As SyntaxToken
    Public ReadOnly Property OptionalVExpression As ExpressionSyntax

  End Class

  Friend Class PaletteShiftStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   paletteKeyword As SyntaxToken,
                   shiftKeyword As SyntaxToken,
                   amountExpression As ExpressionSyntax,
                   optionalCommaToken As SyntaxToken,
                   optionalStartExpression As ExpressionSyntax,
                   optionalToKeyword As SyntaxToken,
                   optionalEndExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.PaletteKeyword = paletteKeyword
      Me.ShiftKeyword = shiftKeyword
      Me.AmountExpression = amountExpression
      Me.OptionalCommaToken = optionalCommaToken
      Me.OptionalStartExpression = optionalStartExpression
      Me.OptionalToKeyword = optionalToKeyword
      Me.OptionalEndExpression = optionalEndExpression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SbPaletteHsvStatement
    Public ReadOnly Property PaletteKeyword As SyntaxToken
    Public ReadOnly Property ShiftKeyword As SyntaxToken
    Public ReadOnly Property AmountExpression As ExpressionSyntax
    Public ReadOnly Property OptionalCommaToken As SyntaxToken
    Public ReadOnly Property OptionalStartExpression As ExpressionSyntax
    Public ReadOnly Property OptionalToKeyword As SyntaxToken
    Public ReadOnly Property OptionalEndExpression As ExpressionSyntax

  End Class

End Namespace
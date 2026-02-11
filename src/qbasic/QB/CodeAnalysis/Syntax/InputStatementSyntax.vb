Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class InputStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, inputKeyword As SyntaxToken, optionalSemiColonToken As SyntaxToken, optionalPromptExpression As ExpressionSyntax, semiColonOrCommaToken As SyntaxToken, tokens As ImmutableArray(Of SyntaxToken), isFileInput As Boolean)
      MyBase.New(tree)
      Me.InputKeyword = inputKeyword
      Me.OptionalSemiColonToken = optionalSemiColonToken
      Me.OptionalPromptExpression = optionalPromptExpression
      Me.SemiColonOrCommaToken = semiColonOrCommaToken
      Me.Tokens = tokens
      Me.IsFileInput = isFileInput
      Me.Pound = Nothing
      Me.FileNumber = Nothing
      Me.Comma = Nothing
    End Sub

    Public Sub New(tree As SyntaxTree, inputKeyword As SyntaxToken, pound As SyntaxToken, fileNumber As ExpressionSyntax, comma As SyntaxToken, tokens As ImmutableArray(Of SyntaxToken))
      MyBase.New(tree)
      Me.InputKeyword = inputKeyword
      Me.Pound = pound
      Me.FileNumber = fileNumber
      Me.Comma = comma
      Me.Tokens = tokens
      Me.IsFileInput = True
      Me.OptionalSemiColonToken = Nothing
      Me.OptionalPromptExpression = Nothing
      Me.SemiColonOrCommaToken = Nothing
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.InputStatement
    Public ReadOnly Property InputKeyword As SyntaxToken

    ' Keyboard input properties
    Public ReadOnly Property OptionalSemiColonToken As SyntaxToken
    Public ReadOnly Property OptionalPromptExpression As ExpressionSyntax
    Public ReadOnly Property SemiColonOrCommaToken As SyntaxToken

    ' File input properties (must come before Tokens for proper child ordering)
    Public ReadOnly Property Pound As SyntaxToken
    Public ReadOnly Property FileNumber As ExpressionSyntax
    Public ReadOnly Property Comma As SyntaxToken

    Public ReadOnly Property Tokens As ImmutableArray(Of SyntaxToken)
    Public ReadOnly Property IsFileInput As Boolean
  End Class

End Namespace
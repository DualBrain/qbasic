Imports System.Collections.Immutable

Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax
Imports QB.CodeAnalysis.Text

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class LexerTests

    <Fact>
    Public Sub LexesSingleKeyword()
      Dim text = "PRINT"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.PrintKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesLetKeyword()
      Dim text = "LET"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.LetKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesIfKeyword()
      Dim text = "IF"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.IfKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesThenKeyword()
      Dim text = "THEN"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.ThenKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesElseKeyword()
      Dim text = "ELSE"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.ElseKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesForKeyword()
      Dim text = "FOR"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.ForKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesToKeyword()
      Dim text = "TO"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.ToKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesNextKeyword()
      Dim text = "NEXT"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.NextKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesWhileKeyword()
      Dim text = "WHILE"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.WhileKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesWendKeyword()
      Dim text = "WEND"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.WendKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesSubKeyword()
      Dim text = "SUB"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.SubKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesFunctionKeyword()
      Dim text = "FUNCTION"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.FunctionKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesEndKeyword()
      Dim text = "END"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.EndKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesDimKeyword()
      Dim text = "DIM"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.DimKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesAsKeyword()
      Dim text = "AS"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.AsKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesNotKeyword()
      Dim text = "NOT"
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.NotKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesNumber()
      Dim text = "123"
      Dim tokens = LexAll(text)

      Assert.Single(tokens)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(0).Kind)
    End Sub

    <Fact>
    Public Sub LexesFloatNumber()
      Dim text = "1.23"
      Dim tokens = LexAll(text)

      Assert.Equal(2, tokens.Count)
      Assert.Equal(SyntaxKind.NumberToken, tokens(0).Kind)
      Assert.Equal(1, tokens(0).LeadingTrivia.Length)
      Assert.Equal(SyntaxKind.LineNumberTrivia, tokens(0).LeadingTrivia(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesScientificNumber()
      Dim text = "1.23E10"
      Dim tokens = LexAll(text)

      Assert.Equal(3, tokens.Count)
      Assert.Equal(SyntaxKind.NumberToken, tokens(0).Kind)
      Assert.Equal(1, tokens(0).LeadingTrivia.Length)
      Assert.Equal(SyntaxKind.LineNumberTrivia, tokens(0).LeadingTrivia(0).Kind)
      Assert.Equal(SyntaxKind.IdentifierToken, tokens(1).Kind)
      Assert.Equal("E10", tokens(1).Text)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(2).Kind)
    End Sub

    <Fact>
    Public Sub LexesBadCharacter()
      Dim text = "@"
      Dim result = LexAllWithDiagnostics(text)

      Assert.Equal(2, result.Tokens.Count)
      Assert.Equal(SyntaxKind.BadToken, result.Tokens(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result.Tokens(1).Kind)
      ' Should have diagnostic
      Assert.Equal(1, result.Diagnostics.Length)
    End Sub

    <Fact>
    Public Sub LexesString()
      Dim text = """Hello World"""
      Dim tokens = LexAll(text)

      Assert.Equal(2, tokens.Count)
      Assert.Equal(SyntaxKind.StringToken, tokens(0).Kind)
      Assert.Equal("Hello World", tokens(0).Value)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesIdentifier()
      Dim text = "variable"
      Dim tokens = LexAll(text)

      Assert.Equal(2, tokens.Count)
      Assert.Equal(SyntaxKind.IdentifierToken, tokens(0).Kind)
      Assert.Equal("variable", tokens(0).Text)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesHatToken()
      Dim text = "^"
      Dim tokens = LexAll(text)

      Assert.Equal(2, tokens.Count)
      Assert.Equal(SyntaxKind.HatToken, tokens(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesBackslashToken()
      Dim text = "\"
      Dim tokens = LexAll(text)

      Assert.Equal(2, tokens.Count)
      Assert.Equal(SyntaxKind.BackslashToken, tokens(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesOpenBraceToken()
      Dim text = "{"
      Dim tokens = LexAll(text)

      Assert.Equal(2, tokens.Count)
      Assert.Equal(SyntaxKind.OpenBraceToken, tokens(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesCloseBraceToken()
      Dim text = "}"
      Dim tokens = LexAll(text)

      Assert.Equal(2, tokens.Count)
      Assert.Equal(SyntaxKind.CloseBraceToken, tokens(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesWhitespaceTrivia()
      Dim text = " "
      Dim tokens = LexAll(text)

      Assert.Single(tokens)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(0).Kind)
      Assert.Equal(1, tokens(0).LeadingTrivia.Length)
      Assert.Equal(SyntaxKind.WhiteSpaceTrivia, tokens(0).LeadingTrivia(0).Kind)
    End Sub

    <Fact>
    Public Sub LexesCommentTrivia()
      Dim text = "' comment"
      Dim tokens = LexAll(text)

      Assert.Single(tokens)
      Assert.Equal(SyntaxKind.EndOfFileToken, tokens(0).Kind)
      Assert.Equal(1, tokens(0).LeadingTrivia.Length)
      Assert.Equal(SyntaxKind.SingleLineCommentTrivia, tokens(0).LeadingTrivia(0).Kind)
    End Sub

    Private Function LexAll(text As String) As List(Of SyntaxToken)
      Dim tokens = SyntaxTree.ParseTokens(text, includeEndOfFile:=True)
      Return tokens.ToList()
    End Function

    Private Function LexAllWithDiagnostics(text As String) As (Tokens As List(Of SyntaxToken), Diagnostics As ImmutableArray(Of Diagnostic))
      Dim diagnostics As ImmutableArray(Of Diagnostic) = Nothing
      Dim tokens = SyntaxTree.ParseTokens(text, diagnostics, includeEndOfFile:=True)
      Return (tokens.ToList(), diagnostics)
    End Function

    <Theory>
    <InlineData("AND", SyntaxKind.AndKeyword)>
    <InlineData("AS", SyntaxKind.AsKeyword)>
    <InlineData("BEEP", SyntaxKind.BeepKeyword)>
    <InlineData("CASE", SyntaxKind.CaseKeyword)>
    <InlineData("CLS", SyntaxKind.ClsKeyword)>
    <InlineData("CONST", SyntaxKind.ConstKeyword)>
    <InlineData("DATA", SyntaxKind.DataKeyword)>
    <InlineData("DIM", SyntaxKind.DimKeyword)>
    <InlineData("DO", SyntaxKind.DoKeyword)>
    <InlineData("ELSE", SyntaxKind.ElseKeyword)>
    <InlineData("ELSEIF", SyntaxKind.ElseIfKeyword)>
    <InlineData("END", SyntaxKind.EndKeyword)>
    <InlineData("FOR", SyntaxKind.ForKeyword)>
    <InlineData("FUNCTION", SyntaxKind.FunctionKeyword)>
    <InlineData("GOSUB", SyntaxKind.GosubKeyword)>
    <InlineData("GOTO", SyntaxKind.GotoKeyword)>
    <InlineData("IF", SyntaxKind.IfKeyword)>
    <InlineData("INPUT", SyntaxKind.InputKeyword)>
    <InlineData("LET", SyntaxKind.LetKeyword)>
    <InlineData("LOOP", SyntaxKind.LoopKeyword)>
    <InlineData("MOD", SyntaxKind.ModKeyword)>
    <InlineData("NEXT", SyntaxKind.NextKeyword)>
    <InlineData("NOT", SyntaxKind.NotKeyword)>
    <InlineData("OR", SyntaxKind.OrKeyword)>
    <InlineData("PRINT", SyntaxKind.PrintKeyword)>
    <InlineData("READ", SyntaxKind.ReadKeyword)>
    <InlineData("REM", SyntaxKind.RemKeyword)>
    <InlineData("RETURN", SyntaxKind.ReturnKeyword)>
    <InlineData("SELECT", SyntaxKind.SelectKeyword)>
    <InlineData("STEP", SyntaxKind.StepKeyword)>
    <InlineData("SUB", SyntaxKind.SubKeyword)>
    <InlineData("THEN", SyntaxKind.ThenKeyword)>
    <InlineData("TO", SyntaxKind.ToKeyword)>
    <InlineData("WHILE", SyntaxKind.WhileKeyword)>
    <InlineData("WEND", SyntaxKind.WendKeyword)>
    <InlineData("XOR", SyntaxKind.XorKeyword)>
    Public Sub LexesKeyword(keywordText As String, expectedKind As SyntaxKind)
      Dim result = LexAll(keywordText)

      Assert.Equal(2, result.Count)
      Assert.Equal(expectedKind, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Theory>
    <InlineData("+", SyntaxKind.PlusToken)>
    <InlineData("-", SyntaxKind.MinusToken)>
    <InlineData("*", SyntaxKind.StarToken)>
    <InlineData("/", SyntaxKind.SlashToken)>
    <InlineData("\", SyntaxKind.BackslashToken)>
    <InlineData("^", SyntaxKind.HatToken)>
    <InlineData("(", SyntaxKind.OpenParenToken)>
    <InlineData(")", SyntaxKind.CloseParenToken)>
    <InlineData("{", SyntaxKind.OpenBraceToken)>
    <InlineData("}", SyntaxKind.CloseBraceToken)>
    <InlineData("=", SyntaxKind.EqualToken)>
    <InlineData(">", SyntaxKind.GreaterThanToken)>
    <InlineData("<", SyntaxKind.LessThanToken)>
    <InlineData(">=", SyntaxKind.GreaterThanEqualToken)>
    <InlineData("<=", SyntaxKind.LessThanEqualToken)>
    <InlineData("<>", SyntaxKind.LessThanGreaterThanToken)>
    <InlineData(",", SyntaxKind.CommaToken)>
    <InlineData(":", SyntaxKind.ColonToken)>
    <InlineData(";", SyntaxKind.SemicolonToken)>
    <InlineData("?", SyntaxKind.QuestionToken)>
    <InlineData("#", SyntaxKind.PoundToken)>
    <InlineData(".", SyntaxKind.PeriodToken)>
    Public Sub LexesOperator(opText As String, expectedKind As SyntaxKind)
      Dim result = LexAll(opText)

      Assert.Equal(2, result.Count)
      Assert.Equal(expectedKind, result(0).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesCaseInsensitiveKeywords()
      Dim text = "print Print PRINT"
      Dim result = LexAll(text)

      Assert.Equal(4, result.Count)
      Assert.Equal(SyntaxKind.PrintKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.PrintKeyword, result(1).Kind)
      Assert.Equal(SyntaxKind.PrintKeyword, result(2).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(3).Kind)
    End Sub

    <Fact>
    Public Sub LexesIdentifiersWithNumbers()
      Dim text = "var1 temp2"
      Dim result = LexAll(text)

      Assert.Equal(3, result.Count)
      Assert.Equal(SyntaxKind.IdentifierToken, result(0).Kind)
      Assert.Equal("var1", result(0).Text)
      Assert.Equal(SyntaxKind.IdentifierToken, result(1).Kind)
      Assert.Equal("temp2", result(1).Text)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(2).Kind)
    End Sub

    <Fact>
    Public Sub LexesIdentifiersWithUnderscore()
      Dim text = "my_var test_var"
      Dim result = LexAll(text)

      Assert.Equal(3, result.Count)
      Assert.Equal(SyntaxKind.IdentifierToken, result(0).Kind)
      Assert.Equal("my_var", result(0).Text)
      Assert.Equal(SyntaxKind.IdentifierToken, result(1).Kind)
      Assert.Equal("test_var", result(1).Text)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(2).Kind)
    End Sub

    <Fact>
    Public Sub LexesStringWithEscapeSequences()
      Dim text = """Hello\nWorld"""
      Dim result = LexAll(text)

      Assert.Equal(2, result.Count)
      Assert.Equal(SyntaxKind.StringToken, result(0).Kind)
      Assert.Equal("Hello\nWorld", result(0).Value)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(1).Kind)
    End Sub

    <Fact>
    Public Sub LexesUnterminatedString()
      Dim text = """Hello World"
      Dim result = LexAllWithDiagnostics(text)

      Assert.Equal(2, result.Tokens.Count)
      Assert.Equal(SyntaxKind.StringToken, result.Tokens(0).Kind)
      Assert.Equal("Hello World", result.Tokens(0).Value)
      Assert.Equal(SyntaxKind.EndOfFileToken, result.Tokens(1).Kind)
      ' Should have diagnostic for unterminated string
      Assert.Equal(1, result.Diagnostics.Length)
    End Sub

    <Fact>
    Public Sub LexesMultipleNumbers()
      Dim text = "123 456.78 9E10"
      Dim result = LexAll(text)

      Assert.Equal(4, result.Count)
      Assert.Equal(SyntaxKind.NumberToken, result(0).Kind)
      Assert.Equal(123, result(0).Value)
      Assert.Equal(SyntaxKind.NumberToken, result(1).Kind)
      Assert.Equal(456.78, result(1).Value)
      Assert.Equal(SyntaxKind.NumberToken, result(2).Kind)
      Assert.Equal(90000000000.0, result(2).Value)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(3).Kind)
    End Sub

    <Fact>
    Public Sub LexesComplexExpression()
      Dim text = "x + y * (z - 1)"
      Dim result = LexAll(text)

      Assert.Equal(10, result.Count)
      Assert.Equal(SyntaxKind.IdentifierToken, result(0).Kind)
      Assert.Equal(SyntaxKind.PlusToken, result(1).Kind)
      Assert.Equal(SyntaxKind.IdentifierToken, result(2).Kind)
      Assert.Equal(SyntaxKind.StarToken, result(3).Kind)
      Assert.Equal(SyntaxKind.OpenParenToken, result(4).Kind)
      Assert.Equal(SyntaxKind.IdentifierToken, result(5).Kind)
      Assert.Equal(SyntaxKind.MinusToken, result(6).Kind)
      Assert.Equal(SyntaxKind.NumberToken, result(7).Kind)
      Assert.Equal(SyntaxKind.CloseParenToken, result(8).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(9).Kind)
    End Sub

    <Fact>
    Public Sub LexesLineContinuation()
      Dim text = "
PRINT _
""Hello""
"
      Dim result = LexAll(text)

      Assert.Equal(3, result.Count)
      Assert.Equal(SyntaxKind.PrintKeyword, result(0).Kind)
      Assert.Equal(SyntaxKind.StringToken, result(1).Kind)
      Assert.Equal(SyntaxKind.EndOfFileToken, result(2).Kind)
    End Sub

  End Class

End Namespace
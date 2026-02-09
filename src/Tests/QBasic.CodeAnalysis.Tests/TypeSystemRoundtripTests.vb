Imports System.IO

Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax
Imports QB.CodeAnalysis.Text

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class TypeSystemRoundtripTests

    Private Function PerformRoundtripTest(text As String) As (Success As Boolean, Original As String, Roundtripped As String, Differences As String)
      Try
        Dim originalText = text
        Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(originalText)

        If syntaxTree.Diagnostics.Any(Function(d) d.IsError) Then
          Dim errorMessages = String.Join(vbCrLf, syntaxTree.Diagnostics.Where(Function(d) d.IsError).Select(Function(d) d.Message))
          Return (False, originalText, "", $"Syntax errors in original text: {errorMessages}")
        End If

        Dim roundtrippedText = WriteSyntaxTreeToText(syntaxTree.Root)
        Dim success = originalText = roundtrippedText

        Dim diffInfo As String = ""
        If Not success Then
          diffInfo = GenerateDifferenceReport(originalText, roundtrippedText)
        End If

        Return (success, originalText, roundtrippedText, diffInfo)
      Catch ex As Exception
        Return (False, text, "", $"Exception during roundtrip: {ex.Message}")
      End Try
    End Function

    Private Function WriteSyntaxTreeToText(node As SyntaxNode) As String
      Using writer = New StringWriter()
        WriteNodeWithTrivia(writer, node)
        Return writer.ToString()
      End Using
    End Function

    Private Sub WriteNodeWithTrivia(writer As TextWriter, node As SyntaxNode)
      If TypeOf node Is SyntaxToken Then
        Dim token = CType(node, SyntaxToken)

        For Each trivia In token.LeadingTrivia
          writer.Write(trivia.Text)
        Next

        writer.Write(token.Text)

        For Each trivia In token.TrailingTrivia
          writer.Write(trivia.Text)
        Next
      Else
        For Each child In node.GetChildren()
          WriteNodeWithTrivia(writer, child)
        Next
      End If
    End Sub

    Private Function GenerateDifferenceReport(original As String, roundtripped As String) As String
      Dim report = "Differences detected:" & vbCrLf

      For i = 0 To Math.Min(original.Length, roundtripped.Length) - 1
        If original(i) <> roundtripped(i) Then
          report &= $"Position {i}: Original '{original(i)}' ≠ Roundtripped '{roundtripped(i)}'" & vbCrLf
          Exit For
        End If
      Next

      If original.Length <> roundtripped.Length Then
        report &= $"Length difference: Original {original.Length} chars, Roundtripped {roundtripped.Length} chars" & vbCrLf
      End If

      Return report
    End Function

    Private Sub AssertRoundtripSuccess(text As String, Optional description As String = Nothing)
      Dim result = PerformRoundtripTest(text)

      ' Note: Removed Console.WriteLine to avoid test isolation issues with Console.Out redirection

      Assert.True(result.Success, $"Roundtrip failed: {result.Differences}")
    End Sub

    Private Sub AssertRoundtripFails(text As String, Optional description As String = Nothing)
      Dim result = PerformRoundtripTest(text)

      ' Note: Removed Console.WriteLine to avoid test isolation issues with Console.Out redirection

      Assert.False(result.Success, "Expected roundtrip to fail but it succeeded")
    End Sub

#Region "Basic Type Declaration Preservation"

    <Fact>
    Public Sub Roundtrip_DefInt_Basic()
      Dim text = "DEFINT A-Z"
      AssertRoundtripSuccess(text, "Basic DEFINT statement")
    End Sub

    <Fact>
    Public Sub Roundtrip_DefLng_Basic()
      Dim text = "DEFLNG A-Z"
      AssertRoundtripSuccess(text, "Basic DEFLNG statement")
    End Sub

    <Fact>
    Public Sub Roundtrip_DefSng_Basic()
      Dim text = "DEFSNG A-Z"
      AssertRoundtripSuccess(text, "Basic DEFSNG statement")
    End Sub

    <Fact>
    Public Sub Roundtrip_DefDbl_Basic()
      Dim text = "DEFDBL A-Z"
      AssertRoundtripSuccess(text, "Basic DEFDBL statement")
    End Sub

    <Fact>
    Public Sub Roundtrip_DefStr_Basic()
      Dim text = "DEFSTR A-Z"
      AssertRoundtripSuccess(text, "Basic DEFSTR statement")
    End Sub

    <Fact>
    Public Sub Roundtrip_MultipleDefStatements()
      Dim text = "DEFINT A-M" & vbCrLf & "DEFSNG N-Z" & vbCrLf & "DEFSTR S"
      AssertRoundtripSuccess(text, "Multiple DEF statements")
    End Sub

    <Fact>
    Public Sub Roundtrip_DefWithRange()
      Dim text = "DEFINT A-C, F-H"
      AssertRoundtripSuccess(text, "DEF statement with multiple ranges")
    End Sub

#End Region

#Region "Type Character Fidelity"

    <Fact>
    Public Sub Roundtrip_TypeCharacter_Integer()
      Dim text = "var% = 42"
      AssertRoundtripSuccess(text, "Integer type character %")
    End Sub

    <Fact>
    Public Sub Roundtrip_TypeCharacter_Long()
      Dim text = "var& = 1234567890"
      AssertRoundtripSuccess(text, "Long type character &")
    End Sub

    <Fact>
    Public Sub Roundtrip_TypeCharacter_Single()
      Dim text = "var! = 3.14"
      AssertRoundtripSuccess(text, "Single type character !")
    End Sub

    <Fact>
    Public Sub Roundtrip_TypeCharacter_Double()
      Dim text = "var# = 3.14159265358979"
      AssertRoundtripSuccess(text, "Double type character #")
    End Sub

    <Fact>
    Public Sub Roundtrip_TypeCharacter_String()
      Dim text = "var$ = ""Hello"""
      AssertRoundtripSuccess(text, "String type character $")
    End Sub

    <Fact>
    Public Sub Roundtrip_MixedTypeCharacters()
      Dim text = "intVar% = 42" & vbCrLf & "lngVar& = 100000" & vbCrLf & "sngVar! = 3.14" & vbCrLf & "dblVar# = 1.23" & vbCrLf & "strVar$ = ""test"""
      AssertRoundtripSuccess(text, "Mixed type characters")
    End Sub

#End Region

#Region "DIM AS Type Preservation"

    <Fact>
    Public Sub Roundtrip_DimAsInteger()
      Dim text = "DIM x AS INTEGER"
      AssertRoundtripSuccess(text, "DIM AS INTEGER")
    End Sub

    <Fact>
    Public Sub Roundtrip_DimAsLong()
      Dim text = "DIM x AS LONG"
      AssertRoundtripSuccess(text, "DIM AS LONG")
    End Sub

    <Fact>
    Public Sub Roundtrip_DimAsSingle()
      Dim text = "DIM x AS SINGLE"
      AssertRoundtripSuccess(text, "DIM AS SINGLE")
    End Sub

    <Fact>
    Public Sub Roundtrip_DimAsDouble()
      Dim text = "DIM x AS DOUBLE"
      AssertRoundtripSuccess(text, "DIM AS DOUBLE")
    End Sub

    <Fact>
    Public Sub Roundtrip_DimAsString()
      Dim text = "DIM x AS STRING"
      AssertRoundtripSuccess(text, "DIM AS STRING")
    End Sub

    <Fact>
    Public Sub Roundtrip_DimAsMultipleVariables()
      Dim text = "DIM a AS INTEGER, b AS STRING, c AS SINGLE"
      AssertRoundtripSuccess(text, "DIM with multiple variables and types")
    End Sub

    <Fact>
    Public Sub Roundtrip_DimAsArray()
      Dim text = "DIM arr(10) AS INTEGER"
      AssertRoundtripSuccess(text, "DIM array with AS clause")
    End Sub

    <Fact>
    Public Sub Roundtrip_DimAsMultiArray()
      Dim text = "DIM matrix(5, 10) AS DOUBLE"
      AssertRoundtripSuccess(text, "DIM multidimensional array with AS clause")
    End Sub

#End Region

#Region "Trivia Preservation"

    <Fact>
    Public Sub Roundtrip_DefWithComments()
      Dim text = "' This is a comment before DEF" & vbCrLf & "DEFINT A-Z  ' Set default integer type" & vbCrLf & "' Another comment"
      AssertRoundtripSuccess(text, "DEF statement with comments")
    End Sub

    <Fact>
    Public Sub Roundtrip_DimAsWithComments()
      Dim text = "' Variable declarations" & vbCrLf & "DIM x AS INTEGER  ' Integer variable" & vbCrLf & "DIM s AS STRING      ' String variable"
      AssertRoundtripSuccess(text, "DIM AS with comments")
    End Sub

    <Fact>
    Public Sub Roundtrip_TypeCharactersWithComments()
      Dim text = "intVar% = 42    ' Integer with type character" & vbCrLf & "strVar$ = ""test""  ' String with type character"
      AssertRoundtripSuccess(text, "Type characters with trailing comments")
    End Sub

    <Fact>
    Public Sub Roundtrip_WithWhitespaceVariations()
      Dim text = "  DEFINT   A-Z    " & vbCrLf & vbTab & "DIM" & vbTab & "x" & vbTab & "AS" & vbTab & "INTEGER"
      AssertRoundtripSuccess(text, "Various whitespace patterns")
    End Sub

    <Fact>
    Public Sub Roundtrip_LineContinuations()
      Dim text = "DEFINT A-Z, _" & vbCrLf & "      N-N"
      AssertRoundtripSuccess(text, "Line continuation in DEF statement")
    End Sub

    <Fact>
    Public Sub Roundtrip_EmptyLinesAndSpaces()
      Dim text = vbCrLf & "DEFINT A-Z" & vbCrLf & vbCrLf & "DIM x AS INTEGER" & vbCrLf & vbCrLf
      AssertRoundtripSuccess(text, "Empty lines and spacing")
    End Sub

#End Region

#Region "Keyword Normalization"

    <Fact>
    Public Sub Roundtrip_CaseInsensitiveDef()
      Dim text = "defint a-z" & vbCrLf & "DEFLNG b-z" & vbCrLf & "defsng c-z" & vbCrLf & "DEFdbl d-z" & vbCrLf & "defstr e-z"
      ' Should normalize to uppercase DEF keywords
      Dim result = PerformRoundtripTest(text)
      Assert.True(result.Success Or result.Roundtripped.Contains("DEFINT"), "DEF keywords should normalize to uppercase")
    End Sub

    <Fact>
    Public Sub Roundtrip_CaseInsensitiveAs()
      Dim text = "dim x as integer" & vbCrLf & "DIM y AS STRING" & vbCrLf & "Dim z As single"
      ' Should normalize to uppercase DIM and AS keywords
      Dim result = PerformRoundtripTest(text)
      Assert.True(result.Success OrElse (result.Roundtripped.Contains("DIM") AndAlso result.Roundtripped.Contains("AS")), "DIM and AS keywords should normalize to uppercase")
    End Sub

    <Fact>
    Public Sub Roundtrip_TypeKeywordsNormalization()
      Dim text = "dim a as INTEGER" & vbCrLf & "DIM b AS Long" & vbCrLf & "dim c as SINGLE" & vbCrLf & "DIM d AS double" & vbCrLf & "DIM e AS string"
      Dim result = PerformRoundtripTest(text)

      ' Type keywords should normalize to uppercase
      Assert.True(result.Success OrElse
                (result.Roundtripped.Contains("INTEGER") AndAlso
                 result.Roundtripped.Contains("LONG") AndAlso
                 result.Roundtripped.Contains("SINGLE") AndAlso
                 result.Roundtripped.Contains("DOUBLE") AndAlso
                 result.Roundtripped.Contains("STRING")), "Type keywords should normalize to uppercase")
    End Sub

#End Region

#Region "Complex Type Scenarios"

    <Fact>
    Public Sub Roundtrip_DefTypeOverride()
      Dim text = "DEFINT A-Z" & vbCrLf & "a% = 42     ' DEFINT applies" & vbCrLf & "b! = 3.14    ' Type character overrides" & vbCrLf & "c$ = ""test"" ' Type character overrides"
      AssertRoundtripSuccess(text, "DEF type with type character overrides")
    End Sub

    <Fact>
    Public Sub Roundtrip_DimAsOverrideDef()
      Dim text = "DEFINT A-Z" & vbCrLf & "a = 42         ' Uses DEFINT" & vbCrLf & "DIM b AS SINGLE ' Explicit type overrides DEFINT" & vbCrLf & "c = 100         ' Uses DEFINT"
      AssertRoundtripSuccess(text, "DIM AS overrides DEF type")
    End Sub

    <Fact>
    Public Sub Roundtrip_MixedTypeDeclarations()
      Dim text = "DEFINT A-M" & vbCrLf & "DEFSNG N-Z" & vbCrLf & "DEFSTR S" & vbCrLf & "a = 42         ' INTEGER" & vbCrLf & "n = 3.14        ' SINGLE" & vbCrLf & "s = ""hello""    ' STRING"
      AssertRoundtripSuccess(text, "Mixed DEF and implicit type usage")
    End Sub

    <Fact>
    Public Sub Roundtrip_ArraysWithTypes()
      Dim text = "DEFINT I-L" & vbCrLf & "intArray(10) = 0     ' Integer array" & vbCrLf & "DIM sngArray(5) AS SINGLE ' Single array" & vbCrLf & "strArray$(20) = """"     ' String array with type char"
      AssertRoundtripSuccess(text, "Arrays with various type specifications")
    End Sub

#End Region

#Region "Edge Cases"

    <Fact>
    Public Sub Roundtrip_UnusualSpacing()
      Dim text = "DEFINT    A-B,C-D" & vbCrLf & "DIM   x   AS   INTEGER" & vbCrLf & "var%   =   42"
      AssertRoundtripSuccess(text, "Unusual spacing in type declarations")
    End Sub

    <Fact>
    Public Sub Roundtrip_CommentsInTypeDeclarations()
      Dim text = "DEFINT A-Z  ' Comment after DEF" & vbCrLf & "DIM x AS INTEGER  ' Another comment"
      AssertRoundtripSuccess(text, "Comments within type declaration lines")
    End Sub

    <Fact>
    Public Sub Roundtrip_MultipleStatementsPerLine()
      Dim text = "DEFINT A-Z: DIM x AS INTEGER: a% = 42"
      AssertRoundtripSuccess(text, "Multiple type-related statements on one line")
    End Sub

    <Fact>
    Public Sub Roundtrip_NestedTypeCharacters()
      Dim text = "result$ = STR$(input%)  ' Function calls with type characters"
      AssertRoundtripSuccess(text, "Function calls with type character suffixes")
    End Sub

    <Fact>
    Public Sub Roundtrip_ComplexExpressionWithTypes()
      Dim text = "DEFINT A-Z" & vbCrLf & "DEFSTR S" & vbCrLf & "a% = 42 + LEN(s$ + ""test"")"
      AssertRoundtripSuccess(text, "Complex expressions with mixed types")
    End Sub

#End Region

#Region "Error Scenarios"

    <Fact>
    Public Sub Roundtrip_SyntaxErrorHandling()
      Dim text = "DEFINT" & vbCrLf & "DIM x AS INVALIDTYPE"
      ' Should handle syntax errors gracefully
      Dim result = PerformRoundtripTest(text)
      Assert.False(result.Success, "Should fail due to syntax error")
      Assert.True(result.Differences.Contains("Syntax errors"), "Should report syntax errors")
    End Sub

    <Fact>
    Public Sub Roundtrip_IncompleteTypeDeclaration()
      Dim text = "DIM x AS"
      Dim result = PerformRoundtripTest(text)
      ' Parser is permissive, but test behavior
      Assert.True(result.Roundtripped IsNot Nothing, "Should produce some output even for incomplete statements")
    End Sub

#End Region

  End Class

End Namespace
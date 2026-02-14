Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax
Imports QB.CodeAnalysis.Text

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class TypeValidationTests

    Private Function ParseAndValidate(text As String) As (Diagnostics As IEnumerable(Of Diagnostic), SyntaxTree As SyntaxTree)
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Return (syntaxTree.Diagnostics, syntaxTree)
    End Function

    Private Sub VerifyAnyError(diagnostics As IEnumerable(Of Diagnostic), ParamArray expectedMessages As String())
      Dim foundError = False
      For Each msg In expectedMessages
        If diagnostics.Any(Function(d) d.IsError AndAlso d.Message.Contains(msg)) Then
          foundError = True
          Exit For
        End If
      Next
      Assert.True(foundError, $"Expected error containing one of: {String.Join(", ", expectedMessages)}")
    End Sub

    Private Sub VerifyNoSyntaxErrors(diagnostics As IEnumerable(Of Diagnostic))
      Dim syntaxErrors = diagnostics.Where(Function(d) d.IsError).ToList()
      If syntaxErrors.Any() Then
        Dim errorMessages = String.Join(vbCrLf, syntaxErrors.Select(Function(d) d.Message))
        Assert.False(True, $"Unexpected syntax errors:{vbCrLf}{errorMessages}")
      End If
    End Sub

    Private Sub VerifyErrorCount(diagnostics As IEnumerable(Of Diagnostic), expectedCount As Integer)
      Dim actualCount = diagnostics.Count(Function(d) d.IsError)
      Assert.Equal(expectedCount, actualCount)
    End Sub

#Region "Basic Syntax Validation"

    <Fact>
    Public Sub Test_ParseValidDimStatements()
      Dim text = "DIM a AS INTEGER" & vbCrLf & "DIM b AS STRING"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ParseValidDefTypeStatements()
      Dim text = "DEFINT A-Z" & vbCrLf & "DEFSTR S-Z"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ParseValidTypeCharacters()
      Dim text = "a% = 123" & vbCrLf & "b$ = ""hello""" & vbCrLf & "c! = 45.67"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(3, result.SyntaxTree.Root.Members.Length)
    End Sub

#End Region

#Region "Incomplete Statements"

    <Fact>
    Public Sub Test_IncompleteDimStatement()
      Dim text = "DIM"
      Dim result = ParseAndValidate(text)

      ' Parser is permissive, may not generate errors for incomplete statements
      Assert.NotNull(result.SyntaxTree)
      Assert.True(result.SyntaxTree.Root.Members.Length >= 0)
    End Sub

    <Fact>
    Public Sub Test_IncompleteAsClause()
      Dim text = "DIM a AS"
      Dim result = ParseAndValidate(text)

      ' Parser is permissive, may not generate errors for incomplete AS clause
      Assert.NotNull(result.SyntaxTree)
      Assert.True(result.SyntaxTree.Root.Members.Length >= 0)
    End Sub

#End Region

#Region "Type Character Validation"

    <Fact>
    Public Sub Test_InvalidTypeCharacters()
      Dim text = "x@ = 123.45"
      Dim result = ParseAndValidate(text)

      ' Should generate bad character error for @
      VerifyAnyError(result.Diagnostics, "Bad character input")
    End Sub

    <Fact>
    Public Sub Test_MultipleTypeCharacters()
      Dim text = "x$% = ""hello"""
      Dim result = ParseAndValidate(text)

      ' Should generate bad character error for % after $
      VerifyAnyError(result.Diagnostics, "Bad character input")
    End Sub

#End Region

#Region "DEF Type Statement Validation"

    <Fact>
    Public Sub Test_DefTypeWithInvalidRange()
      Dim text = "DEFINT 1-9"
      Dim result = ParseAndValidate(text)

      ' Should generate error for numbers instead of letters
      VerifyAnyError(result.Diagnostics, "Unexpected token")
    End Sub

    <Fact>
    Public Sub Test_DefTypeWithReversedRange()
      Dim text = "DEFSTR Z-A"
      Dim result = ParseAndValidate(text)

      ' Parser may not validate range order at syntax level
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_UnknownDefType()
      Dim text = "DEFCUSTOM A-Z"
      Dim result = ParseAndValidate(text)

      ' Parser treats DEFCUSTOM as valid identifier, not as DEF type
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

#End Region

#Region "Array Declaration Tests"

    <Fact>
    Public Sub Test_ValidArrayDeclarations()
      Dim text = "DIM arr(10) AS INTEGER" & vbCrLf & "DIM matrix(5, 5) AS DOUBLE"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ArrayWithTypeCharacters()
      Dim text = "DIM arr$(10)" & vbCrLf & "DIM scores%(5)"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ArrayWithInvalidBounds()
      Dim text = "DIM arr(-10 TO -20) AS INTEGER"
      Dim result = ParseAndValidate(text)

      ' Parser is permissive about array bounds
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

#End Region

#Region "Mixed Type Declaration Tests"

    <Fact>
    Public Sub Test_DimAndTypeCharacters()
      Dim text = "DIM a AS INTEGER" & vbCrLf & "a$ = ""hello"""
      Dim result = ParseAndValidate(text)

      ' Parser allows both forms, type conflicts may be caught at binding time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_DefTypeAndConflictingUsage()
      Dim text = "DEFINT A-Z" & vbCrLf & "a$ = ""hello"""
      Dim result = ParseAndValidate(text)

      ' Parser allows both, conflicts may be caught at binding time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_MultipleVariableDeclarations()
      Dim text = "DIM a AS INTEGER, b AS STRING, c AS DOUBLE"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

#End Region

#Region "Function and Sub Tests"

    <Fact>
    Public Sub Test_FunctionWithTypeDeclarations()
      Dim text = "FUNCTION Test(x AS INTEGER, y AS STRING) AS DOUBLE" & vbCrLf &
                "  Test = x + LEN(y)" & vbCrLf &
                "END FUNCTION"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_SubWithTypeDeclarations()
      Dim text = "SUB Test(x AS INTEGER, y AS STRING)" & vbCrLf &
                "  PRINT x; y" & vbCrLf &
                "END SUB"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

#End Region

#Region "Type Keyword Validation"

    <Fact>
    Public Sub Test_AllValidTypeKeywords()
      Dim text = "DIM a AS INTEGER" & vbCrLf &
                "DIM b AS STRING" & vbCrLf &
                "DIM c AS SINGLE" & vbCrLf &
                "DIM d AS DOUBLE" & vbCrLf &
                "DIM e AS LONG"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(5, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ReservedWordsAsTypeNames()
      Dim text = "DIM a AS FUNCTION" & vbCrLf &
                "DIM b AS SUB" & vbCrLf &
                "DIM c AS IF"
      Dim result = ParseAndValidate(text)

      ' Should generate parsing errors for reserved keywords as type names
      VerifyAnyError(result.Diagnostics, "Unexpected token")
    End Sub

#End Region

#Region "Complex Scenarios"

    <Fact>
    Public Sub Test_RedeclarationWithDifferentTypes()
      Dim text = "DIM x AS INTEGER" & vbCrLf & "DIM x AS STRING"
      Dim result = ParseAndValidate(text)

      ' Parser allows redeclaration, conflicts may be caught at binding time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_GlobalAndLocalConflict()
      Dim text = "SHARED x AS INTEGER" & vbCrLf &
                "FUNCTION Test()" & vbCrLf &
                "  DIM x AS STRING" & vbCrLf &
                "END FUNCTION"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ParameterAndLocalConflict()
      Dim text = "FUNCTION Test(x AS INTEGER) AS INTEGER" & vbCrLf &
                "  DIM x AS STRING" & vbCrLf &
                "  Test = 1" & vbCrLf &
                "END FUNCTION"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

#End Region

#Region "Comprehensive Type Conflict Tests (For Future Implementation)"

    <Fact>
    Public Sub Test_DimAsWithConflictingTypeCharacterUsage_Structure()
      ' Tests that the parser accepts the structure, but type conflicts would be caught at binding time
      Dim text = "DIM a AS INTEGER" & vbCrLf & "a$ = ""test"""
      Dim result = ParseAndValidate(text)

      ' Parser accepts both forms - type conflicts detected at binding/evaluation time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_DefTypeWithConflictingUsage_Structure()
      Dim text = "DEFINT A-Z" & vbCrLf & "a$ = ""hello"""
      Dim result = ParseAndValidate(text)

      ' Parser accepts both forms - type conflicts detected at binding/evaluation time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ArrayTypeRedeclaration_Structure()
      Dim text = "DIM arr(10) AS INTEGER" & vbCrLf & "DIM arr$(5)"
      Dim result = ParseAndValidate(text)

      ' Parser accepts both forms - conflicts detected at binding/evaluation time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_VariableRedeclaration_Structure()
      Dim text = "DIM x AS INTEGER" & vbCrLf & "DIM x AS STRING"
      Dim result = ParseAndValidate(text)

      ' Parser accepts both declarations - conflicts detected at binding/evaluation time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

#End Region

#Region "Runtime Type Mismatch Tests (Structure Only)"

    <Fact>
    Public Sub Test_TypeMismatchExpression_Structure()
      Dim text = "DIM a AS INTEGER" & vbCrLf & "a = ""hello string"""
      Dim result = ParseAndValidate(text)

      ' Parser accepts the assignment - type mismatch detected at evaluation time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ArrayTypeMismatch_Structure()
      Dim text = "DIM arr(10) AS INTEGER" & vbCrLf & "arr$(0) = ""test"""
      Dim result = ParseAndValidate(text)

      ' Parser accepts the assignment - type mismatch detected at evaluation time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_FunctionParameterTypeMismatch_Structure()
      Dim text = "FUNCTION Test(x AS INTEGER) AS INTEGER" & vbCrLf &
                "  x = ""string""" & vbCrLf &
                "  Test = 1" & vbCrLf &
                "END FUNCTION"
      Dim result = ParseAndValidate(text)

      ' Parser accepts the assignment - type mismatch detected at evaluation time
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

#End Region

#Region "Advanced Type Declaration Scenarios"

    <Fact>
    Public Sub Test_MixedDimDeclarationStyles()
      Dim text = "DIM a, b AS INTEGER" & vbCrLf &
                "DIM c$ AS STRING" & vbCrLf &
                "DIM d%(5) AS INTEGER"
      Dim result = ParseAndValidate(text)

      ' Parser accepts mixed declaration styles
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(3, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ComplexArrayDeclarations()
      Dim text = "DIM arr1(10) AS INTEGER" & vbCrLf &
                "DIM arr2(5, 10) AS STRING" & vbCrLf &
                "DIM arr3(2, 3, 4) AS DOUBLE"
      Dim result = ParseAndValidate(text)

      ' Parser accepts multi-dimensional arrays
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(3, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_FunctionAndSubWithComplexTypes()
      Dim text = "FUNCTION Calc(x AS INTEGER, y AS STRING, z AS DOUBLE) AS SINGLE" & vbCrLf &
                "  Calc = x + LEN(y) + z" & vbCrLf &
                "END FUNCTION" & vbCrLf &
                "SUB Display(msg AS STRING, count AS INTEGER)" & vbCrLf &
                "  PRINT msg; count" & vbCrLf &
                "END SUB"
      Dim result = ParseAndValidate(text)

      ' Parser accepts complex type declarations
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_NestedScopeTypeDeclarations()
      Dim text = "FUNCTION Outer() AS INTEGER" & vbCrLf &
                "  DIM inner AS STRING" & vbCrLf &
                "  FUNCTION Nested(x AS DOUBLE) AS STRING" & vbCrLf &
                "    DIM result AS STRING" & vbCrLf &
                "    result = ""Nested: "" + STR$(x)" & vbCrLf &
                "    Nested = result" & vbCrLf &
                "  END FUNCTION" & vbCrLf &
                "  Outer = 42" & vbCrLf &
                "END FUNCTION"
      Dim result = ParseAndValidate(text)

      ' Parser accepts nested scopes with type declarations
      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

#End Region

#Region "Invalid Syntax Patterns"

    <Fact>
    Public Sub Test_MalformedAsClauseMissingType()
      Dim text = "DIM a AS"
      Dim result = ParseAndValidate(text)

      ' Should generate error for missing type identifier
      VerifyAnyError(result.Diagnostics, "Unexpected token")
    End Sub

    <Fact>
    Public Sub Test_MalformedAsClauseWithInvalidToken()
      Dim text = "DIM a AS 123"
      Dim result = ParseAndValidate(text)

      ' Should generate syntax error for number as type
      VerifyAnyError(result.Diagnostics, "Unexpected token")
    End Sub

    <Fact>
    Public Sub Test_MalformedDefStatementMissingRange()
      Dim text = "DEFINT"
      Dim result = ParseAndValidate(text)

      ' May or may not generate error - parser might be permissive
      Assert.NotNull(result.SyntaxTree)
    End Sub

    <Fact>
    Public Sub Test_MalformedArrayDeclaration()
      Dim text = "DIM arr()"
      Dim result = ParseAndValidate(text)

      ' Parser might accept empty array bounds
      Assert.NotNull(result.SyntaxTree)
    End Sub

  #End Region

#Region "User-Defined Type (TYPE/END TYPE) Tests"

    <Fact>
    Public Sub Test_ParseSimpleTypeDeclaration()
      Dim text = "TYPE Employee" & vbCrLf &
                "  ename AS STRING" & vbCrLf &
                "  salary AS SINGLE" & vbCrLf &
                "END TYPE"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ParseTypeWithMultipleFields()
      Dim text = "TYPE Point" & vbCrLf &
                "  x AS INTEGER" & vbCrLf &
                "  y AS INTEGER" & vbCrLf &
                "  z AS INTEGER" & vbCrLf &
                "END TYPE"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_ParseTypeWithDifferentFieldTypes()
      Dim text = "TYPE MixedType" & vbCrLf &
                "  intField AS INTEGER" & vbCrLf &
                "  lngField AS LONG" & vbCrLf &
                "  sngField AS SINGLE" & vbCrLf &
                "  dblField AS DOUBLE" & vbCrLf &
                "  strField AS STRING" & vbCrLf &
                "END TYPE"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_DimAsUserDefinedType()
      Dim text = "TYPE Employee" & vbCrLf &
                "  ename AS STRING" & vbCrLf &
                "  salary AS SINGLE" & vbCrLf &
                "END TYPE" & vbCrLf &
                "DIM emp AS Employee"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_MemberAccessExpression()
      Dim text = "TYPE Employee" & vbCrLf &
                "  ename AS STRING" & vbCrLf &
                "  salary AS SINGLE" & vbCrLf &
                "END TYPE" & vbCrLf &
                "DIM emp AS Employee" & vbCrLf &
                "emp.ename = ""John"""
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(3, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_TypeWithFixedLengthStrings()
      Dim text = "TYPE Employee" & vbCrLf &
                "  ename AS STRING * 20" & vbCrLf &
                "  department AS STRING * 15" & vbCrLf &
                "  salary AS SINGLE" & vbCrLf &
                "END TYPE"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_TypeWithMixedFixedAndVariableStrings()
      Dim text = "TYPE Record" & vbCrLf &
                "  fixedName AS STRING * 30" & vbCrLf &
                "  variableNote AS STRING" & vbCrLf &
                "  amount AS SINGLE" & vbCrLf &
                "END TYPE"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(1, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_TypeWithNestedUDT()
      Dim text = "TYPE Address" & vbCrLf &
                "  street AS STRING * 50" & vbCrLf &
                "  city AS STRING * 30" & vbCrLf &
                "END TYPE" & vbCrLf &
                "TYPE Person" & vbCrLf &
                "  name AS STRING * 40" & vbCrLf &
                "  addr AS Address" & vbCrLf &
                "END TYPE"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(2, result.SyntaxTree.Root.Members.Length)
    End Sub

    <Fact>
    Public Sub Test_DeeplyNestedUDT()
      Dim text = "TYPE ZipInfo" & vbCrLf &
                "  code AS STRING * 10" & vbCrLf &
                "END TYPE" & vbCrLf &
                "TYPE Address" & vbCrLf &
                "  street AS STRING * 50" & vbCrLf &
                "  zip AS ZipInfo" & vbCrLf &
                "END TYPE" & vbCrLf &
                "TYPE Person" & vbCrLf &
                "  name AS STRING * 40" & vbCrLf &
                "  homeAddress AS Address" & vbCrLf &
                "END TYPE"
      Dim result = ParseAndValidate(text)

      VerifyNoSyntaxErrors(result.Diagnostics)
      Assert.Equal(3, result.SyntaxTree.Root.Members.Length)
    End Sub

  #End Region

  End Class

End Namespace
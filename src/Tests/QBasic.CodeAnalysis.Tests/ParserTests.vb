Imports QB.CodeAnalysis.Syntax

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class ParserTests

    <Fact>
    Public Sub ParsesNumberLine()
      Dim text = "123"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      ' For now, just check it parses without errors
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesNumberExpression1()
      Dim text = "LET X = 123"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      ' For now, just check it parses without errors
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesNumberDefaultExpression()
      Dim text = "LET X = 123"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      ' For now, just check it parses without errors
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesNumberIntegerExpression()
      Dim text = "LET X = 123%"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      ' For now, just check it parses without errors
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesNumberSingleExpression()
      Dim text = "LET X = 123!"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      ' For now, just check it parses without errors
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesNumberLongExpression()
      Dim text = "LET X& = 123&"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      ' For now, just check it parses without errors
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesNumberDoubleExpression()
      Dim text = "LET X# = 123#"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      ' For now, just check it parses without errors
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesBinaryExpression()
      Dim text = "LET A = 1 + 2"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesUnaryExpression()
      Dim text = "-123"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesFunctionCall()
      Dim text = "LEN(""hello"")"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesInpFunctionCall()
      Dim text = "LET X = INP(97)"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesComplexExpression()
      Dim text = "LET A = 1 + 2 * 3 - 4"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesIfElseStatement()
      Dim text = "IF x > 0 THEN PRINT ""positive"" ELSE PRINT ""non-positive"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesSubDefinition()
      Dim text = "SUB Test: END SUB"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesWithMissingEndIf()
      Dim text = "IF x THEN PRINT y"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      ' May or may not have diagnostics depending on implementation
    End Sub

    <Fact>
    Public Sub ParsesPrintStatement()
      Dim text = "PRINT ""Hello"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.IsType(GetType(GlobalStatementSyntax), compilationUnit.Members(0))
      Dim globalStmt = DirectCast(compilationUnit.Members(0), GlobalStatementSyntax)
      Assert.IsType(GetType(PrintStatementSyntax), globalStmt.Statement)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesLetStatement()
      Dim text = "LET x = 10"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.IsType(GetType(GlobalStatementSyntax), compilationUnit.Members(0))
      Dim globalStmt = DirectCast(compilationUnit.Members(0), GlobalStatementSyntax)
      Assert.IsType(GetType(LetStatementSyntax), globalStmt.Statement)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesOutStatement()
      Dim text = "OUT 97, 3"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesIfStatement()
      Dim text = "IF x > 0 THEN PRINT ""Positive"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.IsType(GetType(GlobalStatementSyntax), compilationUnit.Members(0))
      Dim globalStmt = DirectCast(compilationUnit.Members(0), GlobalStatementSyntax)
      Assert.IsType(GetType(SingleLineIfStatementSyntax), globalStmt.Statement)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesForStatement()
      Dim text = "FOR i = 1 TO 10: NEXT i"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesWhileStatement()
      Dim text = "WHILE x > 0: WEND"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesDimStatement()
      Dim text = "DIM x AS INTEGER"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesAssignmentWithoutLet()
      Dim text = "x = 42"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.IsType(GetType(GlobalStatementSyntax), compilationUnit.Members(0))
      Dim globalStmt = DirectCast(compilationUnit.Members(0), GlobalStatementSyntax)
      Assert.IsType(GetType(ExpressionStatementSyntax), globalStmt.Statement)
      Dim exprStmt = DirectCast(globalStmt.Statement, ExpressionStatementSyntax)
      Assert.IsType(GetType(AssignmentExpressionSyntax), exprStmt.Expression)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesArrayDeclaration()
      Dim text = "DIM arr(10) AS INTEGER"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesMultiDimensionalArray()
      Dim text = "DIM matrix(5, 5) AS DOUBLE"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesFunctionDefinition()
      Dim text = "FUNCTION Add(x AS INTEGER, y AS INTEGER) AS INTEGER" & vbCrLf &
                "  Add = x + y" & vbCrLf &
                "END FUNCTION"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesSubWithParameters()
      Dim text = "SUB PrintMessage(msg AS STRING)" & vbCrLf &
                "  PRINT msg" & vbCrLf &
                "END SUB"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesNestedIfStatements()
      Dim text = "IF x > 0 THEN" & vbCrLf &
                "  IF y > 0 THEN" & vbCrLf &
                "    PRINT ""Both positive""" & vbCrLf &
                "  END IF" & vbCrLf &
                "END IF"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesSelectCaseStatement()
      Dim text = "SELECT CASE choice" & vbCrLf &
                "  CASE 1" & vbCrLf &
                "    PRINT ""One""" & vbCrLf &
                "  CASE 2" & vbCrLf &
                "    PRINT ""Two""" & vbCrLf &
                "  CASE ELSE" & vbCrLf &
                "    PRINT ""Other""" & vbCrLf &
                "END SELECT"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesSelectCaseWithMultipleValues()
      Dim text = "SELECT CASE x" & vbCrLf &
                "  CASE 1, 2, 3" & vbCrLf &
                "    PRINT ""Small""" & vbCrLf &
                "  CASE 4, 5, 6" & vbCrLf &
                "    PRINT ""Medium""" & vbCrLf &
                "END SELECT"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.NotNull(syntaxTree.Root)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesSelectCaseWithRanges()
      Dim text = "SELECT CASE score" & vbCrLf &
                "  CASE 0 TO 59" & vbCrLf &
                "    PRINT ""Fail""" & vbCrLf &
                "  CASE 60 TO 79" & vbCrLf &
                "    PRINT ""Pass""" & vbCrLf &
                "  CASE 80 TO 100" & vbCrLf &
                "    PRINT ""Excellent""" & vbCrLf &
                "END SELECT"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.NotNull(syntaxTree.Root)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesSelectCaseWithIsComparisons()
      Dim text = "SELECT CASE age" & vbCrLf &
                "  CASE IS < 13" & vbCrLf &
                "    PRINT ""Child""" & vbCrLf &
                "  CASE IS < 20" & vbCrLf &
                "    PRINT ""Teenager""" & vbCrLf &
                "  CASE IS >= 65" & vbCrLf &
                "    PRINT ""Senior""" & vbCrLf &
                "  CASE ELSE" & vbCrLf &
                "    PRINT ""Adult""" & vbCrLf &
                "END SELECT"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.NotNull(syntaxTree.Root)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesSelectCaseWithStringTest()
      Dim text = "SELECT CASE name$" & vbCrLf &
                "  CASE ""John""" & vbCrLf &
                "    PRINT ""Hello John""" & vbCrLf &
                "  CASE ""Jane"", ""Mary""" & vbCrLf &
                "    PRINT ""Hello Lady""" & vbCrLf &
                "  CASE ELSE" & vbCrLf &
                "    PRINT ""Hello Stranger""" & vbCrLf &
                "END SELECT"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.NotNull(syntaxTree.Root)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesDoWhileLoop()
      Dim text = "DO WHILE x < 10" & vbCrLf &
                "  x = x + 1" & vbCrLf &
                "LOOP"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesDoUntilLoop()
      Dim text = "DO" & vbCrLf &
                "  x = x + 1" & vbCrLf &
                "LOOP UNTIL x >= 10"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesForNextWithStep()
      Dim text = "FOR i = 1 TO 10 STEP 2" & vbCrLf &
                "  PRINT i" & vbCrLf &
                "NEXT i"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesGosubReturn()
      Dim text = "
GOSUB MySub
PRINT ""Back""
END

MySub:
  PRINT ""In subroutine""
  RETURN
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Equal(6, compilationUnit.Members.Length) ' Main statements + label
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesComplexExpressionWithParentheses()
      Dim text = "result = (a + b) * (c - d) / e"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesStringConcatenation()
      Dim text = "fullName = firstName + "" "" + lastName"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesBuiltInFunctionCalls()
      Dim text = "PRINT LEN(TRIM$(input$)) + VAL(number$)"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesDataReadStatements()
      Dim text = "DATA 1, 2, 3, ""Hello"", 4.5" & vbCrLf &
                "READ x, y, z, msg$, value"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Equal(2, compilationUnit.Members.Count)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesInputStatement()
      Dim text = "INPUT ""Enter your name: ""; name$"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesLineInputStatement()
      Dim text = "LINE INPUT ""Enter text: ""; text$"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesOnGotoStatement()
      Dim text = "ON choice GOTO 100, 200, 300"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = syntaxTree.Root

      Assert.NotNull(root)
      Assert.IsType(GetType(CompilationUnitSyntax), root)
      Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
      Assert.Single(compilationUnit.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

     <Fact>
     Public Sub ParsesOnGosubStatement()
       Dim text = "ON choice GOSUB Sub1, Sub2, Sub3"
       Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
       Dim root = syntaxTree.Root

       Assert.NotNull(root)
       Assert.IsType(GetType(CompilationUnitSyntax), root)
       Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
       Assert.Single(compilationUnit.Members)
       Assert.Empty(syntaxTree.Diagnostics)
     End Sub

     <Fact>
     Public Sub ParsesSleepStatement()
       Dim text = "SLEEP 5"
       Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
       Dim root = syntaxTree.Root

       Assert.NotNull(root)
       Assert.IsType(GetType(CompilationUnitSyntax), root)
       Dim compilationUnit = DirectCast(root, CompilationUnitSyntax)
       Assert.Single(compilationUnit.Members)
       Assert.Empty(syntaxTree.Diagnostics)
     End Sub

   End Class

End Namespace
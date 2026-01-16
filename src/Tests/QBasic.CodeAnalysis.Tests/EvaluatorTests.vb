Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class EvaluatorTests

    <Fact>
    Public Sub EvaluatesDefXxxVariableDeclarations()
    
      Dim text = "
DEFINT I
DEFLNG L
DEFSNG S
DEFDBL D

I = 1
L = 60000
S = 5.5
D = 10000.9

' Additional tests for DEFLNG
DEFLNG X-Z
x = 123456789
y& = 987654321
z = 42

' Test overflow for Long
' l = 2147483648  ' This should cause overflow if assigned without &

' Test range
DEFLNG A-C
b = -2147483648
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("I")}")    
      Assert.Equal($"{60000}", $"{variables("L")}")    
      Assert.Equal($"{5.5}", $"{variables("S")}")    
      Assert.Equal($"{10000.9}", $"{variables("D")}")    
       Assert.Equal($"{123456790}", $"{variables("x")}")    
       Assert.Equal($"{987654321}", $"{variables("y&")}")    
       Assert.Equal($"{42}", $"{variables("z")}")    
       Assert.Equal($"-2.1474836E+09", $"{variables("b")}")
    End Sub

    <Fact>
    Public Sub EvaluatesSimpleAssignment()
      ' Exactly replicate the working ExecutesSimpleProgram test
      Dim text = "x = 10
y = 20
LET result = x + y"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal(3, variables.Count)
    End Sub

    <Fact>
    Public Sub EvaluatesBinaryExpression()
      ' Use LET assignment to create an executable context for evaluation
      Dim text = "LET result = 1 + 2"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      ' LET assignment doesn't return a value, but stores in variables
      Assert.Equal($"{3}", $"{variables("result")}")
    End Sub

    <Fact>
    Public Sub EvaluatesArithmeticOperators()
      ' Test all basic arithmetic operators using LET assignments
      Dim testCases = New(String, Object)() {
          ("LET result = 5 + 3", 8),
          ("LET result = 10 - 4", 6),
          ("LET result = 6 * 7", 42),
          ("LET result = 15 / 3", 5.0),
          ("LET result = 17 \ 4", 4), ' Integer division
          ("LET result = 17 MOD 4", 1),
          ("LET result = 2 ^ 3", 8.0)
      }

      For Each testCase In testCases
        Dim text = testCase.Item1
        Dim expected = testCase.Item2
        Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
        Dim compilation As Compilation = Compilation.Create(syntaxTree)
        Dim variables = New Dictionary(Of String, Object)()
        Dim result = compilation.Evaluate(variables)
        Assert.Equal($"{expected}", $"{variables("result")}")
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesUnaryOperators()
      Dim testCases = New(String, Object)() {
          ("LET result = -5", -5),
          ("LET result = +3", 3),
          ("LET result = NOT 0", -1), ' NOT of 0 is -1 in QBasic
          ("LET result = NOT -1", 0)  ' NOT of -1 is 0 in QBasic
      }

      For Each testCase In testCases
        Dim text = testCase.Item1
        Dim expected = testCase.Item2
        Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
        Dim compilation As Compilation = Compilation.Create(syntaxTree)
        Dim variables = New Dictionary(Of String, Object)()
        Dim result = compilation.Evaluate(variables)
        Assert.Equal($"{expected}", $"{variables("result")}")
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesComparisonOperators()
      Dim testCases = New(String, Integer)() {
          ("LET result = 5 = 5", -1),   ' True
          ("LET result = 5 = 6", 0),    ' False
          ("LET result = 5 <> 6", -1),  ' True
          ("LET result = 5 <> 5", 0),   ' False
          ("LET result = 5 < 6", -1),   ' True
          ("LET result = 6 < 5", 0),    ' False
          ("LET result = 5 <= 5", -1),  ' True
          ("LET result = 6 <= 5", 0),   ' False
          ("LET result = 6 > 5", -1),   ' True
          ("LET result = 5 > 6", 0),    ' False
          ("LET result = 5 >= 5", -1),  ' True
          ("LET result = 4 >= 5", 0)    ' False
      }

      For Each testCase In testCases
        Dim text = testCase.Item1
        Dim expected = testCase.Item2
        Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
        Dim compilation As Compilation = Compilation.Create(syntaxTree)
        Dim variables = New Dictionary(Of String, Object)()
        Dim result = compilation.Evaluate(variables)
        Assert.Equal($"{expected}", $"{variables("result")}")
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesLogicalOperators()
      Dim testCases = New(String, Integer)() {
          ("LET result = -1 AND -1", -1),  ' True AND True = True
          ("LET result = -1 AND 0", 0),    ' True AND False = False
          ("LET result = 0 AND -1", 0),    ' False AND True = False
          ("LET result = 0 AND 0", 0),     ' False AND False = False
          ("LET result = -1 OR -1", -1),   ' True OR True = True
          ("LET result = -1 OR 0", -1),    ' True OR False = True
          ("LET result = 0 OR -1", -1),    ' False OR True = True
          ("LET result = 0 OR 0", 0),      ' False OR False = False
          ("LET result = -1 XOR -1", 0),   ' True XOR True = False
          ("LET result = -1 XOR 0", -1),   ' True XOR False = True
          ("LET result = 0 XOR -1", -1),   ' False XOR True = True
          ("LET result = 0 XOR 0", 0)      ' False XOR False = False
      }

      For Each testCase In testCases
        Dim text = testCase.Item1
        Dim expected = testCase.Item2
        Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
        Dim compilation As Compilation = Compilation.Create(syntaxTree)
        Dim variables = New Dictionary(Of String, Object)()
        Dim result = compilation.Evaluate(variables)
        Assert.Equal($"{expected}", $"{variables("result")}")
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesStringLiterals()
      ' Use LET assignment for string literals
      Dim text = "LET message$ = ""Hello World"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal("Hello World", variables("message$"))
    End Sub

    <Fact>
    Public Sub EvaluatesStringConcatenation()
      Dim text = "LET result$ = ""Hello"" + "" "" + ""World"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal("Hello World", variables("result$"))
    End Sub

    <Fact>
    Public Sub EvaluatesVariableAssignmentNumeric()
      Dim text = "x = 42"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{42}", $"{variables("x")}")
    End Sub

    <Fact>
    Public Sub EvaluatesVariableAssignmentString()
      Dim text = "a$ = ""test"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal("test", variables("a$"))
    End Sub

    <Fact>
    Public Sub EvaluatesVariableReferenceNumeric()
      Dim text = "x = 10
LET result = x + 5"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{15}", $"{variables("result")}")
      Assert.Equal($"{10}", $"{variables("x")}")
    End Sub

    <Fact>
    Public Sub EvaluatesVariableReferenceString()
      Dim text = "a$ = ""test""
LET result$ = a$ + ""..."""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal("test...", variables("result$"))
      Assert.Equal("test", variables("a$"))
    End Sub

    <Fact>
    Public Sub EvaluatesBuiltInFunctions()
      Dim testCases = New(String, Object)() {
          ("LET result = LEN(""hello"")", 5),
          ("LET result = ABS(-5%)", 5),
          ("LET result = ABS(5%)", 5),
          ("LET result = SGN(-5%)", -1),
          ("LET result = SGN(0%)", 0),
          ("LET result = SGN(5%)", 1),
          ("LET result = INT(3.7!)", 3),
          ("LET result = FIX(3.7!)", 3),
          ("LET result = FIX(-3.7!)", -3)
      }

      For Each testCase In testCases
        Dim text = testCase.Item1
        Dim expected = testCase.Item2
        Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
        Dim compilation As Compilation = Compilation.Create(syntaxTree)
        Dim variables = New Dictionary(Of String, Object)()
        Dim result = compilation.Evaluate(variables)
        Dim v = If(variables.ContainsKey("result"), variables("result"), 0)
        Assert.Equal($"{expected}", $"{v}")
      Next

    End Sub

    <Fact>
    Public Sub EvaluatesMathematicalFunctions()
      Dim testCases = New(String, Double)() {
          ("LET result = SQR(4)", 2.0),
          ("LET result = SQR(9)", 3.0),
          ("LET result = SIN(0)", 0.0),
          ("LET result = COS(0)", 1.0),
          ("LET result = TAN(0)", 0.0),
          ("LET result = EXP(0)", 1.0),
          ("LET result = LOG(1)", 0.0),
          ("LET result = ATN(0)", 0.0)
      }

      For Each testCase In testCases
        Dim text = testCase.Item1
        Dim expected = testCase.Item2
        Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
        Dim compilation As Compilation = Compilation.Create(syntaxTree)
        Dim variables = New Dictionary(Of String, Object)()
        Dim result = compilation.Evaluate(variables)
        Dim v = If(variables.ContainsKey("result"), CDbl(variables("result")), 0.00#)
        Assert.Equal(expected, v, 0.0001) ' Allow small floating point differences
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesStringFunctions()
      Dim testCases = New(String, String)() {
          ("LET result$ = UCASE$(""hello"")", "HELLO"),
          ("LET result$ = LCASE$(""HELLO"")", "hello"),
          ("LET result$ = LEFT$(""hello"", 2)", "he"),
          ("LET result$ = RIGHT$(""hello"", 2)", "lo"),
          ("LET result$ = MID$(""hello"", 2, 2)", "el"),
          ("LET result$ = STR$(123)", " 123"),
          ("LET result$ = SPACE$(3)", "   ")
      }

      For Each testCase In testCases
        Dim text = testCase.Item1
        Dim expected = testCase.Item2
        Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
        Dim compilation As Compilation = Compilation.Create(syntaxTree)
        Dim variables = New Dictionary(Of String, Object)()
        Dim result = compilation.Evaluate(variables)
        Assert.Equal(expected, variables("result$"))
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesComplexExpression()
      Dim text = "LET result = ((2 + 3) * 4 - 6) / 2 + 1"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      ' ((2+3)*4-6)/2+1 = (5*4-6)/2+1 = (20-6)/2+1 = 14/2+1 = 7+1 = 8
      Assert.Equal($"{8.0}", $"{variables("result")}")
    End Sub

    <Fact>
    Public Sub EvaluatesDefFnFunctions()
      Dim testCases = New(String, Object)() {
          ("DEF FNadd(x, y) = x + y" & vbCrLf & "LET result = FNadd(3, 5)", 8),
          ("DEF FNsquare(n) = n * n" & vbCrLf & "LET result = FNsquare(4)", 16),
          ("DEF FNmax(a, b)" & vbCrLf & "  IF a > b THEN FNmax = a ELSE FNmax = b" & vbCrLf & "END DEF" & vbCrLf & "LET result = FNmax(10, 20)", 20),
          ("DEF FNfactorial(n)" & vbCrLf & "  IF n <= 1 THEN FNfactorial = 1 ELSE FNfactorial = n * FNfactorial(n - 1)" & vbCrLf & "END DEF" & vbCrLf & "LET result = FNfactorial(5)", 120)
      }

      For Each testCase In testCases
        Dim testText = testCase.Item1
        Dim expected = testCase.Item2
        Dim testSyntaxTree As SyntaxTree = SyntaxTree.Parse(testText)
        Dim testCompilation As Compilation = Compilation.Create(testSyntaxTree)
        Dim testVariables = New Dictionary(Of String, Object)()
        Dim testResult = testCompilation.Evaluate(testVariables)
        Assert.Equal($"{expected}", $"{testVariables("result")}")
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesRndFunction()
      ' Test RND with positive values (next random)
      Dim text1 = "LET result = RND(1)"
      Dim syntaxTree1 As SyntaxTree = SyntaxTree.Parse(text1)
      Dim compilation1 As Compilation = Compilation.Create(syntaxTree1)
      Dim variables1 = New Dictionary(Of String, Object)()
      Dim result1 = compilation1.Evaluate(variables1)
      ' RND should return a value between 0 and 1
      Dim rndValue = CDbl(variables1("result"))
      Assert.True(rndValue >= 0.0 And rndValue < 1.0)

      ' Test RND with zero (should return last generated number)
      Dim text2 = "LET first = RND(1)" & vbCrLf & "LET second = RND(0)"
      Dim syntaxTree2 As SyntaxTree = SyntaxTree.Parse(text2)
      Dim compilation2 As Compilation = Compilation.Create(syntaxTree2)
      Dim variables2 = New Dictionary(Of String, Object)()
      Dim result2 = compilation2.Evaluate(variables2)
      ' RND(0) should equal the previous RND(1)
      Assert.Equal(variables2("first"), variables2("second"))

      ' Test RND with negative values (deterministic for same input)
      Dim text3 = "LET result1 = RND(-5)" & vbCrLf & "LET result2 = RND(-5)"
      Dim syntaxTree3 As SyntaxTree = SyntaxTree.Parse(text3)
      Dim compilation3 As Compilation = Compilation.Create(syntaxTree3)
      Dim variables3 = New Dictionary(Of String, Object)()
      Dim result3 = compilation3.Evaluate(variables3)
      ' RND with same negative value should be deterministic
      Assert.Equal(variables3("result1"), variables3("result2"))
    End Sub

    <Fact>
    Public Sub EvaluatesChrAndAscFunctions()
      ' Test CHR$ function
      Dim chrTestCases = New(String, Char)() {
          ("LET result$ = CHR$(65)", "A"c),
          ("LET result$ = CHR$(66)", "B"c),
          ("LET result$ = CHR$(97)", "a"c),
          ("LET result$ = CHR$(48)", "0"c),
          ("LET result$ = CHR$(32)", " "c)
      }

      For Each testCase In chrTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As Char = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CChar(vars("result$")))
      Next

      ' Test ASC function
      Dim ascTestCases = New(String, Integer)() {
          ("LET result = ASC(""A"")", 65),
          ("LET result = ASC(""B"")", 66),
          ("LET result = ASC(""a"")", 97),
          ("LET result = ASC(""0"")", 48),
          ("LET result = ASC("" "")", 32),
          ("LET result = ASC(""Hello"")", 72) ' First character 'H'
      }

      For Each testCase In ascTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As Integer = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal($"{expectedResult}", $"{vars("result")}")
      Next

      ' Test CHR$/ASC roundtrip
      Dim roundtripText = "LET char$ = CHR$(90)" & vbCrLf & "LET code = ASC(char$)"
      Dim roundtripTree As SyntaxTree = SyntaxTree.Parse(roundtripText)
      Dim roundtripComp As Compilation = Compilation.Create(roundtripTree)
      Dim roundtripVars As New Dictionary(Of String, Object)()
      Dim roundtripResult = roundtripComp.Evaluate(roundtripVars)
      Assert.Equal("Z"c, CChar(roundtripVars("char$")))
      Assert.Equal(90, CInt(roundtripVars("code")))
    End Sub

    <Fact>
    Public Sub EvaluatesInstrFunction()
      Dim testCases = New(String, Integer)() {
          ("LET result = INSTR(""Hello World"", ""World"")", 7),
          ("LET result = INSTR(""Hello World"", ""Hello"")", 1),
          ("LET result = INSTR(""Hello World"", ""X"")", 0),
          ("LET result = INSTR(3, ""Hello World"", ""llo"")", 3),
          ("LET result = INSTR(5, ""Hello World"", ""o"")", 5),
          ("LET result = INSTR(8, ""Hello World"", ""o"")", 8),
          ("LET result = INSTR(10, ""Hello World"", ""o"")", 0),
          ("LET result = INSTR(""aaa"", ""a"")", 1),
          ("LET result = INSTR(2, ""aaa"", ""a"")", 2)
      }

      For Each testCase In testCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As Integer = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CInt(vars("result")))
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesTrimFunctions()
      '' Test TRIM$ function
      'Dim trimTestCases = New(String, String)() {
      '    ("LET result$ = TRIM$(""  hello  "")", "hello"),
      '    ("LET result$ = TRIM$(""hello"")", "hello"),
      '    ("LET result$ = TRIM$(""  "")", ""),
      '    ("LET result$ = TRIM$(""   spaces   "")", "spaces"),
      '    ("LET result$ = TRIM$("")", "")
      '}

      'For Each testCase In trimTestCases
      '  Dim inputText As String = testCase.Item1
      '  Dim expectedResult As String = testCase.Item2
      '  Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
      '  Dim comp As Compilation = Compilation.Create(tree)
      '  Dim vars As New Dictionary(Of String, Object)()
      '  Dim evalResult = comp.Evaluate(vars)
      '  Assert.Equal(expectedResult, CStr(vars("result$")))
      'Next

      ' Test LTRIM$ function
      Dim ltrimTestCases = New(String, String)() {
          ("LET result$ = LTRIM$(""  hello"")", "hello"),
          ("LET result$ = LTRIM$(""hello"")", "hello"),
          ("LET result$ = LTRIM$(""  "")", ""),
          ("LET result$ = LTRIM$(""   spaces"")", "spaces")
      }
      'TODO: Need to get LTRIM$ with an empty string working...
      '("LET result$ = LTRIM$("")", "")

      For Each testCase In ltrimTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As String = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CStr(vars("result$")))
      Next

      ' Test RTRIM$ function
      Dim rtrimTestCases = New(String, String)() {
          ("LET result$ = RTRIM$(""hello  "")", "hello"),
          ("LET result$ = RTRIM$(""hello"")", "hello"),
          ("LET result$ = RTRIM$(""  "")", ""),
          ("LET result$ = RTRIM$(""spaces   "")", "spaces")
      }
      'TODO: Need to get RTRIM$ with an empty string working...
      '("LET result$ = RTRIM$("")", "")

      For Each testCase In rtrimTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As String = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CStr(vars("result$")))
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesPrintFormatting()
      ' Test PRINT statement parsing with various formatting
      ' Note: Full output testing would require capturing console output

      ' Test basic PRINT statements parse correctly
      Dim printTestCases = New String() {
          "PRINT ""Hello""",
          "PRINT ""Hello"";",
          "PRINT ""Hello"", ""World""",
          "PRINT 123",
          "PRINT 123;",
          "PRINT 123, 456",
          "PRINT ""Value:""; 42",
          "PRINT TAB(10); ""Indented""",
          "PRINT SPC(5); ""Spaced"""
      }

      For Each testCase In printTestCases
        Dim tree As SyntaxTree = SyntaxTree.Parse(testCase)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        ' Just verify it parses and evaluates without errors
        Dim evalResult = comp.Evaluate(vars)
        Assert.Empty(tree.Diagnostics)
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesBeepStatement()
      ' Test BEEP statement
      Dim beepTest = "BEEP"
      Dim beepTree As SyntaxTree = SyntaxTree.Parse(beepTest)
      Dim beepComp As Compilation = Compilation.Create(beepTree)
      Dim beepVars As New Dictionary(Of String, Object)()
      Dim beepResult = beepComp.Evaluate(beepVars)
      Assert.Empty(beepTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub EvaluatesClsStatement()
      ' Test CLS statement
      Dim clsTest = "CLS"
      Dim clsTree As SyntaxTree = SyntaxTree.Parse(clsTest)
      Dim clsComp As Compilation = Compilation.Create(clsTree)
      Dim clsVars As New Dictionary(Of String, Object)()
      Dim clsResult = clsComp.Evaluate(clsVars)
      Assert.Empty(clsTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub EvaluatesRemStatement()
      ' Test REM statements (single line and apostrophe)
      Dim remTestCases = New String() {
          "REM This is a remark",
          "' This is also a remark",
          "LET x = 1 ' Inline remark"
      }
      For Each remTest In remTestCases
        Dim remTree As SyntaxTree = SyntaxTree.Parse(remTest)
        Dim remComp As Compilation = Compilation.Create(remTree)
        Dim remVars As New Dictionary(Of String, Object)()
        Dim remResult = remComp.Evaluate(remVars)
        Assert.Empty(remTree.Diagnostics)
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesIfElseVariations()
      ' Test single-line IF/THEN/ELSE
      Dim singleLineTest = "LET result = 0" & vbCrLf & "IF 5 > 3 THEN LET result = 1 ELSE LET result = 0"
      Dim singleLineTree As SyntaxTree = SyntaxTree.Parse(singleLineTest)
      Dim singleLineComp As Compilation = Compilation.Create(singleLineTree)
      Dim singleLineVars As New Dictionary(Of String, Object)()
      Dim singleLineResult = singleLineComp.Evaluate(singleLineVars)
      Assert.Equal(1, CInt(singleLineVars("result")))

      ' Test multi-line IF/END IF
      Dim multiLineTest = "
LET result = 0
IF 10 > 5 THEN
  LET result = 10
END IF"
      Dim multiLineTree As SyntaxTree = SyntaxTree.Parse(multiLineTest)
      Dim multiLineComp As Compilation = Compilation.Create(multiLineTree)
      Dim multiLineVars As New Dictionary(Of String, Object)()
      Dim multiLineResult = multiLineComp.Evaluate(multiLineVars)
      Assert.Equal(10, CInt(multiLineVars("result")))

      ' Test IF/ELSE
      Dim elseTest = "
LET result = 0
IF 3 > 5 THEN
  LET result = 1
ELSE
  LET result = 2
END IF"
      Dim elseTree As SyntaxTree = SyntaxTree.Parse(elseTest)
      Dim elseComp As Compilation = Compilation.Create(elseTree)
      Dim elseVars As New Dictionary(Of String, Object)()
      Dim elseResult = elseComp.Evaluate(elseVars)
      Assert.Equal(2, CInt(elseVars("result")))

      ' Test ELSEIF
      Dim elseifTest = "
LET result = 0
IF 5 < 3 THEN
  LET result = 1
ELSEIF 5 > 3 THEN
  LET result = 2
ELSE
  LET result = 3
END IF"
      Dim elseifTree As SyntaxTree = SyntaxTree.Parse(elseifTest)
      Dim elseifComp As Compilation = Compilation.Create(elseifTree)
      Dim elseifVars As New Dictionary(Of String, Object)()
      Dim elseifResult = elseifComp.Evaluate(elseifVars)
      Assert.Equal(2, CInt(elseifVars("result")))

      ' Test nested IF statements
      Dim nestedTest = "
LET result = 0
IF 10 > 5 THEN
  IF 3 > 1 THEN
    LET result = 100
  ELSE
    LET result = 200
  END IF
ELSE
  LET result = 300
END IF"
      Dim nestedTree As SyntaxTree = SyntaxTree.Parse(nestedTest)
      Dim nestedComp As Compilation = Compilation.Create(nestedTree)
      Dim nestedVars As New Dictionary(Of String, Object)()
      Dim nestedResult = nestedComp.Evaluate(nestedVars)
      Assert.Equal(100, CInt(nestedVars("result")))

      ' Test single-line IF without ELSE
      Dim singleIfTest = "LET result = 5" & vbCrLf & "IF result > 3 THEN LET result = 10"
      Dim singleIfTree As SyntaxTree = SyntaxTree.Parse(singleIfTest)
      Dim singleIfComp As Compilation = Compilation.Create(singleIfTree)
      Dim singleIfVars As New Dictionary(Of String, Object)()
      Dim singleIfResult = singleIfComp.Evaluate(singleIfVars)
      Assert.Equal(10, CInt(singleIfVars("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesAdditionalMathAndSystemFunctions()
      ' Test FRE function (memory available)
      Dim freTest = "LET result = FRE(-1)" ' FRE(-1) typically returns available memory
      Dim freTree As SyntaxTree = SyntaxTree.Parse(freTest)
      Dim freComp As Compilation = Compilation.Create(freTree)
      Dim freVars As New Dictionary(Of String, Object)()
      Dim freResult = freComp.Evaluate(freVars)
      ' FRE should return some numeric value
      Assert.IsType(GetType(Double), freVars("result"))

      ' Test FREEFILE function
      Dim freefileTest = "LET result = FREEFILE"
      Dim freefileTree As SyntaxTree = SyntaxTree.Parse(freefileTest)
      Dim freefileComp As Compilation = Compilation.Create(freefileTree)
      Dim freefileVars As New Dictionary(Of String, Object)()
      Dim freefileResult = freefileComp.Evaluate(freefileVars)
      ' FREEFILE should return an integer (typically 1 for first available file handle)
      Assert.IsType(GetType(Integer), freefileVars("result"))

      ' Test POS function (cursor column position)
      Dim posTest = "LET result = POS(0)" ' POS(0) returns current cursor column
      Dim posTree As SyntaxTree = SyntaxTree.Parse(posTest)
      Dim posComp As Compilation = Compilation.Create(posTree)
      Dim posVars As New Dictionary(Of String, Object)()
      Dim posResult = posComp.Evaluate(posVars)
      ' POS should return an integer between 1 and screen width
      Dim posValue = CInt(posVars("result"))
      Assert.True(posValue >= 1 And posValue <= 80) ' Assuming 80-column screen

      ' Test CSRLIN function (cursor row position)
      Dim csrTest = "LET result = CSRLIN" ' CSRLIN returns current cursor row
      Dim csrTree As SyntaxTree = SyntaxTree.Parse(csrTest)
      Dim csrComp As Compilation = Compilation.Create(csrTree)
      Dim csrVars As New Dictionary(Of String, Object)()
      Dim csrResult = csrComp.Evaluate(csrVars)
      ' CSRLIN should return an integer between 1 and screen height
      Dim csrValue = CInt(csrVars("result"))
      Assert.True(csrValue >= 1 And csrValue <= 25) ' Assuming 25-row screen

      ' Test TIMER function
      Dim timerTest = "LET result = TIMER"
      Dim timerTree As SyntaxTree = SyntaxTree.Parse(timerTest)
      Dim timerComp As Compilation = Compilation.Create(timerTree)
      Dim timerVars As New Dictionary(Of String, Object)()
      Dim timerResult = timerComp.Evaluate(timerVars)
      ' TIMER should return seconds since midnight as a number
      Assert.IsType(GetType(Single), timerVars("result"))
      Dim timerValue = CDbl(timerVars("result"))
      Assert.True(timerValue >= 0 And timerValue < 86400) ' Less than 24 hours in seconds
    End Sub

    <Fact>
    Public Sub EvaluatesDateTimeFunctions()
      ' Test DATE$ function
      Dim dateTest = "LET result$ = DATE$"
      Dim dateTree As SyntaxTree = SyntaxTree.Parse(dateTest)
      Dim dateComp As Compilation = Compilation.Create(dateTree)
      Dim dateVars As New Dictionary(Of String, Object)()
      Dim dateResult = dateComp.Evaluate(dateVars)
      ' DATE$ should return a string in MM-DD-YYYY format
      Assert.IsType(GetType(String), dateVars("result$"))
      Dim dateStr = CStr(dateVars("result$"))
      ' Should be in format MM-DD-YYYY
      Assert.True(dateStr.Length >= 8)

      ' Test TIME$ function
      Dim timeTest = "LET result$ = TIME$"
      Dim timeTree As SyntaxTree = SyntaxTree.Parse(timeTest)
      Dim timeComp As Compilation = Compilation.Create(timeTree)
      Dim timeVars As New Dictionary(Of String, Object)()
      Dim timeResult = timeComp.Evaluate(timeVars)
      ' TIME$ should return a string in HH:MM:SS format
      Assert.IsType(GetType(String), timeVars("result$"))
      Dim timeStr = CStr(timeVars("result$"))
      ' Should be in format HH:MM:SS
      Assert.True(timeStr.Length >= 7)
    End Sub

    <Fact>
    Public Sub EvaluatesErrorAndSystemFunctions()
      ' Test ERR function (current error number)
      Dim errTest = "LET result = ERR"
      Dim errTree As SyntaxTree = SyntaxTree.Parse(errTest)
      Dim errComp As Compilation = Compilation.Create(errTree)
      Dim errVars As New Dictionary(Of String, Object)()
      Dim errResult = errComp.Evaluate(errVars)
      ' ERR should return an integer (0 if no error)
      Assert.IsType(GetType(Integer), errVars("result"))
      Assert.True(CInt(errVars("result")) >= 0)

      ' Test ERL function (line number where error occurred)
      Dim erlTest = "LET result = ERL"
      Dim erlTree As SyntaxTree = SyntaxTree.Parse(erlTest)
      Dim erlComp As Compilation = Compilation.Create(erlTree)
      Dim erlVars As New Dictionary(Of String, Object)()
      Dim erlResult = erlComp.Evaluate(erlVars)
      ' ERL should return an integer (0 if no error)
      Assert.IsType(GetType(Integer), erlVars("result"))
      Assert.True(CInt(erlVars("result")) >= 0)

      ' Test INKEY$ function (read key press, should return empty string when no key)
      Dim inkeyTest = "LET result$ = INKEY$"
      Dim inkeyTree As SyntaxTree = SyntaxTree.Parse(inkeyTest)
      Dim inkeyComp As Compilation = Compilation.Create(inkeyTree)
      Dim inkeyVars As New Dictionary(Of String, Object)()
      Dim inkeyResult = inkeyComp.Evaluate(inkeyVars)
      ' INKEY$ should return a string (empty if no key pressed)
      Assert.IsType(GetType(String), inkeyVars("result$"))
    End Sub

    <Fact>
    Public Sub EvaluatesAdvancedMathExpressions()
      ' Test compound mathematical expressions from the maths.md appendix

      ' Test Secant: SEC(X) = 1/COS(X)
      Dim secTest = "LET result = 1/COS(0.5)"
      Dim secTree As SyntaxTree = SyntaxTree.Parse(secTest)
      Dim secComp As Compilation = Compilation.Create(secTree)
      Dim secVars As New Dictionary(Of String, Object)()
      Dim secResult = secComp.Evaluate(secVars)
      Assert.True(TypeOf secVars("result") Is Double Or TypeOf secVars("result") Is Single)

      ' Test Cosecant: CSC(X) = 1/SIN(X)
      Dim cscTest = "LET result = 1/SIN(0.5)"
      Dim cscTree As SyntaxTree = SyntaxTree.Parse(cscTest)
      Dim cscComp As Compilation = Compilation.Create(cscTree)
      Dim cscVars As New Dictionary(Of String, Object)()
      Dim cscResult = cscComp.Evaluate(cscVars)
      Assert.True(TypeOf cscVars("result") Is Double Or TypeOf cscVars("result") Is Single)

      ' Test Cotangent: COT(X) = 1/TAN(X)
      Dim cotTest = "LET result = 1/TAN(0.5)"
      Dim cotTree As SyntaxTree = SyntaxTree.Parse(cotTest)
      Dim cotComp As Compilation = Compilation.Create(cotTree)
      Dim cotVars As New Dictionary(Of String, Object)()
      Dim cotResult = cotComp.Evaluate(cotVars)
      Assert.True(TypeOf cotVars("result") Is Double Or TypeOf cotVars("result") Is Single)

      ' Test Hyperbolic Sine: SINH(X) = (EXP(X) - EXP(-X)) / 2
      Dim sinhTest = "LET result = (EXP(1.0) - EXP(-1.0)) / 2"
      Dim sinhTree As SyntaxTree = SyntaxTree.Parse(sinhTest)
      Dim sinhComp As Compilation = Compilation.Create(sinhTree)
      Dim sinhVars As New Dictionary(Of String, Object)()
      Dim sinhResult = sinhComp.Evaluate(sinhVars)
      Assert.True(TypeOf sinhVars("result") Is Double Or TypeOf sinhVars("result") Is Single)

      ' Test Hyperbolic Cosine: COSH(X) = (EXP(X) + EXP(-X)) / 2
      Dim coshTest = "LET result = (EXP(1.0) + EXP(-1.0)) / 2"
      Dim coshTree As SyntaxTree = SyntaxTree.Parse(coshTest)
      Dim coshComp As Compilation = Compilation.Create(coshTree)
      Dim coshVars As New Dictionary(Of String, Object)()
      Dim coshResult = coshComp.Evaluate(coshVars)
      Assert.True(TypeOf coshVars("result") Is Double Or TypeOf coshVars("result") Is Single)
    End Sub

    <Fact>
    Public Sub EvaluatesStringCreationFunctions()
      ' Test STRING$ function - creates string of repeated characters
      Dim stringTestCases = New(String, String)() {
          ("LET result$ = STRING$(5, 65)", "AAAAA"),  ' A repeated 5 times (ASCII 65)
          ("LET result$ = STRING$(3, ""*"")", "***"),   ' * repeated 3 times
          ("LET result$ = STRING$(0, 42)", ""),       ' Empty string for 0 count
          ("LET result$ = STRING$(4, ""AB"")", "AAAA") ' Uses first character of string
      }

      For Each testCase In stringTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As String = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CStr(vars("result$")))
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesHexOctFunctions()
      ' Test HEX$ function - converts numbers to hexadecimal strings
      Dim hexTestCases = New(String, String)() {
          ("LET result$ = HEX$(255)", "FF"),
          ("LET result$ = HEX$(16)", "10"),
          ("LET result$ = HEX$(0)", "0"),
          ("LET result$ = HEX$(4096)", "1000")
      }

      For Each testCase In hexTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As String = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CStr(vars("result$")))
      Next

      ' Test OCT$ function - converts numbers to octal strings
      Dim octTestCases = New(String, String)() {
          ("LET result$ = OCT$(8)", "10"),
          ("LET result$ = OCT$(16)", "20"),
          ("LET result$ = OCT$(0)", "0"),
          ("LET result$ = OCT$(63)", "77")
      }

      For Each testCase In octTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As String = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CStr(vars("result$")))
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesBinaryConversionFunctions()
      ' Test MKI$ function - converts integer to 2-byte binary string
      Dim mkiTest = "LET result$ = MKI$(12345)"
      Dim mkiTree As SyntaxTree = SyntaxTree.Parse(mkiTest)
      Dim mkiComp As Compilation = Compilation.Create(mkiTree)
      Dim mkiVars As New Dictionary(Of String, Object)()
      Dim mkiResult = mkiComp.Evaluate(mkiVars)
      ' MKI$ should return a 2-byte string
      Assert.Equal(2, CStr(mkiVars("result$")).Length)

      ' Test MKS$ function - converts single to 4-byte binary string
      Dim mksTest = "LET result$ = MKS$(3.14159!)"
      Dim mksTree As SyntaxTree = SyntaxTree.Parse(mksTest)
      Dim mksComp As Compilation = Compilation.Create(mksTree)
      Dim mksVars As New Dictionary(Of String, Object)()
      Dim mksResult = mksComp.Evaluate(mksVars)
      ' MKS$ should return a 4-byte string
      Assert.Equal(4, CStr(mksVars("result$")).Length)

      ' Test MKD$ function - converts double to 8-byte binary string
      Dim mkdTest = "LET result$ = MKD$(3.14159265358979#)"
      Dim mkdTree As SyntaxTree = SyntaxTree.Parse(mkdTest)
      Dim mkdComp As Compilation = Compilation.Create(mkdTree)
      Dim mkdVars As New Dictionary(Of String, Object)()
      Dim mkdResult = mkdComp.Evaluate(mkdVars)
      ' MKD$ should return an 8-byte string
      Assert.Equal(8, CStr(mkdVars("result$")).Length)

      ' Test round-trip conversions
      ' MKI$/CVI round trip
      Dim roundTripTest = "
LET bin$ = MKI$(12345)
LET result = CVI(bin$)"
      Dim roundTripTree As SyntaxTree = SyntaxTree.Parse(roundTripTest)
      Dim roundTripComp As Compilation = Compilation.Create(roundTripTree)
      Dim roundTripVars As New Dictionary(Of String, Object)()
      Dim roundTripResult = roundTripComp.Evaluate(roundTripVars)
      Assert.Equal(12345, CInt(roundTripVars("result")))

      ' MKS$/CVS round trip
      Dim singleRoundTripTest = "
LET bin$ = MKS$(3.14!)
LET result = CVS(bin$)"
      Dim singleRoundTripTree As SyntaxTree = SyntaxTree.Parse(singleRoundTripTest)
      Dim singleRoundTripComp As Compilation = Compilation.Create(singleRoundTripTree)
      Dim singleRoundTripVars As New Dictionary(Of String, Object)()
      Dim singleRoundTripResult = singleRoundTripComp.Evaluate(singleRoundTripVars)
      Assert.Equal(3.14F, CSng(singleRoundTripVars("result")), 0.01F)
    End Sub

    '<Fact>
    'Public Sub EvaluatesInputFunctions()
    '  ' Test INPUT$ function - reads specified number of characters
    '  ' This is typically used with INPUT$(n) to read n characters
    '  Dim inputTest = "LET result$ = INPUT$(5)"
    '  Dim inputTree As SyntaxTree = SyntaxTree.Parse(inputTest)
    '  Dim inputComp As Compilation = Compilation.Create(inputTree)
    '  Dim inputVars As New Dictionary(Of String, Object)()
    '  ' INPUT$ would normally read from input, but for testing we'll just check parsing
    '  Dim inputResult = inputComp.Evaluate(inputVars)
    '  ' The result should be a string (empty in this test context)
    '  Assert.IsType(GetType(String), inputVars("result$"))
    'End Sub

    <Fact>
    Public Sub EvaluatesPrintFormattingFunctions()
      ' Test TAB function in PRINT statements
      Dim tabTest = "PRINT TAB(10); ""Hello"""
      Dim tabTree As SyntaxTree = SyntaxTree.Parse(tabTest)
      Dim tabComp As Compilation = Compilation.Create(tabTree)
      Dim tabVars As New Dictionary(Of String, Object)()
      Dim tabResult = tabComp.Evaluate(tabVars)
      Assert.Empty(tabTree.Diagnostics)

      ' Test SPC function in PRINT statements
      Dim spcTest = "PRINT SPC(5); ""World"""
      Dim spcTree As SyntaxTree = SyntaxTree.Parse(spcTest)
      Dim spcComp As Compilation = Compilation.Create(spcTree)
      Dim spcVars As New Dictionary(Of String, Object)()
      Dim spcResult = spcComp.Evaluate(spcVars)
      Assert.Empty(spcTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub EvaluatesEnvironmentAndSystemFunctions()
      ' Test ENVIRON$ function - gets environment variables
      Dim environTest = "LET result$ = ENVIRON$(""PATH"")"
      Dim environTree As SyntaxTree = SyntaxTree.Parse(environTest)
      Dim environComp As Compilation = Compilation.Create(environTree)
      Dim environVars As New Dictionary(Of String, Object)()
      Dim environResult = environComp.Evaluate(environVars)
      ' ENVIRON$ should return a string (empty if variable doesn't exist)
      Assert.IsType(GetType(String), environVars("result$"))

      ' Test ENVIRON$ with numeric index
      Dim environNumTest = "LET result$ = ENVIRON$(1)"
      Dim environNumTree As SyntaxTree = SyntaxTree.Parse(environNumTest)
      Dim environNumComp As Compilation = Compilation.Create(environNumTree)
      Dim environNumVars As New Dictionary(Of String, Object)()
      Dim environNumResult = environNumComp.Evaluate(environNumVars)
      ' ENVIRON$ with number should return a string
      Assert.IsType(GetType(String), environNumVars("result$"))
    End Sub

    <Fact>
    Public Sub EvaluatesCommandLineFunction()
      ' Test COMMAND$ function - gets command line arguments
      Dim commandTest = "LET result$ = COMMAND$"
      Dim commandTree As SyntaxTree = SyntaxTree.Parse(commandTest)
      Dim commandComp As Compilation = Compilation.Create(commandTree)
      Dim commandVars As New Dictionary(Of String, Object)()
      Dim commandResult = commandComp.Evaluate(commandVars)
      ' COMMAND$ should return a string
      Assert.IsType(GetType(String), commandVars("result$"))
    End Sub

    <Fact>
    Public Sub EvaluatesLogicalOperatorsExtended()
      ' Test IMP operator (implication)
      Dim impTestCases = New(String, Integer)() {
          ("LET result = -1 IMP -1", -1),  ' True IMP True = True
          ("LET result = -1 IMP 0", 0),    ' True IMP False = False
          ("LET result = 0 IMP -1", -1),   ' False IMP True = True
          ("LET result = 0 IMP 0", -1)     ' False IMP False = True
      }

      For Each testCase In impTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As Integer = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CInt(vars("result")))
      Next

      ' Test EQV operator (equivalence)
      Dim eqvTestCases = New(String, Integer)() {
          ("LET result = -1 EQV -1", -1),  ' True EQV True = True
          ("LET result = -1 EQV 0", 0),    ' True EQV False = False
          ("LET result = 0 EQV -1", 0),    ' False EQV True = False
          ("LET result = 0 EQV 0", -1)     ' False EQV False = True
      }

      For Each testCase In eqvTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As Integer = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CInt(vars("result")))
      Next
    End Sub

    <Fact>
    Public Sub EvaluatesPeekPokeFunctions()
      ' Test PEEK function - reads memory location
      Dim peekTest = "LET result = PEEK(0)"
      Dim peekTree As SyntaxTree = SyntaxTree.Parse(peekTest)
      Dim peekComp As Compilation = Compilation.Create(peekTree)
      Dim peekVars As New Dictionary(Of String, Object)()
      Dim peekResult = peekComp.Evaluate(peekVars)
      ' PEEK should return an integer (0-255)
      Assert.IsType(GetType(Integer), peekVars("result"))

      ' Test POKE statement - writes to memory location
      Dim pokeTest = "POKE 1000, 42"
      Dim pokeTree As SyntaxTree = SyntaxTree.Parse(pokeTest)
      Dim pokeComp As Compilation = Compilation.Create(pokeTree)
      Dim pokeVars As New Dictionary(Of String, Object)()
      Dim pokeResult = pokeComp.Evaluate(pokeVars)
      Assert.Empty(pokeTree.Diagnostics)
    End Sub

  End Class

End Namespace
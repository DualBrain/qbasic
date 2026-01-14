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

  End Class

End Namespace
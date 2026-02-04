Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

Imports QBLib

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
    Public Sub EvaluatesDefIntAllLetters()
      Dim text = "
DEFINT A-Z
a = 42
b = 100
c = 200
z = 999
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      
      ' All variables should be integers (type coercion)
      Assert.Equal($"{42}", $"{variables("a")}")
      Assert.Equal($"{100}", $"{variables("b")}")
      Assert.Equal($"{200}", $"{variables("c")}")
      Assert.Equal($"{999}", $"{variables("z")}")
    End Sub

    ' <Fact>
    ' Public Sub EvaluatesDefStrSingleLetter()
    '   Dim text = "
    ' DEFSTR S
    ' s = ""hello""
    ' s2 = ""world""
    ' t = 12345  ' Should be converted to string
    ' "

    '   Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
    '   Dim compilation As Compilation = Compilation.Create(syntaxTree)
    '   Dim variables = New Dictionary(Of String, Object)()
    '   Dim result = compilation.Evaluate(variables)
    '   
    '   ' Variables starting with S should be strings
    '   Assert.Equal("hello", $"{variables("s")}")
    '   Assert.Equal("world", $"{variables("s2")}")
    '   Assert.Equal("12345", $"{variables("t")}")  ' Numeric to string conversion
    ' End Sub

    <Fact>
    Public Sub EvaluatesDefSngMultipleRanges()
      Dim text = "
DEFSNG A-M
DEFSNG X-Z
a = 3.14
m = 2.71
x = 1.5
z = 9.99
b = 100  ' Should be single (not in range)
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      
      ' Variables in A-M and X-Z should be single precision
      Assert.Equal($"{3.14}", $"{variables("a")}")
      Assert.Equal($"{2.71}", $"{variables("m")}")
      Assert.Equal($"{1.5}", $"{variables("x")}")
      Assert.Equal($"{9.99}", $"{variables("z")}")
      
      ' Variable b should be default type (single, since it's the default)
      Assert.Equal($"{100.0}", $"{variables("b")}")
    End Sub

<Fact>
    Public Sub EvaluatesDefDblPartialRange()
      Dim text = "
DEFDBL F-P
f = 123.456789
g = 987.654321
a = 42  ' Should be default type
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      ' Variables in F-P should be double precision
      Assert.Equal(123.456789, CDbl(variables("f")), 0.0001)
      Assert.Equal(987.654321, CDbl(variables("g")), 0.0001)

      ' Variable a should be default type (single)
      Assert.Equal($"{42}", $"{variables("a")}")
    End Sub

    <Fact>
    Public Sub EvaluatesDefLngOverlappingRanges()
      Dim text = "
DEFLNG A-C
DEFLNG B-E
x = 123456
d = 987654
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      
      ' All should be long integers
      Assert.Equal($"{123456}", $"{variables("x")}")
      Assert.Equal($"{987654}", $"{variables("d")}")
    End Sub

    ' <Fact>
    ' Public Sub EvaluatesDefStmtCaseSensitivity()
    '   Dim text = "
    '   defint a-z
    '   DEFSTR s
    '   A = 10
    '   s = ""test""
    '   "

    '   Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
    '   Dim compilation As Compilation = Compilation.Create(syntaxTree)
    '   Dim variables = New Dictionary(Of String, Object)()
    '   Dim result = compilation.Evaluate(variables)
    '   
    '   ' Should work regardless of case
    '   Assert.Equal($"{10}", $"{variables("A")}")
    '   Assert.Equal("test", $"{variables("s")}")
    ' End Sub

    <Fact>
    Public Sub EvaluatesDimAsInteger()
      Dim text = "
DIM x AS INTEGER
x = 42
y = 100
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      
      ' Variables should be integers
      Assert.Equal(42, CInt(variables("x")))
      Assert.Equal(100, CInt(variables("y")))
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsLong()
      Dim text = "
DIM a AS LONG
a = 2147483647
b = -2147483648
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      
      ' Variables should be long integers
      Assert.Equal(2147483647, CLng(variables("a")))
      Assert.Equal(-2147483648, CLng(variables("b")))
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsSingle()
      Dim text = "
DIM s AS SINGLE
s = 3.14159
t = 2.71828
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      ' Variables should be single precision
      Assert.Equal(3.14159, CSng(variables("s")), 0.0001)
      Assert.Equal(2.71828, CSng(variables("t")), 0.0001)
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsDouble()
      Dim text = "
DIM d AS DOUBLE
d = 1.23456789012345
e = 9.87654321098765
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      ' Variables should be double precision
      Assert.Equal(1.23456789012345, CDbl(variables("d")), 0.0001)
      Assert.Equal(9.87654321098765, CSng(variables("e")), 0.0001)
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsString()
      Dim text = "
DIM strVar AS STRING
strVar = ""hello""
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      ' Variables should be strings
      Assert.Equal("hello", variables("strVar"))
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsMultipleVariables()
      Dim text = "
DIM intVar AS INTEGER, strVar AS STRING, sngVar AS SINGLE
intVar = 42
strVar = ""test""
sngVar = 3.14
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      
      ' Different variable types should be preserved
      Assert.Equal(42, CInt(variables("intVar")))
      Assert.Equal("test", variables("strVar"))
      Assert.Equal(3.14, CSng(variables("sngVar")), 0.0001)
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsWithDef()
      Dim text = "
DEFINT X-Z
DIM x AS INTEGER  ' Should use explicit DIM type
DIM y AS SINGLE   ' Should use default DEF type
x = 100
y = 100
z = 100
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      
      ' x should be INTEGER from DIM AS (highest precedence)
      ' y should be SINGLE from DEFINT (lower precedence than DIM AS)
      ' z should be INTEGER from DEFINT
      Assert.Equal(100, CInt(variables("x")))
      Assert.Equal(100, CSng(variables("y")))
      Assert.Equal(100, CInt(variables("z")))
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsStringArray()
      Dim text = "
DIM strArray(3) AS STRING
strArray(0) = ""first""
strArray(1) = ""second""
strArray(2) = ""third""
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      ' String array should work correctly
      Dim strArray = CType(variables("strArray"), List(Of Object))
      Assert.Equal("first", strArray(0))
      Assert.Equal("second", strArray(1))
      Assert.Equal("third", strArray(2))
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsDoubleArray()
      Dim text = "
DIM dblArray(2) AS DOUBLE
dblArray(0) = 1.23456789012345
dblArray(1) = 9.87654321098765
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      ' Double array should work correctly
      Dim dblArray = CType(variables("dblArray"), List(Of Object))
      Assert.Equal(1.23456789012345, CDbl(dblArray(0)))
      Assert.Equal(9.87654321098765, CDbl(dblArray(1)))
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsOverwritesDef()
      Dim text = "
DEFSNG A-C
DIM a AS SINGLE  ' Should override DEFSNG for 'a'
DIM b AS DOUBLE  ' Should override DEFSNG for 'b'
a = 1.1
b = 2.2
c = 3.3  ' Should remain SINGLE from DEFSNG
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      ' DIM AS should override DEF statements
      Assert.Equal(1.1, CSng(variables("a")), 0.0000001)
      Assert.Equal(2.2, CDbl(variables("b")), 0.0000001)
      Assert.Equal(3.3, CSng(variables("c")), 0.0000001)
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsComplexScenario()
      Dim text = "
DIM intCounter AS INTEGER
DIM strName AS STRING
DIM sngTotal AS SINGLE
DIM dblAverage AS DOUBLE

intCounter = 0
strName = ""John""
sngTotal = 0.0
dblAverage = 0.0

' Simulate some calculations
FOR i = 1 TO 5
  intCounter = intCounter + i
  sngTotal = sngTotal + i * 1.5
  dblAverage = dblAverage + i * 2.5
NEXT i
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      
      ' All variable types should be preserved correctly
      Assert.Equal(15, CInt(variables("intCounter")))
      Assert.Equal("John", variables("strName"))
      Assert.Equal(22.5, CSng(variables("sngTotal")))
      Assert.Equal(37.5, CDbl(variables("dblAverage")))
    End Sub

    <Fact>
    Public Sub EvaluatesDimAsCaseSensitivity()
      Dim text = "
dim x as integer
dim y as long
dim z as single
"

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      
      ' Should work regardless of case
      Assert.Equal(0, CInt(variables("x")))
      Assert.Equal(0, CLng(variables("y")))
      Assert.Equal(0.0, CSng(variables("z")))
    End Sub

    <Fact>
    Public Sub EvaluatesSleepStatement()
      ' Exactly replicate the working ExecutesSimpleProgram test
      Dim text = "start = TIMER
SLEEP 2
slept = TIMER - start"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Dim v = If(variables.ContainsKey("slept"), CDbl(variables("slept")), 0)
      Assert.Equal(2, v, 0.2) ' Allow timing variations
    End Sub

    <Fact>
    Public Sub EvaluatesOnTimerEvent()
      ' Test ON TIMER event functionality - timer should interrupt SLEEP
      Dim text = "ON TIMER(1) GOSUB HandleTimer
TIMER ON
started = TIMER
SLEEP 10
slept = TIMER - started
END

HandleTimer:
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Dim v = If(variables.ContainsKey("slept"), CDbl(variables("slept")), 0)
      ' Timer event should interrupt SLEEP after ~1 second
      Assert.Equal(1, v, 0.2) ' Allow timing variations
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
      Dim text0 = "LET result = RND"
      Dim syntaxTree0 As SyntaxTree = SyntaxTree.Parse(text0)
      Dim compilation0 As Compilation = Compilation.Create(syntaxTree0)
      Dim variables0 = New Dictionary(Of String, Object)()
      Dim result0 = compilation0.Evaluate(variables0)
      ' RND should return a value between 0 and 1
      Dim rndValue0 = CDbl(variables0("result"))
      Assert.True(rndValue0 >= 0.0 And rndValue0 < 1.0)

      ' Test RND with positive values (next random)
      Dim text1 = "LET result = RND(1)"
      Dim syntaxTree1 As SyntaxTree = SyntaxTree.Parse(text1)
      Dim compilation1 As Compilation = Compilation.Create(syntaxTree1)
      Dim variables1 = New Dictionary(Of String, Object)()
      Dim result1 = compilation1.Evaluate(variables1)
      ' RND should return a value between 0 and 1
      Dim rndValue1 = CDbl(variables1("result"))
      Assert.True(rndValue1 >= 0.0 And rndValue1 < 1.0)

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

      Dim originalStdoutMode = Video.StdoutMode
      Try
        Video.StdoutMode = True ' Run in stdout mode like --stdout flag
        For Each testCase In printTestCases
          Dim tree As SyntaxTree = SyntaxTree.Parse(testCase)
          Dim comp As Compilation = Compilation.Create(tree)
          Dim vars As New Dictionary(Of String, Object)()
          ' Just verify it parses and evaluates without errors
          Dim evalResult = comp.Evaluate(vars)
          Assert.Empty(tree.Diagnostics)
        Next
      Finally
        Video.StdoutMode = originalStdoutMode ' Restore original mode
      End Try
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
      Dim originalStdoutMode = Video.StdoutMode
      Try
        Video.StdoutMode = True ' Run in stdout mode like --stdout flag
        Dim clsTest = "CLS"
        Dim clsTree As SyntaxTree = SyntaxTree.Parse(clsTest)
        Dim clsComp As Compilation = Compilation.Create(clsTree)
        Dim clsVars As New Dictionary(Of String, Object)()
        Dim clsResult = clsComp.Evaluate(clsVars)
        Assert.Empty(clsTree.Diagnostics)
      Finally
        Video.StdoutMode = originalStdoutMode ' Restore original mode
      End Try
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
LET bin$ = MKS$(3.14)
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
      Dim originalStdoutMode = Video.StdoutMode
      Try
        Video.StdoutMode = True ' Run in stdout mode like --stdout flag

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
      Finally
        Video.StdoutMode = originalStdoutMode ' Restore original mode
      End Try
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

      ' Test ENVIRON statement (setting environment variables)
      Dim environSetTest = "ENVIRON ""QBTEST_VAR=test_value"""
      Dim environSetTree As SyntaxTree = SyntaxTree.Parse(environSetTest)
      Dim environSetComp As Compilation = Compilation.Create(environSetTree)
      Dim environSetVars As New Dictionary(Of String, Object)()
      Dim environSetResult = environSetComp.Evaluate(environSetVars)

      ' Now read it back
      Dim environReadTest = "LET result$ = ENVIRON$(""QBTEST_VAR"")"
      Dim environReadTree As SyntaxTree = SyntaxTree.Parse(environReadTest)
      Dim environReadComp As Compilation = Compilation.Create(environReadTree)
      Dim environReadVars As New Dictionary(Of String, Object)()
      Dim environReadResult = environReadComp.Evaluate(environReadVars)

      Assert.IsType(GetType(String), environReadVars("result$"))
      Assert.Equal("test_value", CStr(environReadVars("result$")))

      ' Test file I/O functions (EOF, LOF, LOC) - these will error since no files are open
      ' but we test that they exist and have correct signatures
      Dim eofTest = "LET result = EOF(1)"
      Dim eofTree As SyntaxTree = SyntaxTree.Parse(eofTest)
      Dim eofComp As Compilation = Compilation.Create(eofTree)
      ' Should compile without errors
      Assert.NotNull(eofComp)

      Dim lofTest = "LET result = LOF(1)"
      Dim lofTree As SyntaxTree = SyntaxTree.Parse(lofTest)
      Dim lofComp As Compilation = Compilation.Create(lofTree)
      Assert.NotNull(lofComp)

      Dim locTest = "LET result = LOC(1)"
      Dim locTree As SyntaxTree = SyntaxTree.Parse(locTest)
      Dim locComp As Compilation = Compilation.Create(locTree)
      Assert.NotNull(locComp)
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
    Public Sub EvaluatesCommandFunctionWithArguments()
      ' Test COMMAND$ function with different argument scenarios

      ' Test with no arguments (should return empty string)
      Dim commandTest1 = "LET result$ = COMMAND$"
      Dim commandTree1 As SyntaxTree = SyntaxTree.Parse(commandTest1)
      Dim commandComp1 As Compilation = Compilation.Create(commandTree1)
      Dim commandVars1 As New Dictionary(Of String, Object)()
      Dim commandResult1 = commandComp1.Evaluate(commandVars1, Nothing)
      Assert.IsType(GetType(String), commandVars1("result$"))
      Assert.Equal("", CStr(commandVars1("result$")))

      ' Test with single argument
      Dim commandVars2 As New Dictionary(Of String, Object)()
      Dim args2 As String() = {"single"}
      Dim commandResult2 = commandComp1.Evaluate(commandVars2, args2)
      Assert.IsType(GetType(String), commandVars2("result$"))
      Assert.Equal("single", CStr(commandVars2("result$")))

      ' Test with multiple arguments
      Dim commandVars3 As New Dictionary(Of String, Object)()
      Dim args3 As String() = {"arg1", "arg2", "arg3"}
      Dim commandResult3 = commandComp1.Evaluate(commandVars3, args3)
      Assert.IsType(GetType(String), commandVars3("result$"))
      Assert.Equal("arg1 arg2 arg3", CStr(commandVars3("result$")))

      ' Test with arguments containing spaces
      Dim commandVars4 As New Dictionary(Of String, Object)()
      Dim args4 As String() = {"hello", "world with spaces", "final"}
      Dim commandResult4 = commandComp1.Evaluate(commandVars4, args4)
      Assert.IsType(GetType(String), commandVars4("result$"))
      Assert.Equal("hello world with spaces final", CStr(commandVars4("result$")))

      ' Test with empty array
      Dim commandVars5 As New Dictionary(Of String, Object)()
      Dim args5 As String() = Array.Empty(Of String)()
      Dim commandResult5 = commandComp1.Evaluate(commandVars5, args5)
      Assert.IsType(GetType(String), commandVars5("result$"))
      Assert.Equal("", CStr(commandVars5("result$")))
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

    <Fact>
    Public Sub EvaluatesLBoundUBoundFunctions()
      ' Test LBOUND function with arrays
      Dim lboundTestCases = New(String, Long)() {
          ("DIM arr(1 TO 5) : LET result = LBOUND(arr)", 1L),
          ("DIM arr(0 TO 10) : LET result = LBOUND(arr)", 0L),
          ("DIM arr(-5 TO 5) : LET result = LBOUND(arr)", -5L),
          ("DIM arr(10 TO 20) : LET result = LBOUND(arr)", 10L)
      }

      For Each testCase In lboundTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As Long = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CLng(vars("result")))
      Next

      ' Test UBOUND function with arrays
      Dim uboundTestCases = New(String, Long)() {
          ("DIM arr(1 TO 5) : LET result = UBOUND(arr)", 5L),
          ("DIM arr(0 TO 10) : LET result = UBOUND(arr)", 10L),
          ("DIM arr(-5 TO 5) : LET result = UBOUND(arr)", 5L),
          ("DIM arr(10 TO 20) : LET result = UBOUND(arr)", 20L)
      }

      For Each testCase In uboundTestCases
        Dim inputText As String = testCase.Item1
        Dim expectedResult As Long = testCase.Item2
        Dim tree As SyntaxTree = SyntaxTree.Parse(inputText)
        Dim comp As Compilation = Compilation.Create(tree)
        Dim vars As New Dictionary(Of String, Object)()
        Dim evalResult = comp.Evaluate(vars)
        Assert.Equal(expectedResult, CLng(vars("result")))
      Next

      ' Test LBOUND/UBOUND on non-array variables (TODO: implement this)

      ' Test with REDIM arrays
      Dim redimTest = "
 DIM arr(1 TO 3)
 LET result1 = LBOUND(arr)
 LET result2 = UBOUND(arr)
 REDIM arr(5 TO 8)
 LET result3 = LBOUND(arr)
 LET result4 = UBOUND(arr)
 "
      Dim redimTree As SyntaxTree = SyntaxTree.Parse(redimTest)
      Dim redimComp As Compilation = Compilation.Create(redimTree)
      Dim redimVars As New Dictionary(Of String, Object)()
      Dim redimResult = redimComp.Evaluate(redimVars)
      Assert.Equal(1L, CLng(redimVars("result1")))
      Assert.Equal(3L, CLng(redimVars("result2")))
      Assert.Equal(5L, CLng(redimVars("result3")))
      Assert.Equal(8L, CLng(redimVars("result4")))
    End Sub

    <Fact>
    Public Sub EvaluatesSelectCaseBasic()
      Dim text = "
SELECT CASE 2
  CASE 1
    result = 10
  CASE 2
    result = 20
  CASE 3
    result = 30
END SELECT
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(20, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesSelectCaseWithMultipleValues()
      Dim text = "
SELECT CASE 5
  CASE 1, 2, 3
    result = 100
  CASE 4, 5, 6
    result = 200
  CASE 7, 8, 9
    result = 300
END SELECT
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(200, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesSelectCaseWithRanges()
      Dim text = "
SELECT CASE 75
  CASE 0 TO 59
    result$ = ""Fail""
  CASE 60 TO 79
    result$ = ""Pass""
  CASE 80 TO 100
    result$ = ""Excellent""
END SELECT
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal("Pass", CStr(variables("result$")))
    End Sub

    <Fact>
    Public Sub EvaluatesSelectCaseWithIsComparisons()
      Dim text = "
SELECT CASE 25
  CASE IS < 13
    result$ = ""Child""
  CASE IS < 20
    result$ = ""Teenager""
  CASE IS >= 65
    result$ = ""Senior""
  CASE ELSE
    result$ = ""Adult""
END SELECT
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal("Adult", CStr(variables("result$")))
    End Sub

    <Fact>
    Public Sub EvaluatesSelectCaseWithCaseElse()
      Dim text = "
SELECT CASE 99
  CASE 1
    result = 1
  CASE 2
    result = 2
  CASE ELSE
    result = 999
END SELECT
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(999, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesSelectCaseWithStringTest()
      Dim text = "
SELECT CASE ""Jane""
  CASE ""John""
    result$ = ""Male""
  CASE ""Jane"", ""Mary""
    result$ = ""Female""
  CASE ELSE
    result$ = ""Unknown""
END SELECT
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal("Female", CStr(variables("result$")))
    End Sub

    <Fact>
    Public Sub EvaluatesSelectCaseExitsAfterFirstMatch()
      Dim text = "
SELECT CASE 2
  CASE 1, 2, 3
    result = result + 10
  CASE 2, 4, 6
    result = result + 20
  CASE 3, 6, 9
    result = result + 30
END SELECT
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      variables("result") = 0
      Dim result = compilation.Evaluate(variables)

      ' Should only match first case (CASE 1, 2, 3) and add 10, not continue to CASE 2, 4, 6
      Assert.Equal(10, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesSelectCaseWithVariableTest()
      Dim text = "
x = 42
SELECT CASE x
  CASE 1 TO 40
    result$ = ""Low""
  CASE 41 TO 80
    result$ = ""Medium""
  CASE 81 TO 100
    result$ = ""High""
END SELECT
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal("Medium", CStr(variables("result$")))
    End Sub

    <Fact>
    Public Sub EvaluatesSelectCaseWithIsEqualComparison()
      Dim text = "
SELECT CASE 4
  CASE IS = 1
    result = 1
  CASE IS = 2
    result = 2
  CASE IS = 4
    result = 4
  CASE ELSE
    result = 999
END SELECT
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(4, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesDoWhileLoop()
      Dim text = "
counter = 0
DO WHILE counter < 3
  counter = counter + 1
LOOP
result = counter
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(3, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesDoUntilLoop()
      Dim text = "
counter = 3
DO UNTIL counter = 0
  counter = counter - 1
LOOP
result = counter
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(0, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesDoLoopWhile()
      Dim text = "
counter = 0
DO
  counter = counter + 1
LOOP WHILE counter < 3
result = counter
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(3, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesDoLoopUntil()
      Dim text = "
counter = 3
DO
  counter = counter - 1
LOOP UNTIL counter = 0
result = counter
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(0, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesDoWhileWithFalseCondition()
      Dim text = "
executed = 0
DO WHILE 1 = 2  ' Always false
  executed = 1
LOOP
result = executed
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(0, CInt(variables("result")))
    End Sub

    <Fact>
    Public Sub EvaluatesDoUntilWithTrueCondition()
      Dim text = "
executed = 0
DO UNTIL 1 = 1  ' Always true
  executed = executed + 1
  IF executed > 5 THEN EXIT DO ' Prevent infinite loop
LOOP
result = executed
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(0, CInt(variables("result"))) ' Should skip loop when condition is initially true
    End Sub

    <Fact>
    Public Sub EvaluatesDoUntilWithStringEvalCondition()
      Dim text = "
key$ = """"
count = 0
DO
  count = count + 1
  IF count > 10 THEN key$ = CHR$(27)
LOOP UNTIL key$ = CHR$(27) ' ESC key
result = count
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(11, CInt(variables("result")))

    End Sub

    <Fact>
    Public Sub EvaluatesGotoAcrossLoopBoundaries()
      Dim text = "
c = 0
for x = 1 to 10
  for y = 1 to 10
    for z = 1 to 10
      if z > 1 then goto skip
      c = c + 1
    next
skip:
  next
next
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(100, CInt(variables("c")))
    End Sub

    <Fact>
    Public Sub EvaluatesSimpleForLoop()
      Dim text = "
total = 0
for i = 1 to 5
  total = total + 1
next i
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      Assert.Equal(5, CInt(variables("total")))
    End Sub

    <Fact>
    Public Sub EvaluatesNestedForLoops()
      Dim text = "
total = 0
for i = 1 to 2
  for j = 1 to 2
    for k = 1 to 2
      total = total + 1
    next k
  next j
next i
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      ' 2 * 2 * 2 = 8 iterations
      Assert.Equal(8, CInt(variables("total")))
    End Sub

    <Fact>
    Public Sub EvaluatesForLoop()
      Dim text = "
sum = 0
for x = 2 to 6
  sum = sum + x
next x
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)

      ' 2+3+4+5+6 = 20
      Assert.Equal(20, CInt(variables("sum")))
    End Sub

    <Fact>
    Public Sub EvaluatesOnComEventSetup()
      ' Test ON COM(n) GOSUB sets up handler
      Dim text = "ON COM(1) GOSUB HandleCom
x = 1"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.True(variables.ContainsKey("x"))
      Assert.Equal($"{1}", $"{variables("x")}") ' Handler not called until COM ON
    End Sub

    <Fact>
    Public Sub EvaluatesOnKeyEventSetup()
      ' Test ON KEY(n) GOSUB sets up handler
      Dim text = "ON KEY(1) GOSUB HandleKey
x = 1
END

HandleKey:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("x")}") ' Handler not called until KEY ON
    End Sub

    <Fact>
    Public Sub EvaluatesOnStrigEventSetup()
      ' Test ON STRIG(n) GOSUB sets up handler
      Dim text = "ON STRIG(0) GOSUB HandleStrig
x = 1
END

HandleStrig:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("x")}") ' Handler not called until STRIG ON
    End Sub

    <Fact>
    Public Sub EvaluatesOnPlayEventSetup()
      ' Test ON PLAY(n) GOSUB sets up handler
      Dim text = "ON PLAY(1) GOSUB HandlePlay
x = 1
END

HandlePlay:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("x")}") ' Handler not called until PLAY ON
    End Sub

    <Fact>
    Public Sub EvaluatesOnPenEventSetup()
      ' Test ON PEN GOSUB sets up handler and RETURN without GOSUB errors
      Dim text = "ON PEN GOSUB HandlePen
PEN ON
x = 1
END

HandlePen:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("x")}") ' PEN events not implemented yet
    End Sub

    <Fact>
    Public Sub EvaluatesComStatement()
      ' Test COM(n) ON/OFF/STOP commands
      Dim text = "ON COM(1) GOSUB HandleCom
COM(1) ON
x = 1
END

HandleCom:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("x")}") ' COM events not implemented yet
    End Sub

    <Fact>
    Public Sub EvaluatesKeyEventStatement()
      ' Test KEY(n) ON/OFF/STOP commands
      Dim text = "ON KEY(1) GOSUB HandleKey
KEY(1) ON
x = 1
END

HandleKey:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("x")}") ' KEY events not implemented yet
    End Sub

    <Fact>
    Public Sub EvaluatesStrigStatement()
      ' Test STRIG(n) ON/OFF/STOP commands
      Dim text = "ON STRIG(0) GOSUB HandleStrig
STRIG(0) ON
x = 1
END

HandleStrig:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("x")}") ' STRIG events not implemented yet
    End Sub

    <Fact>
    Public Sub EvaluatesPlayEventStatement()
      ' Test PLAY(n) ON/OFF/STOP commands
      Dim text = "ON PLAY(1) GOSUB HandlePlay
PLAY(1) ON
x = 1
END

HandlePlay:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("x")}") ' PLAY events not implemented yet
    End Sub

    <Fact>
    Public Sub EvaluatesPenStatement()
      ' Test PEN ON/OFF/STOP commands
      Dim text = "ON PEN GOSUB HandlePen
PEN ON
x = 1
END

HandlePen:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim result = compilation.Evaluate(variables)
      Assert.Equal($"{1}", $"{variables("x")}") ' PEN events not implemented yet
    End Sub

    <Fact>
    Public Sub EvaluatesPenStatement_ShouldFail_MissingEndStatement()
      Dim text = "ON PEN GOSUB HandlePen
PEN ON
x = 1

HandlePen:
x = 2
RETURN"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)()
      Dim ex = Assert.ThrowsAny(Of Exception)(Sub() compilation.Evaluate(variables))
      Assert.Contains("RETURN without GOSUB", ex.Message)
    End Sub

  End Class

End Namespace
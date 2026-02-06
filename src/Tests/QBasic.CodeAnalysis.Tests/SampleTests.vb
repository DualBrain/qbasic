Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax
Imports QB.CodeAnalysis.Binding

Imports QBLib

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class SampleTests

    Private Function Evaluate(text As String) As (Result As EvaluationResult, Output As String, Variables As Dictionary(Of String, Object))
      Using sw As New IO.StringWriter
        Dim originalOut = Console.Out
        Dim originalStdoutMode = Video.StdoutMode
        Try
          Video.StdoutMode = True ' Run in stdout mode like --stdout flag
          Console.SetOut(sw)
          Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
          Dim compilation As Compilation = Compilation.Create(syntaxTree)
          Dim variables = New Dictionary(Of String, Object)
          Dim result = compilation.Evaluate(variables)
          Dim output = sw.ToString
          Return (result, output, variables)
        Finally
          Console.SetOut(originalOut)
          Video.StdoutMode = originalStdoutMode ' Restore original mode
        End Try
      End Using
    End Function

    <Fact>
    Public Sub Sample_Hex_Hnn()

      ' Name: &Hnn

      Dim sample = "
    PRINT &H76
"

      Dim expected = "118"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Hex_HnnF()

      ' Name: &HnnF

      Dim sample = "
    PRINT &H32F
"

      Dim expected = "815"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Hex_O1234()

      ' Name: &O1234

      Dim sample = "
    PRINT &O1234
"

      Dim expected = "668"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Hex_O347()

      ' Name: &O347

      Dim sample = "
    PRINT &O347
"

      Dim expected = "231"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ABS_2()

      ' Name: ABS (2)

      Dim sample = "
PRINT ""Absolute value of -3 * 5 is""; ABS(-3 * 5)
PRINT ""Absolute value of 3 * 5 is""; ABS(3 *5)
"

      Dim expected = $"Absolute value of -3 * 5 is 15{vbCrLf}Absolute value of 3 * 5 is 15"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ABS()

      ' Name: ABS

      Dim sample = "
    PRINT ABS(7*(-5))
"

      Dim expected = "35"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Add_2_strings()

      ' Name: Add 2 strings

      Dim sample = "
    A$=""HELLO""
    B$=""WORLD""
    C$=A$+"" ""+B$
    PRINT C$
"

      Dim expected = "HELLO WORLD"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Addition_1()

      ' Name: Addition (1)

      Dim sample = "
    PRINT 2+2
"

      Dim expected = "4"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Addition_2()

      ' Name: Addition (2)

      Dim sample = "
    A = 1: B=2
    B = B + A
    PRINT B
"

      Dim expected = "3"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Addition_3()

      ' Name: Addition (3)

      Dim sample = "
    x = 3
    PRINT 2 + X
"

      Dim expected = "5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_AND_1()

      ' Name: AND (1)

      Dim sample = "
    PRINT 63 AND 16
"

      Dim expected = "16"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_AND_2()

      ' Name: AND (2)

      Dim sample = "
    PRINT 15 AND 14
"

      Dim expected = "14"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_AND_3()

      ' Name: AND (3)

      Dim sample = "
    PRINT -1 AND 8
"

      Dim expected = "8"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ASC_1()

      ' Name: ASC (1)

      Dim sample = "
    X$=""TEN""
    PRINT ASC(x$)
"

      Dim expected = "84"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ASC_2()

      ' Name: ASC (2)

      Dim sample = "
ON ERROR GOTO Handler
X$=""""
PRINT ASC(x$)
END
Handler:
  PRINT ""ERR =""; ERR
  END
"

      Dim expected = "ERR = 5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(0, result.Diagnostics.Count)
      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ASC_3()

      ' Name: ASC (3)

      Dim sample = "
    stringvar$ = ""ABC""
    PRINT ASC(stringvar$)
    PRINT ASC(""abc"")
"

      Dim expected = $"65{vbCrLf} 97"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ATN_2()

      ' Name: ATN (2)

      Dim sample = "
    value = TAN(.7854)   'Tangent of pi/4
    PRINT ""Arctangent is""; ATN(value)
"

      Dim expected = "Arctangent is .7854"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ATN()

      ' Name: ATN

      Dim sample = "
    X = 3
    PRINT ATN(X)
"

      Dim expected = "1.24905"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CALL_1()

      ' Name: CALL (1)

      Dim sample = "
DECLARE SUB SwapVal (a,b)

a = 1
b = 2
CALL SwapVal(a, b)
PRINT a; b
END
    
SUB SwapVal(x,y)
  temp = x
  x = y
  y = temp
END SUB
"

      Dim expected = "2 1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CDBL_1()

      ' Name: CDBL (1)

      Dim sample = "
A=454.67
PRINT A;CDBL(A)
"

      Dim expected = "454.67  454.670013427734"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CDBL_2()

      ' Name: CDBL (2)

      Dim sample = "
A$=""454.67""
PRINT CDBL(A$)
"

      Dim expected = "Type mismatch"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CDBL_3()

      ' Name: CDBL (3)

      Dim sample = "
PRINT 5 / 6
PRINT CDBL(5 / 6)
"

      Dim expected = $".8333333{vbCrLf} .8333333333333334"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CHRStr_2()

      ' Name: CHR$ (2)

      Dim sample = "
    ' Display the ASCII and extended ASCII character sets
    
    FOR i = 65 TO 75
      PRINT i; CHR$(i)
    NEXT i
"

      Dim expected = $"65 A{vbCrLf} 66 B{vbCrLf} 67 C{vbCrLf} 68 D{vbCrLf} 69 E{vbCrLf} 70 F{vbCrLf} 71 G{vbCrLf} 72 H{vbCrLf} 73 I{vbCrLf} 74 J{vbCrLf} 75 K"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CHRStr()

      ' Name: CHR$

      Dim sample = "
    PRINT CHR$(66);
"

      Dim expected = "B"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CINT_1()

      ' Name: CINT (1)

      Dim sample = "
    PRINT CINT(45.67)
"

      Dim expected = "46"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CINT_2()

      ' Name: CINT (2)

      Dim sample = "
    PRINT CINT(-32768.1)
"

      Dim expected = "-32768"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CINT_3()

      ' Name: CINT (3)

      Dim sample = "
    PRINT CINT(32767.1)
"

      Dim expected = "32767"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CINT_4()

      ' Name: CINT (4)

      Dim sample = "
    PRINT CINT(34.51)
    PRINT CINT(34.49)
"

      Dim expected = $"35{vbCrLf} 34"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CLEAR_1()

      ' Name: CLEAR (1)

      Dim sample = "
    COMMON A$, B
    A$ = ""HELLO"": B=100: C$ = ""WORLD"": D=200
    CLEAR
    PRINT ""|"";A$;C$;B+D;""|""
"

      Dim expected = "| 0 |"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CLEAR_2()

      ' Name: CLEAR (2)

      Dim sample = "
    COMMON A$, B
    A$ = ""HELLO"": B=100: C$ = ""WORLD"": D=200
    CLEAR 32768
    PRINT ""|"";A$;C$;B+D;""|""
"

      Dim expected = "Overflow line 3"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CLEAR_3()

      ' Name: CLEAR (3)

      Dim sample = "
    COMMON A$, B
    A$ = ""HELLO"": B=100: C$ = ""WORLD"": D=200
    CLEAR ,,2000
    PRINT ""|"";A$;C$;B+D;""|""
"

      Dim expected = "| 0 |"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CLEAR_4()

      ' Name: CLEAR (4)

      Dim sample = "
    COMMON A$, B
    A$ = ""HELLO"": B=100: C$ = ""WORLD"": D=200
    CLEAR ,32768,2000
    PRINT ""|"";A$;C$;B+D;""|""
"

      Dim expected = "| 0 |"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CLEAR_5()

      ' Name: CLEAR (5)

      Dim sample = "
    DIM A(500)
    CLEAR
    DIM A(20)
    PRINT ""SUCCESS""
"

      Dim expected = "SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CLNG_1()

      ' Name: CLNG (1)

      Dim sample = "
    PRINT CLNG(338457.8)
    PRINT CLNG(2147358.28)
"

      Dim expected = $"338458{vbCrLf} 2147358"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_COMMON_1()

      ' Name: COMMON (1)

      Dim sample = "
    COMMON A$
    PRINT ""SUCCESS""
"

      Dim expected = "SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_COMMON_2()

      ' Name: COMMON (2)

      Dim sample = "
    COMMON SHARED A$
    PRINT ""SUCCESS""
"

      Dim expected = "SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_COMMON_3()

      ' Name: COMMON (3)

      Dim sample = "
    ' Set common variables.
    COMMON a, b, c
    ' Create chained program.
    OPEN ""COMMON.BAS"" FOR OUTPUT AS #1
    PRINT #1, ""COMMON x, y, z""
    PRINT #1, ""'COMMON is by variable location, not variable name.""
    PRINT #1, ""PRINT x; y; z""
    CLOSE #1
    ' Set variables.
    a = 1: b = 2: c = 3
    ' Chain to chained target.
    CHAIN ""COMMON.BAS""
"

      Dim expected = "1 2 3"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_COS_1()

      ' Name: COS (1)

      Dim sample = "
    X=2*COS(.4)
    PRINT X
"

      Dim expected = "1.84212"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_COS_2()

      ' Name: COS (2)

      Dim sample = "
    PI=3.141593
    PRINT COS(PI)
    DEGREES=180
    RADIANS=DEGREES*PI/180
    PRINT COS(RADIANS)
"

      Dim expected = $"-1{vbCrLf}-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CSNG()

      ' Name: CSNG

      Dim sample = "
a#=975.3421222#
PRINT A#; CSNG(A#)
"

      Dim expected = "975.3421221999999  975.3421"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CSRLIN_1()

      ' Name: CSRLIN (1)

      Dim sample = "
CLS
PRINT ""HELLO""
Y=CSRLIN
PRINT Y
"

      Dim expected = $"HELLO{vbCrLf} 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CSRLIN_2()

      ' Name: CSRLIN (2)

      Dim sample = "
CLS
Y=CSRLIN
X=POS(0)
LOCATE 24,1
PRINT ""HELLO""
LOCATE Y,X
PRINT Y
"

      Dim expected = $"HELLO{vbCrLf}{vbCrLf} 1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CVD()

      ' Name: CVD

      Dim sample = "
    PRINT CVD(MKD$(5.5))
"

      Dim expected = "5.5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CVI()

      ' Name: CVI

      Dim sample = "
    PRINT CVI(MKI$(5))
"

      Dim expected = "5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_CVS()

      ' Name: CVS

      Dim sample = "
    PRINT CVS(MKS$(5.5))
"

      Dim expected = "5.5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DATA_1()

      ' Name: DATA (1)

      Dim sample = "
    DEFSNG A-Z
    FOR I=1 TO 10
    READ A(I)
    NEXT I
    DATA 3.08,5.19,3.12,3.98,4.24
    DATA 5.08,5.55,4.00,3.16,3.37
    PRINT A(I)
"

      Dim expected = "3.37"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DATA_2()

      ' Name: DATA (2)

      Dim sample = "
    PRINT ""CITY"",""STATE"",""ZIP""
    READ C$,S$,Z
    DATA ""DENVER,"",""COLORADO"",80211
    PRINT C$,S$,Z
"

      Dim expected = $"CITY          STATE         ZIP{vbCrLf}DENVER,       COLORADO       80211"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DATEStr_Statement()

      ' Name: DATE$ - Statement()

      Dim sample = "
    DATE$ = ""01-01-89""
"

      Dim expected = "Permission Denied line 1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DEF_FN()

      ' Name: DEF FN

      Dim sample = "
R=1:S=2
DEF FNAB(X,Y)=X^3/Y^2
T=FNAB(R,S)
PRINT T
"

      Dim expected = ".25"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DEFDBL()

      ' Name: DEFDBL

      Dim sample = "
  DEFINT I-N,W-Z
  W=5
  PRINT W
"

      Dim expected = "5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DEFINT()

      ' Name: DEFINT

      Dim sample = "
DEFINT I-N,W-Z
W=5.555
PRINT W
"

      Dim expected = "6"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DEFSNG()

      ' Name: DEFSNG

      Dim sample = "
DEFINT I-N,W-Z
W=5
PRINT W
"

      Dim expected = "5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DEFSTR()

      ' Name: DEFSTR

      Dim sample = "
DEFSTR A
A=""120#""
B=3.5
PRINT A
PRINT B
"

      Dim expected = $"120#{vbCrLf} 3.5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DIM_1()

      ' Name: DIM (1)

      Dim sample = "
    DIM A(20)
    FOR I=0 TO 20
    READ A(I)
    NEXT I
    PRINT A(I)
    DATA 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20
"

      Dim expected = "20"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(0, result.Diagnostics.Count)
      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DIM_2()

      ' Name: DIM (2)

      Dim sample = "
    DIM B(250)
    DIM B(3,4)
"

      Dim expected = "Duplicate Definition line 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(0, result.Diagnostics.Count)
      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Division_By_Zero_1()

      ' Name: Division By Zero (1)

      Dim sample = "
ON ERROR GOTO Handler
A=100/0
PRINT A
PRINT ""SUCCESS""
END
Handler:
  PRINT ""ERR =""; ERR
"

      Dim expected = $"ERR = 11"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(0, result.Diagnostics.Count)
      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Division_By_Zero_2()

      ' Name: Division By Zero (2)

      Dim sample = "
    A=0^-1
    PRINT A
    PRINT ""SUCCESS""
"

      Dim expected = $"Division by zero{vbCrLf} 1.701412E+38{vbCrLf}SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(0, result.Diagnostics.Count)
      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Division()

      ' Name: Division

      Dim sample = "
    A = 100: B=5
    B = A / B
    PRINT B
"

      Dim expected = "20"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Double_1()

      ' Name: Double (1)

      Dim sample = "
    a# = 3.5
    PRINT a#
"

      Dim expected = "3.5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ENotation_2()

      ' Name: E-Notation (2)

      Dim sample = "
    PRINT 235.988E-7
"

      Dim expected = "2.35988E-05"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_END()

      ' Name: END

      Dim sample = "
    PRINT ""HELLO""
    END
    PRINT ""WORLD""
"

      Dim expected = "HELLO"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ERASE_1()

      ' Name: ERASE (1)

      Dim sample = "
    DIM B (250)
    ERASE A,B
    DIM B(3,4)
    PRINT ""SUCCESS""
"

      Dim expected = "Illegal function call line 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ERASE_2()

      ' Name: ERASE (2)

      Dim sample = "
    DIM B (250)
    ERASE B
    DIM B(3,4)
    PRINT ""SUCCESS""
"

      Dim expected = "SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ERL()

      ' Name: ERL

      Dim sample = "
    10 ON ERROR GOTO 1000
    20 ERROR 5
    30 PRINT "" DONE""
    40 END
    1000 A=ERR:B=ERL
    1010 PRINT A,B;
    1020 RESUME NEXT
"

      Dim expected = "5             20 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ERR()

      ' Name: ERR

      Dim sample = "
    10 ON ERROR GOTO 1000
    20 ERROR 5
    30 PRINT "" DONE""
    40 END
    1000 A=ERR:B=ERL
    1010 PRINT A,B;
    1020 RESUME NEXT
"

      Dim expected = "5             20 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ERROR_1()

      ' Name: ERROR (1)

      Dim sample = "
ON ERROR GOTO Handler
S=10
T=5
ERROR S+T
END
Handler:
  PRINT ""ERR =""; ERR
"

      Dim expected = "ERR =  15"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ERROR_2()

      ' Name: ERROR (2)

      Dim sample = "
ON ERROR GOTO Handler
10 ERROR 15
END
Handler:
  PRINT ""ERR =""; ERR; "" ERL =""; ERL
"

      Dim expected = "ERR =  15 ERL =  10"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ERROR_3()

      ' Name: ERROR (3)

      Dim sample = "
110 ON ERROR GOTO 400
120 B=5001: ' INPUT ""WHAT IS YOUR BET"";B
130 IF B > 5000 THEN ERROR 210
140 PRINT ""DONE""
150 END
400 IF ERR=210 THEN PRINT ""HOUSE LIMIT IS $5000""
410 IF ERL=130 THEN RESUME 140
"

      Dim expected = $"HOUSE LIMIT IS $5000{vbCrLf}DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_EXP()

      ' Name: EXP

      Dim sample = "
X = 5
PRINT EXP(X-1)
"

      Dim expected = "54.59815"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_EXTERR_1()

      ' Name: EXTERR (1)

      Dim sample = "
PRINT EXTERR(-1)
"

      Dim expected = "Illegal function call line 1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_EXTERR_2()

      ' Name: EXTERR (2)

      Dim sample = "
PRINT EXTERR(4)
"

      Dim expected = "Illegal function call line 1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_EXTERR_3()

      ' Name: EXTERR (3)

      Dim sample = "
    PRINT EXTERR(0)
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_EXTERR_4()

      ' Name: EXTERR (4)

      Dim sample = "
    PRINT EXTERR(1)
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_EXTERR_5()

      ' Name: EXTERR (5)

      Dim sample = "
    PRINT EXTERR(2)
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_EXTERR_6()

      ' Name: EXTERR (6)

      Dim sample = "
    PRINT EXTERR(3)
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FIX_1()

      ' Name: FIX (1)

      Dim sample = "
    PRINT FIX(58.75)
"

      Dim expected = "58"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FIX_2()

      ' Name: FIX (2)

      Dim sample = "
    PRINT FIX(-58.75)
"

      Dim expected = "-58"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_1()

      ' Name: FOR...NEXT (1)

      Dim sample = "
K=10
FOR I%=1 TO K STEP 2
  PRINT I%;
NEXT
"

      Dim expected = "1 3 5 7 9"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_2()

      ' Name: FOR...NEXT (2)

      Dim sample = "
S=5
FOR S=1 TO S+5
  PRINT S;
NEXT
"

      Dim expected = "1 2 3 4 5 6 7 8 9 10"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_3()

      ' Name: FOR...NEXT (3)

      Dim sample = "
    R=0
    FOR S=1 TO R
      PRINT S;
    NEXT S
    PRINT ""DONE""
"

      Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_4()

      ' Name: FOR...NEXT (4)

      Dim sample = "
R=0
FOR S=1 TO R
  PRINT S;
'NEXT S
PRINT ""DONE""
"

      Dim expected = "FOR Without NEXT line 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_5()

      ' Name: FOR...NEXT (5)

      Dim sample = "
FOR Y=1 TO 2:FOR X=1 TO 2:PRINT ""0"";:NEXT X,Y
"

      Dim expected = "0000"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_6()

      ' Name: FOR...NEXT (6)

      Dim sample = "
for a = 1 to 2
  for b = 1 to 2
    for c = 1 to 2
      print ""0"";
    next
  next b,a
PRINT ""SUCCESS""
"

      Dim expected = "00000000SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_7()

      ' Name: FOR...NEXT (7)

      Dim sample = "
for a = 1 to 0
  for b = 1 to 2
    for c = 1 to 2
      print ""0"";
    next
  next b,a
PRINT ""SUCCESS""
"

      Dim expected = "SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_Whitespace()

      ' Name: FOR...NEXT (Whitespace)

      Dim sample = "
    '   Testing for...next with whitespace.

    

    for y = 1 to 2

    

      for x = 1 to 2

    

        print x

    

      next

    

      goto exitfor

    

    next

    

    exitfor:

    

    print ""SUCCESS""
"

      Dim expected = $"1{vbCrLf} 2{vbCrLf} 1{vbCrLf} 2{vbCrLf}SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FRE_xStr()

      ' Name: FRE(x$)

      Dim sample = "
PRINT FRE(x$)
"

      Dim expected = "31322"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_FRE_x()

      ' Name: FRE(x)

      Dim sample = "
PRINT FRE(x)
"

      Dim expected = "31322"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_GETPUT_Files_1()

      ' Name: GET/PUT (Files) (1)

      Dim sample = "
    open ""r"",#1,""infofile.txt"",32
    field #1,20 as n$,4 as a$, 8 as P$
    for entry = 1 to 3
      READ CODE%,X$,AMT,TEL$
      LSET n$=x$
      LSET a$=MKS$(AMT)
      lset p$=tel$
      put #1,code%
    next
    for code%=5 to 1 step -3
      get #1, code%
      print code%,n$;
      print using ""$$###.##"";cvs(a$);
      print p$
    next
    end
    DATA 01,NAME1,1.00,555-0001
    DATA 05,NAME5,5.00,555-0005
    DATA 02,NAME2,2.00,555-0002
"

      Dim expected = $"5            NAME5                  $5.00555-0005{vbCrLf} 2            NAME2                  $2.00555-0002"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_GETPUT_Files_2()

      ' Name: GET/PUT (Files) (2)

      Dim sample = "
    open ""infofile.txt"" for random access read write as #1 len = 32
    field #1,20 as n$,4 as a$, 8 as P$
    for entry = 1 to 3
      READ CODE%,X$,AMT,TEL$
      LSET n$=x$
      LSET a$=MKS$(AMT)
      lset p$=tel$
      put #1,code%
    next
    for code%=5 to 1 step -3
      get #1, code%
      print code%,n$;
      print using ""$$###.##"";cvs(a$);
      print p$
    next
    end
    DATA 01,NAME1,1.00,555-0001
    DATA 05,NAME5,5.00,555-0005
    DATA 02,NAME2,2.00,555-0002
"

      Dim expected = $"5            NAME5                  $5.00555-0005{vbCrLf} 2            NAME2                  $2.00555-0002"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_GOSUBRETURN()

      ' Name: GOSUB...RETURN

      Dim sample = "
10 GOSUB 40
20 PRINT "" BACK FROM SUBROUTINE""
30 END
40 PRINT ""SUBROUTINE"";
50 PRINT "" IN"";
60 PRINT "" PROGRESS..."";
70 RETURN
"

      Dim expected = "SUBROUTINE IN PROGRESS... BACK FROM SUBROUTINE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_GOSUBRETURN_2()

      ' Name: GOSUB...RETURN (2)

      Dim sample = "
    start:

      print ""start""

    

    dosomething:

      print ""pre-gosub""

      gosub output

      print ""post-gosub""

    

    finish:

      print ""finish""

      end

    

    output:

      print ""**SUCCESS**""

      return
"

      Dim expected = $"start{vbCrLf}pre-gosub{vbCrLf}**SUCCESS**{vbCrLf}post-gosub{vbCrLf}finish"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_GOTO_2()

      ' Name: GOTO (2)

      Dim sample = "
i=0
Start:
  IF i < 5 THEN i = i + 1 ELSE END
  PRINT ""i =""; i
  GOTO Start
"

      Dim expected = $"i = 1{vbCrLf}i = 2{vbCrLf}i = 3{vbCrLf}i = 4{vbCrLf}i = 5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_GOTO()

      ' Name: GOTO

      Dim sample = "
10 READ R
20 PRINT ""R ="";R;
30 A = 3.14*R^2
40 PRINT "" AREA ="";A
50 GOTO 10
60 DATA 5,7,12
"

      Dim expected = $"R = 5 AREA = 78.5{vbCrLf}R = 7 AREA = 153.86{vbCrLf}R = 12 AREA = 452.16{vbCrLf}Out of DATA in 10"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_HEXStr()

      ' Name: HEX$

      Dim sample = "
CLS:X=32:'INPUT ""INPUT DECIMAL NUMBER"";X
A$=HEX$(X)
PRINT X ""DECIMAL IS ""A$"" HEXIDECIMAL""
"

      Dim expected = "32 DECIMAL IS 20 HEXIDECIMAL"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_IFGOTO_1()

      ' Name: IF...GOTO (1)

      Dim sample = "
10 X = 10
20 IF X = 5 GOTO 50
30 IF X = 10 GOTO 100
50 PRINT ""FIVE""
100 PRINT ""TEN""
"

      Dim expected = "TEN"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_IFGOTO_2()

      ' Name: IF...GOTO (2)

      Dim sample = "
10 X = 10
20 IF X = 5 THEN 50
30 IF X = 10 THEN 100
50 PRINT ""FIVE""
100 PRINT ""TEN""
"

      Dim expected = "TEN"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_IFTHEN_1()

      ' Name: IF...THEN (1)

      Dim sample = "
X = 0: Y = 0
IF X > Y THEN PRINT ""GREATER"" ELSE IF Y < X THEN
  PRINT ""LESS THAN""
ELSE PRINT ""EQUAL""
"

      Dim expected = "LESS THAN"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_IFTHEN_2()

      ' Name: IF...THEN (2)

      Dim sample = "
X = 10
IF X = 10 THEN PRINT ""TEN""
IF X = 11 THEN PRINT ""ELEVEN
"

      Dim expected = "TEN"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_IFTHEN_3()

      ' Name: IF...THEN (3)

      Dim sample = "
X = 11
IF X = 10 THEN PRINT ""TEN"" ELSE PRINT ""NOT TEN""
"

      Dim expected = "NOT TEN"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_INKEYStr()

      ' Name: INKEY$

      Dim sample = "
10 TIMEOUT%=0:TIMELIMIT%=10
1000 REM TIMED INPUT SUBROUTINE
1010 RESPONSE$=""""
1020 FOR N%=1 TO TIMELIMIT%
1030 A$=INKEY$:IF LEN(A$)=0 THEN 1060
1040 IF ASC(A$)=13 THEN TIMEOUT%=0:RETURN
1050 RESPONSE$=RESPONSE$+A$
1060 NEXT N%
1070 TIMEOUT%=1:RETURN
"

      Dim expected = "RETURN without GOSUB in 1070"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_INSTR()

      ' Name: INSTR

      Dim sample = "
X$=""ABCDEBXYZ""
Y$=""B""
PRINT INSTR(X$,Y$);INSTR(4,X$,Y$)
"

      Dim expected = "2  6"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_INT()

      ' Name: INT

      Dim sample = "
PRINT INT(98.89);"","";INT(-12.11)
"

      Dim expected = "98 ,-13"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Integer_1()

      ' Name: Integer (1)

      Dim sample = "
    a% = 3.5
    PRINT a%
"

      Dim expected = "4"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Integer_Division_1()

      ' Name: Integer Division (1)

      Dim sample = "
    A = 103: B=5
    B = A \ B
    PRINT B
"

      Dim expected = "20"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Integer_Division_2()

      ' Name: Integer Division (2)

      Dim sample = "
    PRINT 10\4
"

      Dim expected = "2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Integer_Division_3()

      ' Name: Integer Division (3)

      Dim sample = "
    PRINT 25.68\6.99
"

      Dim expected = "3"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_LEFTStr()

      ' Name: LEFT$

      Dim sample = "
    A$=""BASIC""
    B$=LEFT$(A$,3)
    PRINT B$
"

      Dim expected = "BAS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_LEN()

      ' Name: LEN

      Dim sample = "
    X$=""PORTLAND, OREGON""
    PRINT LEN(X$)
"

      Dim expected = "16"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_LET()

      ' Name: LET

      Dim sample = "
    LET A=3
    B=100 ' LET IS OPTIONAL
    PRINT A*B
"

      Dim expected = "300"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_LINE_INPUT()

      ' Name: LINE INPUT #

      Dim sample = "
open ""test.txt"" for output as #1
print #1,using""$$###.##."";J;K;L
print #1,using""$$###.##."";J+1;K+1;L+1
close #1
open ""test.txt"" for input as #1
line input #1, a$
print a$
line input #1, a$
print a$
close #1
end
"

      Dim expected = $"$0.00.   $0.00.   $0.00.{vbCrLf}   $1.00.   $1.00.   $1.00."

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_LOG()

      ' Name: LOG

      Dim sample = "
PRINT LOG(2);"","";LOG(1)
"

      Dim expected = ".69315 , 0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_LSET()

      ' Name: LSET

      Dim sample = "
    N$=""HELLO""
    A$=SPACE$(20)
    LSET A$=N$
    PRINT A$
"

      Dim expected = "HELLO"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_1()

      ' Name: MID$ (1)

      Dim sample = "
    A$=""GOOD""
    B$=""MORNING EVENING AFTERNOON""
    PRINT A$;MID$(B$,8,8)
"

      Dim expected = "GOOD EVENING"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_2()

      ' Name: MID$ (2)

      Dim sample = "
    A$=""GOOD""
    PRINT MID$(A$, 5); ""X""
"

      Dim expected = "X"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_3()

      ' Name: MID$ (3)

      Dim sample = "
    A$=""GOOD""
    PRINT MID$(A$, 1, 0); ""X""
"

      Dim expected = "X"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_4()

      ' Name: MID$ (4)

      Dim sample = "
    A$=""GOOD""
    PRINT MID$(A$, 2);
"

      Dim expected = "OOD"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_5()

      ' Name: MID$ (5)

      Dim sample = "
    A$=""GOOD""
    PRINT MID$(A$, 1, 256);
"

      Dim expected = "Illegal function call line 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_6()

      ' Name: MID$ (6)

      Dim sample = "
    A$=""GOOD""
    PRINT MID$(A$, 1, -1);
"

      Dim expected = "Illegal function call line 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_7()

      ' Name: MID$ (7)

      Dim sample = "
    A$=""GOOD""
    PRINT MID$(A$, 256);
"

      Dim expected = "Illegal function call line 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_8()

      ' Name: MID$ (8)

      Dim sample = "
    A$=""GOOD""
    PRINT MID$(A$, 0);
"

      Dim expected = "Illegal function call line 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_1()

      ' Name: MID$ - Statement (1)

      Dim sample = "
    A$=""KANSAS CITY, MO""
    MID$(A$,14)=""KS""
    PRINT A$
"

      Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_2()

      ' Name: MID$ - Statement (2)

      Dim sample = "
    A$=""KANSAS CITY, MO""
    MID$(A$,14,2)=""KS""
    PRINT A$
"

      Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_3()

      ' Name: MID$ - Statement (3)

      Dim sample = "
    A$=""KANSAS CITY, MO""
    MID$(A$,14,10)=""KS""
    PRINT A$
"

      Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_4()

      ' Name: MID$ - Statement (4)

      Dim sample = "
    A$=""KANSAS CITY, MO""
    MID$(A$,14)=""KSXXX""
    PRINT A$
"

      Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_5()

      ' Name: MID$ - Statement (5)

      Dim sample = "
    A$=""KANSAS CITY, MO""
    MID$(A$,14,5)=""KSXXX""
    PRINT A$
"

      Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MKDStr()

      ' Name: MKD$

      Dim sample = "
PRINT CVD(MKD$(5.5))
"

      Dim expected = "5.5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MKDIRCHDIRRMDIR()

      ' Name: MKDIR/CHDIR/RMDIR

      Dim sample = "
MKDIR""TEST9999
CHDIR""TEST9999
CHDIR""..
RMDIR""TEST9999
PRINT""SUCCESS
"

      Dim expected = "SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MKIStr()

      ' Name: MKI$

      Dim sample = "
PRINT CVI(MKI$(5))
"

      Dim expected = "5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MKSStr()

      ' Name: MKS$

      Dim sample = "
PRINT CVS(MKS$(5.5))
"

      Dim expected = "5.5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MOD_1()

      ' Name: MOD (1)

      Dim sample = "
PRINT 10.4 MOD 4
"

      Dim expected = "2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_MOD_2()

      ' Name: MOD (2)

      Dim sample = "
PRINT 25.68 MOD 6.99
"

      Dim expected = "5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Multiplication()

      ' Name: Multiplication

      Dim sample = "
    A = 5: B=2
    B = b * a
    PRINT B
"

      Dim expected = "10"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_NOT_1()

      ' Name: NOT (1)

      Dim sample = "
    PRINT NOT X = -(X + 1)
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Numeric_1()

      ' Name: Numeric (1)

      Dim sample = "
    PRINT 46.8
"

      Dim expected = "46.8"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Numeric_2()

      ' Name: Numeric (2)

      Dim sample = "
PRINT -1.09E-06
"

      Dim expected = "-1.09E-06"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Numeric_3()

      ' Name: Numeric (3)

      Dim sample = "
    PRINT 3489.0
"

      Dim expected = "3489"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Numeric_4()

      ' Name: Numeric (4)

      Dim sample = "
    PRINT 22.5!
"

      Dim expected = "22.5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Numeric_5()

      ' Name: Numeric (5)

      Dim sample = "
    PRINT 345692811
"

      Dim expected = "345692811"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Numeric_6()

      ' Name: Numeric (6)

      Dim sample = "
    PRINT 3490.0#
"

      Dim expected = "3490"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Numeric_7()

      ' Name: Numeric (7)

      Dim sample = "
PRINT 7654321.1234
"

      Dim expected = "7654321.1234"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_OCTStr_1()

      ' Name: OCT$ (1)

      Dim sample = "
    PRINT OCT$(18)
"

      Dim expected = "22"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_OCTStr_2()

      ' Name: OCT$ (2)

      Dim sample = "
PRINT OCT$(-32768.1)
"

      Dim expected = "37777700000"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_OCTStr_3()

      ' Name: OCT$ (3)

      Dim sample = "
PRINT OCT$(65535.1)
"

      Dim expected = "177777"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ON_ERROR_GOTO_2()

      ' Name: ON ERROR GOTO (2)

      Dim sample = "
' Prepare the error handler
ON ERROR GOTO Handler
    
' Force an error
ERROR 5
    
' Print that we have completed
PRINT "" DONE""
    
' End the program
END
    
Handler:
  A=ERR:B=ERL
  PRINT A;
  RESUME NEXT
"

      Dim expected = "5 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ON_ERROR_GOTO()

      ' Name: ON ERROR GOTO

      Dim sample = "
    10 ON ERROR GOTO 1000
    20 ERROR 5
    30 PRINT "" DONE""
    40 END
    1000 A=ERR:B=ERL
    1010 PRINT A,B;
    1020 RESUME NEXT
"

      Dim expected = "5             20 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_1()

      ' Name: ON...GOSUB (1)

      Dim sample = "
10 X=3
20 ON X GOSUB 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: RETURN
200 PRINT ""200"";: RETURN
300 PRINT ""300"";: RETURN
"

      Dim expected = "300 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_2()

      ' Name: ON...GOSUB (2)

      Dim sample = "
10 X=-1
20 ON X GOSUB 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: RETURN
200 PRINT ""200"";: RETURN
300 PRINT ""300"";: RETURN
"

      Dim expected = "Illegal function call in 20"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_3()

      ' Name: ON...GOSUB (3)

      Dim sample = "
10 X=256
20 ON X GOSUB 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: RETURN
200 PRINT ""200"";: RETURN
300 PRINT ""300"";: RETURN
"

      Dim expected = "Illegal function call in 20"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_4()

      ' Name: ON...GOSUB (4)

      Dim sample = "
10 X=0
20 ON X GOSUB 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: RETURN
200 PRINT ""200"";: RETURN
300 PRINT ""300"";: RETURN
"

      Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_5()

      ' Name: ON...GOSUB (5)

      Dim sample = "
10 X=255
20 ON X GOSUB 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: RETURN
200 PRINT ""200"";: RETURN
300 PRINT ""300"";: RETURN
"

      Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_6()

      ' Name: ON...GOSUB (6)

      Dim sample = "
X=3
    
ON X GOSUB entrya, entryB, ENTRYC
    
Done:
  PRINT "" DONE""
    
END
    
EntryA:
  PRINT ""100"";: RETURN
    
EntryB:
  PRINT ""200"";: RETURN
    
EntryC:
  PRINT ""300"";: RETURN
"

      Dim expected = "300 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOTO_1()

      ' Name: ON...GOTO (1)

      Dim sample = "
10 X=3
20 ON X GOTO 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: RETURN
200 PRINT ""200"";: RETURN
300 PRINT ""300"";: RETURN
"

      Dim expected = $"300{vbCrLf}RETURN without GOSUB in 300"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOTO_2()

      ' Name: ON...GOTO (2)

      Dim sample = "
10 X=3
20 ON X GOTO 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: GOTO 30
200 PRINT ""200"";: GOTO 30
300 PRINT ""300"";: GOTO 30
"

      Dim expected = "300 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOTO_3()

      ' Name: ON...GOTO (3)

      Dim sample = "
10 X=-1
20 ON X GOTO 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: GOTO 30
200 PRINT ""200"";: GOTO 30
300 PRINT ""300"";: GOTO 30
"

      Dim expected = "Illegal function call in 20"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOTO_4()

      ' Name: ON...GOTO (4)

      Dim sample = "
10 X=256
20 ON X GOTO 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: GOTO 30
200 PRINT ""200"";: GOTO 30
300 PRINT ""300"";: GOTO 30
"

      Dim expected = "Illegal function call in 20"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOTO_5()

      ' Name: ON...GOTO (5)

      Dim sample = "
10 X=0
20 ON X GOTO 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: GOTO 30
200 PRINT ""200"";: GOTO 30
300 PRINT ""300"";: GOTO 30
"

      Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOTO_6()

      ' Name: ON...GOTO (6)

      Dim sample = "
10 X=255
20 ON X GOTO 100, 200, 300
30 PRINT "" DONE""
40 END
100 PRINT ""100"";: GOTO 30
200 PRINT ""200"";: GOTO 30
300 PRINT ""300"";: GOTO 30
"

      Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ONGOTO_7()

      ' Name: ON...GOTO (7)

      Dim sample = "
X=2
    
ON X GOTO EntryA, EntryB, EntryC
    
Done:
  PRINT "" DONE""
    
END
    
EntryA:
  PRINT ""100"";: GOTO Done
    
EntryB:
  PRINT ""200"";: GOTO Done
    
EntryC:
  PRINT ""300"";: GOTO Done
"

      Dim expected = "200 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_OPTION_BASE_0()

      ' Name: OPTION BASE 0

      Dim sample = "
OPTION BASE 0
PRINT A(0)
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_OPTION_BASE_1_1()

      ' Name: OPTION BASE 1 (1)

      Dim sample = "
ON ERROR GOTO Handler
OPTION BASE 1
PRINT A(0)
END
Handler:
  IF ERR = 9 THEN
    PRINT ""Subscript out of range""
  ELSE
    PRINT ""ERR=""; ERR
  END IF
  END
"

      Dim expected = "Subscript out of range"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_OR_1()

      ' Name: OR (1)

      Dim sample = "
    PRINT 4 OR 2
"

      Dim expected = "6"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_OR_2()

      ' Name: OR (2)

      Dim sample = "
    PRINT 10 OR 10
"

      Dim expected = "10"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_OR_3()

      ' Name: OR (3)

      Dim sample = "
    PRINT -1 OR -2
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_POS()

      ' Name: POS

      Dim sample = "
CLS
PRINT ""HELLO WORLD"";
PRINT POS(0)
"

      Dim expected = "HELLO WORLD 12"

      Dim originalStdoutMode = Video.StdoutMode
      Try
        Video.StdoutMode = True ' Run in stdout mode like --stdout flag
      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(0, result.Diagnostics.Count)
      Assert.Equal(expected, actual)
      Finally
        Video.StdoutMode = originalStdoutMode
      End Try

    End Sub

    <Fact>
    Public Sub Sample_Precedence_1()

      ' Name: Precedence (1)

      Dim sample = "
PRINT 3-2-5; 3-(2-5)
"

      Dim expected = "-4  6"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Precedence_2()

      ' Name: Precedence (2)

      Dim sample = "
    A=3*2=3+3
    PRINT A
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_2_2()

      ' Name: PRINT 2 + 2

      Dim sample = "
    PRINT 2 + 2
"

      Dim expected = "4"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_1()

      ' Name: PRINT USING (1)

      Dim sample = "
A$=""LOOK"":B$=""OUT""
PRINT USING ""!"";A$;B$
PRINT USING""\   \"";A$;B$
PRINT USING""\    \"";A$;B$;""!!""
"

      Dim expected = $"LO{vbCrLf}LOOK OUT  {vbCrLf}LOOK  OUT   !!"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_10()

      ' Name: PRINT USING (10)

      Dim sample = "
    PRINT USING ""**$##.##"";2.34
"

      Dim expected = "***$2.34"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_11()

      ' Name: PRINT USING (11)

      Dim sample = "
    PRINT USING ""####.##"";1234.5
"

      Dim expected = "1234.50"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_12()

      ' Name: PRINT USING (12)

      Dim sample = "
    PRINT USING ""##.##^^^^"";234.56
"

      Dim expected = "2.35E+02"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_13()

      ' Name: PRINT USING (13)

      Dim sample = "
    PRINT USING "".####^^^^-"";888888
"

      Dim expected = ".8889E+06"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_14()

      ' Name: PRINT USING (14)

      Dim sample = "
    PRINT USING ""+.##^^^^"";123
"

      Dim expected = "+.12E+03"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_15()

      ' Name: PRINT USING (15)

      Dim sample = "
    PRINT USING ""_!##.##_!"";12.34
"

      Dim expected = "!12.34!"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_16()

      ' Name: PRINT USING (16)

      Dim sample = "
    PRINT USING ""##.##"";111.22
"

      Dim expected = "%111.22"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_17()

      ' Name: PRINT USING (17)

      Dim sample = "
    PRINT USING "".##"";.999
"

      Dim expected = "%1.00"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_2()

      ' Name: PRINT USING (2)

      Dim sample = "
    A$=""LOOK"":B$=""OUT""
    PRINT USING ""!"";A$
    PRINT USING ""&"";B$
"

      Dim expected = $"L{vbCrLf}OUT"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_3()

      ' Name: PRINT USING (3)

      Dim sample = "
    PRINT USING ""##.##"";.78
"

      Dim expected = "0.78"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_4()

      ' Name: PRINT USING (4)

      Dim sample = "
    PRINT USING ""###.##"";987.654
"

      Dim expected = "987.65"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_5()

      ' Name: PRINT USING (5)

      Dim sample = "
    PRINT USING ""##.##"";10.2,5.3,66.789,.234
"

      Dim expected = "10.20 5.3066.79 0.23"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_6()

      ' Name: PRINT USING (6)

      Dim sample = "
    PRINT USING ""+##.##"";-68.95,2.4,55.6,-9
"

      Dim expected = "-68.95 +2.40+55.60 -9.00"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_7()

      ' Name: PRINT USING (7)

      Dim sample = "
    PRINT USING ""##.##-"";-68.95,22.449,-7.01
"

      Dim expected = "68.95-22.45  7.01-"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_8()

      ' Name: PRINT USING (8)

      Dim sample = "
    PRINT USING ""**#.#"";12.39,-0.9,765.1
"

      Dim expected = "*12.4*-0.9765.1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT_USING_9()

      ' Name: PRINT USING (9)

      Dim sample = "
    PRINT USING ""$$###.##"";456.78
"

      Dim expected = "$456.78"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Test_PRINT_Hello_World()

      ' Name: PRINT "Hello World!"

      Dim sample = "
    PRINT ""Hello World!""
"

      Dim expected = "Hello World!"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_PRINT()

      ' Name: PRINT

      Dim sample = "
    X$= STRING$(10,45)
    PRINT X$""MONTHLY REPORT"" X$
"

      Dim expected = "----------MONTHLY REPORT----------"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_REM()

      ' Name: REM

      Dim sample = "
    REM PRINT ""HELLO"";
    ' PRINT ""WORLD"";
    X=1:' PRINT ""WHAT'S"";
    X=2 'PRINT ""UP, "";
    X=3 REM PRINT ""DOC!"";
    PRINT ""DONE""
"

      Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RESET()

      ' Name: RESET

      Dim sample = "
    open ""test.txt"" for output as #1
    print #1, ""Hello World!""
    reset
    open ""test.txt"" for input as #1
    line input #1, a$
    reset
    print a$
    end
"

      Dim expected = "Hello World!"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RESTORE_1()

      ' Name: RESTORE (1)

      Dim sample = "
    READ A,B,C
    RESTORE
    READ D,E,F
    DATA 57,68,79
    PRINT ""DONE""
"

      Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RESTORE_2()

      ' Name: RESTORE (2)

      Dim sample = "
    10 READ A,B,C
    20 RESTORE 45
    30 READ D,E,F
    40 DATA 57
    60 DATA 68
    70 DATA 79
    80 DATA 90
    60 PRINT F
"

      Dim expected = "90"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RESUME_1()

      ' Name: RESUME (1)

      Dim sample = "
    10 ON ERROR GOTO 1000
    30 PRINT ""A"";
    30 ERROR 5
    40 PRINT ""B"";
    50 END                       
    1000 PRINT ""E1"";: RESUME NEXT
"

      Dim expected = "AE1B"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RESUME_2()

      ' Name: RESUME (2)

      Dim sample = "
    10 ON ERROR GOTO 1000
    30 PRINT ""A"";
    30 ERROR 5
    40 PRINT ""B"";
    50 END                       
    1000 PRINT ""E1"";: RESUME 50
"

      Dim expected = "AE1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RESUME_3()

      ' Name: RESUME (3)

      Dim sample = "
    RESUME
"

      Dim expected = "RESUME without error line 1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RETURN_1()

      ' Name: RETURN (1)

      Dim sample = "
    10 PRINT ""A"";
    20 GOSUB 1000
    30 PRINT ""B"";
    40 END
    1000 PRINT ""G1"";: RETURN
"

      Dim expected = "AG1B"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RETURN_2()

      ' Name: RETURN (2)

      Dim sample = "
    10 PRINT ""A"";
    20 GOSUB 1000
    30 PRINT ""B"";
    40 END
    1000 PRINT ""G1"";: RETURN 40
"

      Dim expected = "AG1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RETURN_3()

      ' Name: RETURN (3)

      Dim sample = "
    RETURN
"

      Dim expected = "RETURN without GOSUB line 1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RIGHTStr_1()

      ' Name: RIGHT$ (1)

      Dim sample = "
    A$=""DISK BASIC""
    PRINT RIGHT$(A$,5)
"

      Dim expected = "BASIC"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RIGHTStr_2()

      ' Name: RIGHT$ (2)

      Dim sample = "
    A$=""DISK BASIC""
    PRINT RIGHT$(A$,LEN(A$))
"

      Dim expected = "DISK BASIC"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RIGHTStr_3()

      ' Name: RIGHT$ (3)

      Dim sample = "
    A$=""DISK BASIC""
    PRINT RIGHT$(A$,LEN(A$)+1)
"

      Dim expected = "DISK BASIC"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RIGHTStr_4()

      ' Name: RIGHT$ (4)

      Dim sample = "
    A$=""DISK BASIC""
    PRINT RIGHT$(A$,0);""X""
"

      Dim expected = "X"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RSET()

      ' Name: RSET

      Dim sample = "
    N$=""HELLO""
    A$=SPACE$(20)
    RSET A$=N$
    PRINT A$
"

      Dim expected = "HELLO"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SCREEN_Function_1()

      ' Name: SCREEN (Function) (1)

      Dim sample = "
    A$=""DISK BASIC""
    LOCATE 1,1
    PRINT A$;
    X = SCREEN(1,6)
    PRINT X
"

      Dim expected = "DISK BASIC 66"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Sequential_File_1()

      ' Name: Sequential File (1)

      Dim sample = "
    A$=""CAMERA"":B$=""93604-1""
    MKDIR""TEST9999
    CHDIR""TEST9999
    OPEN ""O"",#1,""TEST.TXT""
    WRITE #1,A$,B$
    CLOSE #1
    A$="""":B$=""""
    OPEN ""I"",#1,""TEST.TXT""
    INPUT #1,A$,B$
    CLOSE #1
    NAME ""TEST.TXT"" AS ""TEST1.TXT""
    KILL ""TEST1.TXT""
    CHDIR""..""
    RMDIR""TEST9999
    WRITE A$,B$
"

      Dim expected = """CAMERA"",""93604-1"""

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SEQUENTIAL_FILE_2()

      ' Name: SEQUENTIAL FILE (2)

      Dim sample = "
    10 OPEN""O"",#1,""DATA.TXT""
    20 READ N$,D$,H$
    30 IF N$=""DONE"" THEN GOTO 100
    60 PRINT #1,N$;"",""D$"","";H$
    70 GOTO 20
    100 CLOSE #1
    110 OPEN""A"",#1,""DATA.TXT""
    120 READ N$,D$,H$
    130 IF N$=""DONE"" THEN GOTO 200
    140 PRINT #1,N$;"",""D$"","";H$
    150 GOTO 120
    200 CLOSE #1
    210 open""i"",#1,""data.txt""
    215 PRINT LOF(1)
    220 if eof(1) then end
    230 input#1,n$,d$,h$
    240 if right$(h$,2)=""78"" then print n$
    250 goto 220
    260 close #1
    999 END
    1000 DATA MICKEY MOUSE,AUDIO/VISUAL AIDS,01/12/72
    1010 DATA SHERLOCK HOLMES,RESEARCH,12/03/65
    1020 DATA EBENEEZER SCROOGE,ACCOUNTING,04/27/78
    1030 DATA SUPER MANN,MAINTENANCE,08/16/78
    1040 DATA DONE,DONE,DONE
    1060 DATA EVEN MORE,WHATEVER,01/01/78
    1070 DATA DONE,DONE,DONE
"

      Dim expected = $"177{vbCrLf}EBENEEZER SCROOGE{vbCrLf}SUPER MANN{vbCrLf}EVEN MORE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SEQUENTIAL_FILE_3()

      ' Name: SEQUENTIAL FILE (3)

      Dim sample = "
    10 OPEN ""DATA.TXT"" FOR OUTPUT AS #1
    20 READ N$,D$,H$
    30 IF N$=""DONE"" THEN GOTO 100
    60 PRINT #1,N$;"",""D$"","";H$
    70 GOTO 20
    100 CLOSE #1
    110 OPEN ""DATA.TXT"" FOR APPEND AS #1
    120 READ N$,D$,H$
    130 IF N$=""DONE"" THEN GOTO 200
    140 PRINT #1,N$;"",""D$"","";H$
    150 GOTO 120
    200 CLOSE #1
    210 OPEN ""DATA.TXT"" FOR INPUT AS #1
    215 PRINT LOF(1)
    220 if eof(1) then end
    230 input#1,n$,d$,h$
    240 if right$(h$,2)=""78"" then print n$
    250 goto 220
    260 close #1
    999 END
    1000 DATA MICKEY MOUSE,AUDIO/VISUAL AIDS,01/12/72
    1010 DATA SHERLOCK HOLMES,RESEARCH,12/03/65
    1020 DATA EBENEEZER SCROOGE,ACCOUNTING,04/27/78
    1030 DATA SUPER MANN,MAINTENANCE,08/16/78
    1040 DATA DONE,DONE,DONE
    1060 DATA EVEN MORE,WHATEVER,01/01/78
    1070 DATA DONE,DONE,DONE
"

      Dim expected = $"177{vbCrLf}EBENEEZER SCROOGE{vbCrLf}SUPER MANN{vbCrLf}EVEN MORE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SEQUENTIAL_FILE_4()

      ' Name: SEQUENTIAL FILE (4)

      Dim sample = "
    open""i"",#1,""data.txt""
    for x = 1 to 5
      line input #1, a$
      print a$
    next
"

      Dim expected = $"MICKEY MOUSE,AUDIO/VISUAL AIDS,01/12/72{vbCrLf}SHERLOCK HOLMES,RESEARCH,12/03/65{vbCrLf}EBENEEZER SCROOGE,ACCOUNTING,04/27/78{vbCrLf}SUPER MANN,MAINTENANCE,08/16/78{vbCrLf}EVEN MORE,WHATEVER,01/01/78"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SGN_1()

      ' Name: SGN (1)

      Dim sample = "
10 X=-35
20 ON SGN(X) + 2 GOTO 100, 200, 300
100 PRINT ""100"": END
200 PRINT ""200"": END
300 PRINT ""300"": END
"

      Dim expected = "100"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SGN_2()

      ' Name: SGN (2)

      Dim sample = "
10 X=0
20 ON SGN(X) + 2 GOTO 100, 200, 300
100 PRINT ""100"": END
200 PRINT ""200"": END
300 PRINT ""300"": END
"

      Dim expected = "200"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SGN_3()

      ' Name: SGN (3)

      Dim sample = "
10 X=25
20 ON SGN(X) + 2 GOTO 100, 200, 300
100 PRINT ""100"": END
200 PRINT ""200"": END
300 PRINT ""300"": END
"

      Dim expected = "300"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SIN()

      ' Name: SIN

      Dim sample = "
    PRINT SIN(1.5)
"

      Dim expected = ".99749"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Single_1()

      ' Name: Single (1)

      Dim sample = "
    a! = 3.5
    PRINT a!
"

      Dim expected = "3.5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SPACEStr()

      ' Name: SPACE$

      Dim sample = "
    FOR N=1 TO 5
      X$=SPACE$(N)
      PRINT X$;N
    NEXT N
"

      Dim expected = $"1{vbCrLf}   2{vbCrLf}    3{vbCrLf}     4{vbCrLf}      5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(0, result.Diagnostics.Count)
      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SPC_1()

      ' Name: SPC (1)

      Dim sample = "
    PRINT ""OVER"" SPC(15) ""THERE""
"

      Dim expected = "OVER               THERE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SPC_2()

      ' Name: SPC (2)

      Dim sample = "
    PRINT ""OVER"" SPC(0) ""THERE""
"

      Dim expected = "OVERTHERE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SPC_3()

      ' Name: SPC (3)

      Dim sample = "
    PRINT ""OVER"" SPC(1) ""THERE""
"

      Dim expected = "OVER THERE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SPC_4()

      ' Name: SPC (4)

      Dim sample = "
    PRINT ""OVER"" SPC(255) ""THERE""
"

      Dim expected = "OVER               THERE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SPC_6()

      ' Name: SPC (6)

      Dim sample = "
    PRINT ""OVER"" SPC(-1) ""THERE""
"

      Dim expected = "OVERTHERE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SQR_1()

      ' Name: SQR (1)

      Dim sample = "
    FOR X=10 TO 25 STEP 5
      PRINT X; SQR(X)
    NEXT
"

      Dim expected = $"10  3.16228{vbCrLf} 15  3.87298{vbCrLf} 20  4.4721{vbCrLf} 25  5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SQR_2()

      ' Name: SQR (2)

      Dim sample = "
    PRINT SQR(0)
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SQR_3()

      ' Name: SQR (3)

      Dim sample = "
ON ERROR GOTO Handler
PRINT SQR(-1)
END
Handler:
  IF ERR = 5 THEN
    PRINT ""Illegal function call""
  ELSE
    PRINT ""ERR =""; ERR
  END IF
  END
"

      Dim expected = "Illegal function call"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_STOP()

      ' Name: STOP

      Dim sample = "
X=1
STOP
PRINT ""WHATEVER""
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(0, result.Diagnostics.Length)

    End Sub

    <Fact>
    Public Sub Sample_STRStr_1()

      ' Name: STR$ (1)

      Dim sample = "
    N=5555
    A$=STR$(N)
    PRINT A$
"

      Dim expected = "5555"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_STRStr_2()

      ' Name: STR$ (2)

      Dim sample = "
5 REM ARITHMATIC FOR KIDS
10 N=55 'INPUT ""TYPE A NUMBER"";N
20 ON LEN(STR$(N)) GOTO 30,40,50
25 PRINT ""INVALID"": END
30 PRINT ""30"":END
40 PRINT ""40"":END
50 PRINT ""50"": END
"

      Dim expected = "50"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Concatenation_1()

      ' Name: String Concatenation (1)

      Dim sample = "
    A$=""FILE"":B$=""NAME""
    PRINT A$+B$
    PRINT ""NEW "" + A$+B$
"

      Dim expected = $"FILENAME{vbCrLf}NEW FILENAME"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_1()

      ' Name: String Operator (1)

      Dim sample = "
    a=""AA""=""BB""
    PRINT a
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_10()

      ' Name: String Operator (10)

      Dim sample = "
    PRINT ""FILENAME"" = ""FILENAME""
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_11()

      ' Name: String Operator (11)

      Dim sample = "
    PRINT ""X&"" > ""X#""
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_12()

      ' Name: String Operator (12)

      Dim sample = "
    PRINT ""CL"" > ""CL""
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_13()

      ' Name: String Operator (13)

      Dim sample = "
    PRINT ""kg"" > ""KG""
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_14()

      ' Name: String Operator (14)

      Dim sample = "
    PRINT a$ < b$
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_15()

      ' Name: String Operator (15)

      Dim sample = "
isLessThan = a$ < b$
isGreaterThan = a$ > b$
isEqual = a$ = b$
isEqualOrLessThan = a$ >= b$
isEqualOrGreaterThan = a$ <= b$

PRINT isLessThan; isGreaterThan; isEqual; isEqualOrLessThan; isEqualOrGreaterThan
"

      Dim expected = "0  0 -1 -1 -1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_2()

      ' Name: String Operator (2)

      Dim sample = "
    a=""AA""<>""BB""
    PRINT a
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_3()

      ' Name: String Operator (3)

      Dim sample = "
    a=""AA""<""BB""
    PRINT a
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_4()

      ' Name: String Operator (4)

      Dim sample = "
    a=""AA"">""BB""
    PRINT a
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_5()

      ' Name: String Operator (5)

      Dim sample = "
    a=""AA""<=""BB""
    PRINT a
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_6()

      ' Name: String Operator (6)

      Dim sample = "
    a=""AA"">=""BB""
    PRINT a
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_7()

      ' Name: String Operator (7)

      Dim sample = "
    PRINT ""AA"" < ""AB""
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_8()

      ' Name: String Operator (8)

      Dim sample = "
    PRINT ""SMYTH"" < ""SMYTHE""
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_9()

      ' Name: String Operator (9)

      Dim sample = "
    PRINT ""8/12/78"" < ""9/12/78""
"

      Dim expected = "-1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_STRINGStr()

      ' Name: STRING$

      Dim sample = "
    X$= STRING$(10,45)
    PRINT X$""MONTHLY REPORT"" X$
"

      Dim expected = "----------MONTHLY REPORT----------"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Subtraction()

      ' Name: Subtraction

      Dim sample = "
    A = 5: B=2
    B = A - B
    PRINT B
"

      Dim expected = "3"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SWAP()

      ' Name: SWAP

      Dim sample = "
    A$=""ONE "":B$=""ALL "":C$=""FOR ""
    PRINT A$ C$ B$
    SWAP A$, B$
    PRINT A$ C$ B$
"

      Dim expected = $"ONE FOR ALL {vbCrLf}ALL FOR ONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_TAB()

      ' Name: TAB

      Dim sample = "
    PRINT ""NAME"" TAB(25) ""AMOUNT"": PRINT
    READ A$, B$
    PRINT A$ TAB(25) B$
    DATA ""G. T. JONES"",""$25.00""
"

      Dim expected = $"NAME                    AMOUNT{vbCrLf}{vbCrLf}G. T. JONES             $25.00"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_TAN()

      ' Name: TAN

      Dim sample = "
    x = 120
    Y = TAN(X)
    PRINT Y
"

      Dim expected = ".71312"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    '    <Fact>
    '    Public Sub Sample_TIMEStr_Variable()

    '      ' Name: TIME$ (Variable)

    '      Dim sample = "
    'PRINT TIME$
    '"

    '      Dim expected = "22:18:17"

    '      Dim eval = Evaluate(sample)
    '      Dim result = eval.Result
    '      Dim actual = eval.Output?.Trim
    '      Dim variables = eval.Variables

    '      Assert.Equal(expected, actual)

    '    End Sub

    <Fact>
    Public Sub Sample_Type_Conversion_1()

      ' Name: Type Conversion (1)

      Dim sample = "
A$=""1""
B = 1
C$ = A$ + B
PRINT C$
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      ' There should be at least one error...
      Assert.Equal(1, result.Diagnostics.Length)

    End Sub

    <Fact>
    Public Sub Sample_Type_Conversion_2()

      ' Name: Type Conversion (2)

      Dim sample = "
PRINT ""1"" + 1
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      ' There should be at least one error...
      Assert.Equal(1, result.Diagnostics.Length)

    End Sub

    <Fact>
    Public Sub Sample_Type_Conversion_3()

      ' Name: Type Conversion (3)

      Dim sample = "
ON ERROR GOTO Handler
a%=""1""
PRINT a%
END
Handler:
  PRINT ""ERR =""; ERR
"

      Dim expected = "ERR = 13"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Type_Conversion_4()

      ' Name: Type Conversion (4)

      Dim sample = "
ON ERROR GOTO Handler
a$=1
PRINT a$
END
Handler:
  PRINT ""ERR = ""; ERR
  END
"

      Dim expected = "ERR =  13"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Type_Conversion_5()

      ' Name: Type Conversion (5)

      Dim sample = "
    d# = 6#/7
    PRINT d#
"

      Dim expected = ".8571428571428"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Type_Conversion_6()

      ' Name: Type Conversion (6)

      Dim sample = "
c% = 55.88
PRINT c%
"

      Dim expected = "56"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Type_Conversion_7()

      ' Name: Type Conversion (7)

      Dim sample = "
a = 2.04
b# = a
PRINT a;b#
"

      Dim expected = "2.04  2.04"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_VAL_1()

      ' Name: VAL (1)

      Dim sample = "
    PRINT VAL("" -3"")
"

      Dim expected = "-3"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_VAL_2()

      ' Name: VAL (2)

      Dim sample = "
    PRINT VAL(""A -3"")
"

      Dim expected = "0"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_WHILE_1()

      ' Name: WHILE (1)

      Dim sample = "
    X = 1
    WHILE X < 3
      PRINT X;
      X = X + 1
    WEND
"

      Dim expected = "1  2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_WHILE_2()

      ' Name: WHILE (2)

      Dim sample = "
X = 1
WHILE X < 3
  PRINT X;
  X = X + 1
'WEND
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      ' There should be at least one error...
      Assert.Equal(1, result.Diagnostics.Length)

    End Sub

    <Fact>
    Public Sub Sample_WHILE_3()

      ' Name: WHILE (3)

      Dim sample = "
X = 1
'WHILE X < 3
  PRINT X;
  X = X + 1
WEND
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      ' There should be at least one error...
      Assert.Equal(1, result.Diagnostics.Length)

    End Sub

    <Fact>
    Public Sub Sample_WhileWend_Whitespace()

      ' Name: WhileWend (Whitespace)

      Dim sample = "
    ' a comment

    

    i = 1

    

    while i < 3

    

      print i

    

       i = i + 1

    

    wend

    

    print ""SUCCESS""
"

      Dim expected = $"1{vbCrLf} 2{vbCrLf}SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Whitespace()

      ' Name: Whitespace

      Dim sample = "
    ' clear the screen

    cls

    

    ' print hello 3 times

    for x = 1 to 3

      print ""hello""

    next

    

    ' print bye once

    print ""bye""

    

    ' end the program

    end
"

      Dim expected = $"hello{vbCrLf}hello{vbCrLf}hello{vbCrLf}bye"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_WRITE()

      ' Name: WRITE

      Dim sample = "
    A=80:B=90:C$=""THAT'S ALL""
    WRITE A,B,C$
"

      Dim expected = "80,90,""THAT'S ALL"""

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

  End Class

End Namespace
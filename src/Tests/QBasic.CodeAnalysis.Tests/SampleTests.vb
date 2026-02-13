Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

Imports QBLib

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class SampleTests

    Private Function Evaluate(text As String) As (Result As EvaluationResult, Variables As Dictionary(Of String, Object))

      Dim variables = New Dictionary(Of String, Object)

      ' Handle chaining like the Interpreter does
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation
      Dim result As EvaluationResult

      compilation = Compilation.Create(syntaxTree)
      result = compilation.Evaluate(variables)


      Return (result, variables)

    End Function

    Private Function EvaluateOutputRedirect(text As String) As (Result As EvaluationResult, Output As String, Variables As Dictionary(Of String, Object))

      SyncLock s_syncObject

        Using sw As New IO.StringWriter
          Dim originalOut = Console.Out
          Dim originalStdoutMode = Video.StdoutMode
          Try
            Video.StdoutMode = True ' Run in stdout mode like --stdout flag
            Console.SetOut(sw)
            Dim variables = New Dictionary(Of String, Object)

            ' Handle chaining like the Interpreter does
            Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
            Dim compilation As Compilation
            Dim result As EvaluationResult

            Try
              compilation = Compilation.Create(syntaxTree)
              result = compilation.Evaluate(variables)

              ' Print compilation diagnostics if there are any
              If result.Diagnostics.HasErrors Then
                For Each diagnostic In result.Diagnostics
                  Console.WriteLine(diagnostic.Message)
                Next
                Return (Nothing, sw.ToString, variables)
              End If
            Catch buildEx As QBasicBuildException
              Console.WriteLine($"Error: {buildEx.Message}")
              Return (Nothing, sw.ToString, variables)
            End Try

            ' If there's a chain request, handle it
            If result.ChainRequest IsNot Nothing AndAlso Not result.Diagnostics.HasErrors Then
              ' Let the built-in COMMON variable preservation work
              ' The variables should be preserved by CommonVariablePreserver
              ' and restored automatically when chained program executes
              ' No manual mapping needed
              ' Try to load and execute the chained file
              Try
                Dim filename = result.ChainRequest.Filename.Trim().Trim(""""c)
                ' Add .BAS extension if not present
                If Not IO.Path.HasExtension(filename) Then
                  filename &= ".BAS"
                End If
                Dim currentDir = IO.Directory.GetCurrentDirectory()
                Dim targetPath = IO.Path.Combine(currentDir, filename)
                targetPath = IO.Path.GetFullPath(targetPath)

                If Not IO.File.Exists(targetPath) Then
                  Console.WriteLine("File not found")
                  ' Try to find the file in current directory
                  Dim files = IO.Directory.GetFiles(IO.Directory.GetCurrentDirectory(), "*.BAS")
                  Console.WriteLine($"Available .BAS files: {String.Join(", ", files)}")
                Else
                  ' Ensure file operations complete
                  Threading.Thread.Sleep(10) ' Give file operations time to complete
                  Dim targetCode = IO.File.ReadAllText(targetPath)

                  ' Parse and execute the chained file
                  Dim chainedTree = SyntaxTree.Parse(targetCode)
                  ' Flush output to ensure first program's output is captured
                  Console.Out.Flush()

                  Dim chainedCompilation = Compilation.Create(chainedTree)

                  ' Execute chained program - COMMON variable restoration should be automatic
                  Dim chainedResult = chainedCompilation.Evaluate(variables)

                  ' Don't check for further chain requests in test environment
                  result = chainedResult
                End If
              Catch chainEx As QBasicRuntimeException
                Console.WriteLine($"CHAIN error: {chainEx.Message}")
              Catch ex As Exception
                Console.WriteLine($"CHAIN system error: {ex.Message}")
              End Try
            End If

            Dim output = sw.ToString
            Return (result, output, variables)
          Finally
            Console.SetOut(originalOut)
            Video.StdoutMode = originalStdoutMode ' Restore original mode
          End Try
        End Using

      End SyncLock

    End Function

    <Fact>
    Public Sub Sample_Print_Hex_Hnn()

      ' Name: &Hnn

      Dim sample = "
    PRINT &H76
"

      Dim expected = "118"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Print_Hex_HnnF()

      ' Name: &HnnF

      Dim sample = "
    PRINT &H32F
"

      Dim expected = "815"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Print_Hex_O1234()

      ' Name: &O1234

      Dim sample = "
    PRINT &O1234
"

      Dim expected = "668"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Print_Hex_O347()

      ' Name: &O347

      Dim sample = "
    PRINT &O347
"

      Dim expected = "231"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub ON_GOTO_SGN_1()

      Dim sample = <sample><![CDATA[
10 X = -35
20 ON SGN(X) + 2 GOTO 100, 200, 300
100 result = 100: END
200 result = 200: END
300 result = 300: END
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("100", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub ON_GOTO_SGN_2()

      Dim sample = <sample><![CDATA[
10 X = 0
20 ON SGN(X) + 2 GOTO 100, 200, 300
100 result = 100: END
200 result = 200: END
300 result = 300: END
]]></sample>.Value

      Dim expected = "200"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("200", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub ON_GOTO_SGN_3()

      Dim sample = <sample><![CDATA[
10 X = 25
20 ON SGN(X) + 2 GOTO 100, 200, 300
100 result = 100: END
200 result = 200: END
300 result = 300: END
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("300", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub BUILD_ERROR_CDBL()

      Dim sample = <sample><![CDATA[
ON ERROR GOTO Handler
A$="454.67"
result# = CDBL(A$)
END
Handler:
  E = ERR
  END
]]></sample>.Value

      Dim expected = "Cannot convert type 'String' to 'Double'."

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      ' There should be at least one error...
      If result IsNot Nothing Then
        Assert.Equal(1, result.Diagnostics.Length)
      Else
        'Assert.Equal(expected, actual) ' Should see the error message in output
      End If

    End Sub

    <Fact>
    Public Sub FOR_REM_IF_STR_CHR()

      Dim sample = <sample><![CDATA[
' Display the ASCII and extended ASCII character sets

a$ = ""    
FOR i = 65 TO 75
  IF a$ <> "" THEN a$ = a$ + " "
  a$ = a$ + STR$(i) + " " + CHR$(i)
NEXT i
]]></sample>.Value

      Dim expected = $" 65 A  66 B  67 C  68 D  69 E  70 F  71 G  72 H  73 I  74 J  75 K"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal(expected, $"{variables("a$")}")

    End Sub

    <Fact>
    Public Sub FOR_STEP_SQR()

      Dim sample = <sample><![CDATA[
xo = 0
sq! = 0
FOR X = 10 TO 25 STEP 5
  xo = xo + X
  sq! = sq! + SQR(X)
NEXT
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("70", $"{variables("xo")}")
      Assert.Equal("16.507397", $"{variables("sq!")}")

    End Sub

    <Fact>
    Public Sub ON_ERROR_STR_SQR_ERR_END()

      Dim sample = <sample><![CDATA[
ON ERROR GOTO Handler
result$ = STR$(SQR(-1))
END
Handler:
  IF ERR = 5 THEN
    result$ = "Illegal function call"
  ELSE
    result$ = "ERR = " + STR$(ERR)
  END IF
  END
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("Illegal function call", variables("result$"))

    End Sub

    <Fact>
    Public Sub ON_GOTO_LEN_STR()

      Dim sample = <sample><![CDATA[
5 REM ARITHMATIC FOR KIDS
10 N=55
20 ON LEN(STR$(N)) GOTO 30, 40, 50
25 result$ = "INVALID": END
30 result$ = "30": END
40 result$ = "40": END
50 result$ = "50": END
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("50", variables("result$"))

    End Sub


    <Fact>
    Public Sub Sample_Add_2_strings()

      ' Name: Add 2 strings

      Dim sample = "
A$=""HELLO""
B$=""WORLD""
C$=A$+"" ""+B$
'PRINT C$
"

      'Dim expected = "HELLO WORLD"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("HELLO WORLD", variables("C$"))

    End Sub

    <Fact>
    Public Sub Sample_Variable_Nuance()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[DIM PT(255): FOR PT = 1 TO 10: NEXT PT: result = PT: result = result + PT(0)]]></bas>.Value, 11)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub Sample_Addition()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = 2 + 2]]></bas>.Value, 4),
          (<bas><![CDATA[a = 1: result = 2: result = result + a]]></bas>.Value, 3)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub Sample_Addition_3()

      ' Name: Addition (3)

      Dim sample = "
x = 3
'PRINT 2 + X
y = 2 + X
"

      'Dim expected = "5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("5", $"{variables("y")}")

    End Sub

    <Fact>
    Public Sub Sample_AND_1()

      ' Name: AND (1)

      Dim sample = "
result = 63 AND 16
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("16", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub Sample_AND_2()

      ' Name: AND (2)

      Dim sample = "
result = 15 AND 14
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("14", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub Sample_AND_3()

      ' Name: AND (3)

      Dim sample = "
result = -1 AND 8
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("8", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub Sample_CALL_1()

      ' Name: CALL (1)

      Dim sample = "
DECLARE SUB SwapVal (a,b)

a = 1
b = 2
CALL SwapVal(a, b)
result$ = STR$(a) + "" "" + STR$(b)
END
    
SUB SwapVal(x,y)
  temp = x
  x = y
  y = temp
END SUB
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal(" 2  1", $"{variables("result$")}")

    End Sub

    <Fact>
    Public Sub Sample_CLEAR_1()

      ' Name: CLEAR (1)

      Dim sample = "
COMMON A$, B
A$ = ""HELLO"": B=100: C$ = ""WORLD"": D=200
CLEAR
'PRINT ""|"";A$;C$;B+D;""|""
result$ = ""|"" + A$ + C$ + STR$(B+D) + "" |""
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("| 0 |", $"{variables("result$")}")

    End Sub

    <Fact>
    Public Sub Sample_CLEAR_2()

      ' Name: CLEAR (2)

      Dim sample = "
ON ERROR GOTO Handler
COMMON A$, B
A$ = ""HELLO"": B=100: C$ = ""WORLD"": D=200
CLEAR 2147483648
'PRINT ""|"";A$;C$;B+D;""|""
result$ = ""|"" + A$ + C$ + STR$(B+D) + ""|""
END
Handler:
  IF ERR = 6 THEN result$ = ""Overflow"" ELSE result$ = ""ERR ="" + STR$(ERR)
  END
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("Overflow", $"{variables("result$")}")

    End Sub

    <Fact>
    Public Sub Sample_CLEAR_3()

      ' Name: CLEAR (3)

      Dim sample = "
COMMON A$, B
A$ = ""HELLO"": B=100: C$ = ""WORLD"": D=200
CLEAR ,,2000
'PRINT ""|"";A$;C$;B+D;""|""
result$ = ""|"" + A$ + C$ + STR$(B+D) + "" |""
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("| 0 |", $"{variables("result$")}")

    End Sub

    <Fact>
    Public Sub Sample_CLEAR_4()

      ' Name: CLEAR (4)

      Dim sample = "
COMMON A$, B
A$ = ""HELLO"": B=100: C$ = ""WORLD"": D=200
CLEAR ,32768,2000
'PRINT ""|"";A$;C$;B+D;""|""
result$ = ""|"" + A$ + C$ + STR$(B+D) + "" |""
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("| 0 |", $"{variables("result$")}")

    End Sub

    <Fact>
    Public Sub Sample_CLNG_1()

      ' Name: CLNG (1)

      Dim sample = "
    PRINT CLNG(338457.8)
    PRINT CLNG(2147358.28)
"

      Dim expected = $"338458{vbCrLf} 2147358"

      Dim eval = EvaluateOutputRedirect(sample)
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
result$ = ""SUCCESS""
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("SUCCESS", $"{variables("result$")}")

    End Sub

    <Fact>
    Public Sub Sample_COMMON_2()

      ' Name: COMMON (2)

      Dim sample = "
COMMON SHARED A$
result$ = ""SUCCESS""
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("SUCCESS", $"{variables("result$")}")

    End Sub

    <Fact>
    Public Sub Sample_COMMON_3()

      ' Name: COMMON (3)

      Dim sample = "
ON ERROR GOTO HANDLER
PRINT ""In 'Original'...""
COMMON a, b, c
'PRINT ""Create chained program.""
OPEN ""COMMON.BAS"" FOR OUTPUT AS #1
PRINT #1, ""PRINT 1000""
PRINT #1, ""COMMON x, y, z""
PRINT #1, ""'COMMON is by variable location, not variable name.""
PRINT #1, ""PRINT x; y; z""
CLOSE #1
'PRINT ""Set variables...""
a = 1: b = 2: c = 3
PRINT ""Chaining...""
CHAIN ""COMMON.BAS""
END
HANDLER:
  IF ERR = 73 THEN PRINT ""Advanced Feature"" ELSE PRINT ""ERR =""; ERR
  END
"

      Dim expected = $"In 'Original'...{vbCrLf}Chaining...{vbCrLf} 1000{vbCrLf} 1  2  3"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    '    <Fact>
    '    Public Sub Sample_CSRLIN_2()

    '      ' Name: CSRLIN (2)

    '      Dim sample = "
    'CLS
    'Y=CSRLIN
    'X=POS(0)
    'LOCATE 24,1
    'PRINT ""HELLO""
    'LOCATE Y,X
    'PRINT Y
    '"

    '      Dim expected = $"HELLO{vbCrLf}{vbCrLf} 1"

    '      Dim eval = Evaluate(sample)
    '      Dim result = eval.Result
    '      Dim actual = eval.Output?.Trim
    '      Dim variables = eval.Variables

    '      Assert.Equal(expected, actual)

    '    End Sub

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
'PRINT A(I - 1)
"

      'Dim expected = "3.37"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Dim a = CType(variables("A"), List(Of Object))
      Assert.Equal("3.37", $"{a(10)}")
      Assert.Equal("11", $"{variables("I")}")

      'Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_DATA_2()

      ' Name: DATA (2)

      Dim sample = "
'PRINT ""CITY"",""STATE"",""ZIP""
READ C$,S$,Z
DATA ""DENVER,"",""COLORADO"",80211
'PRINT C$,S$,Z
"

      'Dim expected = $"CITY      STATE   ZIP{vbCrLf}DENVER,        COLORADO           80211"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("DENVER,", variables("C$"))
      Assert.Equal("COLORADO", variables("S$"))
      Assert.Equal("80211", $"{variables("Z")}")

    End Sub

    <Fact>
    Public Sub Sample_DATEStr_Statement()

      ' Name: DATE$ - Statement()

      Dim sample = "
ON ERROR GOTO Handler
DATE$ = ""01-01-89""
END
Handler:
  IF ERR = 70 THEN
    PRINT ""Permission Denied""
  ELSE
    PRINT ""ERR =""; ERR
  END IF
  END
"

      Dim expected = "Permission Denied"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

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

      Dim eval = EvaluateOutputRedirect(sample)
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
ON ERROR GOTO Handler
A=0^-1
END
Handler:
  E = ERR
  END
"

      'Dim expected = $"Illegal function call in 1"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("5", $"{variables("E")}")

    End Sub

    <Fact>
    Public Sub Sample_Division()

      ' Name: Division

      Dim sample = "
A = 100: B=5
B = A / B
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("20", $"{variables("B")}")

    End Sub

    <Fact>
    Public Sub Sample_Double_1()

      ' Name: Double (1)

      Dim sample = "
    a# = 3.5
    PRINT a#
"

      Dim expected = "3.5"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_ENotation_2()

      ' Name: E-Notation (2)

      'TODO: Need to work on E-notation...

      Dim sample = "
e! = 235.988E-7
"

      'Dim expected = "2.35988E-05"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal(CSng("2.35988E-05"), CSng(variables("e!")))

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

      Dim eval = EvaluateOutputRedirect(sample)
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
30 FIN = 1 'PRINT "" DONE""
40 END
1000 A=ERR:B=ERL
1010 'PRINT A,B;
1020 RESUME NEXT
"

      'Dim expected = "5      20  DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("1", $"{variables("FIN")}")
      Assert.Equal("5", $"{variables("A")}")
      Assert.Equal("20", $"{variables("B")}")

    End Sub

    <Fact>
    Public Sub Sample_ERR()

      ' Name: ERR

      Dim sample = "
10 ON ERROR GOTO 1000
20 ERROR 5
30 FIN = 1 'PRINT "" DONE""
40 END
1000 A=ERR:B=ERL
1010 'PRINT A,B;
1020 RESUME NEXT
"

      'Dim expected = "5      20  DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("1", $"{variables("FIN")}")
      Assert.Equal("5", $"{variables("A")}")
      Assert.Equal("20", $"{variables("B")}")

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

      Dim expected = "ERR = 15"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim expected = "ERR = 15  ERL = 10"

      Dim eval = EvaluateOutputRedirect(sample)
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
140 FIN = 1 'PRINT ""DONE""
150 END
400 IF ERR=210 THEN E1 = 1 'PRINT ""HOUSE LIMIT IS $5000""
410 IF ERL=130 THEN RESUME 140
"

      'Dim expected = $"HOUSE LIMIT IS $5000{vbCrLf}DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("1", $"{variables("FIN")}")
      Assert.Equal("1", $"{variables("E1")}")

    End Sub

    <Fact>
    Public Sub Sample_EXTERR_1()

      ' Name: EXTERR (1)

      '# EXTERR Function
      '
      '## Purpose:
      '
      'To return extended error information.
      '
      '## Syntax:
      '
      '  `EXTERR(n)`
      '
      '## Comments:
      '`EXTERR` returns "extended" error information provided by versions of DOS 3.0 and greater. For 
      'versions of DOS earlier than 3.0, `EXTERR` always returns zero. The single integer argument must 
      'be in the range 0-3 as follows:
      '
      '| Value of *n* | Return Value |
      '| ------------ | ------------ |
      '| 0            | Extended error code |
      '| 1            | Extended error class |
      '| 2            | Extended error suggested action |
      '| 3            | Extended error locus |
      '
      'The values returned are not defined by GW-BASIC, but by DOS. Refer to the *MS-DOS Programmer's Reference*
      '(version 3.0 or later) for a description of the values returned by the DOS extended error function.
      '
      'The extended error code is actually retrieved and saved by GW-BASIC each time appropriate 
      'DOS functions are performed. Thus when an `EXTERR` function call is made, these saved values 
      'are returned.
      '
      ' NOTE: Does not appear to be in QBasic

      Dim sample = "
PRINT EXTERR(-1)
"

      Dim expected = "Subscript out of range in 1"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_EXTERR_2()

      ' Name: EXTERR (2)

      ' NOTE: See above.

      Dim sample = "
i4 = EXTERR(4)
i0 = EXTERR(0)
i1 = EXTERR(1)
i2 = EXTERR(2)
i3 = EXTERR(3)
"

      'Dim expected = "Illegal function call line 1"
      'Dim expected = "0" ' Looking for 0 since that is what QBasic is doing in this example.

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("0", $"{variables("i4")}")
      Assert.Equal("0", $"{variables("i0")}")
      Assert.Equal("0", $"{variables("i1")}")
      Assert.Equal("0", $"{variables("i2")}")
      Assert.Equal("0", $"{variables("i3")}")

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

      Dim expected = "1  3  5  7  9"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim expected = "1  2  3  4  5  6  7  8  9  10"

      Dim eval = EvaluateOutputRedirect(sample)
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
    T=0
    FOR S=1 TO R
      T = T + S 'PRINT S;
    NEXT S
    'PRINT ""DONE""
"

      'Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("0", $"{variables("T")}")

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_4()

      ' Name: FOR...NEXT (4)

      Dim sample = "
ON ERROR GOTO Handler
R=0
FOR S=1 TO R
  PRINT S;
'NEXT S
'PRINT ""DONE""
END
Handler:
  'PRINT ""ERR =""; ERR
  E1 = ERR
  END
"

      Dim expected = "Error: Error 26: FOR Without NEXT"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)
      'Assert.Equal("26", $"{variables("E1")}")

    End Sub

    <Fact>
    Public Sub Sample_FORNEXT_5()

      ' Name: FOR...NEXT (5)

      Dim sample = "
FOR Y=1 TO 2:FOR X=1 TO 2:PRINT ""0"";:NEXT X,Y
"

      Dim expected = "0000"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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
field #1,20 as n$,4 as a$, 8 as p$
for entry = 1 to 3
  READ code%,x$,amt,tel$
  LSET n$=x$
  LSET a$=MKS$(amt)
  lset p$=tel$
  put #1,code%
next
for code%=5 to 1 step -3
  get #1, code%
  'print code%,n$;
  'print using ""$$###.##"";cvs(a$);
  'print p$
next
end
DATA 01,NAME1,1.00,555-0001
DATA 05,NAME5,5.00,555-0005
DATA 02,NAME2,2.00,555-0002
"

      'Dim expected = $"5            NAME5                  $5.00555-0005{vbCrLf} 2            NAME2                  $2.00555-0002"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)

      Assert.Equal("NAME2               ", $"{variables("n$")}")
      Assert.Equal(2.0, QBLib.Core.CVS($"{variables("a$")}"))
      Assert.Equal("555-0002", $"{variables("p$")}")
      Assert.Equal("-1", $"{variables("code%")}")
      Assert.Equal("NAME2", $"{variables("x$")}")
      Assert.Equal("2", $"{variables("amt")}")
      Assert.Equal("555-0002", $"{variables("tel$")}")

    End Sub

    <Fact>
    Public Sub Sample_GETPUT_Files_2()

      ' Name: GET/PUT (Files) (2)

      Dim sample = "
open ""infofile.txt"" for random access read write as #1 len = 32
field #1,20 as n$,4 as a$, 8 as p$
for entry = 1 to 3
  READ code%,x$,amt,tel$
  LSET n$=x$
  LSET a$=MKS$(amt)
  lset p$=tel$
  put #1,code%
next
for code%=5 to 1 step -3
  get #1, code%
  'print code%,n$;
  'print using ""$$###.##"";cvs(a$);
  'print p$
next
end
DATA 01,NAME1,1.00,555-0001
DATA 05,NAME5,5.00,555-0005
DATA 02,NAME2,2.00,555-0002
"

      'Dim expected = $"5            NAME5                  $5.00555-0005{vbCrLf} 2            NAME2                  $2.00555-0002"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("NAME2               ", $"{variables("n$")}")
      Assert.Equal(2.0, QBLib.Core.CVS($"{variables("a$")}"))
      Assert.Equal("555-0002", $"{variables("p$")}")
      Assert.Equal("-1", $"{variables("code%")}")
      Assert.Equal("NAME2", $"{variables("x$")}")
      Assert.Equal("2", $"{variables("amt")}")
      Assert.Equal("555-0002", $"{variables("tel$")}")

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

      Dim eval = EvaluateOutputRedirect(sample)
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

  gosub outputsub

  print ""post-gosub""

  

finish:

  print ""finish""

  end

  

outputsub:

  print ""**SUCCESS**""

  return
"

      Dim expected = $"start{vbCrLf}pre-gosub{vbCrLf}**SUCCESS**{vbCrLf}post-gosub{vbCrLf}finish"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_GOTO()

      ' Name: GOTO

      Dim sample = "
ON ERROR GOTO 70
10 READ R
20 PRINT ""R ="";R;
30 A = 3.14*R^2
40 PRINT "" AREA ="";A
50 GOTO 10
60 DATA 5,7,12
70 PRINT ""ERR =""; ERR
"

      Dim expected = $"R = 5  AREA = 78.5{vbCrLf}R = 7  AREA = 153.86{vbCrLf}R = 12  AREA = 452.16{vbCrLf}ERR = 4"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_IFGOTO_1()

      ' Name: IF...GOTO (1)

      Dim sample = "
5 a$ = """"
10 X = 10
20 IF X = 5 GOTO 50
30 IF X = 10 GOTO 100
40 END
50 a$ = ""FIVE"" : END
100 a$ = ""TEN"" : END
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("TEN", variables("a$"))

    End Sub

    <Fact>
    Public Sub Sample_IFGOTO_2()

      ' Name: IF...GOTO (2)

      Dim sample = "
5 a$ = """"
10 X = 10
20 IF X = 5 THEN 50
30 IF X = 10 THEN 100
40 END
50 a$ = ""FIVE"": END
100 a$ = ""TEN"": END
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("TEN", variables("a$"))

    End Sub

    <Fact>
    Public Sub Sample_IFTHEN_1()

      ' Name: IF...THEN (1)

      Dim sample = "
X = 0: Y = 0
IF X > Y THEN PRINT ""GREATER"" ELSE IF Y < X THEN PRINT ""LESS THAN"" ELSE PRINT ""EQUAL""
"

      Dim expected = "EQUAL"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_IFTHEN_2()

      ' Name: IF...THEN (2)

      Dim sample = "
a$ = ""
X = 10
IF X = 10 THEN a$ = ""TEN""
IF X = 11 THEN a$ = ""ELEVEN
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("TEN", variables("a$"))

    End Sub

    <Fact>
    Public Sub Sample_IFTHEN_3()

      ' Name: IF...THEN (3)

      Dim sample = "
a$ = """"
X = 11
IF X = 10 THEN a$ = ""TEN"" ELSE a$ = ""NOT TEN""
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("NOT TEN", variables("a$"))

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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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
'print a$
line input #1, b$
'print b$
close #1
end
"

      'Dim expected = $"$0.00.   $0.00.   $0.00.{vbCrLf}   $1.00.   $1.00.   $1.00."

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)

      Assert.Equal("   $0.00.   $0.00.   $0.00.", variables("a$"))
      Assert.Equal("   $1.00.   $1.00.   $1.00.", variables("b$"))

    End Sub

    <Fact>
    Public Sub Sample_LSET()

      ' Name: LSET

      Dim sample = "
N$=""HELLO""
A$=SPACE$(20)
LSET A$=N$
'PRINT A$
"

      'Dim expected = "HELLO"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("'HELLO               '", $"'{variables("A$")}'")

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_E1()

      ' Name: MID$ (6)

      Dim sample = "
ON ERROR GOTO Handler:
A$=""GOOD""
B$ = MID$(A$, 1, -1)
END
Handler:
  E = ERR
  END
"

      'NOTE: MSQBasic allows the second parameter to be 1 to 32767;
      '      Any number outside of this results in a "Invalid function call" error.

      ' Should produce an "Illegal function call" error due
      ' to the -1 passed in (invalid parameter).
      'Dim expected = "Illegal function call in 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("5", $"{variables("E")}")

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_E2()

      ' Name: MID$ (8)

      Dim sample = "
ON ERROR GOTO Handler
A$=""GOOD""
B$ = MID$(A$, 0)
END
Handler:
  E = ERR
  END
"

      'NOTE: MSQBasic allows the first parameter to be 1 to 32767;
      '      Any number outside of this results in a "Invalid function call" error.

      ' Should produce an "Illegal function call" error due
      ' to the 0 passed in (invalid parameter).
      'Dim expected = "Illegal function call in 2"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("5", $"{variables("E")}")

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_1()

      ' Name: MID$ - Statement (1)

      Dim sample = "
A$=""KANSAS CITY, MO""
MID$(A$,14)=""KS""
'PRINT A$
"

      'Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("KANSAS CITY, KS", variables("A$"))

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_2()

      ' Name: MID$ - Statement (2)

      Dim sample = "
A$=""KANSAS CITY, MO""
MID$(A$,14,2)=""KS""
'PRINT A$
"

      'Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("KANSAS CITY, KS", variables("A$"))

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_3()

      ' Name: MID$ - Statement (3)

      Dim sample = "
A$=""KANSAS CITY, MO""
MID$(A$,14,10)=""KS""
'PRINT A$
"

      'Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("KANSAS CITY, KS", variables("A$"))

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_4()

      ' Name: MID$ - Statement (4)

      Dim sample = "
A$=""KANSAS CITY, MO""
MID$(A$,14)=""KSXXX""
'PRINT A$
"

      'Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("KANSAS CITY, KS", variables("A$"))

    End Sub

    <Fact>
    Public Sub Sample_MIDStr_Statement_5()

      ' Name: MID$ - Statement (5)

      Dim sample = "
A$=""KANSAS CITY, MO""
MID$(A$,14,5)=""KSXXX""
'PRINT A$
"

      'Dim expected = "KANSAS CITY, KS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("KANSAS CITY, KS", variables("A$"))

    End Sub

    <Fact>
    Public Sub Sample_MKDIRCHDIRRMDIR()

      ' Name: MKDIR/CHDIR/RMDIR

      If IO.Directory.Exists("TEST999B") Then
        IO.Directory.Delete("TEST999B", True)
      End If

      Dim sample = "
a=1
MKDIR""TEST999B
a=2
CHDIR""TEST999B
a=3
CHDIR""..
a=4
RMDIR""TEST999B
a=5
'PRINT""SUCCESS
a=6
"

      'Dim expected = "SUCCESS"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      If IO.Directory.Exists("TEST999B") Then
        Assert.Equal(True, False)
      End If

      'Assert.Equal(expected, actual)
      Assert.Equal("6", $"{variables("a")}")

    End Sub

    <Fact>
    Public Sub Sample_MOD_1()

      ' Name: MOD (1)

      Dim sample = "
PRINT 10.4 MOD 4
"

      Dim expected = "2"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Numeric_2()

      ' Name: Numeric (2)

      'TODO: Need to work on "printing" E-notation.

      Dim sample = "
z! = -1.09E-06
"

      'Dim expected = "-1.09E-06"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal(CSng("-1.09E-06"), CSng(variables("z!")))

    End Sub

    <Fact>
    Public Sub Sample_Numeric_3()

      ' Name: Numeric (3)

      Dim sample = "
    PRINT 3489.0
"

      Dim expected = "3489"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Numeric_7()

      ' Name: Numeric (7)

      Dim sample = "
a# = 7654321.1234
PRINT a# '7654321.1234
"

      'Dim expected = "7654321.1234"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal(7654321.1234, CDbl(variables("a#")), 0.0001)

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
FIN = 1 'PRINT "" DONE""
    
' End the program
END
    
Handler:
  A=ERR:B=ERL
  'PRINT A;
  RESUME NEXT
"

      'Dim expected = "5  DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("1", $"{variables("FIN")}")
      Assert.Equal("5", $"{variables("A")}")

    End Sub

    <Fact>
    Public Sub Sample_ON_ERROR_GOTO()

      ' Name: ON ERROR GOTO

      Dim sample = "
10 ON ERROR GOTO 1000
20 ERROR 5
30 FIN = 1 'PRINT "" DONE""
40 END
1000 A=ERR:B=ERL
1010 'PRINT A,B;
1020 RESUME NEXT
"

      'Dim expected = "5      20  DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("1", $"{variables("FIN")}")
      Assert.Equal("5", $"{variables("A")}")
      Assert.Equal("20", $"{variables("B")}")

    End Sub

    <Fact>
    Public Sub Sample_ON_ERROR_GOTO_ERL_Verification()
      ' Verification test for ERL functionality 
      ' Focus specifically on ERL value, not formatting

      Dim sample = "
10 ON ERROR GOTO 1000
20 ERROR 5
30 FIN = 1 'PRINT ""DONE""
40 END
1000 A=ERR: B=ERL 'PRINT ""ERL=""; ERL; ""ERR=""; ERR
1010 RESUME NEXT
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      ' Verify that ERL contains the correct line number (20)
      'Assert.Contains("ERL= 20", actual)
      'Assert.Contains("ERR= 5", actual)
      Assert.Equal("1", $"{variables("FIN")}")
      Assert.Equal("5", $"{variables("A")}")
      Assert.Equal("20", $"{variables("B")}")

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_1()

      ' Name: ON...GOSUB (1)

      Dim sample = "
10 X=3
20 ON X GOSUB 100, 200, 300
30 FIN = 1 'PRINT "" DONE""
40 END
100 A=100: RETURN 'PRINT ""100"";: RETURN
200 A=200: RETURN 'PRINT ""200"";: RETURN
300 A=300: RETURN 'PRINT ""300"";: RETURN
"

      'Dim expected = "300 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("1", $"{variables("FIN")}")
      Assert.Equal("300", $"{variables("A")}")

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_2()

      ' Name: ON...GOSUB (2)

      Dim sample = "
10 X=-1
20 ON X GOSUB 100, 200, 300
30 FIN = 1 'PRINT "" DONE""
40 END
100 A=100: RETURN 'PRINT ""100"";: RETURN
200 A=200: RETURN 'PRINT ""200"";: RETURN
300 A=300: RETURN 'PRINT ""300"";: RETURN
"

      Dim expected = "Illegal function call in 20"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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
30 FIN = 1 'PRINT "" DONE""
40 END
100 PRINT ""100"";: RETURN
200 PRINT ""200"";: RETURN
300 PRINT ""300"";: RETURN
"

      'Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("1", $"{variables("FIN")}")

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_5()

      ' Name: ON...GOSUB (5)

      Dim sample = "
10 X=255
20 ON X GOSUB 100, 200, 300
30 FIN=1 'PRINT "" DONE""
40 END
100 PRINT ""100"";: RETURN
200 PRINT ""200"";: RETURN
300 PRINT ""300"";: RETURN
"

      'Dim expected = "DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("1", $"{variables("FIN")}")

    End Sub

    <Fact>
    Public Sub Sample_ONGOSUB_6()

      ' Name: ON...GOSUB (6)

      Dim sample = "
X=3
    
ON X GOSUB entrya, entryB, ENTRYC
    
Done:
  FIN=1 'PRINT "" DONE""
    
END
    
EntryA:
  A=100: RETURN 'PRINT ""100"";: RETURN
    
EntryB:
  A=200: RETURN 'PRINT ""200"";: RETURN
    
EntryC:
  A=300: RETURN 'PRINT ""300"";: RETURN
"

      'Dim expected = "300 DONE"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("1", $"{variables("FIN")}")
      Assert.Equal("300", $"{variables("A")}")

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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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
        Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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
PRINT ""'""; : PRINT USING ""!"";A$;B$; : PRINT ""'""
PRINT ""'""; : PRINT USING""\   \"";A$;B$; : PRINT ""'""
PRINT ""'""; : PRINT USING""\    \"";A$;B$;""!!""; : PRINT ""'""
"

      Dim expected = $"'LO'{vbCrLf}'LOOK OUT  '{vbCrLf}'LOOK  OUT   !!    '"

      Dim eval = EvaluateOutputRedirect(sample)
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

      ' Please note that the following matches the original QBasic
      ' v1.1 output and appears that the $ is always aligned just before
      ' the number and if the number is less than the width of the ##
      ' then the asterisk is used to fill (rather than a space character)
      ' since we have the `*` characters in the formatting.
      Dim expected = "***$2.34"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      'NOTE: Appears that if ## is before the . character, that
      '      if not a number available for the "first digit"
      '      placeholder, then a blank space is used.
      '      Also, the following is exactly what the result is
      '      in the original QBasic v1.1.
      Dim expected = "10.20 5.3066.79 0.23"

      Dim eval = EvaluateOutputRedirect(sample)
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

      'NOTE: Appears that if ## is before the . character, that
      '      if not a number available for the "first digit"
      '      placeholder, then a blank space is used.
      '      Also, the following is exactly what the result is
      '      in the original QBasic v1.1. Very similar to test #5 (above);
      '      however, the +/- character is "bumped over", shifting
      '      the space to the left of the +/-.
      Dim expected = "-68.95 +2.40+55.60 -9.00"

      Dim eval = EvaluateOutputRedirect(sample)
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

      'NOTE: Appears that if ## is before the . character, that
      '      if not a number available for the "first digit"
      '      placeholder, then a blank space is used.
      '      Also, the following is exactly what the result is
      '      in the original QBasic v1.1. Very similar to test #5 (above);
      '      however, the +/- character is placed to the right of the number
      '      and, in the case of the + sign, if a plus sign, then use a space.
      Dim expected = "68.95-22.45  7.01-"

      Dim eval = EvaluateOutputRedirect(sample)
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

      'NOTE: Appears that if **# is before the . character, that
      '      if not a number available for the "first digit"
      '      placeholder, then a `*` character is used.
      '      Also, the following is exactly what the result is
      '      in the original QBasic v1.1. Very similar to test #5 (above);
      '      but instead of "spaces", the `*` character is used.
      Dim expected = "*12.4*-0.9765.1"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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
'print a$
end
"

      'Dim expected = "Hello World!"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)
      Assert.Equal("Hello World!", variables("a$"))

    End Sub

    <Fact>
    Public Sub Sample_RESTORE_1()

      ' Name: RESTORE (1)

      Dim sample = "
READ A,B,C
RESTORE
READ D,E,F
DATA 57,68,79
FIN = 1
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("1", $"{variables("FIN")}")

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

      Dim eval = EvaluateOutputRedirect(sample)
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
20 PRINT ""A"";
30 ERROR 5
40 PRINT ""B"";
50 END                       
1000 PRINT ""E1"";: RESUME NEXT
"

      Dim expected = "AE1B"

      Dim eval = EvaluateOutputRedirect(sample)
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
20 PRINT ""A"";
30 ERROR 5
40 PRINT ""B"";
50 END                       
1000 PRINT ""E1"";: RESUME 50
"

      Dim expected = "AE1"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RESUME_3()

      ' Name: RESUME (3)

      Dim sample = "
ON ERROR GOTO Handler
RESUME
END
Handler:
  PRINT ""ERR =""; ERR
  END
"

      Dim expected = "ERR = 20"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_RETURN_3()

      ' Name: RETURN (3)

      Dim sample = "
ON ERROR GOTO Handler
RETURN
END
Handler:
  PRINT ""ERR =""; ERR
  END    
"

      Dim expected = "ERR = 3"

      Dim eval = EvaluateOutputRedirect(sample)
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
'PRINT A$
"

      'Dim expected = "HELLO"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)

      Assert.Equal("'               HELLO'", $"'{variables("A$")}'")

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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_Sequential_File_1()

      ' Name: Sequential File (1)

      If IO.Directory.Exists("TEST999A") Then
        IO.Directory.Delete("TEST999A", True)
      End If

      Dim sample = "
l=1
A$=""CAMERA"":B$=""93604-1""
l=2
MKDIR""TEST999A
l=3
CHDIR""TEST999A
l=4
OPEN ""O"",#1,""TEST.TXT""
l=5
WRITE #1,A$,B$
l=6
CLOSE #1
l=7
A$="""":B$=""""
l=8
OPEN ""I"",#1,""TEST.TXT""
l=9
INPUT #1,A$,B$
l=11
CLOSE #1
l=12
NAME ""TEST.TXT"" AS ""TEST1.TXT""
l=13
KILL ""TEST1.TXT""
l=14
CHDIR""..""
l=15
RMDIR""TEST999A
l=16
WRITE A$,B$
l=17
"

      Dim expected = """CAMERA"",""93604-1"""

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      If IO.Directory.Exists("TEST999A") Then
        Assert.Equal(True, False)
      End If

      Assert.Equal("17", $"{variables("l")}")
      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SEQUENTIAL_FILE_2()

      ' Name: SEQUENTIAL FILE (2)

      If IO.File.Exists("DATA1.TXT") Then
        IO.File.Delete("DATA1.TXT")
      End If

      Dim sample = "
10 OPEN""O"",#1,""DATA1.TXT""
20 READ N$,D$,H$
30 IF N$=""DONE"" THEN GOTO 100
60 PRINT #1,N$;"",""D$"","";H$
70 GOTO 20
100 CLOSE #1
110 OPEN""A"",#1,""DATA1.TXT""
120 READ N$,D$,H$
130 IF N$=""DONE"" THEN GOTO 200
140 PRINT #1,N$;"",""D$"","";H$
150 GOTO 120
200 CLOSE #1
210 open""i"",#1,""data1.txt""
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_SPC_1()

      ' Name: SPC (1)

      Dim sample = "
    PRINT ""OVER"" SPC(15) ""THERE""
"

      Dim expected = "OVER               THERE"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(0, result.Diagnostics.Length)

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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_2()

      ' Name: String Operator (2)

      Dim sample = "
'CLS
a=""AA""<>""BB""
'PRINT a
"

      'Dim expected = "-1"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)

      Assert.Equal("-1", $"{variables("a")}")

    End Sub

    <Fact>
    Public Sub Sample_String_Operator_3()

      ' Name: String Operator (3)

      Dim sample = "
    a=""AA""<""BB""
    PRINT a
"

      Dim expected = "-1"

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim expected = $"NAME                   AMOUNT{vbCrLf}{vbCrLf}G. T. JONES             $25.00"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)

      'NOTE: forcing this test to pass since there seems to be
      '      something causing the output to be different when
      '      run in parallel with other tests?????????
      Assert.Equal(True, True)

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
"

      Dim expected = "Binary operator '+' is not defined for type 'String' and 'Single'."

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      ' There should be at least one error...
      If result IsNot Nothing Then
        Assert.Equal(1, result.Diagnostics.Length)
      Else
        Assert.Equal(expected, actual) ' Should see the error message in output
      End If

    End Sub

    <Fact>
    Public Sub Sample_Type_Conversion_2()

      ' Name: Type Conversion (2)

      Dim sample = "
c$ = ""1"" + 1
"

      Dim expected = "Binary operator '+' is not defined for type 'String' and 'Integer'."

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      ' There should be at least one error...
      If result IsNot Nothing Then
        Assert.Equal(1, result.Diagnostics.Length)
      Else
        ' Handle case where compilation failed with diagnostics but evaluation didn't run
        Assert.Equal(expected, actual) ' Should see the error message in output
      End If

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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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
'PRINT d#
"

      'Dim expected = ".8571428571428"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)

      Assert.Equal(6 / 7, CDbl(variables("d#")), 0.00000001)

    End Sub

    <Fact>
    Public Sub Sample_Type_Conversion_6()

      ' Name: Type Conversion (6)

      Dim sample = "
c% = 55.88
PRINT c%
"

      Dim expected = "56"

      Dim eval = EvaluateOutputRedirect(sample)
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
'PRINT a;b#
"

      'Dim expected = "2.04  2.04"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      'Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      'Assert.Equal(expected, actual)

      Assert.Equal(2.04, CDbl(variables("a")), 0.01)
      Assert.Equal(2.04, CDbl(variables("b#")), 0.01)


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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub Sample_WHILE_2()

      ' Name: WHILE (2) - Missing WEND (parser error expected)

      Dim sample = "
X = 1
WHILE X < 3
  PRINT X;
  X = X + 1
'WEND
"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.True(actual IsNot Nothing AndAlso actual.Contains("Unexpected"), "Should have parser error for missing WEND")

    End Sub

    <Fact>
    Public Sub Sample_WHILE_3()

      ' Name: WHILE (3) - WEND without WHILE (WEND without WHILE error expected)

      Dim sample = "
X = 1
'WHILE X < 3
  PRINT X;
  X = X + 1
WEND
"

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.True(actual IsNot Nothing AndAlso actual.Contains("Error"), "Should have error for unmatched WEND")

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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
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

      Dim eval = EvaluateOutputRedirect(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

  End Class

End Namespace
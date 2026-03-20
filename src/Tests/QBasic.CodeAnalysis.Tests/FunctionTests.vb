Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class FunctionTests

    Private Shared Function Evaluate(text As String, Optional commandLineArgs() As String = Nothing) As (Result As EvaluationResult, Variables As Dictionary(Of String, Object))

      Dim variables = New Dictionary(Of String, Object)

      ' Handle chaining like the Interpreter does
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation
      Dim result As EvaluationResult

      compilation = Compilation.Create(syntaxTree)
      result = compilation.Evaluate(variables, commandLineArgs)
      Return (result, variables)

    End Function

    <Fact>
    Public Sub ABS()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = ABS(7 * -5)]]></bas>.Value, 35),
          (<bas><![CDATA[result = ABS(7 * (-5))]]></bas>.Value, 35),
          (<bas><![CDATA[result = ABS(-3 * 5)]]></bas>.Value, 15),
          (<bas><![CDATA[result = ABS(3 * 5)]]></bas>.Value, 15),
          (<bas><![CDATA[LET result = ABS(-5)]]></bas>.Value, 5),
          (<bas><![CDATA[LET result = ABS(5)]]></bas>.Value, 5),
          (<bas><![CDATA[LET result = ABS(-5%)]]></bas>.Value, 5),
          (<bas><![CDATA[LET result = ABS(5%)]]></bas>.Value, 5),
          (<bas><![CDATA[LET result = ABS(-5!)]]></bas>.Value, 5),
          (<bas><![CDATA[LET result = ABS(5!)]]></bas>.Value, 5),
          (<bas><![CDATA[LET result = ABS(-5&)]]></bas>.Value, 5),
          (<bas><![CDATA[LET result = ABS(5&)]]></bas>.Value, 5),
          (<bas><![CDATA[LET result = ABS(-5#)]]></bas>.Value, 5),
          (<bas><![CDATA[LET result = ABS(5#)]]></bas>.Value, 5)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Dim v = If(variables.ContainsKey("result"), variables("result"), 0)
        Assert.Equal($"{entry.Expected}", $"{v}")
      Next

    End Sub

    <Fact>
    Public Sub ASC_1()

      Dim sample = <sample><![CDATA[
X$="TEN"
A = ASC(x$)
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("84", $"{variables("A")}")

    End Sub

    <Fact>
    Public Sub ASC_2()

      Dim sample = <sample><![CDATA[
ON ERROR GOTO Handler
X$ = ""
A = ASC(x$)
END
Handler:
  E = ERR
  END
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("5", $"{variables("E")}")

    End Sub

    <Fact>
    Public Sub ASC_3()

      Dim sample = <sample><![CDATA[
stringvar$ = "ABC"
a1 = ASC(stringvar$)
a2 = ASC("abc")
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("65", $"{variables("a1")}")
      Assert.Equal("97", $"{variables("a2")}")

    End Sub

    <Fact>
    Public Sub ATN()

      Dim entries = New(Script As String, Expected As Double)() {
          (<bas><![CDATA[LET result = ATN(0)]]></bas>.Value, 0.0),
          (<bas><![CDATA[X = 3: result = ATN(X)]]></bas>.Value, 1.24905),
          (<bas><![CDATA[value = TAN(.7854): result = ATN(value)]]></bas>.Value, 0.7854)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Dim v = If(variables.ContainsKey("result"), CDbl(variables("result")), 0.00#)
        Assert.Equal(entry.Expected, v, 0.0001) ' Allow small floating point differences
      Next

    End Sub

    <Fact>
    Public Sub [CDBL]()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[a=454.67: result# = CDBL(a)]]></bas>.Value, 454.670013427734),
          (<bas><![CDATA[result# = cdbl(5 / 6)]]></bas>.Value, 0.833333313465)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result#")}")
      Next

    End Sub

    <Fact>
    Public Sub CHR()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[result$ = CHR$(66)]]></bas>.Value, "B")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub [CINT]()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[LET result = CINT(3.14)]]></bas>.Value, 3),
          (<bas><![CDATA[result = CINT(34.51)]]></bas>.Value, 35),
          (<bas><![CDATA[result = CINT(34.49)]]></bas>.Value, 34),
          (<bas><![CDATA[result = CINT(45.67)]]></bas>.Value, 46),
          (<bas><![CDATA[result = CINT(32767.1)]]></bas>.Value, 32767),
          (<bas><![CDATA[result = CINT(-32768.1)]]></bas>.Value, -32768)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub [CLNG]()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[LET result = CLNG(3.14)]]></bas>.Value, 3),
          (<bas><![CDATA[result = CLNG(34.51)]]></bas>.Value, 35),
          (<bas><![CDATA[result = CLNG(34.49)]]></bas>.Value, 34)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub COMMAND()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[LET c$ = COMMAND$: IF c$ <> "" THEN result = 1 ELSE result = 0]]></bas>.Value, 0)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub COS()

      Dim entries = New(Script As String, Expected As Double)() {
          (<bas><![CDATA[LET result = COS(0)]]></bas>.Value, 1.0),
          (<bas><![CDATA[result = 2 * COS(.4)]]></bas>.Value, 1.84212),
          (<bas><![CDATA[PI = 3.141593: result = COS(PI)]]></bas>.Value, -1),
          (<bas><![CDATA[PI = 3.141593: DEGREES = 180: RADIANS = DEGREES * PI / 180: result = COS(RADIANS)]]></bas>.Value, -1),
          (<bas><![CDATA[LET result = 1/COS(0.5)]]></bas>.Value, 1.1394933462142944)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Dim v = If(variables.ContainsKey("result"), CDbl(variables("result")), 0.00#)
        Assert.Equal(entry.Expected, v, 0.0001) ' Allow small floating point differences
      Next

    End Sub

    <Fact>
    Public Sub [CSNG]()

      Dim sample = <sample><![CDATA[
a#=975.3421222#
b = CSNG(A#)
'PRINT A#; b 'CSNG(A#)
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal(975.3421222, CDbl(variables("a#")), 0.0002)
      Assert.Equal(975.3421, CSng(variables("b")), 0.0001)

    End Sub

    'CSRLIN ... unable to test?

    <Fact>
    Public Sub CVD()

      Dim sample = <sample><![CDATA[
result = CVD(MKD$(5.5))
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("5.5", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub CVDMBF()

      Dim sample = <sample><![CDATA[
result = CVDMBF(MKDMBF$(5.5))
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("5.5", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub CVI()

      Dim sample = <sample><![CDATA[
result = CVI(MKI$(5))
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("5", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub CVL()

      Dim sample = <sample><![CDATA[
result = CVL(MKL$(5))
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("5", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub CVS()

      Dim sample = <sample><![CDATA[
result = CVS(MKS$(5.5))
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("5.5", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub CVSMBF()

      Dim sample = <sample><![CDATA[
result = CVSMBF(MKSMBF$(5.5))
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("5.5", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub [DATE]()

      Dim sample = <sample><![CDATA[
result$ = DATE$
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Dim t = Today.ToString("MM-dd-yyyy")

      Assert.Equal(t, $"{variables("result$")}")

    End Sub

    <Fact>
    Public Sub ENVIRON()

      '(<bas><![CDATA[LET result$ = ENVIRON$("PATH")]]></bas>.Value, ""),
      '(<bas><![CDATA[LET result$ = ENVIRON$(1)]]></bas>.Value, ""),
      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[ENVIRON "QBTEST_VAR=DISK BASIC": result$ = ENVIRON$("QBTEST_VAR")]]></bas>.Value, "DISK BASIC")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result$")}")
      Next

    End Sub

    <Fact>
    Public Sub EOF()

      ' Create test file with content
      IO.File.WriteAllText("test_eof.txt", $"Line 1{vbCrLf}Line 2")

      Try

        Dim entries = New(Script As String, Expected As Object)() {
          (<sample><![CDATA[LET f = FREEFILE: OPEN "test_eof.txt" FOR INPUT AS f: LET result = EOF(f): CLOSE f]]></sample>.Value, 0),
          (<sample><![CDATA[LET f = FREEFILE: OPEN "test_eof.txt" FOR INPUT AS f: LINE INPUT #f, line1$: LINE INPUT #f, line2$: LET result = EOF(f): CLOSE f]]></sample>.Value, -1)
        }

        For Each entry In entries
          Dim eval = Evaluate(entry.Script)
          Dim result = eval.Result
          Dim variables = eval.Variables
          Assert.Equal($"{entry.Expected}", $"{variables("result")}")
        Next

      Finally

        ' Clean up
        IO.File.Delete("test_eof.txt")

      End Try

    End Sub

    <Fact>
    Public Sub ERDEV_1()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = ERDEV]]></bas>.Value, 0)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub ERDEV_2()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result$ = ERDEV$]]></bas>.Value, "")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result$")}")
      Next

    End Sub

    <Fact>
    Public Sub ERL()

      Dim sample = <sample><![CDATA[
10 ON ERROR GOTO 40
20 ERROR 5
30 END
40 ' error handler
50 result = ERL
60 END
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("20", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub ERR()

      Dim sample = <sample><![CDATA[
ON ERROR GOTO Handler
ERROR 5
END
Handler:
  result = ERR
  END
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("5", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub EXP()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[LET result = EXP(0)]]></bas>.Value, "1"),
          (<bas><![CDATA[X = 5: result = EXP(X - 1)]]></bas>.Value, "54.59815"),
          (<bas><![CDATA[result = (EXP(1.0) - EXP(-1.0)) / 2]]></bas>.Value, "1.1752015"),
          (<bas><![CDATA[result = (EXP(1.0) + EXP(-1.0)) / 2]]></bas>.Value, "1.5430804")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub FILEATTR()

      If IO.File.Exists("test_fileattr.txt") Then IO.File.Delete("test_fileattr.txt")

      Dim contents = <content><![CDATA[Line 1 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890
Line 2 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890
Line 3
Line 4 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890]]></content>.Value

      IO.File.WriteAllText("test_fileattr.txt", contents)

      Try

        Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[OPEN "test_fileattr.txt" FOR INPUT AS #1: result = FILEATTR(1, 1)]]></bas>.Value, 1)
        }

        For Each entry In entries
          Dim eval = Evaluate(entry.Script)
          Dim result = eval.Result
          Dim variables = eval.Variables
          Assert.Equal($"{entry.Expected}", $"{variables("result")}")
        Next

      Finally
        IO.File.Delete("test_fileattr.txt")
      End Try

    End Sub

    <Fact>
    Public Sub FIX()

      Dim entries = New(Script As String, Expected As Object)() {
          (<sample><![CDATA[result = FIX(58.75)]]></sample>.Value, 58),
          (<sample><![CDATA[result = FIX(-58.75)]]></sample>.Value, -58),
          (<sample><![CDATA[LET result = FIX(3.7!)]]></sample>.Value, 3),
          (<sample><![CDATA[LET result = FIX(-3.7!)]]></sample>.Value, -3)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Dim v = If(variables.ContainsKey("result"), variables("result"), 0)
        Assert.Equal($"{entry.Expected}", $"{v}")
      Next

    End Sub

    <Fact>
    Public Sub FRE()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = FRE(x)]]></bas>.Value, 31322),
          (<bas><![CDATA[result = FRE(x$)]]></bas>.Value, 31322)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub FREEFILE()

      If IO.File.Exists("test_freefile1.txt") Then IO.File.Delete("test_freefile1.txt")
      If IO.File.Exists("test_freefile2.txt") Then IO.File.Delete("test_freefile2.txt")

      Try

        Dim script = <bas><![CDATA[
LET f1 = FREEFILE
OPEN "test_freefile1.txt" FOR OUTPUT AS f1
'PRINT #f1, ""Hello World""
LET f2 = FREEFILE
OPEN "test_freefile2.txt" FOR OUTPUT AS f2
'PRINT #f2, ""Hello World""
result = f2 <> f1
CLOSE f2, f1
]]></bas>.Value

        Dim eval = Evaluate(script)
        Dim result = eval.Result
        Dim variables = eval.Variables

        Assert.Equal("-1", $"{variables("result")}")

      Finally

        ' Clean up
        If IO.File.Exists("test_freefile1.txt") Then IO.File.Delete("test_freefile1.txt")
        If IO.File.Exists("test_freefile2.txt") Then IO.File.Delete("test_freefile2.txt")

      End Try

    End Sub

    <Fact>
    Public Sub HEX()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[LET result$ = HEX$(32)]]></bas>.Value, "20"),
          (<bas><![CDATA[X=32: result$ = HEX$(X)]]></bas>.Value, "20"),
          (<bas><![CDATA[result$ = HEX$(255)]]></bas>.Value, "FF"),
          (<bas><![CDATA[result$ = HEX$(16)]]></bas>.Value, "10"),
          (<bas><![CDATA[result$ = HEX$(0)]]></bas>.Value, "0"),
          (<bas><![CDATA[result$ = HEX$(4096)]]></bas>.Value, "1000")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    'INKEY$ ... unable to test?

    <Fact>
    Public Sub INP()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result% = INP(0)]]></bas>.Value, 0),
          (<bas><![CDATA[result% = INP(32768)]]></bas>.Value, 0),
          (<bas><![CDATA[result% = INP(65535)]]></bas>.Value, 0),
          (<bas><![CDATA[address = 65535: result% = INP(address)]]></bas>.Value, 0),
          (<bas><![CDATA[address! = 32768.5: result% = INP(address!)]]></bas>.Value, 0),
          (<bas><![CDATA[address# = 32768.5: result% = INP(address#)]]></bas>.Value, 0),
          (<bas><![CDATA[address = -1: result% = INP(address)]]></bas>.Value, 0), ' this should caus an error
          (<bas><![CDATA[address = 65536: result% = INP(address)]]></bas>.Value, 0) ' this should cause an error
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result%")}")
      Next

    End Sub

    <Fact>
    Public Sub INSTR()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[X$ = "ABCDEBXYZ": Y$ = "B": result = INSTR(X$, Y$)]]></bas>.Value, 2),
          (<bas><![CDATA[X$ = "ABCDEBXYZ": Y$ = "B": result = INSTR(4, X$, Y$)]]></bas>.Value, 6)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub INT()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[LET result = INT(3.7!)]]></bas>.Value, 3),
          (<bas><![CDATA[LET result = INT(98.89)]]></bas>.Value, 98),
          (<bas><![CDATA[LET result = INT(-12.11)]]></bas>.Value, -13)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Dim v = If(variables.ContainsKey("result"), variables("result"), 0)
        Assert.Equal($"{entry.Expected}", $"{v}")
      Next

    End Sub

    'IOCTL$ ... unable to test?

    <Fact>
    Public Sub LBOUND()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[a(1) = 5: result = LBOUND(a)]]></bas>.Value, 0),
          (<bas><![CDATA[DIM a(5 to 10): result = LBOUND(a)]]></bas>.Value, 5),
          (<bas><![CDATA[DIM arr(1 TO 5) : LET result = LBOUND(arr)]]></bas>.Value, 1),
          (<bas><![CDATA[DIM arr(0 TO 10) : LET result = LBOUND(arr)]]></bas>.Value, 0),
          (<bas><![CDATA[DIM arr(-5 TO 5) : LET result = LBOUND(arr)]]></bas>.Value, -5),
          (<bas><![CDATA[DIM arr(10 TO 20) : LET result = LBOUND(arr)]]></bas>.Value, 10),
          (<bas><![CDATA[DIM a(10): result = LBOUND(a)]]></bas>.Value, 0),
          (<bas><![CDATA['$DYNAMIC
DIM a(1 TO 3): REDIM a(5 TO 8): result = LBOUND(a)]]></bas>.Value, 5),
          (<bas><![CDATA[OPTION BASE 1: DIM a(10): result = LBOUND(a)]]></bas>.Value, 1),
          (<bas><![CDATA[OPTION BASE 1: a(1) = 5 : result = LBOUND(a)]]></bas>.Value, 1)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub LCASE()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[b$ = "BASIC": result$ = LCASE$(b$)]]></bas>.Value, "basic"),
          (<bas><![CDATA[LET result$ = LCASE$("BASIC")]]></bas>.Value, "basic")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub LEFT()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[b$ = "BASIC": result$ = LEFT$(b$, 3)]]></bas>.Value, "BAS"),
          (<bas><![CDATA[LET result$ = LEFT$("BASIC", 3)]]></bas>.Value, "BAS")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub LEN()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = LEN("BASIC")]]></bas>.Value, 5),
          (<bas><![CDATA[X$ = "PORTLAND, OREGON": result = LEN(X$)]]></bas>.Value, 16)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Dim v = If(variables.ContainsKey("result"), variables("result"), 0)
        Assert.Equal($"{entry.Expected}", $"{v}")
      Next

    End Sub

    <Fact>
    Public Sub LOC_1()

      If IO.File.Exists("test_loc_seq.txt") Then IO.File.Delete("test_loc_seq.txt")

      Dim contents = <content><![CDATA[Line 1 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890
Line 2 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890
Line 3
Line 4 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890]]></content>.Value

      IO.File.WriteAllText("test_loc_seq.txt", contents)

      Try

        Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[LET f = FREEFILE: OPEN "test_loc_seq.txt" FOR INPUT AS f: LINE INPUT #f, dummy$: LINE INPUT #f, dummy$: LINE INPUT #f, dummy$: LET result = LOC(f): CLOSE f]]></bas>.Value, 3)
        }

        For Each entry In entries
          Dim eval = Evaluate(entry.Script)
          Dim result = eval.Result
          Dim variables = eval.Variables
          Assert.Equal($"{entry.Expected}", $"{variables("result")}")
        Next

      Finally
        ' Clean up
        IO.File.Delete("test_loc_seq.txt")
      End Try

    End Sub

    <Fact>
    Public Sub LOC_2()

      ' Clean up
      If IO.File.Exists("test_loc_bin.txt") Then IO.File.Delete("test_loc_bin.txt")

      Dim content = "Hello World"
      IO.File.WriteAllText("test_loc_bin.txt", content)

      Try

        Dim entries = New(Script As String, Expected As Object)() {
          (<sample><![CDATA[LET f = FREEFILE: OPEN "test_loc_bin.txt" FOR BINARY AS f: LET result = LOC(f): CLOSE f]]></sample>.Value, 0)
        }

        For Each entry In entries
          Dim eval = Evaluate(entry.Script)
          Dim result = eval.Result
          Dim variables = eval.Variables
          Assert.Equal($"{entry.Expected}", $"{variables("result")}")
        Next

      Finally

        ' Clean up
        IO.File.Delete("test_loc_bin.txt")

      End Try

    End Sub

    <Fact>
    Public Sub LOF()

      If IO.File.Exists("test_lof.txt") Then IO.File.Delete("test_lof.txt")

      Dim content = $"This is a test file{vbCrLf}with multiple lines"
      IO.File.WriteAllText("test_lof.txt", content)

      Try

        Dim entries = New(Script As String, Expected As Object)() {
          (<sample><![CDATA[LET f = FREEFILE: OPEN "test_lof.txt" FOR INPUT AS f: LET result = LOF(f): CLOSE f]]></sample>.Value, CLng(content.Length))
        }

        For Each entry In entries
          Dim eval = Evaluate(entry.Script)
          Dim result = eval.Result
          Dim variables = eval.Variables
          Assert.Equal($"{entry.Expected}", $"{variables("result")}")
        Next

      Finally
        ' Clean up
        IO.File.Delete("test_lof.txt")
      End Try

    End Sub

    <Fact>
    Public Sub LOG()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[LET result$ = STR$(LOG(2))]]></bas>.Value, " .693147"),
          (<bas><![CDATA[LET result$ = STR$(LOG(1))]]></bas>.Value, " 0")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result$")}")
      Next

    End Sub

    'LPOS ... unable to test?

    <Fact>
    Public Sub LTRIM()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[LET result$ = LTRIM$("     BASIC")]]></bas>.Value, "BASIC")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub MID()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[A$="GOOD": B$="MORNING EVENING AFTERNOON": result$ = A$ + MID$(B$, 8, 8)]]></bas>.Value, "GOOD EVENING"),
          (<bas><![CDATA[A$="GOOD": result$ = MID$(A$, 5) + "X"]]></bas>.Value, "X"),
          (<bas><![CDATA[A$="GOOD": result$ = MID$(A$, 2)]]></bas>.Value, "OOD"),
          (<bas><![CDATA[A$="GOOD": result$ = MID$(A$, 1, 32767)]]></bas>.Value, "GOOD"),
          (<bas><![CDATA[A$="GOOD": result$ = MID$(A$, 256)]]></bas>.Value, "")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub MKD()

      Dim s As Double = 5.5
      Dim m1 = QBLib.Core.MKD(s)

      Dim b1_1 = AscW(m1.Substring(0, 1))
      Dim b1_2 = AscW(m1.Substring(1, 1))
      Dim b1_3 = AscW(m1.Substring(2, 1))
      Dim b1_4 = AscW(m1.Substring(3, 1))
      Dim b1_5 = AscW(m1.Substring(4, 1))
      Dim b1_6 = AscW(m1.Substring(5, 1))
      Dim b1_7 = AscW(m1.Substring(6, 1))
      Dim b1_8 = AscW(m1.Substring(7, 1))

      Assert.Equal(0, b1_1)
      Assert.Equal(0, b1_2)
      Assert.Equal(0, b1_3)
      Assert.Equal(0, b1_4)
      Assert.Equal(0, b1_5)
      Assert.Equal(0, b1_6)
      Assert.Equal(22, b1_7)
      Assert.Equal(64, b1_8)

      Dim entries = New(Script As String, Expected As String)() {
        (<bas><![CDATA[LET bin$ = MKD$(5.5): LET result = CVD(bin$)]]></bas>.Value, "5.5")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub MKDMBF()

      Dim s As Single = 5.5

      Dim b = QBLib.Core.MKDMBF(s)
      Dim r = QBLib.Core.CVDMBF(b)

      Dim b_1 = AscW(b.Substring(0, 1))
      Dim b_2 = AscW(b.Substring(1, 1))
      Dim b_3 = AscW(b.Substring(2, 1))
      Dim b_4 = AscW(b.Substring(3, 1))
      Dim b_5 = AscW(b.Substring(4, 1))
      Dim b_6 = AscW(b.Substring(5, 1))
      Dim b_7 = AscW(b.Substring(6, 1))
      Dim b_8 = AscW(b.Substring(7, 1))

      Assert.Equal(0, b_1)
      Assert.Equal(0, b_2)
      Assert.Equal(0, b_3)
      Assert.Equal(0, b_4)
      Assert.Equal(0, b_5)
      Assert.Equal(0, b_6)
      Assert.Equal(48, b_7)
      Assert.Equal(131, b_8)

      Assert.Equal(5.5, r)

      Dim entries = New(Script As String, Expected As String)() {
        (<bas><![CDATA[LET bin$ = MKDMBF$(5.5): LET result = CVDMBF(bin$)]]></bas>.Value, "5.5")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub MKI()

      Dim i As Short = 5
      Dim v = QBLib.Core.MKI(i)

      Dim b1 = AscW(v.Substring(0, 1))

      Assert.Equal(5, b1)

      Dim sample = <sample><![CDATA[
i = CVI(MKI$(5))
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("5", $"{variables("i")}")

    End Sub

    <Fact>
    Public Sub MKL()

      Dim entries = New(Script As String, Expected As String)() {
        (<bas><![CDATA[LET bin$ = MKL$(12345): LET result = CVL(bin$)]]></bas>.Value, "12345")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub MKS()

      Dim s As Single = 5.5
      Dim b = QBLib.Core.MKS(s)
      Dim r = QBLib.Core.CVS(b)

      ' Test what IEEE 754 little-endian bytes would be
      'Dim ieeeBytes(3) As Byte
      'BitConverter.GetBytes(s).CopyTo(ieeeBytes, 0)

      Dim b_1 = AscW(b.Substring(0, 1))
      Dim b_2 = AscW(b.Substring(1, 1))
      Dim b_3 = AscW(b.Substring(2, 1))
      Dim b_4 = AscW(b.Substring(3, 1))

      Assert.Equal(0, b_1)
      Assert.Equal(0, b_2)
      Assert.Equal(176, b_3)
      Assert.Equal(64, b_4)

      Assert.Equal(5.5, r)

      Dim entries = New(Script As String, Expected As String)() {
        (<bas><![CDATA[LET bin$ = MKS$(5.5): LET result = CVS(bin$)]]></bas>.Value, "5.5")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub MKSMBF()

      Dim s As Single = 5.5

      Dim b = QBLib.Core.MKSMBF(s)
      Dim r = QBLib.Core.CVSMBF(b)

      Dim b_1 = AscW(b.Substring(0, 1))
      Dim b_2 = AscW(b.Substring(1, 1))
      Dim b_3 = AscW(b.Substring(2, 1))
      Dim b_4 = AscW(b.Substring(3, 1))

      Assert.Equal(0, b_1)
      Assert.Equal(0, b_2)
      Assert.Equal(48, b_3)
      Assert.Equal(131, b_4)

      Assert.Equal(5.5, r)

      Dim entries = New(Script As String, Expected As String)() {
        (<bas><![CDATA[LET bin$ = MKSMBF$(5.5): LET result = CVSMBF(bin$)]]></bas>.Value, "5.5")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub OCT()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[result$ = OCT$(18)]]></bas>.Value, "22"),
          (<bas><![CDATA[result$ = OCT$(-32768.1)]]></bas>.Value, "37777700000"),
          (<bas><![CDATA[result$ = OCT$(65535.1)]]></bas>.Value, "177777"),
          (<bas><![CDATA[result$ = OCT$(8)]]></bas>.Value, "10"),
          (<bas><![CDATA[result$ = OCT$(16)]]></bas>.Value, "20"),
          (<bas><![CDATA[result$ = OCT$(0)]]></bas>.Value, "0"),
          (<bas><![CDATA[result$ = OCT$(63)]]></bas>.Value, "77")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub PEEK()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result% = PEEK(0)]]></bas>.Value, 0),
          (<bas><![CDATA[result% = PEEK(32768)]]></bas>.Value, 0),
          (<bas><![CDATA[result% = PEEK(65535)]]></bas>.Value, 0),
          (<bas><![CDATA[address = 65535: result% = PEEK(address)]]></bas>.Value, 0),
          (<bas><![CDATA[address! = 32768.5: result% = PEEK(address!)]]></bas>.Value, 0),
          (<bas><![CDATA[address# = 32768.5: result% = PEEK(address#)]]></bas>.Value, 0),
          (<bas><![CDATA[address = -1: result% = PEEK(address)]]></bas>.Value, 0), ' this should caus an error
          (<bas><![CDATA[address = 65536: result% = PEEK(address)]]></bas>.Value, 0) ' this should cause an error
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result%")}")
      Next

    End Sub

    'POINT ... unable to test?

    'POS ... unable to test?

    <Fact>
    Public Sub RIGHT()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[LET result$ = RIGHT$("DISK BASIC", 5)]]></bas>.Value, "BASIC"),
          (<bas><![CDATA[b$ = "DISK BASIC": result$ = RIGHT$(b$, 5)]]></bas>.Value, "BASIC"),
          (<bas><![CDATA[b$ = "DISK BASIC": result$ = RIGHT$(b$, LEN(b$))]]></bas>.Value, "DISK BASIC"),
          (<bas><![CDATA[b$ = "DISK BASIC": result$ = RIGHT$(b$, LEN(b$) + 1)]]></bas>.Value, "DISK BASIC"),
          (<bas><![CDATA[b$ = "DISK BASIC": result$ = RIGHT$(b$, 0) + "X"]]></bas>.Value, "X")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub RND()

      Dim sample = <sample><![CDATA[
' this test just checks to see if calling timer about
' a second apart will provide two different number.
'RANDOMIZE 'TIMER
r1! = RND
FOR x = 1 TO 100
  r2! = RND
  IF r1! <> r2! THEN EXIT FOR
NEXT
IF r1! <> r2! THEN result = 1 ELSE result = 0
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("1", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub RTRIM()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[LET result$ = RTRIM$("BASIC     ")]]></bas>.Value, "BASIC")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    'SCREEN ... unable to test

    <Fact>
    Public Sub SGN()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[LET result = SGN(-5%)]]></bas>.Value, -1),
          (<bas><![CDATA[LET result = SGN(0%)]]></bas>.Value, 0),
          (<bas><![CDATA[LET result = SGN(5%)]]></bas>.Value, 1)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Dim v = If(variables.ContainsKey("result"), variables("result"), 0)
        Assert.Equal($"{entry.Expected}", $"{v}")
      Next

    End Sub

    <Fact>
    Public Sub SIN()

      Dim entries = New(Script As String, Expected As Double)() {
          (<bas><![CDATA[LET result = SIN(0)]]></bas>.Value, 0.0),
          (<bas><![CDATA[LET result = SIN(1.5)]]></bas>.Value, 0.99749),
          (<bas><![CDATA[LET result = 1/SIN(0.5)]]></bas>.Value, 2.0858275890350342)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, CSng(variables("result")), 0.00001)
      Next

    End Sub

    <Fact>
    Public Sub SPACE()

      Dim sample = <sample><![CDATA[
A$ = ""
FOR N = 1 TO 5
  X$ = SPACE$(N)
  A$ = A$ + X$ + STR$(N)
NEXT N
]]></sample>.Value

      Dim expected = $"  1   2    3     4      5"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal(expected, variables("A$"))

    End Sub

    <Fact>
    Public Sub SQR()

      Dim entries = New(Script As String, Expected As Double)() {
          (<bas><![CDATA[LET result = SQR(0)]]></bas>.Value, 0.0),
          (<bas><![CDATA[LET result = SQR(4)]]></bas>.Value, 2.0),
          (<bas><![CDATA[LET result = SQR(9)]]></bas>.Value, 3.0)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub STR()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[N = 5555: result$ = STR$(N)]]></bas>.Value, " 5555")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub [STRING]()

      Dim entries = New(Script As String, Expected As String)() {
        (<bas><![CDATA[X$ = STRING$(10, 45): result$ = X$ + "DISK BASIC" + X$]]></bas>.Value, "----------DISK BASIC----------"),
        (<bas><![CDATA[result$ = STRING$(5, 65)]]></bas>.Value, "AAAAA"),
        (<bas><![CDATA[result$ = STRING$(3, "*")]]></bas>.Value, "***"),
        (<bas><![CDATA[result$ = STRING$(0, 42)]]></bas>.Value, ""),
        (<bas><![CDATA[result$ = STRING$(4, "AB")]]></bas>.Value, "AAAA")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub TAN()

      Dim entries = New(Script As String, Expected As Double)() {
          (<bas><![CDATA[LET result = TAN(0)]]></bas>.Value, 0.0),
          (<bas><![CDATA[X = 120: result = TAN(X)]]></bas>.Value, 0.713123),
          (<bas><![CDATA[LET result = 1/TAN(0.5)]]></bas>.Value, 1.8304893)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub TIME()

      Dim sample = <sample><![CDATA[
' this test just checks to see if calling TIME$ about
' a second apart will provide two different values.
t1$ = TIME$
SLEEP 1
t2$ = TIME$
IF t1$ <> t2$ THEN result = 1 ELSE result = 0
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("1", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub TIMER()

      Dim sample = <sample><![CDATA[
' this test just checks to see if calling timer about
' a second apart will provide two different number.
t1! = TIMER
SLEEP 1
t2! = TIMER
IF t1! <> t2! THEN result = 1 ELSE result = 0
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("1", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub UBOUND()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[a(1) = 5: result = UBOUND(a)]]></bas>.Value, 10),
          (<bas><![CDATA[DIM a(5 to 10): result = UBOUND(a)]]></bas>.Value, 10),
          (<bas><![CDATA[DIM arr(1 TO 5) : LET result = UBOUND(arr)]]></bas>.Value, 5),
          (<bas><![CDATA[DIM arr(0 TO 10) : LET result = UBOUND(arr)]]></bas>.Value, 10),
          (<bas><![CDATA[DIM arr(-5 TO 5) : LET result = UBOUND(arr)]]></bas>.Value, 5),
          (<bas><![CDATA[DIM arr(10 TO 20) : LET result = UBOUND(arr)]]></bas>.Value, 20),
          (<bas><![CDATA[DIM a(10): result = UBOUND(a)]]></bas>.Value, 10),
          (<bas><![CDATA['$DYNAMIC
DIM a(1 TO 3): REDIM a(5 TO 8): result = UBOUND(a)]]></bas>.Value, 8),
          (<bas><![CDATA[OPTION BASE 1: DIM a(10): result = UBOUND(a)]]></bas>.Value, 10),
          (<bas><![CDATA[OPTION BASE 1: a(1) = 5 : result = UBOUND(a)]]></bas>.Value, 10)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub UCASE()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[LET result$ = UCASE$("basic")]]></bas>.Value, "BASIC")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub VAL()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = VAL(" -3")]]></bas>.Value, -3),
          (<bas><![CDATA[result = VAL("A -3")]]></bas>.Value, 0)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub VARPTR()

      'NOTE: Although VARPTR/VARPTR$ isn't something that can "work", there is
      '      a side affect of using VARPTR and VARPTR$ that does need to function;
      '      and that is the fact that if the variable doesn't exist / isn't
      '      initialized before being passed to these functions, then it needs to
      '      be initialized (thus existing).
      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[z = VARPTR(result)]]></bas>.Value, ""),
          (<bas><![CDATA[z$ = VARPTR$(result)]]></bas>.Value, ""),
          (<bas><![CDATA[z = VARPTR(result)]]></bas>.Value, ""),
          (<bas><![CDATA[z$ = VARPTR$(result)]]></bas>.Value, "")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub VARSEG()

      'NOTE: Although VARSEG isn't something that can "work", there is
      '      a side affect of using VARSEG that does need to function;
      '      and that is the fact that if the variable doesn't exist / isn't
      '      initialized before being passed to these functions, then it needs to
      '      be initialized (thus existing).
      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[z = VARSEG(result)]]></bas>.Value, ""),
          (<bas><![CDATA[z = VARSEG(result)]]></bas>.Value, "")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub DefFnAccessingGlobalStringArray()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[DIM A$(3): A$(0) = "HELLO": DEF FNT$(X) = A$(X): result$ = FNT$(0)]]></bas>.Value, "HELLO"),
          (<bas><![CDATA[DIM A$(3): A$(1) = "WORLD": DEF FNT$(X) = A$(X): result$ = FNT$(1)]]></bas>.Value, "WORLD")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result$")}")
      Next

    End Sub

    <Fact>
    Public Sub DefFnAccessingGlobalNumericArray()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[DIM N(3): N(0) = 10: DEF FNV(X) = N(X): result = FNV(0)]]></bas>.Value, 10),
          (<bas><![CDATA[DIM N(3): N(1) = 20: DEF FNV(X) = N(X) * 2: result = FNV(1)]]></bas>.Value, 40),
          (<bas><![CDATA[DIM N(3): N(2) = 5: N(3) = 7: DEF FNV(X) = N(X) + N(X + 1): result = FNV(2)]]></bas>.Value, 12)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub DefFnWithComputedArrayIndex()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[DIM A$(3): A$(0) = "A": A$(1) = "B": DEF FNI(X) = ASC(A$(X \ 2)): result = FNI(0)]]></bas>.Value, 65),
          (<bas><![CDATA[DIM A$(3): A$(0) = "X": A$(1) = "Y": DEF FNI(X) = ASC(A$(X MOD 2)): result = FNI(1)]]></bas>.Value, 89),
          (<bas><![CDATA[DIM A$(3): A$(1) = "HELLO": DEF FNL(X) = LEN(A$(X AND 1)): result = FNL(1)]]></bas>.Value, 5),
          (<bas><![CDATA[DIM A$(3): A$(2) = "TEST": DEF FNT(X) = LEN(A$(X)): result = FNT(2)]]></bas>.Value, 4)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub DefFnWithMultipleGlobalVariables()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[M = 256: DIM RM$(255): RM$(100) = "BYTE": DEF FNB(X) = ASC(MID$(RM$(INT(X / 128) MOD M), (X AND 127) + 1, 1)): result = FNB(12800)]]></bas>.Value, 66),
          (<bas><![CDATA[M = 10: DIM ARR(9): ARR(5) = 100: DEF FNA(I) = ARR((I MOD M)): result = FNA(15)]]></bas>.Value, 100),
          (<bas><![CDATA[DIM A$(2): A$(0) = "HI": M = 3: DEF FNM(I) = LEN(A$(I)) + M: result = FNM(0)]]></bas>.Value, 5)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub DefFnNestedWithMidAndAsc()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[DIM B$(1): B$(0) = "ABC": DEF FNC(C) = ASC(MID$(B$(0), C, 1)): result = FNC(2)]]></bas>.Value, 66),
          (<bas><![CDATA[DIM B$(1): B$(0) = "HELLO": DEF FNC(C) = ASC(MID$(B$(0), C + 1, 1)): result = FNC(2)]]></bas>.Value, 76),
          (<bas><![CDATA[DIM B$(1): B$(0) = "XYZ": DEF FNC(C) = ASC(MID$(B$(C MOD 1), 1, 1)): result = FNC(0)]]></bas>.Value, 88),
          (<bas><![CDATA[DIM RM$(2): FOR A = 0 TO 2: FOR I = 0 TO 127: RM$(A) = RM$(A) + CHR$(I + 65): NEXT: NEXT: DEF FNB(X) = ASC(MID$(RM$(INT(X / 128)), (X AND 127) + 1, 1)): result = FNB(0)]]></bas>.Value, 65),
          (<bas><![CDATA[DIM RM$(2): FOR A = 0 TO 2: FOR I = 0 TO 127: RM$(A) = RM$(A) + CHR$(I + 65): NEXT: NEXT: DEF FNB(X) = ASC(MID$(RM$(INT(X / 128)), (X AND 127) + 1, 1)): result = FNB(128)]]></bas>.Value, 65),
          (<bas><![CDATA[DIM RM$(2): FOR A = 0 TO 2: FOR I = 0 TO 127: RM$(A) = RM$(A) + CHR$((I MOD 26) + 65): NEXT: NEXT: DEF FNB(X) = ASC(MID$(RM$(INT(X / 128)), (X AND 127) + 1, 1)): result = FNB(129)]]></bas>.Value, 66)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub GosubWithGlobalArrayAccess()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[DIM A(3): A(0) = 10: GOSUB 100: result = total: END: 100: total = total + A(0): RETURN]]></bas>.Value, 10),
          (<bas><![CDATA[DIM B$(2): B$(0) = "HELLO": GOSUB 100: result = LEN(B$(0)): END: 100: RETURN]]></bas>.Value, 5),
          (<bas><![CDATA[DIM N(2): N(0) = 5: N(1) = 10: GOSUB 100: result = N(0) + N(1): END: 100: N(0) = N(0) * 2: RETURN]]></bas>.Value, 20)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub DefFnParameterShadowingGlobal()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[M = 10: DEF FNM(M) = M * 2: result = FNM(5)]]></bas>.Value, 10),
          (<bas><![CDATA[DIM M(3): M(2) = 100: DEF FNA(A) = A + 1: result = FNA(M(2))]]></bas>.Value, 101),
          (<bas><![CDATA[DIM X(2): X(1) = 50: DEF FNF(X) = X(1) + 5: result = FNF(0)]]></bas>.Value, 55)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub DefFnExpressionWithGlobalAndParameter()

      Dim sample = <sample><![CDATA[
M = 256
DIM RM$(255)
RM$(100) = STRING$(128, 65)
DEF FNB(X) = ASC(MID$(RM$(INT(X / 128) MOD M), (X AND 127) + 1, 1))
result = FNB(12800)
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("65", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub IntegerDivisionThroughIntAndFloat()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[X = 256: result = INT(X / 256)]]></bas>.Value, 1),
          (<bas><![CDATA[X = 512: result = INT(X / 256)]]></bas>.Value, 2),
          (<bas><![CDATA[X = 300: result = INT(X / 256)]]></bas>.Value, 1),
          (<bas><![CDATA[X = 255: result = INT(X / 256)]]></bas>.Value, 0),
          (<bas><![CDATA[X = 257: result = INT(X / 256)]]></bas>.Value, 1),
          (<bas><![CDATA[X = 12800: result = INT(X / 128)]]></bas>.Value, 100),
          (<bas><![CDATA[X = 1000: result = INT(X / 128)]]></bas>.Value, 7),
          (<bas><![CDATA[X = 32768: result = INT(X / 256)]]></bas>.Value, 128)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub BitwiseAndOperations()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = 255 AND 255]]></bas>.Value, 255),
          (<bas><![CDATA[result = 256 AND 255]]></bas>.Value, 0),
          (<bas><![CDATA[result = 257 AND 255]]></bas>.Value, 1),
          (<bas><![CDATA[result = 128 AND 127]]></bas>.Value, 0),
          (<bas><![CDATA[result = 129 AND 127]]></bas>.Value, 1),
          (<bas><![CDATA[result = 255 AND 127]]></bas>.Value, 127),
          (<bas><![CDATA[result = 300 AND 255]]></bas>.Value, 44),
          (<bas><![CDATA[result = -1 AND 255]]></bas>.Value, 255),
          (<bas><![CDATA[X = 128: result = X AND 127]]></bas>.Value, 0),
          (<bas><![CDATA[X = 255: result = X AND 127]]></bas>.Value, 127),
          (<bas><![CDATA[X = 100: result = X AND 255]]></bas>.Value, 100)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub MidStatementNotFunction()

      Dim entries = New(Script As String, Expected As String)() {
          (<bas><![CDATA[DIM A$(1): A$(0) = "XXXXXXXX": MID$(A$(0), 1, 1) = "A": result$ = A$(0)]]></bas>.Value, "AXXXXXXX"),
          (<bas><![CDATA[DIM A$(1): A$(0) = "XXXXXXXX": MID$(A$(0), 2, 3) = "BCD": result$ = A$(0)]]></bas>.Value, "XBCDXXXX"),
          (<bas><![CDATA[DIM A$(1): A$(0) = "XXXXXXXX": MID$(A$(0), 4, 2) = "YY": result$ = A$(0)]]></bas>.Value, "XXXYYXXX"),
          (<bas><![CDATA[DIM A$(1): A$(0) = "HELLO": MID$(A$(0), 2, 1) = "A": result$ = A$(0)]]></bas>.Value, "HALLO")
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal(entry.Expected, variables("result$"))
      Next

    End Sub

    <Fact>
    Public Sub StringArrayBuildingWithConcatenation()

      Dim sample = <sample><![CDATA[
DIM FT$(2)
FOR X = 0 TO 2
  FT$(X) = ""
NEXT X
FOR Y = 0 TO 3
  FOR X = 0 TO 2
    FT$(X) = FT$(X) + CHR$(65 + X + Y)
  NEXT X
NEXT Y
result$ = FT$(0) + "|" + FT$(1) + "|" + FT$(2)
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("ABCD|BCDE|CDEF", variables("result$"))

    End Sub

    <Fact>
    Public Sub ForLoopShadowingArray()

      Dim sample = <sample><![CDATA[
DIM PT(3)
PT(0) = 10
PT(1) = 20
PT(2) = 30
FOR PT = 1 TO 5
  PT = PT + PT
NEXT PT
result = PT
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("7", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub CombinedIntDivisionAndBitwise()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[X = 1000: result = INT(X / 256) AND 255]]></bas>.Value, 3),
          (<bas><![CDATA[X = 1000: result = (X \ 256) AND 255]]></bas>.Value, 3),
          (<bas><![CDATA[X = 700: result = INT(X / 128) MOD 2]]></bas>.Value, 1),
          (<bas><![CDATA[X = 256: result = INT(X / 128) MOD 2]]></bas>.Value, 0),
          (<bas><![CDATA[X = 255: result = X AND 127]]></bas>.Value, 127),
          (<bas><![CDATA[X = 256: result = X AND 127]]></bas>.Value, 0)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub


    <Fact>
    Public Sub HexConstants()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = &HFF]]></bas>.Value, 255),
          (<bas><![CDATA[result = &H10]]></bas>.Value, 16),
          (<bas><![CDATA[result = &H100]]></bas>.Value, 256),
          (<bas><![CDATA[result = &H3C00]]></bas>.Value, 15360)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub IfComparisonOperators()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[X = 5: IF X = 5 THEN result = 1 ELSE result = 0]]></bas>.Value, 1),
          (<bas><![CDATA[X = 5: IF X <> 5 THEN result = 1 ELSE result = 0]]></bas>.Value, 0),
          (<bas><![CDATA[X = 5: IF X > 3 THEN result = 1 ELSE result = 0]]></bas>.Value, 1),
          (<bas><![CDATA[X = 5: IF X < 3 THEN result = 1 ELSE result = 0]]></bas>.Value, 0),
          (<bas><![CDATA[X = 5: IF X >= 5 THEN result = 1 ELSE result = 0]]></bas>.Value, 1),
          (<bas><![CDATA[X = 5: IF X <= 5 THEN result = 1 ELSE result = 0]]></bas>.Value, 1)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub ComparisonToZero()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = 5 = 0]]></bas>.Value, 0),
          (<bas><![CDATA[result = 0 = 0]]></bas>.Value, -1),
          (<bas><![CDATA[result = -1 = 0]]></bas>.Value, 0),
          (<bas><![CDATA[result = 5 <> 0]]></bas>.Value, -1),
          (<bas><![CDATA[result = 0 <> 0]]></bas>.Value, 0)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub BitwiseNot()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[result = NOT 0]]></bas>.Value, -1),
          (<bas><![CDATA[result = NOT -1]]></bas>.Value, 0),
          (<bas><![CDATA[result = NOT 255]]></bas>.Value, -256)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub NegativeNumberComparisons()

      Dim entries = New(Script As String, Expected As Object)() {
          (<bas><![CDATA[X = -1: result = X > 127]]></bas>.Value, 0),
          (<bas><![CDATA[X = -1: result = X < 0]]></bas>.Value, -1),
          (<bas><![CDATA[X = -128: result = X >= -128]]></bas>.Value, -1),
          (<bas><![CDATA[X = -129: result = X < -128]]></bas>.Value, -1)
      }

      For Each entry In entries
        Dim eval = Evaluate(entry.Script)
        Dim result = eval.Result
        Dim variables = eval.Variables
        Assert.Equal($"{entry.Expected}", $"{variables("result")}")
      Next

    End Sub

    <Fact>
    Public Sub RestoreWithLabelName()

      ' it appears that a label *can* be on the same line as *more code*
      ' so we need to account for the fact that a line starting with a string
      ' token followed by a colon can be either a label or parameter-less SUB

      Dim sample = "
RESTORE mylabel
READ A
RESTORE another
READ B
'PRINT A
'PRINT B
END
mylabel: DATA 42
another: DATA 99
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Empty(result.Diagnostics)
      Assert.Equal("42", $"{variables("A")}")
      Assert.Equal("99", $"{variables("B")}")

    End Sub

    <Fact>
    Public Sub TypeWithArrayParameter()

      Dim sample = "
TYPE snakeBody
  row AS INTEGER
  col AS INTEGER
END TYPE

TYPE snaketype
  length AS INTEGER
  head AS INTEGER
END TYPE

DECLARE SUB TestSub (snake() AS snaketype)

DIM s(10) AS snaketype
s(1).length = 5
result = s(1).length
END
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Empty(result.Diagnostics)
      Assert.Equal("5", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub TypeWithMultipleArrayParameters()

      Dim sample = "
TYPE snakeBody
  row AS INTEGER
  col AS INTEGER
END TYPE

TYPE snaketype
  length AS INTEGER
  head AS INTEGER
END TYPE

DECLARE SUB TestSub (snake() AS snaketype, snakeBod() AS snakeBody, snakeNum)

DIM s(10) AS snaketype
DIM b(10) AS snakeBody
s(1).length = 4 '5
b(2).row = 9 '10
CALL TestSub(s(), b(), 1)
result = s(1).length + b(2).row
END

SUB TestSub (snake() AS snaketype, snakeBod() AS snakeBody, snakeNum)
  snake(1).length = snake(1).length + snakeNum
  snakeBod(2).row = snakeBod(2).row + snakeNum
END SUB
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Empty(result.Diagnostics)
      Assert.Equal("15", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub SubParameterShadowNameResolution()

      Dim sample = "
'DECLARE SUB Adder (z() AS INTEGER, num AS INTEGER)

DIM arr(5) AS INTEGER
arr(1) = 10
CALL Adder(arr(), 20)
result = arr(1)
END

SUB Adder(x() AS INTEGER, num AS INTEGER)
  x(1) = x(1) + num
  'x(1) = num
END SUB
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Empty(result.Diagnostics)
      Assert.Equal("30", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub SubParameterTypeResolution()

      Dim sample = "
TYPE PointType
  x AS INTEGER
  y AS INTEGER
END TYPE

DECLARE SUB MoveSnake (snake() AS PointType, num)

DIM snake(5) AS PointType
snake(1).x = 10
snake(1).y = 20
CALL MoveSnake(snake(), 5)
result = snake(1).x + snake(1).y

END

SUB MoveSnake(snake() AS PointType, num)
  snake(1).x = snake(1).x + num
END SUB
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Empty(result.Diagnostics)
      Assert.Equal("35", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub VariableScopeInLoopWhile()

      Dim sample = "
DECLARE SUB TestSub ()
StillWantsToPlay = -1
DO
  CALL TestSub
LOOP WHILE StillWantsToPlay
result = 1
END

SUB TestSub
  StillWantsToPlay = 0
END SUB
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Empty(result.Diagnostics)
      Assert.Equal("1", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub SubCallsWithMixedParameterTypes()

      Dim sample = "
'DECLARE SUB GetInputs (NumPlayers, speed, diff$, monitor$)

'DIM NumPlayers AS INTEGER
'DIM speed AS SINGLE
'DIM diff$ AS STRING
'DIM monitor$ AS STRING

CALL GetInputs(NumPlayers, speed, diff$, monitor$)

result$ = diff$ + monitor$
END

SUB GetInputs (NumPlayers, speed, diff$, monitor$)
  NumPlayers = 1
  speed = 5.5
  diff$ = ""EASY""
  monitor$ = ""Mono""
END SUB
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Empty(result.Diagnostics)
      Assert.Equal("EASYMono", $"{variables("result$")}")

    End Sub

  End Class

End Namespace
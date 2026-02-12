Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

Imports QBLib

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class ArrayTests

    Private Shared Function Evaluate(text As String) As (Result As EvaluationResult, Output As String, Variables As Dictionary(Of String, Object))
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
    End Function

    <Fact>
    Public Sub CLEAR()

      Dim sample = <sample><![CDATA[
DIM a(20)
a(1) = 5
CLEAR
'PRINT a(1)
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Dim a = CType(variables("a"), List(Of Object))
      Assert.Equal("0", $"{a(1)}")

    End Sub

    <Fact>
    Public Sub DIM_1()

      Dim sample = "
DIM A(20)
FOR I=0 TO 20
  READ A(I)
NEXT I
'PRINT A(I - 1)
DATA 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20
"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Dim a = CType(variables("A"), List(Of Object))
      Assert.Equal("20", $"{a(20)}")
      Assert.Equal("21", $"{variables("I")}")

    End Sub

    <Fact>
    Public Sub DIM_2E()

      Dim sample = "
DIM B(250)
DIM B(3,4)
"

      Dim expected = "Wrong number of dimensions"

      Dim eval = Evaluate(sample)
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
    Public Sub ERASE_1E()

      Dim sample = <sample><![CDATA[
ERASE A
]]></sample>.Value

      Dim expected = "Array not defined"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub ERASE_2E()

      ' Doesn't matter if '$STATIC or '$DYNAMIC
      ' Arrays appear to not be able to have dimension
      ' changed after initialized; even after ERASE

      Dim sample = <sample><![CDATA[
DIM B(250)
ERASE B
DIM B(3,4)
]]></sample>.Value

      Dim expected = "Wrong number of dimensions"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub ERASE_3E()

      ' With '$STATIC (the default), arrays elements are reset to default
      ' With `$DYNAMIC, the array is reset - requiring a new DIM statement.

      Dim sample = <sample><![CDATA[
'$STATIC
DIM B(250)
ERASE B
DIM B(3)
]]></sample>.Value

      Dim expected = "Array already dimensioned"

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub ERASE_4()

      ' Test '$DYNAMIC ERASE/DIM

      Dim sample = <sample><![CDATA[
'$DYNAMIC
DIM B(250)
ERASE B
DIM B(3)
FIN = 1
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("1", $"{variables("FIN")}")

    End Sub

    <Fact>
    Public Sub ERASE_5()

      ' Test static array element reset to defaults

      Dim sample = <sample><![CDATA[
'$STATIC
DIM numbers(3)
numbers(0) = 100
numbers(1) = 200
numbers(2) = 300
ERASE numbers
result1 = numbers(0)
result2 = numbers(1)
result3 = numbers(2)
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal(0L, CLng(variables("result1")))
      Assert.Equal(0L, CLng(variables("result2")))
      Assert.Equal(0L, CLng(variables("result3")))

    End Sub

    <Fact>
    Public Sub ERASE_6()

      ' Test string array element reset to empty strings

      Dim sample = <sample><![CDATA[
'$STATIC
DIM names(2)
names(0) = "Alice"
names(1) = "Bob"
names(2) = "Charlie"
ERASE names
result1$ = names(0)
result2$ = names(1)
result3$ = names(2)
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal("", variables("result1$"))
      Assert.Equal("", variables("result2$"))
      Assert.Equal("", variables("result3$"))

    End Sub

    <Fact>
    Public Sub ERASE_7()

      ' Test ERASE on different numeric types

      Dim sample = <sample><![CDATA[
'$STATIC
DIM intArray%(2)
DIM sngArray!(2)
DIM dblArray#(2)
DIM lngArray&(2)
intArray%(0) = 42
sngArray!(0) = 3.14
dblArray#(0) = 2.71828
lngArray&(0) = 987654321
ERASE intArray%, sngArray!, dblArray#, lngArray&
result1% = intArray%(0)
result2! = sngArray!(0)
result3# = dblArray#(0)
result4& = lngArray&(0)
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal(0%, variables("result1%"))
      Assert.Equal(0!, variables("result2!"))
      Assert.Equal(0#, variables("result3#"))
      Assert.Equal(0&, variables("result4&"))

    End Sub

    <Fact>
    Public Sub ERASE_8()

      ' Test ERASE on non-array variable should produce error

      Dim sample = <sample><![CDATA[
'$STATIC
DIM x AS INTEGER
x = 42
ERASE x
]]></sample>.Value

      Dim expected = "ERASE can only be used on arrays, not on variable 'x'."

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim actual = eval.Output?.Trim
      Dim variables = eval.Variables

      Assert.Equal(expected, actual)

    End Sub

    <Fact>
    Public Sub ERASE_9()

      ' Test multiple arrays in single ERASE statement

      Dim sample = <sample><![CDATA[
'$STATIC
DIM arr1(2)
DIM arr2(2)
arr1(0) = 100
arr1(1) = 200
arr2(0) = 300
arr2(1) = 400
ERASE arr1, arr2
result1 = arr1(0)
result2 = arr1(1)
result3 = arr2(0)
result4 = arr2(1)
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim result = eval.Result
      Dim variables = eval.Variables

      Assert.Equal(0L, CLng(variables("result1")))
      Assert.Equal(0L, CLng(variables("result2")))
      Assert.Equal(0L, CLng(variables("result3")))
      Assert.Equal(0L, CLng(variables("result4")))

    End Sub

  End Class

End Namespace
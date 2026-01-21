Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax
Imports QB.CodeAnalysis.Text

Imports QBLib

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class ErrorTests

    Private Function Evaluate(text As String) As (Result As EvaluationResult, Variables As Dictionary(Of String, Object))
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim variables = New Dictionary(Of String, Object)
      Dim result = compilation.Evaluate(variables)
      Return (result, variables)
    End Function

    <Fact>
    Public Sub Test_ERR_ERL_NoError()
      ' Test ERR and ERL functions return 0 when no error
      Dim noErrorTest = "LET errVal = ERR : LET erlVal = ERL"
      Dim noErrorTree As SyntaxTree = SyntaxTree.Parse(noErrorTest)
      Dim noErrorComp As Compilation = Compilation.Create(noErrorTree)
      Dim noErrorVars As New Dictionary(Of String, Object)()
      Dim noErrorResult = noErrorComp.Evaluate(noErrorVars)
      Assert.Equal(0L, CLng(noErrorVars("errVal")))
      Assert.Equal(0L, CLng(noErrorVars("erlVal")))
    End Sub

    <Fact>
    Public Sub Test_ERROR_Number()

      ' Test various error codes
      Dim errorCodes = New Integer() {2, 5, 6, 7, 11, 13}
      Dim expectedMessages = New String() {
           "Syntax error",
           "Illegal function call",
           "Overflow",
           "Out of memory",
           "Division by zero",
           "Type mismatch"
       }

      For i = 0 To errorCodes.Length - 1
        Dim errorCodeTest = $"ERROR {errorCodes(i)}"
        Dim errorCodeTree As SyntaxTree = SyntaxTree.Parse(errorCodeTest)
        Dim errorCodeComp As Compilation = Compilation.Create(errorCodeTree)
        Dim errorCodeVars As New Dictionary(Of String, Object)()
        Dim errorEx As Exception = Nothing
        Try
          errorCodeComp.Evaluate(errorCodeVars)
        Catch e As Exception
          errorEx = e
        End Try
        Assert.NotNull(errorEx)
        Assert.Contains($"Error {errorCodes(i)}: {expectedMessages(i)}", errorEx.Message)
      Next

    End Sub

    <Fact>
    Public Sub Test_ON_ERROR_GOTO_100()

      ' Test ON ERROR GOTO statement (basic parsing and execution)
      Dim onErrorTest = "
ON ERROR GOTO 100
PRINT 1/0
PRINT ""Should not print""
100
PRINT ""Done""
"
      Dim originalStdoutMode = Video.StdoutMode
      Try
        Video.StdoutMode = True ' Run in stdout mode like --stdout flag
        Dim onErrorTree As SyntaxTree = SyntaxTree.Parse(onErrorTest)
        Dim onErrorComp As Compilation = Compilation.Create(onErrorTree)
        Dim onErrorVars As New Dictionary(Of String, Object)()
        Dim onErrorResult = onErrorComp.Evaluate(onErrorVars)
      Finally
        Video.StdoutMode = originalStdoutMode ' Restore original mode
      End Try

    End Sub

    <Fact>
    Public Sub Test_ON_ERROR_GOTO_Handler()

      ' Test ON ERROR GOTO statement (basic parsing and execution)
      Dim onErrorTest = "
ON ERROR GOTO Handler
PRINT 1/0
PRINT ""Should not print""

Handler:
  PRINT ""Done""
  END
"
      Dim originalStdoutMode = Video.StdoutMode
      Try
        Video.StdoutMode = True ' Run in stdout mode like --stdout flag
        Dim onErrorTree As SyntaxTree = SyntaxTree.Parse(onErrorTest)
        Dim onErrorComp As Compilation = Compilation.Create(onErrorTree)
        Dim onErrorVars As New Dictionary(Of String, Object)()
        Dim onErrorResult = onErrorComp.Evaluate(onErrorVars)
      Finally
        Video.StdoutMode = originalStdoutMode ' Restore original mode
      End Try

    End Sub

    <Fact>
    Public Sub Test_ON_ERROR_GOTO_Handler_RESUME_NEXT()

      ' Test RESUME NEXT with error handling
      Dim resumeNextTest = "
ON ERROR GOTO Handler
PRINT ""Attempt...""
PRINT 1/0
PRINT ""Done.""
END

Handler:
  PRINT ""ERROR PROCESSING AT LINE""; ERL
  PRINT ""ERROR NUMBER""; ERR
  RESUME NEXT
"
      Dim originalStdoutMode = Video.StdoutMode
      Try
        Video.StdoutMode = True ' Run in stdout mode like --stdout flag
        Dim resumeTree As SyntaxTree = SyntaxTree.Parse(resumeNextTest)
        Dim resumeComp As Compilation = Compilation.Create(resumeTree)
        Dim resumeVars As New Dictionary(Of String, Object)()
        Dim resumeResult = resumeComp.Evaluate(resumeVars)
      Finally
        Video.StdoutMode = originalStdoutMode ' Restore original mode
      End Try
    End Sub

    <Fact>
    Public Sub TestError4_OutOfDATA()
      ' 4 Out of DATA: A READ statement is executed when there are no DATA statements with unread data remaining in the program.

      Dim text = "
ON ERROR GOTO Handler
READ X
END

handler:
  output$ = ""Error"" + STR$(ERR)
  RESUME NEXT
"

      Dim evalResult = Evaluate(text)
      Dim result = evalResult.Result
      Dim vars = evalResult.Variables
      If result.Diagnostics.Length > 0 Then
        Assert.Fail("Diagnostics: " & String.Join(", ", result.Diagnostics.Select(Function(d) d.Message)))
      End If
      Assert.True(vars.ContainsKey("output$"))
      Assert.Equal("Error 4", vars("output$"))
    End Sub

    '    <Fact>
    '    Public Sub TestError5_IllegalFunctionCall_ArrayBoundsInvalid()
    '      ' 5 Illegal function call: An out-of-range parameter is passed to a math or string function. An illegal function call error may also occur as the result of: a negative or unreasonably large subscript, etc.

    '      Dim text = "
    'ON ERROR GOTO handler
    'DIM A(-1 TO 10)
    'END
    'handler:
    'output$ = ""Error"" + STR$(ERR)
    'RESUME NEXT
    '    "

    '      Dim evalResult = Evaluate(text)
    '      Dim result = evalResult.Result
    '      Dim vars = evalResult.Variables
    '      Assert.Equal("Error 5", vars("OUTPUT$"))
    '    End Sub

    '    <Fact>
    '    Public Sub TestError7_OutOfMemory_ArrayTooLarge()
    '      ' 7 Out of memory: A program is too large, has too many FOR loops, GOSUBs, variables, or expressions that are too complicated. Use the CLEAR statement to set aside more stack space or memory area.

    '      Dim text = "
    'ON ERROR GOTO handler
    'DIM A(65536)
    'END
    'handler:
    'output$ = ""Error"" + STR$(ERR)
    'RESUME NEXT
    '"

    '      Dim evalResult = Evaluate(text)
    '      Dim result = evalResult.Result
    '      Dim vars = evalResult.Variables
    '      Assert.Equal("Error 7", vars("OUTPUT$"))
    '    End Sub

    '    <Fact>
    '    Public Sub TestError9_SubscriptOutOfRange()
    '      ' 9 Subscript out of range: An array element is referenced either with a subscript that is outside the dimensions of the array, or with the wrong number of subscripts.

    '      Dim text = "
    'ON ERROR GOTO handler
    'DIM A(10)
    'A(11) = 1
    'END
    'handler:
    'output$ = ""Error"" + STR$(ERR)
    'RESUME NEXT
    '"

    '      Dim evalResult = Evaluate(text)
    '      Dim result = evalResult.Result
    '      Dim vars = evalResult.Variables
    '      Assert.Equal("Error 9", vars("OUTPUT$"))
    '    End Sub

    '    <Fact>
    '    Public Sub TestError13_TypeMismatch_VariableNotArray()
    '      ' 13 Type mismatch: A string variable name is assigned a numeric value or vice versa; a function that expects a numeric argument is given a string argument or vice versa.

    '      Dim text = "
    'ON ERROR GOTO handler
    'X = 1
    'X(1) = 2
    'END
    'handler:
    'output$ = ""Error"" + STR$(ERR)
    'RESUME NEXT
    '"

    '      Dim evalResult = Evaluate(text)
    '      Dim result = evalResult.Result
    '      Dim vars = evalResult.Variables
    '      Assert.Equal("Error 13", vars("OUTPUT$"))
    '    End Sub

    '    <Fact>
    '    Public Sub TestVariableAssignment()
    '      Dim text = "output$ = ""test"""
    '      Dim evalResult = Evaluate(text)
    '      Dim result = evalResult.Result
    '      Dim vars = evalResult.Variables
    '      If result.Diagnostics.Count > 0 Then
    '        Assert.Fail("Diagnostics: " & String.Join(", ", result.Diagnostics.Select(Function(d) d.Message)))
    '      End If
    '      Console.WriteLine("Vars count: " & vars.Count)
    '      For Each kv In vars
    '        Console.WriteLine("Key: '" & kv.Key & "', Value: '" & kv.Value.ToString & "'")
    '      Next
    '      Assert.Equal("test", vars("OUTPUT$"))
    '    End Sub

  End Class

End Namespace
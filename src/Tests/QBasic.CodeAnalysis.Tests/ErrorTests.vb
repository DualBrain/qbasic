Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

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
    Public Sub TestError04_OutOfDATA()
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

    <Fact>
    Public Sub TestError05_IllegalFunctionCall_ArrayBoundsInvalid()
      ' 5 Illegal function call: An out-of-range parameter is passed to a math or string function. An illegal function call error may also occur as the result of: a negative or unreasonably large subscript, etc.

      Dim text = "
ON ERROR GOTO handler
A$ = CHR$(-1)
END

handler:
  output$ = ""Error"" + STR$(ERR)
  RESUME NEXT
"

      Dim evalResult = Evaluate(text)
      Dim result = evalResult.Result
      Dim vars = evalResult.Variables
      Assert.Equal("Error 5", vars("output$"))

    End Sub

    '    'TODO: Need to decide how to handle "build" errors.
    '    '      Currently being handled as "diagnostics", but might want
    '    '      to consider a more "QBasic" approach?
    '    <Fact>
    '    Public Sub TestError06_Overflow_ArrayTooLarge()
    '      ' 6 Overflow: The result of a calculation is too large to be represented in GW-BASIC's number format. If underflow occurs, the result is zero, and execution continues without an error.

    '      Dim text = "
    'ON ERROR GOTO handler
    'DIM A(65536)
    'END

    'handler:
    '  output$ = ""Error"" + STR$(ERR)
    '  RESUME NEXT
    '"

    '      Dim evalResult = Evaluate(text)
    '      Dim result = evalResult.Result
    '      Dim vars = evalResult.Variables
    '      Assert.Equal("Error 6", vars("output$"))

    '    End Sub

    <Fact>
    Public Sub TestError09_SubscriptOutOfRange()
      ' 9 Subscript out of range: An array element is referenced either with a subscript that is outside the dimensions of the array, or with the wrong number of subscripts.

      Dim text = "
ON ERROR GOTO handler
DIM A(10)
A(11) = 1
END

handler:
  output$ = ""Error"" + STR$(ERR)
  RESUME NEXT
"

      Dim evalResult = Evaluate(text)
      Dim result = evalResult.Result
      Dim vars = evalResult.Variables
      Assert.Equal("Error 9", vars("output$"))

    End Sub

    <Fact>
    Public Sub TestError13_TypeMismatch_NumEqualString()
      ' 13 Type mismatch: A string variable name is assigned a numeric value or vice versa; a function that expects a numeric argument is given a string argument or vice versa.

      Dim text = "
ON ERROR GOTO handler
X = 1
X = ""2""
END

handler:
  output$ = ""Error"" + STR$(ERR)
  RESUME NEXT
"

      Dim evalResult = Evaluate(text)
      Dim result = evalResult.Result
      Dim vars = evalResult.Variables
      Assert.Equal("Error 13", vars("output$"))

    End Sub

    <Fact>
    Public Sub TestError13_TypeMismatch_StringEqualNum()
      ' 13 Type mismatch: A string variable name is assigned a numeric value or vice versa; a function that expects a numeric argument is given a string argument or vice versa.

      Dim text = "
ON ERROR GOTO handler
X$ = ""1""
X$ = 2
END

handler:
  output$ = ""Error"" + STR$(ERR)
  RESUME NEXT
"

      Dim evalResult = Evaluate(text)
      Dim result = evalResult.Result
      Dim vars = evalResult.Variables
      Assert.Equal("Error 13", vars("output$"))

    End Sub

  End Class

End Namespace
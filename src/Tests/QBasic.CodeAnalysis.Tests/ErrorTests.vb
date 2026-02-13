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

        Dim test = $"
ON ERROR GOTO Handler
ERROR {errorCodes(i)}
END
Handler:
  e = ERR: l = ERL
  END"

        Dim eval = Evaluate(test)
        Dim result = eval.Result
        Dim vars = eval.Variables

        Assert.Equal($"{errorCodes(i)}", $"{vars("e")}")
        Assert.Equal("0", $"{vars("l")}")

      Next

    End Sub

    <Fact>
    Public Sub Test_ON_ERROR_GOTO_100()

      ' Test ON ERROR GOTO statement (basic parsing and execution)

      Dim onErrorTest = "
ON ERROR GOTO 100
'PRINT 1/0
result = 0
a = 1/0
result = 1 'PRINT ""Should not print""
END
100
result = -1 'PRINT ""Done""
"

      Dim eval = Evaluate(onErrorTest)
      Dim result = eval.Result
      Dim vars = eval.Variables

      Assert.Equal("-1", $"{vars("result")}")

    End Sub

    <Fact>
    Public Sub Test_ON_ERROR_GOTO_Handler()

      ' Test ON ERROR GOTO statement (basic parsing and execution)

      Dim onErrorTest = "
ON ERROR GOTO Handler
result = 0
'PRINT 1/0
a = 1/0
'PRINT ""Should not print""
result = 1

Handler:
  'PRINT ""Done""
  result = -1
  END
"

      Dim eval = Evaluate(onErrorTest)
      Dim result = eval.Result
      Dim vars = eval.Variables

      Assert.Equal("-1", $"{vars("result")}")

    End Sub

    <Fact>
    Public Sub Test_ON_ERROR_GOTO_Handler_RESUME_NEXT()

      ' Test RESUME NEXT with error handling

      Dim resumeNextTest = "
ON ERROR GOTO Handler
'PRINT ""Attempt...""
result = 0
'PRINT 1/0
a = 1/0
'PRINT ""Done.""
result = 1 
END

Handler:
  'PRINT ""ERROR PROCESSING AT LINE""; ERL
  'PRINT ""ERROR NUMBER""; ERR
  result = -1
  RESUME NEXT
"

      Dim eval = Evaluate(resumeNextTest)
      Dim result = eval.Result
      Dim vars = eval.Variables

      Assert.Equal("1", $"{vars("result")}")

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
Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class ExitTests

    Private Shared Function Evaluate(text As String, Optional commandLineArgs() As String = Nothing) As (Result As EvaluationResult, Variables As Dictionary(Of String, Object))

      Dim variables = New Dictionary(Of String, Object)

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation
      Dim result As EvaluationResult

      compilation = Compilation.Create(syntaxTree)
      result = compilation.Evaluate(variables, commandLineArgs)
      Return (result, variables)

    End Function

    <Fact>
    Public Sub ExitFunction_StopsExecutingRemainingStatements()

      Dim sample = <sample><![CDATA[
FUNCTION F%()
  F% = 1
  EXIT FUNCTION
  F% = 2
END FUNCTION

result = F%()
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim variables = eval.Variables

      Assert.Equal("1", $"{variables("result")}")

    End Sub

    <Fact>
    Public Sub ExitSub_StopsExecutingRemainingStatements()

      Dim sample = <sample><![CDATA[
SUB S()
  X = 1
  EXIT SUB
  X = 2
END SUB

CALL S
result = X
]]></sample>.Value

      Dim eval = Evaluate(sample)
      Dim variables = eval.Variables

      Assert.Equal("1", $"{variables("result")}")

    End Sub

  End Class

End Namespace

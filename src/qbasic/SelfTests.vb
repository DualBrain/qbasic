Imports System.IO

Module SelfTests

  Public Sub RunExitFunctionTest()
    Dim interpreter = New Global.QB.Interpreter()

    ' Ensure PRINT writes to stdout
    QBLib.Video.StdoutMode = True

    Dim originalOut = Console.Out
    Dim sw As New StringWriter()
    Console.SetOut(sw)
    Try
      Dim program = """
FUNCTION F()
  F = 1
  EXIT FUNCTION
  F = 2
END FUNCTION

A = F()
PRINT A
"""

      interpreter.Run(program, False, Nothing, Nothing, True)

      Dim output = sw.ToString()
      originalOut.WriteLine("--- SelfTest: EXIT FUNCTION ---")
      If output.Contains("1") Then
        originalOut.WriteLine("SelfTest EXIT FUNCTION: PASS")
      Else
        originalOut.WriteLine("SelfTest EXIT FUNCTION: FAIL")
        originalOut.WriteLine("Captured output:")
        originalOut.WriteLine(output)
      End If
    Finally
      Console.SetOut(originalOut)
      QBLib.Video.StdoutMode = False
    End Try
  End Sub

End Module

Imports System.IO

Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Emit
Imports QB.CodeAnalysis.Syntax

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class VbEmitterTests

    Private Shared Function Emit(source As String, Optional moduleName As String = "Test") As String

      Dim tree = SyntaxTree.Parse(source)
      Dim comp = Compilation.Create(tree)

      Dim temp = $"{Path.GetTempFileName}.vb"
      Try
        Dim diagnostics = comp.Emit(TargetPlatform.MicrosoftVisualBasic,
                                    moduleName,
                                    New String() {},
                                    temp)

        If diagnostics.Any() Then
          Dim errors = String.Join(Environment.NewLine, diagnostics.Select(Function(d) d.ToString()))
          Return $"Emit failed: {errors}"
        End If

        Return File.ReadAllText(temp)

      Finally
        If File.Exists(temp) Then File.Delete(temp)
      End Try

    End Function

    <Fact>
    Public Sub EmitsOptionStatements()
      Dim source = "PRINT ""Hello"""
      Dim result = Emit(source)
      Assert.Contains("Option Explicit Off", result)
      Assert.Contains("Option Strict Off", result)
      Assert.Contains("Option Infer Off", result)
    End Sub

    <Fact>
    Public Sub EmitsNamespace()
      Dim source = "PRINT ""Hello"""
      Dim result = Emit(source)
      Assert.Contains("Namespace ConvertedFromBsharp", result)
      Assert.Contains("End Namespace", result)
    End Sub

    <Fact>
    Public Sub EmitsModuleName()
      Dim source = "PRINT ""Hello"""
      Dim result = Emit(source, "MyProgram")
      Assert.Contains("Public Module MyProgram", result)
    End Sub

    <Fact>
    Public Sub EmitsMultipleStatements()
      Dim source = "CLS: PRINT ""Hello"""
      Dim result = Emit(source)
      Assert.Contains("System.Console.Clear()", result)
      Assert.Contains("System.Console.WriteLine(""Hello"")", result)
    End Sub

    <Fact>
    Public Sub EmitsClsStatement()
      Dim source = "CLS"
      Dim result = Emit(source)
      Assert.Contains("System.Console.Clear()", result)
    End Sub

    <Fact>
    Public Sub EmitsPrintStatement()
      Dim source = "PRINT ""Hello"""
      Dim result = Emit(source)
      Assert.Contains("System.Console.WriteLine(""Hello"")", result)
    End Sub

    <Fact>
    Public Sub EmitsPrintWithSemicolon()
      Dim source = "PRINT ""Hello"";"
      Dim result = Emit(source)
      Assert.Contains("System.Console.Write(""Hello"")", result)
    End Sub

    <Fact>
    Public Sub EmitsPrintMultipleWithSemicolons()
      Dim source = "PRINT ""Hello""; ""World"""
      Dim result = Emit(source)
      Assert.Contains("System.Console.Write(""Hello"")", result)
      Assert.Contains("System.Console.WriteLine(""World"")", result)
    End Sub

    <Fact>
    Public Sub EmitsPrintExpression()
      Dim text = "PRINT 1 + 1"
      Dim result = Emit(text)
      Assert.Contains("System.Console.WriteLine(2)", result)
    End Sub

    <Fact>
    Public Sub EmitsPrintWithConstantFolding()
      Dim source = "PRINT 1 + 1; ""test""; ""there"""
      Dim result = Emit(source)
      Assert.Contains("System.Console.Write(2)", result)
      Assert.Contains("System.Console.Write(""test"")", result)
      Assert.Contains("System.Console.WriteLine(""there"")", result)
    End Sub

  End Class

End Namespace

Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class CompilationTests

    <Fact>
    Public Sub CompilesValidCode()
      Dim text = "PRINT ""Hello"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub CompilesValidCodeWithIdentifier()
      Dim text = "PRINT invalid"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub CreatesVariableInGlobalScope()
      Dim text = "x = 42"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Assert.Single(compilation.Variables)
      Assert.Equal("x", compilation.Variables(0).Name)
    End Sub

  End Class

End Namespace
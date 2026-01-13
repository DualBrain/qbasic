Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax
Imports QB.CodeAnalysis.Text

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class DiagnosticTests

    <Fact>
    Public Sub ParsesInvalidIdentifier()
      Dim text = "123abc"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub ParsesPrintWithIdentifier()
      Dim text = "PRINT invalid"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

  End Class

End Namespace
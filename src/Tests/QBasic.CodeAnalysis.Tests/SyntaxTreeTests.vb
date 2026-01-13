Imports QB.CodeAnalysis.Syntax

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class SyntaxTreeTests

    <Fact>
    Public Sub SyntaxTreeHasCorrectRoot()
      Dim text = "PRINT ""Hello"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.NotNull(syntaxTree.Root)
      Assert.Equal(SyntaxKind.CompilationUnit, syntaxTree.Root.Kind)
    End Sub

    <Fact>
    Public Sub SyntaxTreeTracksSourceText()
      Dim text = "123"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.Equal(text, syntaxTree.Text.ToString())
    End Sub

    <Fact>
    Public Sub SyntaxTreeContainsMembers()
      Dim text = "PRINT ""A"": PRINT ""B"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.True(root.Members.Count >= 2)
    End Sub

    <Fact>
    Public Sub SyntaxTreePreservesSourceText()
      Dim text = "PRINT ""Hello World"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.Equal(text, syntaxTree.Text.ToString())
    End Sub

    <Fact>
    Public Sub SyntaxTreeHandlesEmptyInput()
      Dim text = ""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.Equal(SyntaxKind.CompilationUnit, root.Kind)
      Assert.Empty(root.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreeHandlesWhitespaceOnly()
      Dim text = "   " & vbCrLf & vbTab & "  "
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.Equal(SyntaxKind.CompilationUnit, root.Kind)
      Assert.Empty(root.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreeParsesMultipleStatements()
      Dim text = "x = 1" & vbCrLf & "PRINT x" & vbCrLf & "y = 2"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.Equal(3, root.Members.Count)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreeParsesStatementsWithLineNumbers()
      Dim text = "10 PRINT ""Hello""" & vbCrLf & "20 x = 42"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.Equal(2, root.Members.Count)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreeParsesStatementsWithLabels()
      Dim text = "Main:" & vbCrLf & "PRINT ""Start""" & vbCrLf & "GOTO Main"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.Equal(3, root.Members.Count) ' Label + Print + Goto
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreeHandlesSyntaxErrors()
      Dim text = "PRINT ABS(1" ' missing closing paren
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.NotEmpty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreePreservesTrivia()
      Dim text = "
' This is a comment
PRINT ""Hello""
  ' Another comment
"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.Single(root.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreeParsesComplexProgram()
      Dim text = "DIM x AS INTEGER" & vbCrLf &
                "DIM y(10) AS STRING" & vbCrLf &
                "" & vbCrLf &
                "SUB Test" & vbCrLf &
                "  x = 5" & vbCrLf &
                "  PRINT ""Value: ""; x" & vbCrLf &
                "END SUB" & vbCrLf &
                "" & vbCrLf &
                "Test" & vbCrLf &
                "PRINT ""Done"""

      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.True(root.Members.Count > 3) ' DIM statements, SUB, CALL, PRINT
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreeSpansCoverEntireText()
      Dim text = "PRINT ""Test"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.Equal(0, syntaxTree.Root.Span.Start)
      Assert.Equal(text.Length, syntaxTree.Root.Span.Length)
    End Sub

    <Fact>
    Public Sub SyntaxTreeHandlesUnicodeCharacters()
      Dim text = "PRINT ""Hello 世界"""
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)

      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreeParsesEmptySub()
      Dim text = "SUB EmptySub" & vbCrLf & "END SUB"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.Single(root.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

    <Fact>
    Public Sub SyntaxTreeParsesEmptyFunction()
      Dim text = "FUNCTION EmptyFunc() AS INTEGER" & vbCrLf & "END FUNCTION"
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim root = DirectCast(syntaxTree.Root, CompilationUnitSyntax)

      Assert.Single(root.Members)
      Assert.Empty(syntaxTree.Diagnostics)
    End Sub

  End Class

End Namespace
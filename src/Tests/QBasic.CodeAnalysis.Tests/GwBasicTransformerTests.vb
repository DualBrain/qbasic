' ============================================================================'
' Unit Tests for GW-BASIC to QBasic Transformer
' Tests the transformation of line-numbered GW-BASIC to structured QBasic
' ============================================================================'

Imports QB.CodeAnalysis.Syntax

Imports Xunit

Namespace QB.CodeAnalysis.Syntax.Tests

  Public Class GwBasicTransformerTests

    <Fact>
    Public Sub DetectsGotoStatements()
      Dim input = "100 GOTO 150" & vbCrLf & "150 PRINT ""done"""
      Dim syntaxTree1 = SyntaxTree.Parse(input)
      Dim transformer = New GwBasicToQBasicRewriter()

      transformer.CollectTargetsOnce(syntaxTree1.Root)

      Assert.Equal(1, transformer.Analysis.GotoStatementsFound)
    End Sub

    <Fact>
    Public Sub DetectsGosubStatements()
      Dim input = "100 GOSUB 200" & vbCrLf & "200 RETURN"
      Dim syntaxTree1 = SyntaxTree.Parse(input)
      Dim transformer = New GwBasicToQBasicRewriter()

      transformer.CollectTargetsOnce(syntaxTree1.Root)

      Assert.Equal(1, transformer.Analysis.GosubStatementsFound)
    End Sub

    <Fact>
    Public Sub IdentifiesTargetLineNumbers()
      Dim input = "100 GOTO 200" & vbCrLf & "200 PRINT ""target"""
      Dim syntaxTree1 = SyntaxTree.Parse(input)
      Dim transformer = New GwBasicToQBasicRewriter()

      transformer.CollectTargetsOnce(syntaxTree1.Root)

      Assert.True(transformer.IsTargetLine("200"))
      Assert.False(transformer.IsTargetLine("150"))
    End Sub

  End Class

End Namespace
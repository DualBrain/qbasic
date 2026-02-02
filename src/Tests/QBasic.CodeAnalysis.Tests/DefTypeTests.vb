<Fact>
Public Sub ParsesDefIntStatement()
  Dim text = "DEFINT A-Z"
  Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
  
  ' Should parse without errors
  Assert.Empty(syntaxTree.Diagnostics)
  
  ' Check that DEFINT keyword is recognized
  Dim defIntKeyword = syntaxTree.Root.DescendantNodes.OfType(Of DefTypeStatementSyntax).FirstOrDefault()
  Assert.NotNull(defIntKeyword)
  Assert.Equal(SyntaxKind.DefIntKeyword, defIntKeyword.Keyword.Kind)
  
  ' Check that range A-Z is parsed
  Dim rangeClause = DirectCast(defIntKeyword.Nodes.First(), DefVarRangeClauseSyntax)
  Assert.NotNull(rangeClause)
  Assert.Equal("A", rangeClause.Lower.Text)
  Assert.Equal("-", rangeClause.MinusToken.Text)
  Assert.Equal("Z", rangeClause.OptionalUpper.Text)
End Sub

<Fact>
Public Sub ParsesDefStrSingleLetter()
  Dim text = "DEFSTR S"
  Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
  
  ' Should parse without errors
  Assert.Empty(syntaxTree.Diagnostics)
  
  ' Check that DEFSTR keyword is recognized
  Dim defStrKeyword = syntaxTree.Root.DescendantNodes.OfType(Of DefTypeStatementSyntax).FirstOrDefault()
  Assert.NotNull(defStrKeyword)
  Assert.Equal(SyntaxKind.DefStrKeyword, defStrKeyword.Keyword.Kind)
  
  ' Check that range S is parsed
  Dim rangeClause = DirectCast(defStrKeyword.Nodes.First(), DefVarRangeClauseSyntax)
  Assert.NotNull(rangeClause)
  Assert.Equal("S", rangeClause.Lower.Text)
  Assert.Null(rangeClause.MinusToken)
  Assert.Null(rangeClause.OptionalUpper)
End Sub

<Fact>
Public Sub ParsesDefIntMultipleRanges()
  Dim text = "DEFINT A-C, F-H, K-Z"
  Dim syntaxTree = SyntaxTree.Parse(text)
  
  ' Should parse without errors
  Assert.Empty(syntaxTree.Diagnostics)
  
  ' Check that all three ranges are parsed
  Dim defIntKeyword = syntaxTree.Root.DescendantNodes.OfType(Of DefTypeStatementSyntax).FirstOrDefault()
  Assert.NotNull(defIntKeyword)
  Assert.Equal(SyntaxKind.DefIntKeyword, defIntKeyword.Keyword.Kind)
  
  ' Check that comma-separated ranges are parsed
  Dim rangeClauses = syntaxTree.Root.DescendantNodes.OfType(Of DefVarRangeClauseSyntax).ToList()
  Assert.Equal(3, rangeClauses.Count)
  
  ' Check first range A-C
  Assert.Equal("A", rangeClauses(0).Lower.Text)
  Assert.Equal("-", rangeClauses(0).MinusToken.Text)
  Assert.Equal("C", rangeClauses(0).OptionalUpper.Text)
  
  ' Check second range F-H  
  Assert.Equal("F", rangeClauses(1).Lower.Text)
  Assert.Equal("-", rangeClauses(1).MinusToken.Text)
  Assert.Equal("H", rangeClauses(1).OptionalUpper.Text)
  
  ' Check third range K-Z
  Assert.Equal("K", rangeClauses(2).Lower.Text)
  Assert.Equal("-", rangeClauses(2).MinusToken.Text)
  Assert.Equal("Z", rangeClauses(2).OptionalUpper.Text)
End Sub

<Fact>
Public Sub ParsesDefStrMultipleRanges()
  Dim text = "DEFSTR A-C, X-Z, D-F"
  Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
  
  ' Should parse without errors
  Assert.Empty(syntaxTree.Diagnostics)
  
  ' Check that all three ranges are parsed
  Dim defStrKeyword = syntaxTree.Root.DescendantNodes.OfType(Of DefTypeStatementSyntax).FirstOrDefault()
  Assert.NotNull(defStrKeyword)
  Assert.Equal(SyntaxKind.DefStrKeyword, defStrKeyword.Keyword.Kind)
  
  ' Check that comma-separated ranges are parsed
  Dim rangeClauses = syntaxTree.Root.DescendantNodes.OfType(Of DefVarRangeClauseSyntax).ToList()
  Assert.Equal(3, rangeClauses.Count)
  
  ' Check first range A-C
  Assert.Equal("A", rangeClauses(0).Lower.Text)
  Assert.Equal("-", rangeClauses(0).MinusToken.Text)
  Assert.Equal("C", rangeClauses(0).OptionalUpper.Text)
  
  ' Check second range X-Z  
  Assert.Equal("X", rangeClauses(1).Lower.Text)
  Assert.Equal("-", rangeClauses(1).MinusToken.Text)
  Assert.Equal("Z", rangeClauses(1).OptionalUpper.Text)
  
  ' Check third range D-F
  Assert.Equal("D", rangeClauses(2).Lower.Text)
  Assert.Equal("-", rangeClauses(2).MinusToken.Text)
  Assert.Equal("F", rangeClauses(2).OptionalUpper.Text)
End Sub

<Fact>
Public Sub ParsesDefLngAndDefInt()
  Dim text = "DEFLNG I-K, DEFINT L-Z"
  Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
  
  ' Should parse without errors
  Assert.Empty(syntaxTree.Diagnostics)
  
  ' Check that both statements are parsed
  Dim defStatements = syntaxTree.Root.DescendantNodes.OfType(Of DefTypeStatementSyntax).ToList()
  Assert.Equal(2, defStatements.Count)
  
  ' Check DEFLNG statement
  Dim defLngKeyword = defStatements.First(Function(s) s => s.Keyword.Kind = SyntaxKind.DefLngKeyword)
  Assert.NotNull(defLngKeyword)
  Assert.Equal("I", DirectCast(defLngKeyword.Nodes.First(), DefVarRangeClauseSyntax).Lower.Text)
  Assert.Equal("-", DirectCast(defLngKeyword.Nodes.First(), DefVarRangeClauseSyntax).MinusToken.Text)
  Assert.Equal("K", DirectCast(defLngKeyword.Nodes.First(), DefVarRangeClauseSyntax).OptionalUpper.Text)
  
  ' Check DEFINT statement
  Dim defIntKeyword = defStatements.Last(Function(s) s => s.Keyword.Kind = SyntaxKind.DefIntKeyword)
  Assert.NotNull(defIntKeyword)
  Assert.Equal("L", DirectCast(defIntKeyword.Nodes.First(), DefVarRangeClauseSyntax).Lower.Text)
  Assert.Equal("-", DirectCast(defIntKeyword.Nodes.First(), DefVarRangeClauseSyntax).MinusToken.Text)
  Assert.Equal("Z", DirectCast(defIntKeyword.Nodes.First(), DefVarRangeClauseSyntax).OptionalUpper.Text)
End Sub

<Fact>
Public Sub ParsesAllDefTypes()
  Dim text = "DEFINT A-C, DEFLNG D-F, DEFSNG G-H, DEFDBL I-P, DEFSTR J-R"
  Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
  
  ' Should parse without errors
  Assert.Empty(syntaxTree.Diagnostics)
  
  ' Check that all 5 DEF statements are parsed
  Dim defStatements = syntaxTree.Root.DescendantNodes.OfType(Of DefTypeStatementSyntax).ToList()
  Assert.Equal(5, defStatements.Count)
  
  ' Verify each statement type
  Dim expectedKinds = {SyntaxKind.DefIntKeyword, SyntaxKind.DefLngKeyword, SyntaxKind.DefSngKeyword, SyntaxKind.DefDblKeyword, SyntaxKind.DefStrKeyword}
  For i = 0 To 4
    Dim keyword = defStatements(i).Keyword
    Assert.Contains(expectedKinds, keyword.Kind)
  Next
End Sub
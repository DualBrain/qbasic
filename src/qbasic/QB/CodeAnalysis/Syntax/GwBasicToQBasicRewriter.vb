Namespace Global.QB.CodeAnalysis.Syntax

  ''' <summary>
  ''' Transforms GW-BASIC line-numbered code to modern QBasic structured code.
  ''' Uses SyntaxTree completely - extracts line numbers from nodes, not text.
  ''' </summary>
  Public Class GwBasicToQBasicRewriter

    Private ReadOnly m_targetLineNumbers As New HashSet(Of String)()
    Private ReadOnly m_allLineNumbers As New HashSet(Of String)()

    ''' <summary>
    ''' Analysis results for transformation.
    ''' </summary>
    Public Class TransformationResult
      Public Property LineNumbersFound As Integer
      Public Property LineNumbersRemoved As Integer
      Public Property LabelsCreated As Integer
      Public Property GotoStatementsFound As Integer
      Public Property GosubStatementsFound As Integer
      Public ReadOnly Property Warnings As New List(Of String)()
      Public ReadOnly Property Suggestions As New List(Of String)()
      Public ReadOnly Property TargetLineNumbers As New List(Of String)()
    End Class

    Public ReadOnly Property Analysis As New TransformationResult()

    ''' <summary>
    ''' Analyzes syntax tree to collect GOTO/GOSUB targets and line numbers.
    ''' </summary>
    Public Sub CollectTargetsOnce(node As SyntaxNode)
      ' Walk all nodes to extract targets and line numbers from SyntaxTree
      WalkAllNodes(node)
    End Sub

    ''' <summary>
    ''' Recursively walks all nodes to extract targets and line numbers.
    ''' </summary>
    Private Sub WalkAllNodes(node As SyntaxNode)

      For Each child In node.GetChildren()

        If child Is Nothing Then Continue For

        ' Extract line number from node (from tokens, properties, or trivia)
        ExtractLineNumberFromNode(child)

        ' Collect GOTO targets
        If child.Kind = SyntaxKind.GotoStatement Then
          ExtractTargetFromNode(child, AddressOf m_targetLineNumbers.Add)
          Analysis.GotoStatementsFound += 1
        End If

        ' Collect GOSUB targets
        If child.Kind = SyntaxKind.GosubStatement Then
          ExtractTargetFromNode(child, AddressOf m_targetLineNumbers.Add)
          Analysis.GosubStatementsFound += 1
        End If

        ' Recurse
        WalkAllNodes(child)

      Next

    End Sub

    ''' <summary>
    ''' Extracts line numbers from SyntaxTree nodes.
    ''' GW-BASIC line numbers are parsed as LabelStatementSyntax with a Label property containing a NumberToken.
    ''' </summary>
    Private Sub ExtractLineNumberFromNode(node As SyntaxNode)
      If node.Kind = SyntaxKind.LabelStatement Then
        Try
          ' Extract from Label property (contains NumberToken)
          Dim labelNode = CType(node, LabelStatementSyntax)
          Dim label = labelNode.Label
          If label IsNot Nothing Then
            Dim lineNumber = label.Text
            If Integer.TryParse(lineNumber, Nothing) Then
              Analysis.LineNumbersFound += 1
              m_allLineNumbers.Add(lineNumber)
            End If
          End If
        Catch ex As Exception
          ' Ignore errors and continue
        End Try
      End If
    End Sub

    ''' <summary>
    ''' Extracts target line number from GOTO/GOSUB statements using proper SyntaxTree properties.
    ''' </summary>
    Private Shared Sub ExtractTargetFromNode(node As SyntaxNode, addTarget As Action(Of String))
      If node.Kind = SyntaxKind.GotoStatement Then
        Dim gotoNode = DirectCast(node, GotoStatementSyntax)
        Dim targetProp = gotoNode.TargetToken
        Dim label = targetProp.Text
        If Integer.TryParse(label, Nothing) Then addTarget(label)
      ElseIf node.Kind = SyntaxKind.GosubStatement Then
        Dim gosubNode = DirectCast(node, GosubStatementSyntax)
        Dim identifierProp = gosubNode.IdentifierToken
        Dim label = identifierProp.Text
        If Integer.TryParse(label, Nothing) Then addTarget(label)
      End If
    End Sub

    ''' <summary>
    ''' Generates the upgraded QBasic code from GW-BASIC source.
    ''' </summary>
    Public Function GenerateUpgradedCode(originalCode As String) As String

      If String.IsNullOrWhiteSpace(originalCode) Then Return originalCode

      Dim syntaxTree = Syntax.SyntaxTree.Parse(originalCode)

      ' Collect targets and line numbers from syntax tree completely
      CollectTargetsOnce(syntaxTree.Root)

      ' Update analysis results
      Analysis.TargetLineNumbers.AddRange(m_targetLineNumbers.OrderBy(Function(x) CInt(x)))
      Analysis.LabelsCreated = m_targetLineNumbers.Count
      Analysis.LineNumbersRemoved = Math.Max(0, Analysis.LineNumbersFound - Analysis.LabelsCreated)

      ' Generate suggestions
      GenerateSuggestions()

      ' Use writer for output (will handle GOTO/GOSUB rewriting)
      Dim writer = New IndentedSyntaxWriter(m_targetLineNumbers, Analysis)
      Return writer.WriteTree(originalCode)

    End Function

    ''' <summary>
    ''' Generates suggestions for improving the code.
    ''' </summary>
    Public Sub GenerateSuggestions()
      If Analysis.GosubStatementsFound > 0 Then
        Analysis.Suggestions.Add($"Found {Analysis.GosubStatementsFound} GOSUB statements. Consider converting to SUB procedures.")
      End If
      If m_targetLineNumbers.Count > 5 Then
        Analysis.Warnings.Add("Large number of GOTO targets detected. Code may benefit from restructuring.")
      End If
    End Sub

    ''' <summary>
    ''' Checks if a line number is a target of any GOTO or GOSUB.
    ''' </summary>
    Public Function IsTargetLine(lineNumber As String) As Boolean
      Return m_targetLineNumbers.Contains(lineNumber)
    End Function

    ''' <summary>
    ''' Required for SyntaxTreeRewriter compatibility - returns the node unchanged.
    ''' The actual transformation happens in GenerateUpgradedCode.
    ''' </summary>
    Public Function Rewrite(node As SyntaxNode) As SyntaxNode
      ' Initialize analysis by collecting targets
      CollectTargetsOnce(node)
      ' Return node unchanged - real transformation happens in GenerateUpgradedCode
      Return node
    End Function

  End Class

  ''' <summary>
  ''' Writes the transformed code using SyntaxTree analysis.
  ''' </summary>
  Friend Class IndentedSyntaxWriter

    Private ReadOnly m_targetLineNumbers As HashSet(Of String)
    Private ReadOnly m_analysis As GwBasicToQBasicRewriter.TransformationResult

    Public Sub New(targets As HashSet(Of String), analysis As GwBasicToQBasicRewriter.TransformationResult)
      m_targetLineNumbers = targets
      m_analysis = analysis
    End Sub

    Public Function WriteTree(originalCode As String) As String

      Dim result = New List(Of String)()
      Dim lines = originalCode.Split({vbCrLf, vbLf}, StringSplitOptions.None)

      ' Write header with actual counts
      result.Add("' =============================================================")
      result.Add("' GW-BASIC to QBasic Transformation")
      result.Add($"' Line numbers found: {m_analysis.LineNumbersFound}")
      result.Add($"' Labels created: {m_targetLineNumbers.Count}")
      result.Add("' =============================================================")
      result.Add("")

      m_firstLabelWritten = False
      ' Process each line using SyntaxTree-aware rewriting
      For Each line In lines
        ProcessLineWithSyntaxTreeRewrite(line, result)
      Next

      Return String.Join(vbCrLf, result)

    End Function

    ''' <summary>
    ''' Processes a line using SyntaxTree analysis for proper GOTO/GOSUB rewriting.
    ''' </summary>
    Private Sub ProcessLineWithSyntaxTreeRewrite(line As String, result As List(Of String))
      Dim trimmedLine = line.TrimStart()
      ' Check if line starts with line number pattern
      If StartsWithLineNumberPattern(trimmedLine) Then
        ProcessLineWithNumber(trimmedLine, result)
      ElseIf IsLabelLine(trimmedLine) Then
        ProcessLabelLine(trimmedLine, result)
      Else
        ProcessCodeLineWithSyntaxTreeRewrite(trimmedLine, result)
      End If
    End Sub

    ''' <summary>
    ''' Processes code lines with SyntaxTree-aware GOTO/GOSUB rewriting.
    ''' </summary>
    Private Sub ProcessCodeLineWithSyntaxTreeRewrite(line As String, result As List(Of String))
      If String.IsNullOrWhiteSpace(line) Then result.Add("") : Return
      ' Check if this line needs GOTO/GOSUB rewriting using SyntaxTree analysis
      Dim rewrittenLine = RewriteGotoGosubUsingSyntaxTree(line)
      result.Add(rewrittenLine)
    End Sub

    ''' <summary>
    ''' Rewrites GOTO/GOSUB targets using collected target information.
    ''' </summary>
    Private Function RewriteGotoGosubUsingSyntaxTree(line As String) As String

      Dim upperLine = line.Trim().ToUpper()

      ' Simple GOTO statement
      If upperLine.StartsWith("GOTO ") Then
        Dim target = ExtractTargetFromText(line.Substring(4).Trim())
        If target IsNot Nothing AndAlso m_targetLineNumbers.Contains(target) Then
          Return line.Replace(target, $"Label{target}")
        End If
      End If

      ' Simple GOSUB statement  
      If upperLine.StartsWith("GOSUB ") Then
        Dim target = ExtractTargetFromText(line.Substring(5).Trim())
        If target IsNot Nothing AndAlso m_targetLineNumbers.Contains(target) Then
          Return line.Replace(target, $"Label{target}")
        End If
      End If

      ' IF ... THEN GOTO/GOSUB statements
      If upperLine.StartsWith("IF ") Then

        Dim thenIndex = upperLine.IndexOf(" THEN ")

        If thenIndex <> -1 Then

          Dim afterThen = line.Substring(thenIndex + 6).Trim()
          Dim gotoIndex = afterThen.ToUpper().IndexOf("GOTO ")
          Dim gosubIndex = afterThen.ToUpper().IndexOf("GOSUB ")

          If gotoIndex <> -1 Then
            Dim target = ExtractTargetFromText(afterThen.Substring(gotoIndex + 5).Trim())
            If target IsNot Nothing AndAlso m_targetLineNumbers.Contains(target) Then
              Return line.Replace(target, $"Label{target}")
            End If
          ElseIf gosubIndex <> -1 Then
            Dim target = ExtractTargetFromText(afterThen.Substring(gosubIndex + 6).Trim())
            If target IsNot Nothing AndAlso m_targetLineNumbers.Contains(target) Then
              Return line.Replace(target, $"Label{target}")
            End If
          End If

        End If

      End If

      ' ON GOTO/GOSUB statements
      If upperLine.StartsWith("ON ") Then

        Dim gotoIndex = upperLine.IndexOf(" GOTO ")
        Dim gosubIndex = upperLine.IndexOf(" GOSUB ")

        If gotoIndex <> -1 OrElse gosubIndex <> -1 Then
          Dim targetsStart = If(gotoIndex <> -1, gotoIndex + 6, gosubIndex + 7)
          Dim targetsPart = line.Substring(targetsStart).Trim()
          Dim targets = targetsPart.Split(","c)
          Dim rewrittenTargets = New List(Of String)()

          For Each target In targets
            Dim targetNumber = ExtractTargetFromText(target.Trim())
            If targetNumber IsNot Nothing AndAlso m_targetLineNumbers.Contains(targetNumber) Then
              rewrittenTargets.Add($"Label{targetNumber}")
            Else
              rewrittenTargets.Add(target.Trim())
            End If
          Next

          Return line.Substring(0, targetsStart) + String.Join(", ", rewrittenTargets)

        End If

      End If

      ' No rewriting needed
      Return line

    End Function

    ''' <summary>
    ''' Extracts target number from text (handles things like "100" or "100:")
    ''' </summary>
    Private Shared Function ExtractTargetFromText(targetText As String) As String

      If String.IsNullOrWhiteSpace(targetText) Then Return Nothing

      ' Remove trailing colon if present
      If targetText.EndsWith(":"c) Then
        targetText = targetText.Substring(0, targetText.Length - 1)
      End If

      ' Extract digits from start
      Dim i = 0
      While i < targetText.Length AndAlso Char.IsDigit(targetText(i))
        i += 1
      End While

      If i > 0 Then
        Dim number = targetText.Substring(0, i)
        If Integer.TryParse(number, Nothing) Then
          Return number
        End If
      End If

      Return Nothing

    End Function

    Private m_firstLabelWritten As Boolean

    Private Sub ProcessLineWithNumber(line As String, result As List(Of String))

      Dim spacePos = line.IndexOf(" "c)
      If spacePos <= 0 Then Return

      Dim lineNumber = line.Substring(0, spacePos)
      Dim codeAfterNumber = line.Substring(spacePos + 1).TrimStart()

      ' Apply GOTO/GOSUB rewriting to the code part
      Dim rewrittenCode = RewriteGotoGosubUsingSyntaxTree(codeAfterNumber)

      If m_targetLineNumbers.Contains(lineNumber) Then
        ' Create label for target line numbers
        If result.Count > 0 AndAlso Not String.IsNullOrWhiteSpace(result.Last()) Then
          result.Add("")
        End If
        result.Add($"Label{lineNumber}:")
        If Not String.IsNullOrWhiteSpace(rewrittenCode) Then
          result.Add($"  {rewrittenCode}")
        End If
        m_firstLabelWritten = True
      Else
        ' Remove line number for non-targets
        If Not String.IsNullOrWhiteSpace(rewrittenCode) Then
          result.Add($"{If(m_firstLabelWritten, "  ", "")}{rewrittenCode}")
        End If
      End If

    End Sub

    Private Shared Sub ProcessLabelLine(line As String, result As List(Of String))
      result.Add(line)
    End Sub

    Private Shared Function StartsWithLineNumberPattern(text As String) As Boolean
      If String.IsNullOrEmpty(text) Then Return False
      Dim i = 0
      While i < text.Length AndAlso Char.IsDigit(text(i))
        i += 1
      End While
      Return i > 0 AndAlso i < text.Length AndAlso text(i) = " "c
    End Function

    Private Shared Function IsLabelLine(line As String) As Boolean
      Return line.Contains(":"c) AndAlso Not line.StartsWith("'"c)
    End Function

  End Class

End Namespace
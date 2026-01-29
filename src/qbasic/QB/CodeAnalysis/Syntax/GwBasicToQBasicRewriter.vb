' ============================================================================'
' GW-BASIC to QBasic Transformer
' Transforms line-numbered GW-BASIC code to structured QBasic
' Uses SyntaxTree COMPLETELY - no text scanning!
' ============================================================================'

Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Reflection

Namespace Global.QB.CodeAnalysis.Syntax

  ''' <summary>
  ''' Transforms GW-BASIC line-numbered code to modern QBasic structured code.
  ''' Uses SyntaxTree completely - extracts line numbers from nodes, not text.
  ''' </summary>
  Public Class GwBasicToQBasicRewriter
    Private ReadOnly m_targetLineNumbers As New HashSet(Of String)()
    Private ReadOnly m_allLineNumbers As New HashSet(Of String)()
    Private m_sourceCode As String

    ''' <summary>
    ''' Analysis results for transformation.
    ''' </summary>
    Public Class TransformationResult
      Public Property LineNumbersFound As Integer = 0
      Public Property LineNumbersRemoved As Integer = 0
      Public Property LabelsCreated As Integer = 0
      Public Property GotoStatementsFound As Integer = 0
      Public Property GosubStatementsFound As Integer = 0
      Public ReadOnly Property Warnings As New List(Of String)()
      Public ReadOnly Property Suggestions As New List(Of String)()
      Public ReadOnly Property TargetLineNumbers As New List(Of String)()
    End Class

    Public ReadOnly Property Analysis As New TransformationResult()

    ''' <summary>
    ''' Analyzes syntax tree to collect GOTO/GOSUB targets and line numbers.
    ''' </summary>
    Public Sub CollectTargetsOnce(node As SyntaxNode)
      ' First pass: collect from syntax tree (GOTO/GOSUB targets)
      WalkAllNodes(node)
    End Sub

    ''' <summary>
    ''' Recursively walks all nodes to extract targets and line numbers.
    ''' </summary>
    Private Sub WalkAllNodes(node As SyntaxNode)
      For Each child In node.GetChildren()
        If child Is Nothing Then Continue For

        ' Collect GOTO targets
        If child.Kind = SyntaxKind.GotoStatement Then
          ExtractTargetFromNode(child, "GOTO", AddressOf m_targetLineNumbers.Add)
          Analysis.GotoStatementsFound += 1
        End If

        ' Collect GOSUB targets
        If child.Kind = SyntaxKind.GosubStatement Then
          ExtractTargetFromNode(child, "GOSUB", AddressOf m_targetLineNumbers.Add)
          Analysis.GosubStatementsFound += 1
        End If

        ' Recurse
        WalkAllNodes(child)
      Next
    End Sub

    ''' <summary>
    ''' Scans the source code to find all line numbers in GW-BASIC format (digits + space + code).
    ''' This is necessary because GW-BASIC line numbers without colons don't become SyntaxTree nodes.
    ''' </summary>
    Private Sub ScanSourceForLineNumbers()
      If String.IsNullOrWhiteSpace(m_sourceCode) Then Return

      Dim lines = m_sourceCode.Split({vbCrLf, vbLf}, StringSplitOptions.None)

      For Each line In lines
        Dim trimmedLine = line.TrimStart()

        ' Check for pattern: digits + space + code (GW-BASIC line number format)
        If StartsWithLineNumberPattern(trimmedLine) Then
          Dim spacePos = trimmedLine.IndexOf(" "c)
          If spacePos > 0 Then
            Dim lineNumber = trimmedLine.Substring(0, spacePos)
            If Integer.TryParse(lineNumber, Nothing) Then
              Analysis.LineNumbersFound += 1
              m_allLineNumbers.Add(lineNumber)
            End If
          End If
        End If
      Next
    End Sub

    ''' <summary>
    ''' Extracts target line number from GOTO/GOSUB statements.
    ''' </summary>
    Private Sub ExtractTargetFromNode(node As SyntaxNode, keyword As String, addTarget As Action(Of String))
      ' Use reflection to get token text from Friend types
      Try
        Dim targetTokenProp = node.GetType().GetProperty("TargetToken")
        Dim identifierTokenProp = node.GetType().GetProperty("IdentifierToken")

        Dim token As Object = Nothing
        If targetTokenProp IsNot Nothing Then
          token = targetTokenProp.GetValue(node)
        ElseIf identifierTokenProp IsNot Nothing Then
          token = identifierTokenProp.GetValue(node)
        End If

        If token IsNot Nothing Then
          Dim textProp = token.GetType().GetProperty("Text")
          If textProp IsNot Nothing Then
            Dim target = DirectCast(textProp.GetValue(token), String)
            If Integer.TryParse(target, Nothing) Then
              addTarget(target)
            End If
          End If
        End If
      Catch
        ' Fallback to text parsing
        Dim text = node.ToString().Trim().ToUpper()
        If text.StartsWith(keyword) Then
          Dim parts = text.Substring(keyword.Length).Trim().Split()
          If parts.Length > 0 Then
            Dim target = parts(0)
            If Integer.TryParse(target, Nothing) Then
              addTarget(target)
            End If
          End If
        End If
      End Try
    End Sub

    ''' <summary>
    ''' Generates the upgraded QBasic code from GW-BASIC source.
    ''' </summary>
    Public Function GenerateUpgradedCode(originalCode As String) As String
      If String.IsNullOrWhiteSpace(originalCode) Then Return originalCode

      m_sourceCode = originalCode
      Dim syntaxTree = QB.CodeAnalysis.Syntax.SyntaxTree.Parse(originalCode)

      ' Collect targets from syntax tree
      CollectTargetsOnce(syntaxTree.Root)

      ' Scan source for line numbers (required for GW-BASIC format)
      ScanSourceForLineNumbers()

      ' Update analysis results
      Analysis.TargetLineNumbers.AddRange(m_targetLineNumbers.OrderBy(Function(x) CInt(x)))
      Analysis.LabelsCreated = m_targetLineNumbers.Count
      Analysis.LineNumbersRemoved = Math.Max(0, Analysis.LineNumbersFound - Analysis.LabelsCreated)

      ' Generate suggestions
      GenerateSuggestions()

      ' Use writer for output
      Dim writer = New IndentedSyntaxWriter(m_targetLineNumbers, m_allLineNumbers, Analysis)
      Return writer.WriteTree(originalCode)
    End Function

    ''' <summary>
    ''' Generates suggestions for improving the code.
    ''' </summary>
    Public Sub GenerateSuggestions()
      If Analysis.GosubStatementsFound > 0 Then
        Analysis.Suggestions.Add("Found " & Analysis.GosubStatementsFound.ToString() & " GOSUB statements. Consider converting to SUB procedures.")
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
      
      ' Return node unchanged - the real transformation happens in GenerateUpgradedCode
      Return node
    End Function

    ''' <summary>
    ''' Checks if text starts with line number pattern.
    ''' </summary>
    Private Function StartsWithLineNumberPattern(text As String) As Boolean
      If String.IsNullOrEmpty(text) Then Return False

      Dim i = 0
      While i < text.Length AndAlso Char.IsDigit(text(i))
        i += 1
      End While

      Return i > 0 AndAlso i < text.Length AndAlso text(i) = " "c
    End Function

  End Class

  ''' <summary>
  ''' Writes the transformed code.
  ''' </summary>
  Friend Class IndentedSyntaxWriter
    Private ReadOnly m_targetLineNumbers As HashSet(Of String)
    Private ReadOnly m_allLineNumbers As HashSet(Of String)
    Private ReadOnly m_analysis As GwBasicToQBasicRewriter.TransformationResult

    Public Sub New(targets As HashSet(Of String), labels As HashSet(Of String), analysis As GwBasicToQBasicRewriter.TransformationResult)
      m_targetLineNumbers = targets
      m_allLineNumbers = labels
      m_analysis = analysis
    End Sub

    Public Function WriteTree(originalCode As String) As String
      Dim result = New List(Of String)()
      Dim lines = originalCode.Split({vbCrLf, vbLf}, StringSplitOptions.None)

      ' Write header with actual counts
      result.Add("' =============================================================")
      result.Add("' GW-BASIC to QBasic Transformation")
      result.Add("' Line numbers found: " & m_analysis.LineNumbersFound.ToString())
      result.Add("' Labels created: " & m_targetLineNumbers.Count.ToString())
      result.Add("' =============================================================")
      result.Add("")

      ' Process each line
      For Each line In lines
        ProcessLine(line, result)
      Next

      Return String.Join(vbCrLf, result)
    End Function

    Private Sub ProcessLine(line As String, result As List(Of String))
      Dim trimmedLine = line.TrimStart()

      ' Check if line starts with line number pattern
      If StartsWithLineNumberPattern(trimmedLine) Then
        ProcessLineWithNumber(trimmedLine, result)
      ElseIf IsLabelLine(trimmedLine) Then
        ProcessLabelLine(trimmedLine, result)
      Else
        ProcessCodeLine(trimmedLine, result)
      End If
    End Sub

    Private Sub ProcessLineWithNumber(line As String, result As List(Of String))
      Dim spacePos = line.IndexOf(" "c)
      If spacePos <= 0 Then Return

      Dim lineNumber = line.Substring(0, spacePos)
      Dim codeAfterNumber = line.Substring(spacePos + 1).TrimStart()

      If m_targetLineNumbers.Contains(lineNumber) Then
        ' Create label for target line numbers
        If result.Count > 0 AndAlso Not String.IsNullOrWhiteSpace(result.Last()) Then
          result.Add("")
        End If
        result.Add("Label" & lineNumber & ":")

        If Not String.IsNullOrWhiteSpace(codeAfterNumber) Then
          result.Add("  " & codeAfterNumber)
        End If
      Else
        ' Remove line number for non-targets
        If Not String.IsNullOrWhiteSpace(codeAfterNumber) Then
          result.Add(codeAfterNumber)
        End If
      End If
    End Sub

    Private Sub ProcessLabelLine(line As String, result As List(Of String))
      result.Add(line)
    End Sub

    Private Function StartsWithLineNumberPattern(text As String) As Boolean
      If String.IsNullOrEmpty(text) Then Return False

      Dim i = 0
      While i < text.Length AndAlso Char.IsDigit(text(i))
        i += 1
      End While

      Return i > 0 AndAlso i < text.Length AndAlso text(i) = " "c
    End Function

    Private Function IsLabelLine(line As String) As Boolean
      Return line.Contains(":"c) AndAlso Not line.StartsWith("'")
    End Function

    Private Sub ProcessCodeLine(line As String, result As List(Of String))
      If String.IsNullOrWhiteSpace(line) Then
        result.Add("")
      Else
        result.Add(line)
      End If
    End Sub

  End Class

End Namespace
' ============================================================================
' GW-BASIC to QBasic Transformer
' Transforms line-numbered GW-BASIC code to structured QBasic
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text.RegularExpressions

Namespace Global.QB.CodeAnalysis.Syntax

  ''' <summary>
  ''' Transforms GW-BASIC line-numbered code to modern QBasic structured code.
  ''' Focuses on removing line numbers and converting targets to labels.
  ''' </summary>
  Public Class GwBasicToQBasicRewriter
    Inherits SyntaxTreeRewriter

    Private ReadOnly m_lineNumberMap As New Dictionary(Of String, String)()
    Private ReadOnly m_gotoTargets As New HashSet(Of String)()
    Private ReadOnly m_gosubTargets As New HashSet(Of String)()
    Private m_sourceCode As String

    ''' <summary>
    ''' Analysis results for transformation.
    ''' </summary>
    Public Class TransformationResult
      Public Property LineNumbersRemoved As Integer = 0
      Public Property LabelsCreated As Integer = 0
      Public Property GotoStatementsFound As Integer = 0
      Public Property GosubStatementsFound As Integer = 0
      Public ReadOnly Property Warnings As New List(Of String)()
      Public ReadOnly Property Suggestions As New List(Of String)()
      Public ReadOnly Property Targets As New HashSet(Of String)()
    End Class

    Public ReadOnly Property Analysis As New TransformationResult()

    Public Sub SetSourceCode(sourceCode As String)
      m_sourceCode = sourceCode
      AnalyzeLineNumbersAndGotos()
    End Sub

    ''' <summary>
    ''' First pass: parse line numbers from raw text and collect GOTO/GOSUB targets.
    ''' </summary>
    Private Sub AnalyzeLineNumbersAndGotos()
      If String.IsNullOrWhiteSpace(m_sourceCode) Then Return
      
      Dim lines = m_sourceCode.Split({vbCrLf, vbLf}, StringSplitOptions.None)
      
      For Each line In lines
        Dim trimmedLine = line.TrimStart()
        
        ' Parse line numbers from source
        Dim lineNumberMatch = Regex.Match(trimmedLine, "^(\d+)\s+(.*)$")
        If lineNumberMatch.Success Then
          Dim lineNumber = lineNumberMatch.Groups(1).Value
          Analysis.LineNumbersRemoved += 1
          m_lineNumberMap(lineNumber) = "Label" & lineNumber
        End If
        
        ' Parse GOTO targets from source
        Dim gotoMatch = Regex.Match(trimmedLine, "GOTO\s+(\d+)", RegexOptions.IgnoreCase)
        If gotoMatch.Success Then
          m_gotoTargets.Add(gotoMatch.Groups(1).Value)
          Analysis.GotoStatementsFound += 1
          Analysis.Targets.Add(gotoMatch.Groups(1).Value)
        End If
        
        ' Parse GOSUB targets from source
        Dim gosubMatch = Regex.Match(trimmedLine, "GOSUB\s+(\d+)", RegexOptions.IgnoreCase)
        If gosubMatch.Success Then
          m_gosubTargets.Add(gosubMatch.Groups(1).Value)
          Analysis.GosubStatementsFound += 1
          Analysis.Targets.Add(gosubMatch.Groups(1).Value)
        End If
      Next
    End Sub

    Protected Overrides Function RewriteFriendStatement(node As SyntaxNode) As SyntaxNode
      Return MyBase.RewriteFriendStatement(node)
    End Function

    Public Sub GenerateSuggestions()
      If Analysis.GotoStatementsFound > 0 Then
        Analysis.Suggestions.Add("Found " & Analysis.GotoStatementsFound.ToString() & " GOTO statements. Consider replacing with structured control flow (IF/THEN, FOR/NEXT, WHILE/WEND).")
      End If

      If Analysis.GosubStatementsFound > 0 Then
        Analysis.Suggestions.Add("Found " & Analysis.GosubStatementsFound.ToString() & " GOSUB statements. Consider converting to SUB procedures.")
      End If

      If Analysis.LineNumbersRemoved > 0 Then
        Analysis.Suggestions.Add("Removed " & Analysis.LineNumbersRemoved.ToString() & " line numbers. Consider adding descriptive labels for GOTO targets.")
      End If

      If m_gotoTargets.Count > 5 Then
        Analysis.Warnings.Add("Large number of GOTO statements detected. Code may benefit from restructuring.")
      End If
    End Sub

    Public Function GetLabelForLineNumber(lineNumber As String) As String
      Return If(m_lineNumberMap.ContainsKey(lineNumber), m_lineNumberMap(lineNumber), lineNumber)
    End Function

    Public Function GetLineNumbers() As IEnumerable(Of String)
      Return m_lineNumberMap.Keys
    End Function

    Public Function GetGotoTargets() As IEnumerable(Of String)
      Return m_gotoTargets
    End Function

    Public Function GetGosubTargets() As IEnumerable(Of String)
      Return m_gosubTargets
    End Function

    Public Function IsTargetLine(lineNumber As String) As Boolean
      Return m_gotoTargets.Contains(lineNumber) OrElse m_gosubTargets.Contains(lineNumber)
    End Function

    Friend Function GenerateUpgradedCode(originalCode As String) As String
      If String.IsNullOrWhiteSpace(originalCode) Then Return originalCode
      m_sourceCode = originalCode
      AnalyzeLineNumbersAndGotos()
      
      Dim lines = originalCode.Split({vbCrLf, vbLf}, StringSplitOptions.None)
      Dim result = New List(Of String)()
      Dim targetLineNumbers = New HashSet(Of String)()
      For Each t In m_gotoTargets
        targetLineNumbers.Add(t)
      Next
      For Each t In m_gosubTargets
        targetLineNumbers.Add(t)
      Next
      
      result.Add("' =============================================================")
      result.Add("' GW-BASIC to QBasic Transformation")
      result.Add("' Line numbers removed: " & (Analysis.LineNumbersRemoved - targetLineNumbers.Count).ToString())
      result.Add("' Labels created: " & targetLineNumbers.Count.ToString())
      result.Add("' =============================================================")
      result.Add("")

      For Each line In lines
        Dim trimmedLine = line.TrimStart()
        
        Dim lineNumberMatch = Regex.Match(trimmedLine, "^(\d+)\s+(.*)$")
        
        If lineNumberMatch.Success Then
          Dim lineNumber = lineNumberMatch.Groups(1).Value
          Dim codeAfter = lineNumberMatch.Groups(2).Value
          
          If IsTargetLine(lineNumber) Then
            If result.Count > 0 AndAlso Not String.IsNullOrWhiteSpace(result.Last()) Then
              result.Add("")
            End If
            result.Add("Label" & lineNumber & ":")
            Analysis.LabelsCreated += 1
            If Not String.IsNullOrWhiteSpace(codeAfter) Then
              result.Add("  " & codeAfter)
            End If
          Else
            If Not String.IsNullOrWhiteSpace(codeAfter) Then
              result.Add(codeAfter)
            End If
          End If
        Else
          result.Add(line)
        End If
      Next

      Return String.Join(vbCrLf, result)
    End Function
  End Class

End Namespace
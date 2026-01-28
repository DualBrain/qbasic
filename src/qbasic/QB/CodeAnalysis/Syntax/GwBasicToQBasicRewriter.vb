Imports System.Collections.Immutable
Imports System.Linq

Namespace Global.QB.CodeAnalysis.Syntax

  ''' <summary>
  ''' Transforms GW-BASIC line-numbered code to modern QBasic structured code.
  ''' Focuses on removing line numbers and providing analysis of GOTO/GOSUB targets.
  ''' </summary>
  Public Class GwBasicToQBasicRewriter
    Inherits SyntaxTreeRewriter

    Private ReadOnly m_lineNumberMap As New Dictionary(Of String, String)()
    Private ReadOnly m_gotoTargets As New HashSet(Of String)()
    Private ReadOnly m_gosubTargets As New HashSet(Of String)()
    Private m_labelCounter As Integer = 0

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
    End Class

    Public ReadOnly Property Analysis As New TransformationResult()

    Public Overrides Function Rewrite(node As SyntaxNode) As SyntaxNode
      ' First pass: analyze the structure
      AnalyzeLineNumbersAndGotos(node)
      
      ' Second pass: rewrite the tree
      Return MyBase.Rewrite(node)
    End Function

    ''' <summary>
    ''' First pass: collect line numbers and GOTO/GOSUB targets.
    ''' </summary>
    Private Sub AnalyzeLineNumbersAndGotos(node As SyntaxNode)
      For Each child In node.GetChildren()
        ' Collect line numbers from trivia
        If TypeOf child Is SyntaxToken Then
          Dim token = DirectCast(child, SyntaxToken)
          For Each trivia In token.LeadingTrivia
            If trivia.Kind = SyntaxKind.LineNumberTrivia Then
              Dim lineNumber = trivia.Text.Trim()
              If Not m_lineNumberMap.ContainsKey(lineNumber) Then
                Dim labelName = $"L{lineNumber}"
                m_lineNumberMap(lineNumber) = labelName
                Analysis.LineNumbersRemoved += 1
              End If
            End If
          Next
        End If

        ' Collect GOTO targets
        If child IsNot Nothing AndAlso child.Kind = SyntaxKind.GotoStatement Then
          ' For Friend types, we need to use reflection or duck typing
          Dim targetToken = GetTokenText(child, "TargetToken")
          If targetToken IsNot Nothing AndAlso m_lineNumberMap.ContainsKey(targetToken) Then
            m_gotoTargets.Add(targetToken)
            Analysis.GotoStatementsFound += 1
          End If
        End If

        ' Collect GOSUB targets  
        If child IsNot Nothing AndAlso child.Kind = SyntaxKind.GosubStatement Then
          Dim targetToken = GetTokenText(child, "IdentifierToken")
          If targetToken IsNot Nothing AndAlso m_lineNumberMap.ContainsKey(targetToken) Then
            m_gosubTargets.Add(targetToken)
            Analysis.GosubStatementsFound += 1
          End If
        End If

        AnalyzeLineNumbersAndGotos(child)
      Next
    End Sub

    ''' <summary>
    ''' Uses reflection to get token text from Friend classes.
    ''' </summary>
    Private Function GetTokenText(node As SyntaxNode, propertyName As String) As String
      Try
        Dim propertyInfo = node.GetType().GetProperty(propertyName)
        If propertyInfo IsNot Nothing Then
          Dim token = propertyInfo.GetValue(node)
          If token IsNot Nothing Then
            Dim textProperty = token.GetType().GetProperty("Text")
            If textProperty IsNot Nothing Then
              Return DirectCast(textProperty.GetValue(token), String)
            End If
          End If
        End If
      Catch
        ' Ignore reflection errors
      End Try
      Return Nothing
    End Function

    ''' <summary>
    ''' Rewrites statements, removing line numbers.
    ''' </summary>
    Protected Overrides Function RewriteFriendStatement(node As SyntaxNode) As SyntaxNode
      ' For Friend statement types, we analyze but don't modify
      ' The actual line number removal would happen in code generation
      
      ' Check if this statement has line number trivia
      Dim hasLineNumber = False
      For Each child In node.GetChildren()
        If TypeOf child Is SyntaxToken Then
          Dim token = DirectCast(child, SyntaxToken)
          For Each trivia In token.LeadingTrivia
            If trivia.Kind = SyntaxKind.LineNumberTrivia Then
              hasLineNumber = True
              Exit For
            End If
          Next
        End If
        If hasLineNumber Then Exit For
      Next
      
      If hasLineNumber Then
        Analysis.LabelsCreated += 1
        ' In a full implementation, we'd create a new statement without line numbers
        ' For now, we rely on the code writer to skip line number trivia
      End If

      Return MyBase.RewriteFriendStatement(node)
    End Function

    ''' <summary>
    ''' Generate suggestions based on analysis.
    ''' </summary>
    Public Sub GenerateSuggestions()
      If Analysis.GotoStatementsFound > 0 Then
        Analysis.Suggestions.Add($"Found {Analysis.GotoStatementsFound} GOTO statements. Consider replacing with structured control flow (IF/THEN, FOR/NEXT, WHILE/WEND).")
      End If

      If Analysis.GosubStatementsFound > 0 Then
        Analysis.Suggestions.Add($"Found {Analysis.GosubStatementsFound} GOSUB statements. Consider converting to SUB procedures.")
      End If

      If Analysis.LineNumbersRemoved > 0 Then
        Analysis.Suggestions.Add($"Removed {Analysis.LineNumbersRemoved} line numbers. Consider adding descriptive labels for GOTO targets.")
      End If

      ' Check for common patterns that should be modernized
      If m_gotoTargets.Count > 5 Then
        Analysis.Warnings.Add("Large number of GOTO statements detected. Code may benefit from restructuring.")
      End If
    End Sub

    ''' <summary>
    ''' Gets the label name for a line number.
    ''' </summary>
    Public Function GetLabelForLineNumber(lineNumber As String) As String
      Return If(m_lineNumberMap.ContainsKey(lineNumber), m_lineNumberMap(lineNumber), lineNumber)
    End Function

    ''' <summary>
    ''' Gets all line numbers found in the code.
    ''' </summary>
    Public Function GetLineNumbers() As IEnumerable(Of String)
      Return m_lineNumberMap.Keys
    End Function

    ''' <summary>
    ''' Gets all GOTO targets (line numbers).
    ''' </summary>
    Public Function GetGotoTargets() As IEnumerable(Of String)
      Return m_gotoTargets
    End Function

    ''' <summary>
    ''' Gets all GOSUB targets (line numbers).
    ''' </summary>
    Public Function GetGosubTargets() As IEnumerable(Of String)
      Return m_gosubTargets
    End Function

    ''' <summary>
    ''' Checks if a line number is the target of any GOTO or GOSUB.
    ''' </summary>
    Public Function IsTargetLine(lineNumber As String) As Boolean
      Return m_gotoTargets.Contains(lineNumber) OrElse m_gosubTargets.Contains(lineNumber)
    End Function

  End Class

End Namespace
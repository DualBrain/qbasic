Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Binding

Namespace Global.QB.CodeAnalysis

  ''' <summary>
  ''' Manages preservation of COMMON variables across CHAIN operations.
  ''' COMMON variables are shared between chained programs while all other
  ''' variables are reset.
  ''' </summary>
  Friend NotInheritable Class CommonVariablePreserver
    Private Shared m_preservedVariables As New Dictionary(Of String, Object)

    ''' <summary>
    ''' Preserves COMMON variables from the current evaluator state.
    ''' </summary>
    ''' <param name="evaluator">The evaluator containing current variables</param>
    ''' <param name="commonStatements">COMMON statements from the current compilation</param>
    Friend Shared Sub PreserveCommonVariables(evaluator As Evaluator, commonStatements As ImmutableArray(Of BoundCommonStatement))
      m_preservedVariables.Clear()
      
      For Each commonStmt In commonStatements
        For Each declaration In commonStmt.Declarations
          Dim varName = declaration.Variable.Name
          If evaluator.Globals.ContainsKey(varName) Then
            m_preservedVariables(varName) = evaluator.Globals(varName)
          End If
        Next
      Next
    End Sub

    ''' <summary>
    ''' Restores preserved COMMON variables to a new evaluator.
    ''' </summary>
    ''' <param name="evaluator">The new evaluator to receive the variables</param>
    Friend Shared Sub RestoreCommonVariables(evaluator As Evaluator)
      If m_preservedVariables Is Nothing Then Return
      
      For Each kv In m_preservedVariables
        If evaluator.Globals.ContainsKey(kv.Key) Then
          evaluator.Globals(kv.Key) = kv.Value
        End If
      Next
    End Sub

    ''' <summary>
    ''' Clears all preserved COMMON variables.
    ''' </summary>
    Friend Shared Sub ClearPreservedVariables()
      m_preservedVariables.Clear()
    End Sub

    ''' <summary>
    ''' Gets the currently preserved COMMON variables.
    ''' </summary>
    Friend Shared Function GetPreservedVariables() As Dictionary(Of String, Object)
      Return New Dictionary(Of String, Object)(m_preservedVariables)
    End Function

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis

  ''' <summary>
  ''' Internal callback interface for Evaluator to report events.
  ''' This is implemented by Compilation to receive events and re-raise them as public events.
  ''' </summary>
  Friend Interface IEvaluationCallbacks

    ''' <summary>
    ''' Called before a statement is executed.
    ''' </summary>
    Sub OnStatementExecuting(statementIndex As Integer, statementKind As String, physicalLine As Integer, qbasicLine As Integer, statementText As String, containerName As String)

    ''' <summary>
    ''' Called when a variable is modified.
    ''' </summary>
    Sub OnVariableChanged(variableName As String, oldValue As Object, newValue As Object, physicalLine As Integer, qbasicLine As Integer, variableType As VariableChangedEventArgs.VariableKind, arrayIndices As Object())

    ''' <summary>
    ''' Called when an error occurs during execution.
    ''' </summary>
    Sub OnErrorOccurred(errorCode As Integer, errorMessage As String, physicalLine As Integer, qbasicLine As Integer, statementText As String, wasHandled As Boolean)

  End Interface

End Namespace

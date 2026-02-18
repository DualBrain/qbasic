Imports System

Namespace Global.QB.CodeAnalysis

  ''' <summary>
  ''' Event arguments for statement execution events.
  ''' </summary>
  Public Class StatementExecutingEventArgs
    Inherits EventArgs

    ''' <summary>
    ''' The index of the statement being executed.
    ''' </summary>
    Public ReadOnly Property StatementIndex As Integer

    ''' <summary>
    ''' The kind of statement being executed.
    ''' </summary>
    Public ReadOnly Property StatementKind As String

    ''' <summary>
    ''' The physical line number in the source file (0-based).
    ''' Returns -1 if line number cannot be determined.
    ''' </summary>
    Public ReadOnly Property PhysicalLineNumber As Integer

    ''' <summary>
    ''' The original QBasic line number (from line number trivia), or 0 if not available.
    ''' </summary>
    Public ReadOnly Property QBasicLineNumber As Integer

    ''' <summary>
    ''' The source text of the statement being executed.
    ''' </summary>
    Public ReadOnly Property StatementText As String

    ''' <summary>
    ''' The name of the current function/sub being executed, if any.
    ''' </summary>
    Public ReadOnly Property ContainerName As String

    Public Sub New(statementIndex As Integer, statementKind As String, physicalLine As Integer, qbasicLine As Integer, statementText As String, containerName As String)
      Me.StatementIndex = statementIndex
      Me.StatementKind = statementKind
      Me.PhysicalLineNumber = physicalLine
      Me.QBasicLineNumber = qbasicLine
      Me.StatementText = statementText
      Me.ContainerName = containerName
    End Sub

  End Class

  ''' <summary>
  ''' Event arguments for variable modification events.
  ''' </summary>
  Public Class VariableChangedEventArgs
    Inherits EventArgs

    ''' <summary>
    ''' The name of the variable being modified.
    ''' </summary>
    Public ReadOnly Property VariableName As String

    ''' <summary>
    ''' The previous value of the variable. Use IsNothing to check for uninitialized.
    ''' </summary>
    Public ReadOnly Property OldValue As Object

    ''' <summary>
    ''' The new value being assigned.
    ''' </summary>
    Public ReadOnly Property NewValue As Object

    ''' <summary>
    ''' The physical line number where the assignment occurs.
    ''' </summary>
    Public ReadOnly Property PhysicalLineNumber As Integer

    ''' <summary>
    ''' The QBasic line number where the assignment occurs.
    ''' </summary>
    Public ReadOnly Property QBasicLineNumber As Integer

    ''' <summary>
    ''' The kind of variable (Scalar, ArrayElement).
    ''' </summary>
    Public ReadOnly Property VariableType As VariableKind

    ''' <summary>
    ''' For array elements, the index expression(s); Nothing for scalar variables.
    ''' </summary>
    Public ReadOnly Property ArrayIndices As Object()

    Public Enum VariableKind
      Scalar
      ArrayElement
    End Enum

    Public Sub New(variableName As String, oldValue As Object, newValue As Object, physicalLine As Integer, qbasicLine As Integer, variableType As VariableKind, Optional arrayIndices As Object() = Nothing)
      Me.VariableName = variableName
      Me.OldValue = oldValue
      Me.NewValue = newValue
      Me.PhysicalLineNumber = physicalLine
      Me.QBasicLineNumber = qbasicLine
      Me.VariableType = variableType
      Me.ArrayIndices = arrayIndices
    End Sub

  End Class

  ''' <summary>
  ''' Event arguments for error events.
  ''' </summary>
  Public Class ErrorOccurredEventArgs
    Inherits EventArgs

    ''' <summary>
    ''' The error code.
    ''' </summary>
    Public ReadOnly Property ErrorCode As Integer

    ''' <summary>
    ''' The error message.
    ''' </summary>
    Public ReadOnly Property ErrorMessage As String

    ''' <summary>
    ''' The physical line number where the error occurred.
    ''' </summary>
    Public ReadOnly Property PhysicalLineNumber As Integer

    ''' <summary>
    ''' The QBasic line number where the error occurred.
    ''' </summary>
    Public ReadOnly Property QBasicLineNumber As Integer

    ''' <summary>
    ''' The statement text where the error occurred.
    ''' </summary>
    Public ReadOnly Property StatementText As String

    ''' <summary>
    ''' Whether this error was handled by an error handler.
    ''' </summary>
    Public ReadOnly Property WasHandled As Boolean

    Public Sub New(errorCode As Integer, errorMessage As String, physicalLine As Integer, qbasicLine As Integer, statementText As String, wasHandled As Boolean)
      Me.ErrorCode = errorCode
      Me.ErrorMessage = errorMessage
      Me.PhysicalLineNumber = physicalLine
      Me.QBasicLineNumber = qbasicLine
      Me.StatementText = statementText
      Me.WasHandled = wasHandled
    End Sub

  End Class

End Namespace
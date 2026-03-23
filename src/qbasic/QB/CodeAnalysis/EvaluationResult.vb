Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis

  Public NotInheritable Class EvaluationResult

    Sub New(diagnostics As ImmutableArray(Of Diagnostic), value As Object)
      Me.Diagnostics = diagnostics
      Me.Value = value
      ErrorDiagnostics = diagnostics.Where(Function(d) d.IsError).ToImmutableArray
      WarningDiagnostics = diagnostics.Where(Function(d) d.IsWarning).ToImmutableArray
      Me.ChainRequest = Nothing
      Me.RuntimeErrorInfo = Nothing
    End Sub

    Sub New(diagnostics As ImmutableArray(Of Diagnostic), value As Object, chainRequest As ChainRequest)
      Me.Diagnostics = diagnostics
      Me.Value = value
      ErrorDiagnostics = diagnostics.Where(Function(d) d.IsError).ToImmutableArray
      WarningDiagnostics = diagnostics.Where(Function(d) d.IsWarning).ToImmutableArray
      Me.ChainRequest = chainRequest
      Me.RuntimeErrorInfo = Nothing
    End Sub

    Sub New(diagnostics As ImmutableArray(Of Diagnostic), value As Object, runtimeErrorInfo As RuntimeErrorInfo)
      Me.Diagnostics = diagnostics
      Me.Value = value
      ErrorDiagnostics = diagnostics.Where(Function(d) d.IsError).ToImmutableArray
      WarningDiagnostics = diagnostics.Where(Function(d) d.IsWarning).ToImmutableArray
      Me.ChainRequest = Nothing
      Me.RuntimeErrorInfo = runtimeErrorInfo
    End Sub

    Public ReadOnly Property RuntimeErrorInfo As RuntimeErrorInfo

    Public ReadOnly Property HasRuntimeError As Boolean
      Get
        Return RuntimeErrorInfo IsNot Nothing
      End Get
    End Property

    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property ErrorDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property WarningDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Value As Object

    Public ReadOnly Property ChainRequest As ChainRequest

  End Class

  Public Class RuntimeErrorInfo

    Sub New(errorCode As Integer, errorMessage As String, physicalLine As Integer, qbasicLine As Integer, statementText As String)
      Me.ErrorCode = errorCode
      Me.ErrorMessage = errorMessage
      Me.PhysicalLine = physicalLine
      Me.QbasicLine = qbasicLine
      Me.StatementText = statementText
    End Sub

    Public ReadOnly Property ErrorCode As Integer
    Public ReadOnly Property ErrorMessage As String
    Public ReadOnly Property PhysicalLine As Integer
    Public ReadOnly Property QbasicLine As Integer
    Public ReadOnly Property StatementText As String

    Public Overrides Function ToString() As String
      Dim lineInfo As String
      If QbasicLine > 0 Then
        lineInfo = $" in line {QbasicLine}"
      ElseIf PhysicalLine > 0 Then
        lineInfo = $" at line {PhysicalLine}"
      Else
        lineInfo = ""
      End If
      Return $"{ErrorMessage}{lineInfo}"
    End Function

  End Class

End Namespace
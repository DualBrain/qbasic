Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis

  Public NotInheritable Class EvaluationResult

    Sub New(diagnostics As ImmutableArray(Of Diagnostic), value As Object)
      Me.Diagnostics = diagnostics
      Me.Value = value
      ErrorDiagnostics = diagnostics.Where(Function(d) d.IsError).ToImmutableArray
      WarningDiagnostics = diagnostics.Where(Function(d) d.IsWarning).ToImmutableArray
      Me.ChainRequest = Nothing
    End Sub

    Sub New(diagnostics As ImmutableArray(Of Diagnostic), value As Object, chainRequest As ChainRequestException)
      Me.Diagnostics = diagnostics
      Me.Value = value
      ErrorDiagnostics = diagnostics.Where(Function(d) d.IsError).ToImmutableArray
      WarningDiagnostics = diagnostics.Where(Function(d) d.IsWarning).ToImmutableArray
      Me.ChainRequest = chainRequest
    End Sub

    ' TODO: I think we should not have separate collections but instead
    '       have an extension method over ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property ErrorDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property WarningDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Value As Object
    
    ''' <summary>
    ''' Contains chain request information if execution requested CHAIN.
    ''' </summary>
    Public ReadOnly Property ChainRequest As ChainRequestException

  End Class

End Namespace
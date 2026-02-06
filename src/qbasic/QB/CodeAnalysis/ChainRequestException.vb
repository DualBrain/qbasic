Namespace Global.QB.CodeAnalysis

  ''' <summary>
  ''' Exception thrown when a CHAIN statement requests file chaining.
  ''' This allows the evaluation system to signal that execution should
  ''' continue with a new file while preserving COMMON variables.
  ''' </summary>
  Public NotInheritable Class ChainRequestException
    Inherits Exception

    Public Sub New(filename As String, Optional lineNumber As Integer? = Nothing)
      MyBase.New($"CHAIN request: {filename}{If(lineNumber.HasValue, $", {lineNumber.Value}", "")}")
      Me.Filename = filename
      Me.LineNumber = lineNumber
    End Sub

    ''' <summary>
    ''' The target filename to chain to.
    ''' </summary>
    Public ReadOnly Property Filename As String

    ''' <summary>
    ''' Optional line number to start execution at in the target file.
    ''' </summary>
    Public ReadOnly Property LineNumber As Integer?

  End Class

End Namespace
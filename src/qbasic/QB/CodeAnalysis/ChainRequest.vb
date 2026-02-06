Namespace Global.QB.CodeAnalysis

  ''' <summary>
  ''' Contains information about a CHAIN request from program execution.
  ''' </summary>
  Public NotInheritable Class ChainRequest
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
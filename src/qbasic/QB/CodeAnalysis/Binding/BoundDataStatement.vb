Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDataStatement
    Inherits BoundStatement

    Public Sub New(data As ImmutableArray(Of Object), lineNumber As Integer)
      Me.Data = data
      Me.LineNumber = lineNumber
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DataStatement
    Public ReadOnly Property Data As ImmutableArray(Of Object)
    Public ReadOnly Property LineNumber As Integer

  End Class

End Namespace
Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDataStatement
    Inherits BoundStatement

    Public Sub New(data As ImmutableArray(Of Object))
      Me.Data = data
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DataStatement
    Public ReadOnly Property Data As ImmutableArray(Of Object)

  End Class

End Namespace
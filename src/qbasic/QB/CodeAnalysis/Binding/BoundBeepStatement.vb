Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBeepStatement
    Inherits BoundStatement

    Public Sub New()
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BeepStatement

  End Class

End Namespace
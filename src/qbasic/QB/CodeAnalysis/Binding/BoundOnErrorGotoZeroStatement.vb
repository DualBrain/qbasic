Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnErrorGotoZeroStatement
    Inherits BoundStatement

    Public Sub New()
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnErrorGotoZeroStatement

  End Class

End Namespace

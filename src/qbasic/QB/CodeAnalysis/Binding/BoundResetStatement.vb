Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundResetStatement
    Inherits BoundStatement

    Public Sub New()
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ResetStatement

  End Class

End Namespace
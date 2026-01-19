Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundResumeNextStatement
    Inherits BoundStatement

    Public Sub New()
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ResumeNextStatement

  End Class

End Namespace
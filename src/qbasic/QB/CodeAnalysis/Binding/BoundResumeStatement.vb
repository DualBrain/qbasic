Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundResumeStatement
    Inherits BoundStatement

    Public Sub New(target As BoundExpression)
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ResumeStatement
    Public ReadOnly Property Target As BoundExpression

  End Class

End Namespace
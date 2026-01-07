Imports QB.CodeAnalysis.Binding

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundCallStatement
    Inherits BoundStatement

    Public Sub New([call] As BoundCallExpression)
      Me.Call = [call]
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CallStatement
    Public ReadOnly Property [Call] As BoundCallExpression

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOutStatement
    Inherits BoundStatement

    Public Sub New(port As BoundExpression, data As BoundExpression)
      Me.Port = port
      Me.Data = data
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OutStatement
    Public ReadOnly Property Port As BoundExpression
    Public ReadOnly Property Data As BoundExpression

  End Class

End Namespace
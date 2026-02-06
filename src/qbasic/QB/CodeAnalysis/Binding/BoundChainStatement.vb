Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundChainStatement
    Inherits BoundStatement

    Public Sub New(filename As BoundExpression, optionalLine As BoundExpression)
      Me.Filename = filename
      Me.OptionalLine = optionalLine
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ChainStatement
    Public ReadOnly Property Filename As BoundExpression
    Public ReadOnly Property OptionalLine As BoundExpression

  End Class

End Namespace
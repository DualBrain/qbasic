Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPokeStatement
    Inherits BoundStatement

    Public Sub New(offset As BoundExpression, value As BoundExpression)
      Me.Offset = offset
      Me.Value = value
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PokeStatement
    Public ReadOnly Property Offset As BoundExpression
    Public ReadOnly Property Value As BoundExpression

  End Class

End Namespace
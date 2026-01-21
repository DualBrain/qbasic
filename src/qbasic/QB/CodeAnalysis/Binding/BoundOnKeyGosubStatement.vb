Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnKeyGosubStatement
    Inherits BoundStatement

    Public Sub New(keyNumber As BoundExpression, target As BoundExpression)
      Me.KeyNumber = keyNumber
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnKeyGosubStatement
    Public ReadOnly Property KeyNumber As BoundExpression
    Public ReadOnly Property Target As BoundExpression

  End Class

End Namespace
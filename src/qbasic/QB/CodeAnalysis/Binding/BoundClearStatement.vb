Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundClearStatement
    Inherits BoundStatement

    Public Sub New(dummyExpression1 As BoundExpression, dummyExpression2 As BoundExpression, stackSpaceExpression As BoundExpression)
      Me.DummyExpression1 = dummyExpression1
      Me.DummyExpression2 = dummyExpression2
      Me.StackSpaceExpression = stackSpaceExpression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ClearStatement
    Public ReadOnly Property DummyExpression1 As BoundExpression
    Public ReadOnly Property DummyExpression2 As BoundExpression
    Public ReadOnly Property StackSpaceExpression As BoundExpression

  End Class

End Namespace
Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLineInputFileStatement
    Inherits BoundStatement

    Public Sub New(fileNumber As BoundExpression, variable As BoundExpression)
      Me.FileNumber = fileNumber
      Me.Variable = variable
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LineInputFileStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property Variable As BoundExpression

  End Class

End Namespace
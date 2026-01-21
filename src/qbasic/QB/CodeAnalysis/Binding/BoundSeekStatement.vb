Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSeekStatement
    Inherits BoundStatement

    Public Sub New(fileNumber As BoundExpression, position As BoundExpression)
      Me.FileNumber = fileNumber
      Me.Position = position
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SeekStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property Position As BoundExpression

  End Class

End Namespace
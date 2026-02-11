Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundGetFileStatement
    Inherits BoundStatement

    Public Sub New(fileNumber As BoundExpression, optionalRecord As BoundExpression, optionalVariable As String)
      Me.FileNumber = fileNumber
      Me.OptionalRecord = optionalRecord
      Me.OptionalVariable = optionalVariable
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.GetFileStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property OptionalRecord As BoundExpression
    Public ReadOnly Property OptionalVariable As String

  End Class

End Namespace

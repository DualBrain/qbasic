Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundFieldStatement
    Inherits BoundStatement

    Public Sub New(fileNumber As BoundExpression, fieldDefinitions As ImmutableArray(Of (Width As BoundExpression, VariableName As String)))
      Me.FileNumber = fileNumber
      Me.FieldDefinitions = fieldDefinitions
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.FieldStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property FieldDefinitions As ImmutableArray(Of (Width As BoundExpression, VariableName As String))

  End Class

End Namespace

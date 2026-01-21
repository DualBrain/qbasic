Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOpenStatement
    Inherits BoundStatement

    Public Sub New(file As BoundExpression, mode As BoundExpression, access As ImmutableArray(Of BoundExpression), lock As ImmutableArray(Of BoundExpression), fileNumber As BoundExpression, recLen As BoundExpression)
      Me.File = file
      Me.Mode = mode
      Me.Access = access
      Me.Lock = lock
      Me.FileNumber = fileNumber
      Me.RecLen = recLen
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OpenStatement
    Public ReadOnly Property File As BoundExpression
    Public ReadOnly Property Mode As BoundExpression
    Public ReadOnly Property Access As ImmutableArray(Of BoundExpression)
    Public ReadOnly Property Lock As ImmutableArray(Of BoundExpression)
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property RecLen As BoundExpression

  End Class

End Namespace
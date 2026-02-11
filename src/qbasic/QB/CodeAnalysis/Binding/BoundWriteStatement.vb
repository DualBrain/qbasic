Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundWriteStatement
    Inherits BoundStatement

    Public Sub New(fileNumber As BoundExpression, expressions As ImmutableArray(Of BoundExpression), suppressCr As Boolean)
      Me.FileNumber = fileNumber
      Me.Expressions = expressions
      Me.SuppressCr = suppressCr
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.WriteStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property Expressions As ImmutableArray(Of BoundExpression)
    Public ReadOnly Property SuppressCr As Boolean

  End Class

End Namespace
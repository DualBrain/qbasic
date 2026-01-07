Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundHandlePrintStatement
    Inherits BoundStatement

    Public Sub New(expression As BoundExpression, noCr As Boolean)
      Me.Expression = expression
      Me.NoCr = noCr
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.HandlePrintStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property NoCr As Boolean

  End Class

End Namespace
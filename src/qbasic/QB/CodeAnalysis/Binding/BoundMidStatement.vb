Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend Class BoundMidStatement
    Inherits BoundStatement

    Public Sub New(targetExpression As BoundExpression, positionExpression As BoundExpression, lengthExpression As BoundExpression, expression As BoundExpression)
      Me.TargetExpression = targetExpression
      Me.PositionExpression = positionExpression
      Me.LengthExpression = lengthExpression
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.MidStatement
    Public ReadOnly Property TargetExpression As BoundExpression
    Public ReadOnly Property PositionExpression As BoundExpression
    Public ReadOnly Property LengthExpression As BoundExpression
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
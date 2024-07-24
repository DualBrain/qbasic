Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundClsStatement
    Inherits BoundStatement

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ClsStatement
    Public ReadOnly Property Expression As BoundExpression

  End Class

  Friend NotInheritable Class BoundLocateStatement
    Inherits BoundStatement

    Public Sub New(row As BoundExpression,
                   col As BoundExpression,
                   visible As BoundExpression,
                   scanStart As BoundExpression,
                   scanstop As BoundExpression)
      Me.Row = row
      Me.Col = col
      Me.Visible = visible
      Me.ScanStart = scanStart
      Me.Scanstop = scanstop
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LocateStatement
    Public ReadOnly Property Row As BoundExpression
    Public ReadOnly Property Col As BoundExpression
    Public ReadOnly Property Visible As BoundExpression
    Public ReadOnly Property ScanStart As BoundExpression
    Public ReadOnly Property Scanstop As BoundExpression

  End Class

End Namespace
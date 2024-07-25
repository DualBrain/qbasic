Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSwapStatement
    Inherits BoundStatement

    Public Sub New(variable1 As BoundExpression, variable2 As BoundExpression)
      Me.Variable1 = variable1
      Me.Variable2 = variable2
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SwapStatement
    Public ReadOnly Property Variable1 As BoundExpression
    Public ReadOnly Property Variable2 As BoundExpression

  End Class

End Namespace
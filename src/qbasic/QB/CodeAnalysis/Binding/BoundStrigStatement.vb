Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundStrigStatement
    Inherits BoundStatement

    Public Sub New(triggerNumber As BoundExpression, verbKind As SyntaxKind)
      Me.TriggerNumber = triggerNumber
      Me.VerbKind = verbKind
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.StrigStatement
    Public ReadOnly Property TriggerNumber As BoundExpression
    Public ReadOnly Property VerbKind As SyntaxKind

  End Class

End Namespace
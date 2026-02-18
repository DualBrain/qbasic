Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPlayEventStatement
    Inherits BoundStatement

    Public Sub New(queueSize As BoundExpression, verbKind As SyntaxKind)
      Me.QueueSize = queueSize
      Me.VerbKind = verbKind
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PlayEventStatement
    Public ReadOnly Property QueueSize As BoundExpression
    Public ReadOnly Property VerbKind As SyntaxKind

  End Class

End Namespace
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundComStatement
    Inherits BoundStatement

    Public Sub New(channel As BoundExpression, verbKind As SyntaxKind)
      Me.Channel = channel
      Me.VerbKind = verbKind
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ComStatement
    Public ReadOnly Property Channel As BoundExpression
    Public ReadOnly Property VerbKind As SyntaxKind

  End Class

End Namespace
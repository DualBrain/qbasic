Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPenStatement
    Inherits BoundStatement

    Public Sub New(verbKind As SyntaxKind)
      Me.VerbKind = verbKind
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PenStatement
    Public ReadOnly Property VerbKind As SyntaxKind

  End Class

End Namespace
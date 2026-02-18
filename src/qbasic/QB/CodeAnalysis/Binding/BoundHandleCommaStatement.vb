Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundHandleCommaStatement
    Inherits BoundStatement

    Public Sub New()
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.HandleCommaStatement

  End Class

End Namespace

Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundNopStatement
    Inherits BoundStatement

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.NopStatement

  End Class

End Namespace
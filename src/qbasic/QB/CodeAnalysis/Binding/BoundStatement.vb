Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend MustInherit Class BoundStatement
    Inherits BoundNode

    Public Overridable ReadOnly Property Syntax As StatementSyntax
      Get
        Return Nothing
      End Get
    End Property

  End Class

End Namespace
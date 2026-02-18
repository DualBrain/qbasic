Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend MustInherit Class BoundStatement
    Inherits BoundNode

    Public Overridable ReadOnly Property Syntax As StatementSyntax
      Get
        Return Nothing
      End Get
    End Property

    'Public Overridable ReadOnly Property SyntaxIndex As Integer?
    '  Get
    '    Return New Integer?
    '  End Get
    'End Property

  End Class

End Namespace
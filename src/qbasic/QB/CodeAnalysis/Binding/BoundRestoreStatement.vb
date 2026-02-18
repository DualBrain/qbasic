Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundRestoreStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As RestoreStatementSyntax

    Sub New(target As BoundLabel)
      Me.Target = target
    End Sub

    Sub New(syntax As RestoreStatementSyntax, target As BoundLabel)
      m_syntax = syntax
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.RestoreStatement
    Public ReadOnly Property Target As BoundLabel

    Public ReadOnly Property HasTarget As Boolean
      Get
        Return Target IsNot Nothing
      End Get
    End Property

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
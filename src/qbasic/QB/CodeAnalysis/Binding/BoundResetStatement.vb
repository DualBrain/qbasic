Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundResetStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As ResetStatementSyntax

    Public Sub New()
    End Sub

    Public Sub New(syntax As ResetStatementSyntax)
      m_syntax = syntax
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ResetStatement

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

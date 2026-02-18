Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundRemStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As RemStatementSyntax

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.RemStatement

    Public Sub New()
    End Sub

    Public Sub New(syntax As RemStatementSyntax)
      m_syntax = syntax
    End Sub

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
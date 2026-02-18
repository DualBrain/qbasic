Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundEndStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As EndStatementSyntax

    Public Sub New()
    End Sub

    Public Sub New(syntax As EndStatementSyntax)
      m_syntax = syntax
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.EndStatement

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
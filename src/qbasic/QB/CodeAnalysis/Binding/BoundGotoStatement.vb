Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundGotoStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As GotoStatementSyntax

    Sub New(label As BoundLabel)
      Me.Label = label
    End Sub

    Sub New(syntax As GotoStatementSyntax, label As BoundLabel)
      m_syntax = syntax
      Me.Label = label
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.GotoStatement
    Public ReadOnly Property Label As BoundLabel

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

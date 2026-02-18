Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundGosubStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As GosubStatementSyntax

    Sub New(label As BoundLabel)
      Me.Label = label
    End Sub

    Sub New(syntax As GosubStatementSyntax, label As BoundLabel)
      m_syntax = syntax
      Me.Label = label
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.GosubStatement
    Public ReadOnly Property Label As BoundLabel

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
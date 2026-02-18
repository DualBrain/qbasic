Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOutStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As OutStatementSyntax

    Public Sub New(port As BoundExpression, data As BoundExpression)
      Me.Port = port
      Me.Data = data
    End Sub

    Public Sub New(syntax As OutStatementSyntax, port As BoundExpression, data As BoundExpression)
      m_syntax = syntax
      Me.Port = port
      Me.Data = data
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OutStatement
    Public ReadOnly Property Port As BoundExpression
    Public ReadOnly Property Data As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
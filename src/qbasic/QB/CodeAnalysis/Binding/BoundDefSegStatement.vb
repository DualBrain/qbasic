Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDefSegStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As DefSegStatementSyntax

    Public Sub New(syntax As DefSegStatementSyntax, address As BoundExpression)
      m_syntax = syntax
      Me.Address = address
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DefSegStatement
    Public ReadOnly Property Address As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
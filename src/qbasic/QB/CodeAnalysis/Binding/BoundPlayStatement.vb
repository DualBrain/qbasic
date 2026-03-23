Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPlayStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PlayStatementSyntax
    Private ReadOnly m_command As BoundExpression

    Public Sub New(syntax As PlayStatementSyntax, command As BoundExpression)
      m_syntax = syntax
      m_command = command
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PlayStatement

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

    Public ReadOnly Property Command As BoundExpression
      Get
        Return m_command
      End Get
    End Property

  End Class

End Namespace
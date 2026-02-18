Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPokeStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PokeStatementSyntax

    Public Sub New(offset As BoundExpression, value As BoundExpression)
      Me.Offset = offset
      Me.Value = value
    End Sub

    Public Sub New(syntax As PokeStatementSyntax, offset As BoundExpression, value As BoundExpression)
      m_syntax = syntax
      Me.Offset = offset
      Me.Value = value
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PokeStatement
    Public ReadOnly Property Offset As BoundExpression
    Public ReadOnly Property Value As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

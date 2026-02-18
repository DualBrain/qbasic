Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundChainStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As ChainStatementSyntax

    Public Sub New(filename As BoundExpression, optionalLine As BoundExpression)
      Me.Filename = filename
      Me.OptionalLine = optionalLine
    End Sub

    Public Sub New(syntax As ChainStatementSyntax, filename As BoundExpression, optionalLine As BoundExpression)
      m_syntax = syntax
      Me.Filename = filename
      Me.OptionalLine = optionalLine
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ChainStatement
    Public ReadOnly Property Filename As BoundExpression
    Public ReadOnly Property OptionalLine As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

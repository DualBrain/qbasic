Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundNoiseStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As NoiseStatementSyntax
    Private ReadOnly m_source As BoundExpression
    Private ReadOnly m_volume As BoundExpression
    Private ReadOnly m_duration As BoundExpression

    Public Sub New(syntax As NoiseStatementSyntax, source As BoundExpression, volume As BoundExpression, duration As BoundExpression)
      m_syntax = syntax
      m_source = source
      m_volume = volume
      m_duration = duration
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.NoiseStatement

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

    Public ReadOnly Property Source As BoundExpression
      Get
        Return m_source
      End Get
    End Property

    Public ReadOnly Property Volume As BoundExpression
      Get
        Return m_volume
      End Get
    End Property

    Public ReadOnly Property Duration As BoundExpression
      Get
        Return m_duration
      End Get
    End Property

  End Class

End Namespace
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSoundStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As SoundStatementSyntax
    Private ReadOnly m_frequency As BoundExpression
    Private ReadOnly m_duration As BoundExpression

    Public Sub New(syntax As SoundStatementSyntax, frequency As BoundExpression, duration As BoundExpression)
      m_syntax = syntax
      m_frequency = frequency
      m_duration = duration
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SoundStatement

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

    Public ReadOnly Property Frequency As BoundExpression
      Get
        Return m_frequency
      End Get
    End Property

    Public ReadOnly Property Duration As BoundExpression
      Get
        Return m_duration
      End Get
    End Property

  End Class

End Namespace

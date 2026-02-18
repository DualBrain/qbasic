Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnPlayGosubStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As OnPlayGosubStatementSyntax

    Public Sub New(queueSize As BoundExpression, target As BoundExpression)
      Me.QueueSize = queueSize
      Me.Target = target
    End Sub

    Public Sub New(syntax As OnPlayGosubStatementSyntax, queueSize As BoundExpression, target As BoundExpression)
      m_syntax = syntax
      Me.QueueSize = queueSize
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnPlayGosubStatement
    Public ReadOnly Property QueueSize As BoundExpression
    Public ReadOnly Property Target As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

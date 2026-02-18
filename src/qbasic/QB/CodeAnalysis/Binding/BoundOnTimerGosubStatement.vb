Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnTimerGosubStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As OnTimerGosubStatementSyntax

    Public Sub New(interval As BoundExpression, target As BoundExpression)
      Me.Interval = interval
      Me.Target = target
    End Sub

    Public Sub New(syntax As OnTimerGosubStatementSyntax, interval As BoundExpression, target As BoundExpression)
      m_syntax = syntax
      Me.Interval = interval
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnTimerGosubStatement
    Public ReadOnly Property Interval As BoundExpression
    Public ReadOnly Property Target As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
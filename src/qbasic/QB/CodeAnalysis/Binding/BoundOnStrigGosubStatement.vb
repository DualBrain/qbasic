Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnStrigGosubStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As OnStrigGosubStatementSyntax

    Public Sub New(triggerNumber As BoundExpression, target As BoundExpression)
      Me.TriggerNumber = triggerNumber
      Me.Target = target
    End Sub

    Public Sub New(syntax As OnStrigGosubStatementSyntax, triggerNumber As BoundExpression, target As BoundExpression)
      m_syntax = syntax
      Me.TriggerNumber = triggerNumber
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnStrigGosubStatement
    Public ReadOnly Property TriggerNumber As BoundExpression
    Public ReadOnly Property Target As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnComGosubStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As OnComGosubStatementSyntax

    Public Sub New(channel As BoundExpression, target As BoundExpression)
      Me.Channel = channel
      Me.Target = target
    End Sub

    Public Sub New(syntax As OnComGosubStatementSyntax, channel As BoundExpression, target As BoundExpression)
      m_syntax = syntax
      Me.Channel = channel
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnComGosubStatement
    Public ReadOnly Property Channel As BoundExpression
    Public ReadOnly Property Target As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

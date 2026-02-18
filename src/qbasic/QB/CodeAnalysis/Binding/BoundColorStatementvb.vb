Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundColorStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As ColorStatementSyntax

    Public Sub New(argumentExpression1 As BoundExpression, argumentExpression2 As BoundExpression, argumentExpression3 As BoundExpression)
      Me.Expression1 = argumentExpression1
      Me.Expression2 = argumentExpression2
      Me.Expression3 = argumentExpression3
    End Sub

    Public Sub New(syntax As ColorStatementSyntax, argumentExpression1 As BoundExpression, argumentExpression2 As BoundExpression, argumentExpression3 As BoundExpression)
      m_syntax = syntax
      Me.Expression1 = argumentExpression1
      Me.Expression2 = argumentExpression2
      Me.Expression3 = argumentExpression3
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ColorStatement
    Public ReadOnly Property Expression1 As BoundExpression
    Public ReadOnly Property Expression2 As BoundExpression
    Public ReadOnly Property Expression3 As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
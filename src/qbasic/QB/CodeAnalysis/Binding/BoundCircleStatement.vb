Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundCircleStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As CircleStatementSyntax

    Public Sub New([step] As Boolean, x As BoundExpression, y As BoundExpression, radius As BoundExpression, color As BoundExpression, start As BoundExpression, [end] As BoundExpression, aspect As BoundExpression)
      Me.Step = [step]
      Me.X = x
      Me.Y = y
      Me.Radius = radius
      Me.Color = color
      Me.Start = start
      Me.End = [end]
      Me.Aspect = aspect
    End Sub

    Public Sub New(syntax As CircleStatementSyntax, [step] As Boolean, x As BoundExpression, y As BoundExpression, radius As BoundExpression, color As BoundExpression, start As BoundExpression, [end] As BoundExpression, aspect As BoundExpression)
      m_syntax = syntax
      Me.Step = [step]
      Me.X = x
      Me.Y = y
      Me.Radius = radius
      Me.Color = color
      Me.Start = start
      Me.End = [end]
      Me.Aspect = aspect
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CircleStatement
    Public ReadOnly Property [Step] As Boolean
    Public ReadOnly Property X As BoundExpression
    Public ReadOnly Property Y As BoundExpression
    Public ReadOnly Property Radius As BoundExpression
    Public ReadOnly Property Color As BoundExpression
    Public ReadOnly Property Start As BoundExpression
    Public ReadOnly Property [End] As BoundExpression
    Public ReadOnly Property Aspect As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
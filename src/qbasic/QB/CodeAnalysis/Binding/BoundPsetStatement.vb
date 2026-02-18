Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPsetStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PsetStatementSyntax

    Public Sub New([step] As Boolean,
                   x As BoundExpression,
                   y As BoundExpression,
                   color As BoundExpression)
      Me.Step = [step]
      Me.X = x
      Me.Y = y
      Me.Color = color
    End Sub

    Public Sub New(syntax As PsetStatementSyntax, [step] As Boolean,
                   x As BoundExpression,
                   y As BoundExpression,
                   color As BoundExpression)
      m_syntax = syntax
      Me.Step = [step]
      Me.X = x
      Me.Y = y
      Me.Color = color
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PsetStatement
    Public ReadOnly Property [Step] As Boolean
    Public ReadOnly Property X As BoundExpression
    Public ReadOnly Property Y As BoundExpression
    Public ReadOnly Property Color As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
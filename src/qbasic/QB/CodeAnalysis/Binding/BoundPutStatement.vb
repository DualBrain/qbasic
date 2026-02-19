Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPutStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PutStatementSyntax

    Public Sub New(syntax As PutStatementSyntax, stepRelative As Boolean, x As BoundExpression, y As BoundExpression, buffer As BoundExpression)
      m_syntax = syntax
      Me.StepRelative = stepRelative
      Me.X = x
      Me.Y = y
      Me.Buffer = buffer
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PutStatement
    Public ReadOnly Property StepRelative As Boolean
    Public ReadOnly Property X As BoundExpression
    Public ReadOnly Property Y As BoundExpression
    Public ReadOnly Property Buffer As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundGetStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As GetStatementSyntax

    Public Sub New(syntax As GetStatementSyntax,
                   step1 As Boolean,
                   x1 As BoundExpression,
                   y1 As BoundExpression,
                   step2 As Boolean,
                   x2 As BoundExpression,
                   y2 As BoundExpression,
                   buffer As BoundExpression)
      m_syntax = syntax
      Me.Step1 = step1
      Me.X1 = x1
      Me.Y1 = y1
      Me.Step2 = step2
      Me.X2 = x2
      Me.Y2 = y2
      Me.Buffer = buffer
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.GetStatement
    Public ReadOnly Property Step1 As Boolean
    Public ReadOnly Property X1 As BoundExpression
    Public ReadOnly Property Y1 As BoundExpression
    Public ReadOnly Property Step2 As Boolean
    Public ReadOnly Property X2 As BoundExpression
    Public ReadOnly Property Y2 As BoundExpression
    Public ReadOnly Property Buffer As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

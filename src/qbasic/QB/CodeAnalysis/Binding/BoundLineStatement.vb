Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLineStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As LineStatementSyntax

    Public Sub New(step1 As Boolean,
                   x1 As BoundExpression,
                   y1 As BoundExpression,
                   step2 As Boolean,
                   x2 As BoundExpression,
                   y2 As BoundExpression,
                   attribute As BoundExpression,
                   mode As Integer,
                   style As BoundExpression)
      Me.Step1 = step1
      Me.X1 = x1
      Me.Y1 = y1
      Me.Step2 = step2
      Me.X2 = x2
      Me.Y2 = y2
      Me.Attribute = attribute
      Me.Mode = mode
      Me.Style = style
    End Sub

    Public Sub New(syntax As LineStatementSyntax, step1 As Boolean,
                   x1 As BoundExpression,
                   y1 As BoundExpression,
                   step2 As Boolean,
                   x2 As BoundExpression,
                   y2 As BoundExpression,
                   attribute As BoundExpression,
                   mode As Integer,
                   style As BoundExpression)
      m_syntax = syntax
      Me.Step1 = step1
      Me.X1 = x1
      Me.Y1 = y1
      Me.Step2 = step2
      Me.X2 = x2
      Me.Y2 = y2
      Me.Attribute = attribute
      Me.Mode = mode
      Me.Style = style
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LineStatement
    Public ReadOnly Property Step1 As Boolean
    Public ReadOnly Property X1 As BoundExpression
    Public ReadOnly Property Y1 As BoundExpression
    Public ReadOnly Property Step2 As Boolean
    Public ReadOnly Property X2 As BoundExpression
    Public ReadOnly Property Y2 As BoundExpression
    Public ReadOnly Property Attribute As BoundExpression
    Public ReadOnly Property Mode As Integer
    Public ReadOnly Property Style As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

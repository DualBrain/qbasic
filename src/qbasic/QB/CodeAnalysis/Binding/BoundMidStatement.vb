Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend Class BoundMidStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As MidStatementSyntax

    Public Sub New(targetExpression As BoundExpression, positionExpression As BoundExpression, lengthExpression As BoundExpression, expression As BoundExpression)
      Me.TargetExpression = targetExpression
      Me.PositionExpression = positionExpression
      Me.LengthExpression = lengthExpression
      Me.Expression = expression
    End Sub

    Public Sub New(syntax As MidStatementSyntax, targetExpression As BoundExpression, positionExpression As BoundExpression, lengthExpression As BoundExpression, expression As BoundExpression)
      m_syntax = syntax
      Me.TargetExpression = targetExpression
      Me.PositionExpression = positionExpression
      Me.LengthExpression = lengthExpression
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.MidStatement
    Public ReadOnly Property TargetExpression As BoundExpression
    Public ReadOnly Property PositionExpression As BoundExpression
    Public ReadOnly Property LengthExpression As BoundExpression
    Public ReadOnly Property Expression As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

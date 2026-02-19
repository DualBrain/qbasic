Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPaintStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PaintStatementSyntax

    Public Sub New(syntax As PaintStatementSyntax, stepRelative As Boolean, x As BoundExpression, y As BoundExpression, colorOrTile As BoundExpression)
      m_syntax = syntax
      Me.StepRelative = stepRelative
      Me.X = x
      Me.Y = y
      Me.ColorOrTile = colorOrTile
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PaintStatement
    Public ReadOnly Property StepRelative As Boolean
    Public ReadOnly Property X As BoundExpression
    Public ReadOnly Property Y As BoundExpression
    Public ReadOnly Property ColorOrTile As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

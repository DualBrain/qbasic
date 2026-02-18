Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundScreenStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As ScreenStatementSyntax

    Public Sub New(mode As BoundExpression,
                   colorBurst As BoundExpression,
                   aPage As BoundExpression,
                   vPage As BoundExpression,
                   [erase] As BoundExpression)
      Me.Mode = mode
      Me.ColorBurst = colorBurst
      Me.APage = aPage
      Me.VPage = vPage
      Me.Erase = [erase]
    End Sub

    Public Sub New(syntax As ScreenStatementSyntax, mode As BoundExpression,
                   colorBurst As BoundExpression,
                   aPage As BoundExpression,
                   vPage As BoundExpression,
                   [erase] As BoundExpression)
      m_syntax = syntax
      Me.Mode = mode
      Me.ColorBurst = colorBurst
      Me.APage = aPage
      Me.VPage = vPage
      Me.Erase = [erase]
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ScreenStatement
    Public ReadOnly Property Mode As BoundExpression
    Public ReadOnly Property ColorBurst As BoundExpression
    Public ReadOnly Property APage As BoundExpression
    Public ReadOnly Property VPage As BoundExpression
    Public ReadOnly Property [Erase] As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

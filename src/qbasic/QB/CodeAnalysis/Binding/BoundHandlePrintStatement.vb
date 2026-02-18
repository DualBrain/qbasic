Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundHandlePrintStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PrintStatementSyntax

    Public Sub New(expression As BoundExpression, noCr As Boolean)
      Me.Expression = expression
      Me.NoCr = noCr
    End Sub

    Public Sub New(syntax As PrintStatementSyntax, expression As BoundExpression, noCr As Boolean)
      m_syntax = syntax
      Me.Expression = expression
      Me.NoCr = noCr
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.HandlePrintStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property NoCr As Boolean

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

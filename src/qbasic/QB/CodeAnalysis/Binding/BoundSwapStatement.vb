Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSwapStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As SwapStatementSyntax

    Public Sub New(variable1 As BoundExpression, variable2 As BoundExpression)
      Me.Variable1 = variable1
      Me.Variable2 = variable2
    End Sub

    Public Sub New(syntax As SwapStatementSyntax, variable1 As BoundExpression, variable2 As BoundExpression)
      m_syntax = syntax
      Me.Variable1 = variable1
      Me.Variable2 = variable2
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SwapStatement
    Public ReadOnly Property Variable1 As BoundExpression
    Public ReadOnly Property Variable2 As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
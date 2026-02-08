Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnGotoStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As OnGotoStatementSyntax

    Public Sub New(expression As BoundExpression, targets As ImmutableArray(Of BoundLabel))
      Me.Expression = expression
      Me.Targets = targets
    End Sub

    Public Sub New(syntax As OnGotoStatementSyntax, expression As BoundExpression, targets As ImmutableArray(Of BoundLabel))
      m_syntax = syntax
      Me.Expression = expression
      Me.Targets = targets
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnGotoStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property Targets As ImmutableArray(Of BoundLabel)

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
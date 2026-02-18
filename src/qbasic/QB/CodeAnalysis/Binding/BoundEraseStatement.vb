Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend Class BoundEraseStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As EraseStatementSyntax

    Public Sub New(variables As ImmutableArray(Of BoundExpression))
      Me.Variables = variables
    End Sub

    Public Sub New(syntax As EraseStatementSyntax, variables As ImmutableArray(Of BoundExpression))
      m_syntax = syntax
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.EraseStatement
    Public ReadOnly Property Variables As ImmutableArray(Of BoundExpression)

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

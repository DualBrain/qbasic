Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundCloseStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As CloseStatementSyntax

    Public Sub New(fileNumbers As ImmutableArray(Of BoundExpression))
      Me.FileNumbers = fileNumbers
    End Sub

    Public Sub New(syntax As CloseStatementSyntax, fileNumbers As ImmutableArray(Of BoundExpression))
      m_syntax = syntax
      Me.FileNumbers = fileNumbers
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CloseStatement
    Public ReadOnly Property FileNumbers As ImmutableArray(Of BoundExpression)

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

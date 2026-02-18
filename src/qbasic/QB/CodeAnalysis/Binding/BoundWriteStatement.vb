Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundWriteStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As WriteStatementSyntax

    Public Sub New(fileNumber As BoundExpression, expressions As ImmutableArray(Of BoundExpression), suppressCr As Boolean)
      Me.FileNumber = fileNumber
      Me.Expressions = expressions
      Me.SuppressCr = suppressCr
    End Sub

    Public Sub New(syntax As WriteStatementSyntax, fileNumber As BoundExpression, expressions As ImmutableArray(Of BoundExpression), suppressCr As Boolean)
      m_syntax = syntax
      Me.FileNumber = fileNumber
      Me.Expressions = expressions
      Me.SuppressCr = suppressCr
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.WriteStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property Expressions As ImmutableArray(Of BoundExpression)
    Public ReadOnly Property SuppressCr As Boolean

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

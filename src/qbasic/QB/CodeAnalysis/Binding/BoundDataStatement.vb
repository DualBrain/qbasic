Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDataStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As DataStatementSyntax

    Public Sub New(data As ImmutableArray(Of Object), lineNumber As Integer)
      Me.Data = data
      Me.LineNumber = lineNumber
    End Sub

    Public Sub New(syntax As DataStatementSyntax, data As ImmutableArray(Of Object), lineNumber As Integer)
      m_syntax = syntax
      Me.Data = data
      Me.LineNumber = lineNumber
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DataStatement
    Public ReadOnly Property Data As ImmutableArray(Of Object)
    Public ReadOnly Property LineNumber As Integer

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

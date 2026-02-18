Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOpenStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As OpenStatementSyntax

    Public Sub New(file As BoundExpression, mode As BoundExpression, access As ImmutableArray(Of BoundExpression), lock As ImmutableArray(Of BoundExpression), fileNumber As BoundExpression, recLen As BoundExpression)
      Me.File = file
      Me.Mode = mode
      Me.Access = access
      Me.Lock = lock
      Me.FileNumber = fileNumber
      Me.RecLen = recLen
    End Sub

    Public Sub New(syntax As OpenStatementSyntax, file As BoundExpression, mode As BoundExpression, access As ImmutableArray(Of BoundExpression), lock As ImmutableArray(Of BoundExpression), fileNumber As BoundExpression, recLen As BoundExpression)
      m_syntax = syntax
      Me.File = file
      Me.Mode = mode
      Me.Access = access
      Me.Lock = lock
      Me.FileNumber = fileNumber
      Me.RecLen = recLen
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OpenStatement
    Public ReadOnly Property File As BoundExpression
    Public ReadOnly Property Mode As BoundExpression
    Public ReadOnly Property Access As ImmutableArray(Of BoundExpression)
    Public ReadOnly Property Lock As ImmutableArray(Of BoundExpression)
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property RecLen As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
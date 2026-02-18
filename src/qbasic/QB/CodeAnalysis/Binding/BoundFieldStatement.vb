Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundFieldStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As FieldStatementSyntax

    Public Sub New(fileNumber As BoundExpression, fieldDefinitions As ImmutableArray(Of (Width As BoundExpression, VariableName As String)))
      Me.FileNumber = fileNumber
      Me.FieldDefinitions = fieldDefinitions
    End Sub

    Public Sub New(syntax As FieldStatementSyntax, fileNumber As BoundExpression, fieldDefinitions As ImmutableArray(Of (Width As BoundExpression, VariableName As String)))
      m_syntax = syntax
      Me.FileNumber = fileNumber
      Me.FieldDefinitions = fieldDefinitions
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.FieldStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property FieldDefinitions As ImmutableArray(Of (Width As BoundExpression, VariableName As String))

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

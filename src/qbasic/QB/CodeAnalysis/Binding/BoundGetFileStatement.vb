Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundGetFileStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As GetFileStatementSyntax

    Public Sub New(fileNumber As BoundExpression, optionalRecord As BoundExpression, optionalVariable As String)
      Me.FileNumber = fileNumber
      Me.OptionalRecord = optionalRecord
      Me.OptionalVariable = optionalVariable
    End Sub

    Public Sub New(syntax As GetFileStatementSyntax, fileNumber As BoundExpression, optionalRecord As BoundExpression, optionalVariable As String)
      m_syntax = syntax
      Me.FileNumber = fileNumber
      Me.OptionalRecord = optionalRecord
      Me.OptionalVariable = optionalVariable
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.GetFileStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property OptionalRecord As BoundExpression
    Public ReadOnly Property OptionalVariable As String

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSeekStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As SeekStatementSyntax

    Public Sub New(fileNumber As BoundExpression, position As BoundExpression)
      Me.FileNumber = fileNumber
      Me.Position = position
    End Sub

    Public Sub New(syntax As SeekStatementSyntax, fileNumber As BoundExpression, position As BoundExpression)
      m_syntax = syntax
      Me.FileNumber = fileNumber
      Me.Position = position
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SeekStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property Position As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
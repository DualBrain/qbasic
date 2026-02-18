Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLineInputFileStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As LineInputFileStatementSyntax

    Public Sub New(fileNumber As BoundExpression, variable As BoundExpression)
      Me.FileNumber = fileNumber
      Me.Variable = variable
    End Sub

    Public Sub New(syntax As LineInputFileStatementSyntax, fileNumber As BoundExpression, variable As BoundExpression)
      m_syntax = syntax
      Me.FileNumber = fileNumber
      Me.Variable = variable
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LineInputFileStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property Variable As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundNameStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As NameStatementSyntax

    Public Sub New(originalPath As BoundExpression, destinationPath As BoundExpression)
      Me.OriginalPath = originalPath
      Me.DestinationPath = destinationPath
    End Sub

    Public Sub New(syntax As NameStatementSyntax, originalPath As BoundExpression, destinationPath As BoundExpression)
      m_syntax = syntax
      Me.OriginalPath = originalPath
      Me.DestinationPath = destinationPath
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.NameStatement
    Public ReadOnly Property OriginalPath As BoundExpression
    Public ReadOnly Property DestinationPath As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
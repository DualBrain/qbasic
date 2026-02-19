Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPcopyStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PcopyStatementSyntax

    Public Sub New(syntax As PcopyStatementSyntax, sourcePage As BoundExpression, destinationPage As BoundExpression)
      m_syntax = syntax
      Me.SourcePage = sourcePage
      Me.DestinationPage = destinationPage
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PcopyStatement
    Public ReadOnly Property SourcePage As BoundExpression
    Public ReadOnly Property DestinationPage As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

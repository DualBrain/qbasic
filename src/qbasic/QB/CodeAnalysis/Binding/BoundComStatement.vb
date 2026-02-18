Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundComStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As ComStatementSyntax

    Public Sub New(channel As BoundExpression, verbKind As SyntaxKind)
      Me.Channel = channel
      Me.VerbKind = verbKind
    End Sub

    Public Sub New(syntax As ComStatementSyntax, channel As BoundExpression, verbKind As SyntaxKind)
      m_syntax = syntax
      Me.Channel = channel
      Me.VerbKind = verbKind
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ComStatement
    Public ReadOnly Property Channel As BoundExpression
    Public ReadOnly Property VerbKind As SyntaxKind

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

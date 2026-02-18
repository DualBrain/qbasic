Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundResumeStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As ResumeStatementSyntax

    Public Sub New(target As BoundExpression)
      Me.Target = target
    End Sub

    Public Sub New(syntax As ResumeStatementSyntax, target As BoundExpression)
      m_syntax = syntax
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ResumeStatement
    Public ReadOnly Property Target As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
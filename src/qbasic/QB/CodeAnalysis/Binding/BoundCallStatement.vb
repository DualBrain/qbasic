Imports QB.CodeAnalysis.Binding
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundCallStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As CallStatementSyntax

    Public Sub New([call] As BoundCallExpression)
      Me.Call = [call]
    End Sub

    Public Sub New(syntax As CallStatementSyntax, [call] As BoundCallExpression)
      m_syntax = syntax
      Me.Call = [call]
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CallStatement
    Public ReadOnly Property [Call] As BoundCallExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

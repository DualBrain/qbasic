Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOnKeyGosubStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As OnKeyGosubStatementSyntax

    Public Sub New(keyNumber As BoundExpression, target As BoundExpression)
      Me.KeyNumber = keyNumber
      Me.Target = target
    End Sub

    Public Sub New(syntax As OnKeyGosubStatementSyntax, keyNumber As BoundExpression, target As BoundExpression)
      m_syntax = syntax
      Me.KeyNumber = keyNumber
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OnKeyGosubStatement
    Public ReadOnly Property KeyNumber As BoundExpression
    Public ReadOnly Property Target As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

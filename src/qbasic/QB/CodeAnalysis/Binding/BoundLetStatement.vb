Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLetStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As LetStatementSyntax

    Public Sub New(variable As VariableSymbol, expression As BoundExpression)
      Me.Variable = variable
      Me.Expression = expression
    End Sub

    Public Sub New(syntax As LetStatementSyntax, variable As VariableSymbol, expression As BoundExpression)
      m_syntax = syntax
      Me.Variable = variable
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LetStatement
    Public ReadOnly Property Variable As VariableSymbol
    Public ReadOnly Property Expression As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace

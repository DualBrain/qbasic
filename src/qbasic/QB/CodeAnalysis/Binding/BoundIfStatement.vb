Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundIfStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As IfStatementSyntax

    Public Sub New(expression As BoundExpression, statements As BoundStatement, elseIfStatements As ImmutableArray(Of BoundElseIfStatement), elseStatement As BoundStatement)
      Me.Expression = expression
      Me.Statements = statements
      Me.ElseIfStatements = elseIfStatements
      Me.ElseStatement = elseStatement
    End Sub

    Public Sub New(syntax As IfStatementSyntax, expression As BoundExpression, statements As BoundStatement, elseIfStatements As ImmutableArray(Of BoundElseIfStatement), elseStatement As BoundStatement)
      m_syntax = syntax
      Me.Expression = expression
      Me.Statements = statements
      Me.ElseIfStatements = elseIfStatements
      Me.ElseStatement = elseStatement
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.IfStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property Statements As BoundStatement
    Public ReadOnly Property ElseIfStatements As ImmutableArray(Of BoundElseIfStatement)
    Public ReadOnly Property ElseStatement As BoundStatement

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
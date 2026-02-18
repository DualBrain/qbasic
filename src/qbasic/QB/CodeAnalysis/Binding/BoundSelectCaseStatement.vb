Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSelectCaseStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As SelectCaseStatementSyntax

    Public Sub New(test As BoundExpression, cases As ImmutableArray(Of BoundCaseStatement), elseStatement As BoundStatement)
      Me.Test = test
      Me.Cases = cases
      Me.ElseStatement = elseStatement
    End Sub

    Public Sub New(syntax As SelectCaseStatementSyntax, test As BoundExpression, cases As ImmutableArray(Of BoundCaseStatement), elseStatement As BoundStatement)
      m_syntax = syntax
      Me.Test = test
      Me.Cases = cases
      Me.ElseStatement = elseStatement
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SelectCaseStatement
    Public ReadOnly Property Test As BoundExpression
    Public ReadOnly Property Cases As ImmutableArray(Of BoundCaseStatement)
    Public ReadOnly Property ElseStatement As BoundStatement

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

  Friend NotInheritable Class BoundCaseStatement
    Inherits BoundStatement

    Public Sub New(matches As ImmutableArray(Of BoundCaseMatchStatement), statement As BoundStatement)
      Me.Matches = matches
      Me.Statement = statement
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CaseStatement
    Public ReadOnly Property Matches As ImmutableArray(Of BoundCaseMatchStatement)
    Public ReadOnly Property Statement As BoundStatement

  End Class

  Friend NotInheritable Class BoundCaseMatchStatement
    Inherits BoundStatement

    Public Sub New(matchType As CaseMatchType, comparison As SyntaxKind, expression As BoundExpression, expressionTo As BoundExpression)
      Me.MatchType = matchType
      Me.Comparison = comparison
      Me.Expression = expression
      Me.ExpressionTo = expressionTo
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CaseMatchStatement
    Public ReadOnly Property MatchType As CaseMatchType
    Public ReadOnly Property Comparison As SyntaxKind
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property ExpressionTo As BoundExpression

  End Class

  Friend Enum CaseMatchType
    Value
    Range
    IsComparison
  End Enum

End Namespace

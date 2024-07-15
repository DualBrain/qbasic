Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Public NotInheritable Class SelectCaseStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   selectKeyword As SyntaxToken,
                   selectCaseKeyword As SyntaxToken,
                   test As ExpressionSyntax,
                   cases As ImmutableArray(Of CaseClauseSyntax),
                   caseElseClause As CaseElseClauseSyntax,
                   endKeyword As SyntaxToken,
                   endSelectKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.SelectKeyword = selectKeyword
      Me.SelectCaseKeyword = selectCaseKeyword
      Me.Test = test
      Me.Cases = cases
      Me.CaseElseClause = caseElseClause
      Me.EndKeyword = endKeyword
      Me.EndSelectKeyword = endSelectKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SelectCaseStatement
    Public ReadOnly Property SelectKeyword As SyntaxToken
    Public ReadOnly Property SelectCaseKeyword As SyntaxToken
    Public ReadOnly Property Test As ExpressionSyntax
    Public ReadOnly Property Cases As ImmutableArray(Of CaseClauseSyntax)
    Public ReadOnly Property CaseElseClause As CaseElseClauseSyntax
    Public ReadOnly Property EndKeyword As SyntaxToken
    Public ReadOnly Property EndSelectKeyword As SyntaxToken
  End Class

  Public NotInheritable Class CaseClauseSyntax
    Inherits SyntaxNode

    Public Sub New(tree As SyntaxTree,
                   caseKeyword As SyntaxToken,
                   matches As ImmutableArray(Of ExpressionSyntax), statement As StatementSyntax)
      MyBase.New(tree)
      Me.CaseKeyword = caseKeyword
      Me.Matches = matches
      Me.Statement = statement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CaseClause
    Public ReadOnly Property CaseKeyword As SyntaxToken
    Public ReadOnly Property Matches As ImmutableArray(Of ExpressionSyntax)
    Public ReadOnly Property Statement As StatementSyntax

  End Class

  Public NotInheritable Class CaseElseClauseSyntax
    Inherits SyntaxNode

    Public Sub New(tree As SyntaxTree,
                   caseKeyword As SyntaxToken,
                   elseKeyword As SyntaxToken,
                   statement As StatementSyntax)
      MyBase.New(tree)
      Me.CaseKeyword = caseKeyword
      Me.ElseKeyword = elseKeyword
      Me.Statement = statement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CaseElseClause
    Public ReadOnly Property CaseKeyword As SyntaxToken
    Public ReadOnly Property ElseKeyword As SyntaxToken
    Public ReadOnly Property Statement As StatementSyntax

  End Class

  Public NotInheritable Class CaseMatchExpressionSyntax
    Inherits ExpressionSyntax

    Public Sub New(tree As SyntaxTree,
                   expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CaseMatchExpression
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

  Public NotInheritable Class CaseIsMatchExpressionSyntax
    Inherits ExpressionSyntax

    Public Sub New(tree As SyntaxTree,
                   isKeyword As SyntaxToken,
                   comparison As SyntaxToken,
                   expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.IsKeyword = isKeyword
      Me.Comparison = comparison
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CaseMatchExpression
    Public ReadOnly Property IsKeyword As SyntaxToken
    Public ReadOnly Property Comparison As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

  Public NotInheritable Class CaseMatchRangeExpressionSyntax
    Inherits ExpressionSyntax

    Public Sub New(tree As SyntaxTree,
                   start As ExpressionSyntax,
                   toKeyword As SyntaxToken,
                   [end] As ExpressionSyntax)
      MyBase.New(tree)
      Me.Start = start
      Me.ToKeyword = toKeyword
      Me.End = [end]
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CaseMatchExpression
    Public ReadOnly Property Start As ExpressionSyntax
    Public ReadOnly Property ToKeyword As SyntaxToken
    Public ReadOnly Property [End] As ExpressionSyntax

  End Class

End Namespace
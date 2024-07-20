Namespace Global.QB.CodeAnalysis.Syntax

  ''' <summary>
  ''' The For statement that begins a For-Next block. This statement 
  ''' always occurs as the Begin of a ForBlock. Most of the time, the 
  ''' End of that ForBlock is the corresponding Next statement. 
  ''' However, multiple nested For statements are ended by a single 
  ''' Next statement with multiple variables, then the inner For 
  ''' statements will have End set to Nothing, and the Next statement 
  ''' is the End of the outermost For statement that is being ended.
  ''' </summary>
  Public NotInheritable Class ForStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   forKeyword As SyntaxToken,
                   controlVariable As SyntaxToken,
                   equalsToken As SyntaxToken,
                   fromValue As ExpressionSyntax,
                   toKeyword As SyntaxToken,
                   toValue As ExpressionSyntax,
                   stepClause As ForStepClause,
                   statements As StatementSyntax,
                   nextKeyword As SyntaxToken,
                   optionalIdentifier As SyntaxToken)
      MyBase.New(tree)
      Me.ForKeyword = forKeyword
      Me.controlVariable = controlVariable
      Me.EqualsToken = equalsToken
      Me.FromValue = fromValue
      Me.ToKeyword = toKeyword
      Me.ToValue = toValue
      Me.StepClause = stepClause
      Me.Statements = statements
      Me.NextKeyword = nextKeyword
      Me.OptionalIdentifier = optionalIdentifier
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ForStatement
    Public ReadOnly Property ForKeyword As SyntaxToken
    Public ReadOnly Property controlVariable As SyntaxToken
    Public ReadOnly Property EqualsToken As SyntaxToken
    Public ReadOnly Property FromValue As ExpressionSyntax
    Public ReadOnly Property ToKeyword As SyntaxToken
    Public ReadOnly Property ToValue As ExpressionSyntax
    Public ReadOnly Property StepClause As ForStepClause
    Public ReadOnly Property Statements As StatementSyntax
    Public ReadOnly Property NextKeyword As SyntaxToken
    Public ReadOnly Property OptionalIdentifier As SyntaxToken

  End Class

  ''' <summary>
  ''' The Step clause in a For Statement.
  ''' </summary>
  Public NotInheritable Class ForStepClause
    Inherits QbSyntaxNode

    Public Sub New(tree As SyntaxTree, stepKeyword As SyntaxToken, stepValue As ExpressionSyntax)
      MyBase.New(tree)
      Me.StepKeyword = stepKeyword
      Me.StepValue = stepValue
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ForStepClause

    Public ReadOnly Property StepKeyword As SyntaxToken
    Public ReadOnly Property StepValue As ExpressionSyntax

  End Class

End Namespace
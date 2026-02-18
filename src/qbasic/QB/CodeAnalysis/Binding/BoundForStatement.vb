Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundForStatement
    Inherits BoundLoopStatement

    Private ReadOnly m_syntax As ForStatementSyntax

    Public Sub New(variable As VariableSymbol, lowerBound As BoundExpression, upperBound As BoundExpression, stepper As BoundExpression, body As BoundStatement, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      Me.Variable = variable
      Me.LowerBound = lowerBound
      Me.UpperBound = upperBound
      Me.Stepper = stepper
      Me.Body = body
    End Sub

    Public Sub New(syntax As ForStatementSyntax, variable As VariableSymbol, lowerBound As BoundExpression, upperBound As BoundExpression, stepper As BoundExpression, body As BoundStatement, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      m_syntax = syntax
      Me.Variable = variable
      Me.LowerBound = lowerBound
      Me.UpperBound = upperBound
      Me.Stepper = stepper
      Me.Body = body
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ForStatement
    Public ReadOnly Property Variable As VariableSymbol
    Public ReadOnly Property LowerBound As BoundExpression
    Public ReadOnly Property UpperBound As BoundExpression
    Public ReadOnly Property Stepper As BoundExpression
    Public ReadOnly Property Body As BoundStatement

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
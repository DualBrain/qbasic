Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundParenExpression
    Inherits BoundExpression

    Public Sub New(expression As BoundExpression, Optional syntax As Syntax.ExpressionSyntax = Nothing)
      MyBase.New(syntax)
      Me.Expression = expression
      Me.Type = expression.Type
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ParenExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
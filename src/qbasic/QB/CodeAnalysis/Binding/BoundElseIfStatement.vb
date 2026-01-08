Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundElseIfStatement
    Inherits BoundStatement

    Public Sub New(expression As BoundExpression, statements As BoundStatement)
      Me.Expression = expression
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ElseIfStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property Statements As BoundStatement

  End Class

End Namespace
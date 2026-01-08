Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundIfStatement
    Inherits BoundStatement

    Public Sub New(expression As BoundExpression, statements As BoundStatement, elseIfStatements As ImmutableArray(Of BoundElseIfStatement), elseStatement As BoundStatement)
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

  End Class

End Namespace
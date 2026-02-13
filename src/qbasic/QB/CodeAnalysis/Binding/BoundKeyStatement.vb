Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundKeyStatement
    Inherits BoundStatement

    Public Sub New(keyNumber As BoundExpression, verbKind As SyntaxKind)
      Me.KeyNumber = keyNumber
      Me.VerbKind = verbKind
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.KeyStatement
    Public ReadOnly Property KeyNumber As BoundExpression
    Public ReadOnly Property VerbKind As SyntaxKind

  End Class

End Namespace
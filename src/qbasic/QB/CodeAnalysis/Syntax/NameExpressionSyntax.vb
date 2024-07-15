Namespace Global.QB.CodeAnalysis.Syntax

  Public NotInheritable Class NameExpressionSyntax
    Inherits ExpressionSyntax

    Public Sub New(tree As SyntaxTree, identifier As IdentifierSyntax)
      MyBase.New(tree)
      Me.Identifier = identifier
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.NameExpression
    Public ReadOnly Property Identifier As IdentifierSyntax

  End Class

End Namespace
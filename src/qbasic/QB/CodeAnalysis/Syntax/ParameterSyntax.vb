Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class ParameterSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, identifier As IdentifierSyntax, asClause As AsClause)
      MyBase.New(tree)
      Me.Identifier = identifier
      Me.AsClause = asClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.Parameter
    Public ReadOnly Property Identifier As IdentifierSyntax
    Public ReadOnly Property AsClause As AsClause

  End Class

End Namespace
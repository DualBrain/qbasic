Namespace Global.QB.CodeAnalysis.Syntax

  Partial Public NotInheritable Class AsClause
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, asKeyword As SyntaxToken, identifier As SyntaxToken, Optional fixedLength As SyntaxToken = Nothing, Optional starToken As SyntaxToken = Nothing)
      MyBase.New(tree)
      Me.AsKeyword = asKeyword
      Me.Identifier = identifier
      Me.FixedLength = fixedLength
      Me.StarToken = starToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.AsClause
    Public ReadOnly Property AsKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property StarToken As SyntaxToken
    Public ReadOnly Property FixedLength As SyntaxToken

    Public ReadOnly Property HasFixedLength As Boolean
      Get
        Return FixedLength IsNot Nothing AndAlso StarToken IsNot Nothing
      End Get
    End Property

  End Class

End Namespace
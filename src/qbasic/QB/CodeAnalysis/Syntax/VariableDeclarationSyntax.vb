Namespace Global.QB.CodeAnalysis.Syntax

  Public NotInheritable Class ConstStatementSyntax
    Inherits StatementSyntax

    ' Const *variable*[*suffix*][ As *type*] = *literal*[, *variable*[*suffix*][ As *type*] = *literal*]...

    Public Sub New(tree As SyntaxTree,
                   constKeyword As SyntaxToken,
                   variables As List(Of SyntaxNode))
      MyBase.New(tree)
      Me.ConstKeyword = constKeyword
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ConstStatement
    Public ReadOnly Property ConstKeyword As SyntaxNode
    Public ReadOnly Property Variables As List(Of SyntaxNode)

  End Class

  Public NotInheritable Class DimStatementSyntax
    Inherits StatementSyntax

    ' Dim *variable*[*suffix*][ As *type*][ = *expression*][, *variable*[*suffix*][ As *type*][ = *expression*]]...
    ' Dim *variable*[*suffix*][(*subscript*)][ As *type*][, *variable*[*suffix*](*subscripts*)[ As *type*]]...

    Public Sub New(tree As SyntaxTree,
                   dimKeyword As SyntaxToken,
                   optionalSharedKeyword As SyntaxToken,
                   variables As List(Of SyntaxNode))
      MyBase.New(tree)
      Me.DimKeyword = dimKeyword
      Me.OptionalSharedKeyword = optionalSharedKeyword
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DimStatement
    Public ReadOnly Property DimKeyword As SyntaxToken
    Public ReadOnly Property OptionalSharedKeyword As SyntaxToken
    Public ReadOnly Property Variables As List(Of SyntaxNode)

  End Class

  Public NotInheritable Class VariableDeclarationSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   identifierToken As SyntaxToken,
                   dimensions As DimensionsClauseSyntax,
                   asClause As AsClause,
                   initClause As InitClauseSyntax)
      MyBase.New(tree)
      Me.IdentifierToken = identifierToken
      Me.Dimensions = dimensions
      Me.AsClause = asClause
      Me.InitClause = initClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.VariableDeclarationStatement
    Public ReadOnly Property KeywordToken As SyntaxToken
    Public ReadOnly Property IdentifierToken As SyntaxToken
    Public ReadOnly Property Dimensions As DimensionsClauseSyntax
    Public ReadOnly Property AsClause As AsClause
    Public ReadOnly Property InitClause As InitClauseSyntax

  End Class

End Namespace
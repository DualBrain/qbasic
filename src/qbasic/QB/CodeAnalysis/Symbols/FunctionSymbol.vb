Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Symbols

  Public NotInheritable Class FunctionSymbol
    Inherits Symbol

    Sub New(name As String, parameters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol)
      MyBase.New(name)
      Me.Parameters = parameters
      Me.Type = type
      Me.Declaration = Nothing
    End Sub

    Sub New(name As String, parameters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol, declaration As FunctionDeclarationSyntax)
      MyBase.New(name)
      Me.Parameters = parameters
      Me.Type = type
      Me.Declaration = declaration
    End Sub

    Sub New(name As String, parameters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol, declaration As DefDeclarationSyntax)
      MyBase.New(name)
      Me.Parameters = parameters
      Me.Type = type
      Me.Declaration = New FunctionDeclarationSyntax(declaration.SyntaxTree, declaration.DefKeyword, declaration.Identifier, declaration.OpenParenToken, declaration.Parameters, declaration.CloseParenToken, declaration.Parameters.First.AsClause, declaration.Statements, declaration.EndKeyword, declaration.EndDefKeyword)
    End Sub

    Sub New(name As String, parameters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol, declaration As SingleLineDefDeclarationSyntax)
      MyBase.New(name)
      Me.Parameters = parameters
      Me.Type = type
      Dim statement = New ExpressionStatementSyntax(declaration.SyntaxTree, New AssignmentExpressionSyntax(declaration.SyntaxTree, declaration.Identifier, Nothing, declaration.Expression))
      Dim statements = ImmutableArray.CreateBuilder(Of StatementSyntax)
      statements.Add(statement)
      Dim block = New BlockStatementSyntax(declaration.SyntaxTree, Nothing, statements.ToImmutable, Nothing)
      Me.Declaration = New FunctionDeclarationSyntax(declaration.SyntaxTree, declaration.DefKeyword, declaration.Identifier.Identifier, declaration.OpenParenToken, declaration.Parameters, declaration.CloseParenToken, declaration.Parameters.First.AsClause, block, Nothing, Nothing)
    End Sub

    Friend Sub New(name As String, parameters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol, declaration As SubStatementSyntax)
      MyBase.New(name)
      Me.Parameters = parameters
      Me.Type = type
      ' Convert SubStatementSyntax to FunctionDeclarationSyntax
      Dim asClause As AsClause = Nothing ' SUB returns nothing
      Me.Declaration = New FunctionDeclarationSyntax(declaration.SyntaxTree, declaration.SubKeyword, declaration.Identifier, declaration.OpenParenToken, declaration.Parameters, declaration.CloseParenToken, asClause, declaration.Statements, declaration.EndKeyword, declaration.EndSubKeyword)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Function
    Public ReadOnly Property Parameters As ImmutableArray(Of ParameterSymbol)
    Public ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Declaration As FunctionDeclarationSyntax

  End Class

End Namespace
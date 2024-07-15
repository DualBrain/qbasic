Namespace Global.QB.CodeAnalysis.Syntax

  Friend Class FunctionStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, functionKeyword As SyntaxToken, identifier As SyntaxToken, openParenToken As SyntaxToken, parameters As SeparatedSyntaxList(Of ParameterSyntax), closeParenToken As SyntaxToken, asClause As AsClause, staticKeyword As SyntaxToken, statements As BlockStatementSyntax, endKeyword As SyntaxToken, endFunctionKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.FunctionKeyword = functionKeyword
      Me.Identifier = identifier
      Me.OpenParenToken = openParenToken
      Me.Parameters = parameters
      Me.CloseParenToken = closeParenToken
      Me.AsClause = asClause
      Me.StaticKeyword = staticKeyword
      Me.Statements = statements
      Me.EndKeyword = endKeyword
      Me.EndFunctionKeyword = endFunctionKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.FunctionStatement
    Public ReadOnly Property FunctionKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Parameters As SeparatedSyntaxList(Of ParameterSyntax)
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property AsClause As AsClause
    Public ReadOnly Property StaticKeyword As SyntaxToken
    Public ReadOnly Property Statements As BlockStatementSyntax
    Public ReadOnly Property EndKeyword As SyntaxToken
    Public ReadOnly Property EndFunctionKeyword As SyntaxToken

  End Class

End Namespace
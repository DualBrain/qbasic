Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

   Friend NotInheritable Class BoundCallExpression
     Inherits BoundExpression

     Sub New([function] As FunctionSymbol, arguments As ImmutableArray(Of BoundExpression), Optional syntax As Syntax.ExpressionSyntax = Nothing)
       MyBase.New(syntax)
       Me.Function = [function]
       Me.Arguments = arguments
     End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CallExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
      Get
        Return [Function].Type
      End Get
    End Property
    Public ReadOnly Property [Function] As FunctionSymbol
    Public ReadOnly Property Arguments As ImmutableArray(Of BoundExpression)

  End Class

End Namespace
Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend Class BoundCommonStatement
    Inherits BoundStatement

    Public Sub New(declarations As ImmutableArray(Of BoundVariableDeclaration), isShared As Boolean)
      Me.Declarations = declarations
      Me.IsShared = isShared
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CommonStatement
    Public ReadOnly Property Declarations As ImmutableArray(Of BoundVariableDeclaration)
    Public ReadOnly Property IsShared As Boolean

  End Class

End Namespace
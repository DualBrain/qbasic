Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundRedimStatement
    Inherits BoundStatement

    Public Sub New(preserve As Boolean, declarations As ImmutableArray(Of BoundVariableDeclaration), isShared As Boolean)
      Me.Declarations = declarations
      Me.Preserve = preserve
      Me.IsShared = isShared
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.RedimStatement
    Public ReadOnly Property Preserve As Boolean
    Public ReadOnly Property Declarations As ImmutableArray(Of BoundVariableDeclaration)
    Public ReadOnly Property IsShared As Boolean

  End Class

End Namespace
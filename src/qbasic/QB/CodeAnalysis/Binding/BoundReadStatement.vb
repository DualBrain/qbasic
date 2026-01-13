Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundReadStatement
    Inherits BoundStatement

    Public Sub New(variables As ImmutableArray(Of VariableSymbol))
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ReadStatement
    Public ReadOnly Property Variables As ImmutableArray(Of VariableSymbol)

  End Class

End Namespace
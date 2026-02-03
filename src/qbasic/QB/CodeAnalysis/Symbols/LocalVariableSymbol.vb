Imports QB.CodeAnalysis.Binding

Namespace Global.QB.CodeAnalysis.Symbols

  Public Class LocalVariableSymbol
    Inherits VariableSymbol

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol, constant As BoundConstant, typeSource As VariableTypeSource)
      MyBase.New(name, isReadOnly, type, constant, typeSource)
    End Sub

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol, constant As BoundConstant)
      MyBase.New(name, isReadOnly, type, constant, VariableTypeSource.DefaultType)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.LocalVariable

  End Class

  Public Class LocalArraySymbol
    Inherits VariableSymbol

    Friend Sub New(name As String, type As TypeSymbol, lower As BoundExpression, upper As BoundExpression, isStaticArray As Boolean, dimensionCount As Integer, typeSource As VariableTypeSource)
      MyBase.New(name, True, type, lower, upper, isStaticArray, dimensionCount, typeSource)
    End Sub

    Friend Sub New(name As String, type As TypeSymbol, lower As BoundExpression, upper As BoundExpression, isStaticArray As Boolean, dimensionCount As Integer)
      MyBase.New(name, True, type, lower, upper, isStaticArray, dimensionCount, VariableTypeSource.DefaultType)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.LocalVariable

  End Class

End Namespace
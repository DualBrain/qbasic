Imports QB.CodeAnalysis.Binding

Namespace Global.QB.CodeAnalysis.Symbols

  Public Class VariableSymbol
    Inherits Symbol

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Variable

    Friend Sub New(name As String, isArray As Boolean, type As TypeSymbol, lower As BoundExpression, upper As BoundExpression, isStaticArray As Boolean, dimensionCount As Integer)
      MyBase.New(name)
      Me.IsReadOnly = False
      Me.IsArray = isArray
      Me.Type = type
      Me.Constant = Nothing
      Me.Lower = lower
      Me.Upper = upper
      Me.IsStaticArray = isStaticArray
      Me.DimensionCount = dimensionCount
    End Sub

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol, constant As BoundConstant)
      MyBase.New(name)
      Me.IsReadOnly = isReadOnly
      Me.Type = type
      Me.Constant = If(isReadOnly, constant, Nothing)
      Me.IsArray = False
      Me.Lower = Nothing
      Me.Upper = Nothing
      Me.DimensionCount = 0
    End Sub

    Public ReadOnly Property IsReadOnly As Boolean
    Public ReadOnly Property Type As TypeSymbol
    Friend ReadOnly Property Constant As BoundConstant
    Public ReadOnly Property IsArray As Boolean
    Friend ReadOnly Property Lower As BoundExpression
    Friend ReadOnly Property Upper As BoundExpression
    Friend ReadOnly Property IsStaticArray As Boolean
    Public ReadOnly Property DimensionCount As Integer

    Public Overrides Function ToString() As String
      If IsArray Then
        Return $"DIM {Name}({Lower} TO {Upper}): {Type}"
      Else
        Return $"{Name}: {Type}"
      End If
    End Function

  End Class

End Namespace
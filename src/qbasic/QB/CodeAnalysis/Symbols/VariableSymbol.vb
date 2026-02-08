Imports QB.CodeAnalysis.Binding

Namespace Global.QB.CodeAnalysis.Symbols

  Public Enum VariableTypeSource
    DefaultType
    DefStatement
    DimAsClause
    TypeCharacter
  End Enum

  Public Class VariableSymbol
    Inherits Symbol

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Variable

    Friend Sub New(name As String, isArray As Boolean, type As TypeSymbol, lower As BoundExpression, upper As BoundExpression, isStaticArray As Boolean, dimensionCount As Integer, typeSource As VariableTypeSource, isCommon As Boolean)
      MyBase.New(name)
      Me.IsReadOnly = False
      Me.IsArray = isArray
      Me.Type = type
      Me.Constant = Nothing
      Me.Lower = lower
      Me.Upper = upper
      Me.IsStaticArray = isStaticArray
      Me.DimensionCount = dimensionCount
      Me.TypeSource = typeSource
      Me.IsCommon = isCommon
    End Sub

    Friend Sub New(name As String, isArray As Boolean, type As TypeSymbol, lower As BoundExpression, upper As BoundExpression, isStaticArray As Boolean, dimensionCount As Integer)
      Me.New(name, isArray, type, lower, upper, isStaticArray, dimensionCount, VariableTypeSource.DefaultType, False)
    End Sub

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol, constant As BoundConstant, typeSource As VariableTypeSource, isCommon As Boolean)
      MyBase.New(name)
      Me.IsReadOnly = isReadOnly
      Me.Type = type
      Me.Constant = If(isReadOnly, constant, Nothing)
      Me.IsArray = False
      Me.Lower = Nothing
      Me.Upper = Nothing
      Me.DimensionCount = 0
      Me.TypeSource = typeSource
      Me.IsCommon = isCommon
    End Sub

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol, constant As BoundConstant)
      Me.New(name, isReadOnly, type, constant, VariableTypeSource.DefaultType, False)
    End Sub

    Public ReadOnly Property IsReadOnly As Boolean
    Public ReadOnly Property Type As TypeSymbol
    Friend ReadOnly Property Constant As BoundConstant
    Public ReadOnly Property IsArray As Boolean
    Friend ReadOnly Property Lower As BoundExpression
    Friend ReadOnly Property Upper As BoundExpression
    Friend ReadOnly Property IsStaticArray As Boolean
    Public ReadOnly Property DimensionCount As Integer
    Public ReadOnly Property TypeSource As VariableTypeSource
    Public ReadOnly Property IsCommon As Boolean

    Public Overrides Function ToString() As String
      Dim typeSourceStr = ""
      Select Case TypeSource
        Case VariableTypeSource.DefaultType : typeSourceStr = " [default]"
        Case VariableTypeSource.DefStatement : typeSourceStr = " [DEF]"
        Case VariableTypeSource.DimAsClause : typeSourceStr = " [DIM AS]"
        Case VariableTypeSource.TypeCharacter : typeSourceStr = " [suffix]"
      End Select

      If IsArray Then
        Return $"DIM {Name}({Lower} TO {Upper}): {Type}{typeSourceStr}"
      Else
        Return $"{Name}: {Type}{typeSourceStr}"
      End If
    End Function

  End Class

End Namespace
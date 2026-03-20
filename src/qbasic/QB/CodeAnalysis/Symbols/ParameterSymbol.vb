Namespace Global.QB.CodeAnalysis.Symbols

  Public NotInheritable Class ParameterSymbol
    Inherits LocalVariableSymbol

    Sub New(name As String, type As TypeSymbol, Optional isByRef As Boolean = False)
      MyBase.New(name, True, type, Nothing)
      Me.Ordinal = 0
      Me.IsByRef = isByRef
    End Sub

    Sub New(name As String, type As TypeSymbol, ordinal As Integer, Optional isByRef As Boolean = False)
      MyBase.New(name, True, type, Nothing)
      Me.Ordinal = ordinal
      Me.IsByRef = isByRef
    End Sub

    ' Constructor for array parameters
    Sub New(name As String, type As TypeSymbol, ordinal As Integer, isArray As Boolean, Optional isByRef As Boolean = False)
      MyBase.New(name, True, type, Nothing)
      Me.Ordinal = ordinal
      Me.IsByRef = isByRef
      Me.IsArrayParameter = isArray
      Me.DimensionCount = 1
    End Sub

    ' Constructor with type name for UDT parameters
    Sub New(name As String, type As TypeSymbol, ordinal As Integer, isArray As Boolean, isByRef As Boolean, typeName As String)
      MyBase.New(name, True, type, Nothing)
      Me.Ordinal = ordinal
      Me.IsByRef = isByRef
      Me.IsArrayParameter = isArray
      Me.DimensionCount = 1
      Me.m_typeName = typeName
    End Sub

    ' Constructor with type name for non-array UDT parameters
    Sub New(name As String, type As TypeSymbol, ordinal As Integer, isByRef As Boolean, typeName As String)
      MyBase.New(name, True, type, Nothing)
      Me.Ordinal = ordinal
      Me.IsByRef = isByRef
      Me.m_typeName = typeName
    End Sub

    Private ReadOnly m_typeName As String

    Public ReadOnly Property TypeName As String
      Get
        Return m_typeName
      End Get
    End Property

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Parameter
    Public ReadOnly Property Ordinal As Integer
    Public ReadOnly Property IsByRef As Boolean
    Public ReadOnly Property IsArrayParameter As Boolean = False

  End Class

End Namespace

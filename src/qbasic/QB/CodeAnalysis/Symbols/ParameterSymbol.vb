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

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Parameter
    Public ReadOnly Property Ordinal As Integer
    Public ReadOnly Property IsByRef As Boolean

  End Class

End Namespace
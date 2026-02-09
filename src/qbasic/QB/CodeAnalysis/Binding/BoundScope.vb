Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundScope

    Private m_symbols As Dictionary(Of String, Symbol)

    Public Sub New(parent As BoundScope)
      Me.Parent = parent
    End Sub

    Public ReadOnly Property Parent As BoundScope

    Public Function TryDeclareVariable(variable As VariableSymbol) As Boolean
      Return TryDeclareSymbol(variable)
    End Function

    Public Function TryRemoveVariable(name As String) As Boolean
      Return TryRemoveSymbol(name.ToLower)
    End Function

    Private Function TryRemoveSymbol(name As String) As Boolean
      If m_symbols IsNot Nothing AndAlso m_symbols.ContainsKey(name) Then
        m_symbols.Remove(name)
        Return True
      End If
      Return False
    End Function

    Public Function TryDeclareFunction(f As FunctionSymbol) As Boolean
      Return TryDeclareSymbol(f)
    End Function

    Private Function TryDeclareSymbol(Of TSymbol As Symbol)(symbol As TSymbol) As Boolean

      Dim name = symbol.Name
      Dim key = $"{name.ToLower}"
      If symbol.Kind = SymbolKind.Function Then
        Dim f = TryCast(symbol, FunctionSymbol)
        key &= $"[{If(f?.Parameters.Length, 0)}]"
      Else
        ' For variables, check if there's already a function with this name
        If m_symbols IsNot Nothing AndAlso m_symbols.Keys.Any(Function(k) k.StartsWith($"{name.ToLower}[")) Then
          Return False
        End If
        ' Also check parent scopes
        If Parent IsNot Nothing AndAlso Parent.TryLookupSymbol($"{name.ToLower}[") IsNot Nothing Then
          Return False
        End If
      End If
      If m_symbols Is Nothing Then
        m_symbols = New Dictionary(Of String, Symbol)
      End If
      If m_symbols.ContainsKey(key) Then
        Return False
      End If
      m_symbols(key) = symbol
      Return True
    End Function

    Public Function TryLookupVariable(name As String) As VariableSymbol
      ' First try exact match
      Dim symbol = TryLookupSymbol(name.ToLower)
      If TypeOf symbol Is VariableSymbol Then
        Return DirectCast(symbol, VariableSymbol)
      End If

      ' Check if this is a variable with type character
      If Not String.IsNullOrEmpty(name) Then
        Dim baseName = name.Substring(0, name.Length - 1)
        Dim baseSymbol = TryLookupSymbol(baseName.ToLower)
        If TypeOf baseSymbol Is VariableSymbol Then
          Return DirectCast(baseSymbol, VariableSymbol)
        End If
      End If

      ' Try array access (for variables like a(0))
      If name.EndsWith("]") Then
        Dim arrayBaseName = name.Substring(0, name.IndexOf("["))
        Dim arraySymbol = TryLookupSymbol(arrayBaseName.ToLower())
        If TypeOf arraySymbol Is VariableSymbol Then
          Return DirectCast(arraySymbol, VariableSymbol)
        End If
      End If

      Return Parent?.TryLookupVariable(name)
    End Function

    Public Function TryLookupFunction(name As String, parameters As List(Of TypeSymbol)) As Symbol
      ' First try exact match
      Dim key = $"{name.ToLower}[{If(parameters?.Count, 0)}]"
      Dim result = TryLookupSymbol(key)
      If result IsNot Nothing Then Return result

      ' If not found, look for any function with this name (for now, just return nothing to avoid bugs)
      ' TODO: Implement proper function overload resolution
      Return Parent?.TryLookupFunction(name, parameters)
    End Function

    Public Function TryLookupSymbol(name As String) As Symbol
      Dim symbol As Symbols.Symbol = Nothing
      'If Not "%&!#$".Contains(name.Last) Then name &= "!"c
      If m_symbols IsNot Nothing AndAlso m_symbols.TryGetValue(name.ToLower, symbol) Then
        Return symbol
      ElseIf m_symbols IsNot Nothing AndAlso name.EndsWith("[") Then
        Dim result = (From p In m_symbols Where p.Key.StartsWith(name.ToLower) Select p.Value).FirstOrDefault
        If result IsNot Nothing Then Return result
      End If
      Return Parent?.TryLookupSymbol(name)
    End Function

    Public Function GetDeclaredVariables() As ImmutableArray(Of VariableSymbol)
      Return GetDeclaredSymbols(Of VariableSymbol)()
    End Function

    Public ReadOnly Property GetDeclaredFunctions As ImmutableArray(Of FunctionSymbol)
      Get
        Return GetDeclaredSymbols(Of FunctionSymbol)()
      End Get
    End Property

    Private Function GetDeclaredSymbols(Of TSymbol As Symbol)() As ImmutableArray(Of TSymbol)
      If m_symbols Is Nothing Then Return ImmutableArray(Of TSymbol).Empty
      Return m_symbols.Values.OfType(Of TSymbol)().ToImmutableArray()
    End Function

  End Class

End Namespace
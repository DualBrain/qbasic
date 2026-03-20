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

    Public Function TryDeclareType(typeSymbol As UdtTypeSymbol) As Boolean
      Return TryDeclareSymbol(typeSymbol)
    End Function

    Public Function TryLookupType(name As String) As UdtTypeSymbol
      Dim symbol = TryLookupSymbol(name.ToLower)
      If symbol IsNot Nothing AndAlso TypeOf symbol Is UdtTypeSymbol Then
        Return DirectCast(symbol, UdtTypeSymbol)
      End If
      Return Parent?.TryLookupType(name)
    End Function

    Public Function GetDeclaredTypes() As ImmutableArray(Of UdtTypeSymbol)
      Dim result = New List(Of UdtTypeSymbol)()

      Dim currentTypes = GetDeclaredSymbols(Of UdtTypeSymbol)()
      result.AddRange(currentTypes)

      If Parent IsNot Nothing Then
        Dim parentTypes = Parent.GetDeclaredTypes()
        result.AddRange(parentTypes)
      End If

      Return result.ToImmutableArray()
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

      ' For functions, always allow overwrite (actual SUB/FUNCTION replaces DECLARE placeholder)
      If symbol.Kind = SymbolKind.Function Then
        m_symbols(key) = symbol
        Return True
      End If

      ' Check if there's already a symbol with this key
      If m_symbols.ContainsKey(key) Then
        ' If there's already a function here, don't overwrite
        ' The lookup logic will handle finding the right one based on parameter types
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
      If name.EndsWith("]"c) Then
        Dim arrayBaseName = name.Substring(0, name.IndexOf("["c))
        Dim arraySymbol = TryLookupSymbol(arrayBaseName.ToLower())
        If TypeOf arraySymbol Is VariableSymbol Then
          Return DirectCast(arraySymbol, VariableSymbol)
        End If
      End If

      ' If not found in current scope, try parent scope
      If Parent IsNot Nothing Then
        Return Parent.TryLookupVariable(name)
      End If
      Return Nothing
    End Function

    Public Function TryLookupFunction(name As String, parameters As List(Of TypeSymbol)) As Symbol
      ' First try exact match in current scope
      Dim key = $"{name.ToLower}[{If(parameters?.Count, 0)}]"
      Dim result = TryLookupSymbol(key)
      If result IsNot Nothing Then Return result

      ' If not found in current scope, try parent scope
      If Parent IsNot Nothing Then
        Return Parent.TryLookupFunction(name, parameters)
      End If
      Return Nothing
    End Function

    ' Lookup function by name and arity (argument count) - ignores parameter types
    Public Function TryLookupFunctionByNameAndArity(name As String, arity As Integer) As Symbol
      Dim key = $"{name.ToLower}[{arity}]"
      Dim result = TryLookupSymbol(key)
      If result IsNot Nothing Then Return result

      ' If not found in current scope, try parent scope
      If Parent IsNot Nothing Then
        Return Parent.TryLookupFunctionByNameAndArity(name, arity)
      End If
      Return Nothing
    End Function

    ' Lookup function by name and arity, preferring specific types over Any and functions with bodies
    Public Function TryLookupFunctionByNameAndArityPreferSpecific(name As String, arity As Integer) As Symbol
      ' First try to find a function with specific (non-Any) parameter types AND has a declaration (body)
      If m_symbols IsNot Nothing Then
        Dim matchingKeys = m_symbols.Keys.Where(Function(k) k.StartsWith($"{name.ToLower}[{arity}]")).ToList()
        For Each k In matchingKeys
          Dim func = TryCast(m_symbols(k), FunctionSymbol)
          If func IsNot Nothing AndAlso func.Parameters.Any(Function(p) p.Type IsNot TypeSymbol.Any) AndAlso func.Declaration IsNot Nothing Then
            Return func
          End If
        Next
      End If

      ' Fall back to Any parameters but prefer ones with declarations
      Dim result = TryLookupFunctionByNameAndArity(name, arity)
      If result IsNot Nothing Then
        Dim funcResult = TryCast(result, FunctionSymbol)
        ' If result has no declaration, try to find one that does in the dictionary
        If funcResult IsNot Nothing AndAlso funcResult.Declaration Is Nothing AndAlso m_symbols IsNot Nothing Then
          Dim matchingKeys = m_symbols.Keys.Where(Function(k) k.StartsWith($"{name.ToLower}[{arity}]")).ToList()
          For Each k In matchingKeys
            Dim func = TryCast(m_symbols(k), FunctionSymbol)
            If func IsNot Nothing AndAlso func.Declaration IsNot Nothing Then
              Return func
            End If
          Next
        End If
        Return result
      End If

      ' Try parent scope
      If Parent IsNot Nothing Then
        Return Parent.TryLookupFunctionByNameAndArityPreferSpecific(name, arity)
      End If
      Return Nothing
    End Function

    Public Function TryLookupSymbol(name As String) As Symbol
      Dim symbol As Symbols.Symbol = Nothing
      'Console.Error.WriteLine($"[LOOKUP] TryLookupSymbol('{name}')")
      'If Not "%&!#$".Contains(name.Last) Then name &= "!"c
      If m_symbols IsNot Nothing AndAlso m_symbols.TryGetValue(name.ToLower, symbol) Then
        'Console.Error.WriteLine($"[LOOKUP] Found '{name}' in current scope")
        Return symbol
      ElseIf m_symbols IsNot Nothing AndAlso name.EndsWith("["c) Then
        Dim result = (From p In m_symbols Where p.Key.StartsWith(name.ToLower) Select p.Value).FirstOrDefault
        If result IsNot Nothing Then Return result
      End If
      'Console.Error.WriteLine($"[LOOKUP] Not found '{name}' in current scope, checking parent")
      ' If current scope has no symbols or symbol not found, try parent scope
      If Parent IsNot Nothing Then
        Return Parent.TryLookupSymbol(name)
      End If
      'Console.Error.WriteLine($"[LOOKUP] Not found '{name}' at all")
      Return Nothing
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
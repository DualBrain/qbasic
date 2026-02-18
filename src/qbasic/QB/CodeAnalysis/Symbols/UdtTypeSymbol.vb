Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Symbols

  Friend NotInheritable Class UdtTypeSymbol
    Inherits Symbol

    Private ReadOnly m_fields As ImmutableArray(Of UdtFieldSymbol)
    Private ReadOnly m_size As Integer
    Private ReadOnly m_fieldOffsets As Dictionary(Of String, Integer)

    Public Sub New(name As String, fields As ImmutableArray(Of UdtFieldSymbol))
      MyBase.New(name)
      m_fields = fields
      m_fieldOffsets = New Dictionary(Of String, Integer)(StringComparer.OrdinalIgnoreCase)

      Dim currentOffset As Integer = 0
      For Each field In fields
        m_fieldOffsets(field.Name) = currentOffset
        Dim fieldSize As Integer = GetFieldSize(field)
        currentOffset += fieldSize
      Next
      m_size = currentOffset
    End Sub

    Private Function GetFieldSize(field As UdtFieldSymbol) As Integer
      If field.FixedLength > 0 Then
        Return field.FixedLength
      End If

      Dim fieldType = field.FieldType
      If fieldType Is TypeSymbol.String Then
        Return 256
      ElseIf fieldType Is TypeSymbol.Byte OrElse fieldType Is TypeSymbol.SByte Then
        Return 1
      ElseIf fieldType Is TypeSymbol.Integer OrElse fieldType Is TypeSymbol.UInteger Then
        Return 2
      ElseIf fieldType Is TypeSymbol.Long OrElse fieldType Is TypeSymbol.ULong Then
        Return 4
      ElseIf fieldType Is TypeSymbol.Single Then
        Return 4
      ElseIf fieldType Is TypeSymbol.Double Then
        Return 8
      ElseIf fieldType Is TypeSymbol.Decimal Then
        Return 16
      Else
        Return 4
      End If
    End Function

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Type

    Public ReadOnly Property Fields As ImmutableArray(Of UdtFieldSymbol)
      Get
        Return m_fields
      End Get
    End Property

    Public ReadOnly Property Size As Integer
      Get
        Return m_size
      End Get
    End Property

    Public ReadOnly Property FieldCount As Integer
      Get
        Return m_fields.Length
      End Get
    End Property

    Public Function GetFieldOffset(fieldName As String) As Integer
      Dim offset As Integer
      If m_fieldOffsets.TryGetValue(fieldName, offset) Then
        Return offset
      End If
      Return -1
    End Function

    Public Function GetField(fieldName As String) As UdtFieldSymbol
      For Each field In m_fields
        If StringComparer.OrdinalIgnoreCase.Equals(field.Name, fieldName) Then
          Return field
        End If
      Next
      Return Nothing
    End Function

    Public Function HasField(fieldName As String) As Boolean
      Return GetField(fieldName) IsNot Nothing
    End Function

  End Class

  Friend Class UdtFieldSymbol
    Inherits Symbol

    Private ReadOnly m_fieldType As TypeSymbol
    Private ReadOnly m_fixedLength As Integer
    Private ReadOnly m_udtTypeName As String
    Private ReadOnly m_nestedUdtType As UdtTypeSymbol

    Public Sub New(name As String, fieldType As TypeSymbol, fixedLength As Integer, Optional udtTypeName As String = Nothing, Optional nestedUdtType As UdtTypeSymbol = Nothing)
      MyBase.New(name)
      m_fieldType = fieldType
      m_fixedLength = fixedLength
      m_udtTypeName = If(udtTypeName, String.Empty)
      m_nestedUdtType = nestedUdtType
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Field

    Public ReadOnly Property FieldType As TypeSymbol
      Get
        Return m_fieldType
      End Get
    End Property

    Public ReadOnly Property FixedLength As Integer
      Get
        Return m_fixedLength
      End Get
    End Property

    Public ReadOnly Property UdtTypeName As String
      Get
        Return m_udtTypeName
      End Get
    End Property

    Public ReadOnly Property NestedUdtType As UdtTypeSymbol
      Get
        Return m_nestedUdtType
      End Get
    End Property

    Public ReadOnly Property IsFixedLengthString As Boolean
      Get
        Return m_fixedLength > 0
      End Get
    End Property

    Public ReadOnly Property IsString As Boolean
      Get
        Return m_fieldType Is TypeSymbol.String OrElse IsFixedLengthString
      End Get
    End Property

    Public ReadOnly Property IsNestedUdt As Boolean
      Get
        Return m_fieldType Is TypeSymbol.Udt AndAlso (Not String.IsNullOrEmpty(m_udtTypeName) OrElse m_nestedUdtType IsNot Nothing)
      End Get
    End Property

  End Class

End Namespace
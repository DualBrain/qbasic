Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundTypeStatement
    Inherits BoundStatement

    Public Sub New(typeName As String, fields As ImmutableArray(Of UdtField))
      MyBase.New()
      Me.TypeName = typeName
      Me.Fields = fields
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.TypeStatement

    Public ReadOnly Property TypeName As String
    Public ReadOnly Property Fields As ImmutableArray(Of UdtField)

  End Class

  Friend Structure UdtField
    Public ReadOnly Name As String
    Public ReadOnly FieldType As TypeSymbol
    Public ReadOnly FixedLength As Integer
    Public ReadOnly UdtTypeName As String
    Public ReadOnly NestedUdtType As Symbols.UdtTypeSymbol

    Public Sub New(name As String, fieldType As TypeSymbol, fixedLength As Integer, Optional udtTypeName As String = Nothing, Optional nestedUdtType As Symbols.UdtTypeSymbol = Nothing)
      Me.Name = name
      Me.FieldType = fieldType
      Me.FixedLength = fixedLength
      Me.UdtTypeName = If(udtTypeName, String.Empty)
      Me.NestedUdtType = nestedUdtType
    End Sub
  End Structure

End Namespace
Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundTypeStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As TypeStatementSyntax

    Public Sub New(typeName As String, fields As ImmutableArray(Of UdtField))
      MyBase.New()
      Me.TypeName = typeName
      Me.Fields = fields
    End Sub

    Public Sub New(syntax As TypeStatementSyntax, typeName As String, fields As ImmutableArray(Of UdtField))
      MyBase.New()
      m_syntax = syntax
      Me.TypeName = typeName
      Me.Fields = fields
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.TypeStatement

    Public ReadOnly Property TypeName As String
    Public ReadOnly Property Fields As ImmutableArray(Of UdtField)

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

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
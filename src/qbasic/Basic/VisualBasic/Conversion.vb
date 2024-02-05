Namespace Global.Basic.VisualBasic

  Friend Class Conversion

    Friend Shared Function Fix(Number As Decimal) As Decimal
      Return Math.Sign(Number) * Conversion.Int(System.Math.Abs(Number))
    End Function

    Friend Shared Function Fix(Number As Double) As Double
      Return Math.Sign(Number) * Conversion.Int(System.Math.Abs(Number))
    End Function

    Friend Shared Function Fix(Number As Integer) As Short
      Return CShort(Number)
    End Function

    Friend Shared Function Fix(Number As Long) As Long
      Return Number
    End Function

    Friend Shared Function Fix(Number As Object) As Object
      'FIXME:ArgumentException 5 Number is not a numeric type. 
      If Number Is Nothing Then
        Throw New ArgumentNullException(NameOf(Number), "Value can not be null.")
      End If

      If TypeOf Number Is Byte Then
        Return Conversion.Fix(Convert.ToByte(Number))
      ElseIf TypeOf Number Is Boolean Then
        If (Convert.ToBoolean(Number)) Then
          Return 1
        End If
        Return 0
      ElseIf TypeOf Number Is Long Then
        Return Conversion.Fix(Convert.ToInt64(Number))
      ElseIf TypeOf Number Is Decimal Then
        Return Conversion.Fix(Convert.ToDecimal(Number))
      ElseIf TypeOf Number Is Short Then
        Return Conversion.Fix(Convert.ToInt16(Number))
      ElseIf TypeOf Number Is Integer Then
        Return Conversion.Fix(Convert.ToInt32(Number))
      ElseIf TypeOf Number Is Double Then
        Return Conversion.Fix(Convert.ToDouble(Number))
      ElseIf TypeOf Number Is Single Then
        Return Conversion.Fix(Convert.ToSingle(Number))
      ElseIf TypeOf Number Is String Then
        Return Conversion.Fix(DoubleType.FromString(Number.ToString()))
      ElseIf TypeOf Number Is Char Then
        Return Conversion.Fix(DoubleType.FromString(Number.ToString()))
      Else 'Date, Object
        Throw New System.ArgumentException("Type of argument 'Number' is '" + Number.GetType.FullName + "', which is not numeric.")
      End If

    End Function

    Friend Shared Function Fix(Number As Short) As Short
      Return Number
    End Function

    Friend Shared Function Fix(Number As Single) As Single
      Return Math.Sign(Number) * Conversion.Int(System.Math.Abs(Number))
    End Function

    Friend Shared Function Int(Number As Decimal) As Decimal
      Return Decimal.Floor(Number)
    End Function

    Friend Shared Function Int(Number As Double) As Double
      Return Math.Floor(Number)
    End Function

    Friend Shared Function Int(Number As Integer) As Short
      Return CShort(Number)
    End Function

    Friend Shared Function Int(Number As Long) As Long
      Return Number
    End Function

    Friend Shared Function Int(Number As Object) As Object

      If Number Is Nothing Then
        Throw New ArgumentNullException(NameOf(Number), "Value can not be null.")
      End If

      If TypeOf Number Is Byte Then
        Return Conversion.Int(Convert.ToByte(Number))
      ElseIf TypeOf Number Is Boolean Then
        Return Conversion.Int(Convert.ToDouble(Number))
      ElseIf TypeOf Number Is Long Then
        Return Conversion.Int(Convert.ToInt64(Number))
      ElseIf TypeOf Number Is Decimal Then
        Return Conversion.Int(Convert.ToDecimal(Number))
      ElseIf TypeOf Number Is Short Then
        Return Conversion.Int(Convert.ToInt16(Number))
      ElseIf TypeOf Number Is Integer Then
        Return Conversion.Int(Convert.ToInt32(Number))
      ElseIf TypeOf Number Is Double Then
        Return Conversion.Int(Convert.ToDouble(Number))
      ElseIf TypeOf Number Is Single Then
        Return Conversion.Int(Convert.ToSingle(Number))
      ElseIf TypeOf Number Is String Then
        Return Conversion.Int(Convert.ToDouble(Number))
      ElseIf TypeOf Number Is Char Then
        Return Conversion.Int(Convert.ToInt16(Number))
      Else 'Date, Object
        Throw New System.ArgumentException("Type of argument 'Number' is '" + Number.GetType.FullName + "', which is not numeric.")
      End If

    End Function

    Friend Shared Function Int(Number As Short) As Short
      Return Number
    End Function

    Friend Shared Function Int(Number As Single) As Single
      Return System.Convert.ToSingle(Math.Floor(Number))
    End Function

    Friend Shared Function Hex(Number As Byte) As String
      Return Convert.ToString(Number, 16).ToUpper
    End Function

    Friend Shared Function Hex(Number As Integer) As String
      Return Convert.ToString(Number, 16).ToUpper
    End Function

    Friend Shared Function Hex(Number As Long) As String
      Return Convert.ToString(Number, 16).ToUpper
    End Function

    Friend Shared Function Hex(Number As Short) As String
      Return Convert.ToString(Number, 16).ToUpper
    End Function

    Friend Shared Function Hex(Number As Object) As String

      If Number Is Nothing Then
        Throw New System.ArgumentNullException(NameOf(Number), "Value cannot be null.")
      End If

      If (TypeOf Number Is IConvertible) Then
        Dim tc As TypeCode = CType(Number, IConvertible).GetTypeCode()

        Select Case tc
          Case TypeCode.Byte
            Return Hex(Convert.ToByte(Number))
          Case TypeCode.Decimal
            Return Hex(SizeDown(Convert.ToInt64(Number)))
          Case TypeCode.Double
            Return Hex(SizeDown(Convert.ToInt64(Number)))
          Case TypeCode.Int16
            Return Hex(Convert.ToInt16(Number))
          Case TypeCode.Int32
            Return Hex(Convert.ToInt32(Number))
          Case TypeCode.Int64
            Return Hex(Convert.ToInt64(Number))
          Case TypeCode.Single
            Return Hex(SizeDown(Convert.ToInt32(Number)))
          Case TypeCode.String
            Dim strNumber As String
            strNumber = Number.ToString
            If strNumber.StartsWith("&"c) Then
              If Char.ToUpper(strNumber.Chars(1)) = "O"c Then
                Return Hex(SizeDown(Convert.ToInt64(strNumber.Substring(2), 8)))
              ElseIf Char.ToUpper(strNumber.Chars(1)) = "H"c Then
                Return Hex(SizeDown(Convert.ToInt64(strNumber.Substring(2), 16)))
              Else
                Return Hex(SizeDown(Convert.ToInt64(Number)))
              End If
            Else
              Return Hex(SizeDown(Convert.ToInt64(Number)))
            End If
          Case TypeCode.SByte
            Return Hex(Convert.ToSByte(Number))
          Case TypeCode.UInt16
            Return Hex(Convert.ToUInt16(Number))
          Case TypeCode.UInt32
            Return Hex(Convert.ToUInt32(Number))
          Case TypeCode.UInt64
            Return Hex(Convert.ToUInt64(Number))
          Case Else
            Throw New System.ArgumentException("Argument 'Number' cannot be converted to type '" + Number.GetType.FullName + "'.")

        End Select
      Else
        Throw New System.ArgumentException("Argument 'Number' is not a number.")
      End If
    End Function

    Friend Shared Function Oct(Number As Byte) As String
      Return Convert.ToString(Number, 8).ToUpper
    End Function

    Friend Shared Function Oct(Number As Integer) As String
      Return Convert.ToString(Number, 8).ToUpper
    End Function

    Friend Shared Function Oct(Number As Long) As String
      Return Convert.ToString(Number, 8).ToUpper
    End Function

    Friend Shared Function Oct(Number As Object) As String

      If Number Is Nothing Then
        Throw New System.ArgumentNullException(NameOf(Number), "Value cannot be null.")
      End If

      If (TypeOf Number Is IConvertible) Then
        Dim tc As TypeCode = CType(Number, IConvertible).GetTypeCode()

        Select Case tc
          Case TypeCode.Byte
            Return Oct(Convert.ToByte(Number))
          Case TypeCode.Decimal
            Return Oct(SizeDown(Convert.ToInt64(Number)))
          Case TypeCode.Double
            Return Oct(SizeDown(Convert.ToInt64(Number)))
          Case TypeCode.Int16
            Return Oct(Convert.ToInt16(Number))
          Case TypeCode.Int32
            Return Oct(Convert.ToInt32(Number))
          Case TypeCode.Int64
            Return Oct(Convert.ToInt64(Number))
          Case TypeCode.Single
            Return Oct(SizeDown(Convert.ToInt32(Number)))
          Case TypeCode.String
            Dim strNumber As String
            strNumber = Number.ToString
            If strNumber.StartsWith("&"c) Then
              If Char.ToUpper(strNumber.Chars(1)) = "O"c Then
                Return Oct(SizeDown(Convert.ToInt64(strNumber.Substring(2), 8)))
              ElseIf Char.ToUpper(strNumber.Chars(1)) = "H"c Then
                Return Oct(SizeDown(Convert.ToInt64(strNumber.Substring(2), 16)))
              Else
                Return Oct(SizeDown(Convert.ToInt64(Number)))
              End If
            Else
              Return Oct(SizeDown(Convert.ToInt64(Number)))
            End If
          Case TypeCode.SByte
            Return Oct(Convert.ToSByte(Number))
          Case TypeCode.UInt16
            Return Oct(Convert.ToUInt16(Number))
          Case TypeCode.UInt32
            Return Oct(Convert.ToUInt32(Number))
          Case TypeCode.UInt64
            Return Oct(Convert.ToUInt64(Number))
          Case Else
            Throw New System.ArgumentException("Argument 'Number' cannot be converted to type '" + Number.GetType.FullName + "'.")

        End Select
      Else
        Throw New System.ArgumentException("Argument 'Number' is not a number.")
      End If
    End Function

    Friend Shared Function Oct(Number As Short) As String
      Return Convert.ToString(Number, 8).ToUpper
    End Function

    Private Shared Function SizeDown(num As Long) As Object

      'If (num <= Byte.MaxValue And num >= 0) Then
      '    Return CType(num, Byte)
      'End If

      'If (num <= SByte.MaxValue And num >= SByte.MinValue) Then
      '    Return CType(num, SByte)
      'End If

      'If (num <= Int16.MaxValue And num >= Int16.MinValue) Then
      '    Return CType(num, Int16)
      'End If

      'If (num <= UInt16.MaxValue And num >= 0) Then
      '    Return CType(num, UInt16)
      'End If

      If (num <= Integer.MaxValue And num >= Integer.MinValue) Then
        Return CType(num, Integer)
      ElseIf (num <= UInteger.MaxValue And num >= 0) Then
        Return CType(num, UInteger)
      End If

      Return num

    End Function

  End Class

End Namespace
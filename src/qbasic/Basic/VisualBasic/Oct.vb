Namespace Global.Basic.VisualBasic

  Friend Module Oct

    Friend Function Oct(number As Short) As String

      Dim result As String = ""

      While number > 0
        result = CStr(number Mod 8) & result
        number \= 8S
      End While

      Return result

    End Function

    Friend Function EncodeOctalString(value As Byte) As String

      'convert to int, for cleaner syntax below. 
      Dim x As Short = CShort(value)

      'return octal encoding \ddd of the character value. 
      Return String.Format("\{0}{1}{2}", ((x >> 6) And 7), ((x >> 3) And 7), (x And 7))

    End Function

    Friend Function DecodeOctalString(octalValue As String) As Byte

      Dim a As Short = Short.Parse(octalValue.Substring(1, 1))
      Dim b As Short = Short.Parse(octalValue.Substring(2, 1))
      Dim c As Short = Short.Parse(octalValue.Substring(3, 1))

      Return CByte((a << 6) Or (b << 3) Or (c))

    End Function

  End Module

End Namespace
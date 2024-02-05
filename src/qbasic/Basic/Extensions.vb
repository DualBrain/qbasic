Imports System.Runtime.CompilerServices

Namespace Global.Basic

  Friend Module Extensions

    Friend Enum CompareMethod As Short
      Binary = 0
      Text = 1
    End Enum

    <Extension()>
    Friend Function [Like](source As String, pattern As String) As Boolean
      Return source.Like(pattern, CompareMethod.Text)
    End Function

    <Extension()>
    Friend Function [Like](source As String, pattern As String, compareOption As CompareMethod) As Boolean

      If (source Is Nothing OrElse source.Length = 0) AndAlso (pattern Is Nothing OrElse pattern.Length = 0) Then
        Return True
        ' LAMESPEC : MSDN states "if either string or pattern is an empty string, the result is False."
        ' but "" Like "[]" returns True
      ElseIf ((source Is Nothing OrElse source.Length = 0) OrElse (pattern Is Nothing OrElse pattern.Length = 0)) AndAlso String.Compare(pattern, "[]", StringComparison.CurrentCulture) <> 0 Then
        Return False
      End If

      Dim options As Text.RegularExpressions.RegexOptions = Text.RegularExpressions.RegexOptions.Singleline
      If compareOption = CompareMethod.Text Then
        options = options Or
                Text.RegularExpressions.RegexOptions.CultureInvariant Or
                Text.RegularExpressions.RegexOptions.IgnoreCase
      End If

      Dim regexString As String = ConvertLikeExpression(pattern)
      Dim regexpr As New Text.RegularExpressions.Regex(regexString, options)

      Return regexpr.IsMatch(source)

    End Function

    Friend Function ConvertLikeExpression(expression As String) As String
      Dim carr() As Char = expression.ToCharArray()

      Dim sb As New Text.StringBuilder
      Dim bDigit As Boolean = False '' need it in order to clode the string pattern

      For pos As Short = 0 To CShort(carr.Length - 1)
        Select Case carr(pos)
          Case "?"c
            sb.Append("."c)
          Case "*"c
            sb.Append("."c).Append("*"c)
          Case "#"c  '' only one digit and only once ->  "^\d{1}$"
            If bDigit Then
              sb.Append("\"c).Append("d"c).Append("{"c).Append("1"c).Append("}"c)
            Else
              sb.Append("^"c).Append("\"c).Append("d"c).Append("{"c).Append("1"c).Append("}"c)
              bDigit = True
            End If
          Case "["c
            Dim gsb As Text.StringBuilder = ConvertGroupSubexpression(carr, pos)
            ' skip groups of form [], i.e. empty strings
            If gsb.Length > 2 Then
              sb.Append(gsb)
            End If
          Case Else
            sb.Append(carr(pos))
        End Select
      Next
      If bDigit Then sb.Append("$"c)

      Return sb.ToString()
    End Function

    Private Function ConvertGroupSubexpression(carr() As Char, ByRef pos As Short) As Text.StringBuilder
      Dim sb As New Text.StringBuilder
      Dim negate As Boolean = False

      While Not carr(pos) = "]"c
        If negate Then
          sb.Append("^"c)
          negate = False
        End If
        If carr(pos) = "!"c Then
          sb.Remove(1, sb.Length - 1)
          negate = True
        Else
          sb.Append(carr(pos))
        End If
        pos += 1S
      End While
      sb.Append("]"c)

      Return sb
    End Function

    Friend Function IsNumeric(value As String, float As Boolean) As Boolean

      Dim periodFound As Boolean

      value = value.Trim

      For index = 0 To value.Length - 1
        Dim c As Char = value(index)
        Select Case c
          Case "-"c
            If index = 0 Then
              ' OK
            Else
              Return False
            End If
          Case " "c
          Case "0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c
          Case "."c
            If float AndAlso Not periodFound Then
              periodFound = True
            Else
              Return False
            End If
          Case Else
            Return False
        End Select
      Next

      Return True

    End Function

#Region "MemoryStream"

    <Extension()>
    Friend Function ToByteArray(text As String) As Byte()
      Dim buffer(text.Length - 1) As Byte
      For index = 0 To text.Length - 1
        Try
          buffer(index) = CByte(AscW(text(index)))
        Catch ex As Exception
          buffer(index) = 32
        End Try
      Next
      Return buffer
    End Function

    <Extension()>
    Friend Function ToText(buffer As Byte()) As String
      Dim sb As New Text.StringBuilder
      For Each b As Byte In buffer
        sb.Append(ChrW(b))
      Next
      Return sb.ToString
    End Function

    <Extension()>
    Friend Function ReadChar(stream As System.IO.MemoryStream) As Char?
      If stream.Position < stream.Length Then
        ' Return character at current position.
        Return ChrW(stream.ReadByte())
      Else
        ' Out of range, return nothing.
        Return New Char?
      End If
    End Function

    <Extension()>
    Friend Function ReadString(stream As System.IO.MemoryStream, length As Integer) As String
      If length > 0 AndAlso stream.Position + length <= stream.Length Then
        ' Create buffer to hold the bytes to be read from the stream.
        Dim buffer(length - 1) As Byte
        ' Fill the buffer.
        stream.Read(buffer, 0, length)
        ' Convert the bytes to a string and return it.
        Return buffer.ToText
      Else
        ' Out of range, return nothing.
        Return Nothing
      End If
    End Function

    <Extension()>
    Friend Function PeekString(stream As System.IO.MemoryStream, offset As Integer, length As Integer) As String
      If (stream.Position + offset + length).Between(0, stream.Length) Then
        ' Remember current position.
        Dim pos = stream.Position
        If offset <> 0 Then stream.Seek(offset, System.IO.SeekOrigin.Current)
        ' Create buffer to hold the bytes to be read from the stream.
        Dim buffer(length - 1) As Byte
        ' Fill the buffer.
        stream.Read(buffer, 0, length)
        ' Convert the bytes to a string and return it.
        Dim result = buffer.ToText
        ' Set position back to original.
        stream.Position = pos
        ' Return result.
        Return result
      Else
        ' Out of range, return nothing.
        Return Nothing
      End If
    End Function

    <Extension()>
    Friend Function EndOfStream(stream As System.IO.MemoryStream, offset As Integer) As Boolean
      If stream.Position + offset < stream.Length Then
        Return False
      Else
        Return True
      End If
    End Function

    <Extension()>
    Friend Function PeekChar(stream As System.IO.MemoryStream) As Char?
      If stream.Position < stream.Length Then
        ' Remember current position.
        Dim pos = stream.Position
        ' Get character as current position.
        Dim ch = ChrW(stream.ReadByte())
        ' Set position back to original.
        stream.Position = pos
        ' Return result.
        Return ch
      Else
        ' Out of range, return nothing.
        Return New Char?
      End If
    End Function

    <Extension()>
    Friend Function PeekChar(stream As System.IO.MemoryStream, offset As Integer) As Char?
      If (stream.Position + offset).Between(0, stream.Length - 1) Then
        ' Remember current position.
        Dim pos = stream.Position
        ' Seek to offset position.
        stream.Seek(offset, System.IO.SeekOrigin.Current)
        ' Get character as current position.
        Dim ch = ChrW(stream.ReadByte())
        ' Set position back to original.
        stream.Position = pos
        ' Return result.
        Return ch
      Else
        ' Out of range, return nothing.
        Return New Char?
      End If
    End Function

    <Extension()>
    Friend Function PeekAt(stream As System.IO.MemoryStream, position As Integer) As Short
      If position.Between(0, CInt(stream.Length - 1)) Then
        Dim pos = stream.Position
        stream.Seek(position, System.IO.SeekOrigin.Begin)
        Dim result = stream.ReadByte()
        stream.Position = pos
        Return CShort(result)
      Else
        Return -1
      End If
    End Function

    <Extension()>
    Friend Function Peek(stream As System.IO.MemoryStream) As Short
      If stream.Position < stream.Length Then
        Dim pos = stream.Position
        Dim result = stream.ReadByte()
        stream.Position = pos
        Return CShort(result)
      Else
        Return -1
      End If
    End Function

    <Extension()>
    Friend Function Read(stream As System.IO.MemoryStream) As Short
      If stream.Position < stream.Length Then
        Return CShort(stream.ReadByte())
      Else
        Return -1
      End If
    End Function

#End Region

#Region "Between"

    <Extension()>
    Friend Function Between(Of T As IComparable)(listToQuery As IEnumerable(Of T),
                                               start As T,
                                               [end] As T,
                                               Optional inclusive As Boolean = True) As IEnumerable(Of T)

      If inclusive Then
        Return listToQuery.Where(Function(x) x.CompareTo(start) >= 0 AndAlso x.CompareTo([end]) <= 0)
      Else
        Return listToQuery.Where(Function(x) x.CompareTo(start) > 0 AndAlso x.CompareTo([end]) < 0)
      End If

    End Function

    <Extension()>
    Friend Function Between(value As Double, min As Double, max As Double) As Boolean
      Return value >= min AndAlso
           value <= max
    End Function

    <Extension()>
    Friend Function Between(value As Single, min As Single, max As Single) As Boolean
      Return value >= min AndAlso
           value <= max
    End Function

    <Extension()>
    Friend Function Between(value As Long, min As Long, max As Long) As Boolean
      Return value >= min AndAlso
           value <= max
    End Function

    <Extension()>
    Friend Function Between(value As Integer, min As Integer, max As Integer) As Boolean
      Return value >= min AndAlso
           value <= max
    End Function

    <Extension()>
    Friend Function Between(value As Short, min As Short, max As Short) As Boolean
      Return value >= min AndAlso
           value <= max
    End Function

    <Extension()>
    Friend Function Between(value As Char, min As Char, max As Char) As Boolean
      Return value >= min AndAlso
           value <= max
    End Function

    <Extension()>
    Friend Function Between(value As Date, min As Date, max As Date) As Boolean
      Return value >= min AndAlso
           value <= max
    End Function

#End Region

    <Extension()>
    Friend Function RemoveSemiColon(text As String) As String

      If text.EndsWith(";"c) Then
        Return text.Substring(0, text.Length - 1)
      Else
        Return text
      End If

    End Function

    <Extension()>
    Friend Function IsEmpty(text As String) As Boolean
      Return String.IsNullOrEmpty(text)
    End Function

    <Extension()>
    Friend Function IsObject(o As Object) As Boolean
      Return TypeOf o Is Object ' Kind of redundant...
    End Function

    <Extension()>
    Friend Function IsNull(o As Object) As Boolean
      Return o Is Nothing
    End Function

    Friend Function IsNumeric(expression As Object) As Boolean

      ' Define variable to collect out parameter of the TryParse method. If the conversion fails, the out parameter is zero.
      Dim number As Double

      ' The TryParse method converts a string in a specified style and culture-specific format to its double-precision floating point number equivalent.
      ' The TryParse method does not generate an exception if the conversion fails. If the conversion passes, True is returned. If it does not, False is returned.
      Return Double.TryParse(Convert.ToString(expression),
                            System.Globalization.NumberStyles.Any,
                            System.Globalization.NumberFormatInfo.InvariantInfo,
                            number)

    End Function

    <Extension()>
    Friend Function ToBasicSng(number As Double) As Double

      Dim value As String = number.ToString
      If value.Replace("-", "").Replace(".", "").Length < 8 Then
        ' Leave as is...
      Else
        ' Determine current decimal position.
        Dim position = value.IndexOf("."c)
        If value.StartsWith("-"c) Then position -= 1
        Dim shift = 10 ^ (7 - position)
        number = CInt(number * shift) / shift
      End If

      Return number

    End Function

    <Extension()>
    Friend Function ToBasicDbl(number As Double) As Double

      ' Stored as 8 or more digits (maximum of 17).

      ' Convert to a string...
      Dim value As String = number.ToString

      'HACK: to get same behavior as GW-BASIC, need to "confuse" things.
      Dim result As Double = CSng(value)

      ' Get the string, again... now that we've confused things...
      value = result.ToString

      ' Truncate appropriately...
      If value.IndexOf("-"c) > -1 Then
        If value.IndexOf("."c) > -1 AndAlso value.Length > 17 Then
          value = value.Substring(0, 17)
        ElseIf value.Length > 16 Then
          value = value.Substring(0, 16)
        End If
      Else
        If value.IndexOf("."c) > -1 AndAlso value.Length > 18 Then
          value = value.Substring(0, 18)
        ElseIf value.Length > 17 Then
          value = value.Substring(0, 17)
        End If
      End If

      ' Convert back to a double before returning.
      result = Double.Parse(value)

      Return result

    End Function

  End Module

End Namespace
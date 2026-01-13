' *******************************************************************
' *
' *  Copyright (C) 2004 Cory Smith
' *  Last Update: 9/12/2004
' * 
' *  THE SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS", WITHOUT WARRANTY
' *  OF ANY KIND, EXPRESS OR IMPLIED. IN NO EVENT SHALL THE AUTHOR BE
' *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY ARISING FROM,
' *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OF THIS
' *  SOFTWARE.
' *
' *******************************************************************
' Modified heavily for Portable Library. 8/26/2011

Namespace Global.Basic.Utils

  Friend NotInheritable Class CsvSplit

    Private Sub New()
    End Sub

    '<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes")> _
    'Public Shared Function CsvSplit(ByVal text As String) As String()

    '  ' This function is for use when parsing(splitting) a data string that
    '  ' has a comma delimiter (Standard .CSV format). The normal VB Split function does
    '  ' not take into consideration of a comma embedded within a Fields data string and
    '  ' will parse the information incorrectly.
    '  '
    '  ' This function takes into consideration the a data field may contain
    '  ' a comma and parses the data as entire string. The data string being defined
    '  ' as the data between the two Double-Quote marks. This function also
    '  ' prunes the leading and trailing doublequote marks
    '  '
    '  '  Notes : Does NOT Correct improperly formatted Numeric amounts that
    '  '        : contain a comma for the thousands placement, unless the number has
    '  '        : leading and trailing Double-Quote marks.
    '  '
    '  '  Errors: NONE
    '  '
    '  '  Call : X() = PSplit(datastring to split.)
    '  '
    '  '  Returns: Single-Dimension array, same result that you get from the SPLIT Function.

    '  Dim expressionLength As Integer
    '  Dim commaPosition As Integer
    '  Dim index As Integer
    '  Dim quotationBegin As Integer
    '  Dim quotationEnd As Integer

    '  Dim expression As String
    '  Dim commaMark As String
    '  Dim quotationMark As String
    '  Dim dataField As String
    '  Dim aryData() As String = Nothing

    '  expression = text.Trim
    '  quotationMark = ChrW(34)
    '  commaMark = ","
    '  expressionLength = expression.Length
    '  index = -1

    '  ' If the length of the data string is greater than zero

    '  If expressionLength > 0 Then

    '    ' Search for a sDelimiter in the datastring
    '    commaPosition = InStr(expression, commaMark)

    '    Do While commaPosition <> 0

    '      ' Do while there is a comma search for a quote-enclosure set.
    '      quotationBegin = InStr(expression, quotationMark)
    '      dataField = ""

    '      If quotationBegin <> 0 AndAlso
    '         quotationBegin < commaPosition Then

    '        ' if starts with a quote, then determine how far the data goes.
    '        If expression.Substring(0, 1) = ChrW(34) Then

    '          ' now find the ", pair.

    '          ' the only two reasons why you would quote is because:
    '          ' a. preserve spacing on either side of the data
    '          ' b. data contains comma(s).

    '          Dim s As Integer = 0
    '          Do
    '            Dim c As Integer = expression.IndexOf(","c, s)
    '            Dim q As Integer
    '            Dim again As Boolean = False
    '            If c = -1 Then
    '              q = expression.Length - 1
    '            Else
    '              q = expression.LastIndexOf(ChrW(34), c)
    '              ' verify the space between the two characters (if any) contains only white space.
    '              For i = q + 1 To c - 1
    '                If expression.Substring(i, 1) <> ChrW(32) Then
    '                  again = True
    '                  Exit For
    '                End If
    '              Next
    '            End If
    '            If Not again Then
    '              ' Just found the Matching quote mark 
    '              ' Data field ends at quotationEnd, not commaPosition
    '              quotationEnd = q + 1

    '              dataField = Left(expression, quotationEnd)

    '              If quotationEnd < expression.Length Then
    '                expression = Right(expression, expression.Length - (dataField.Length + 1))
    '              Else
    '                expression = ""
    '              End If

    '              dataField = Right(dataField, dataField.Length - 1)
    '              Try
    '                dataField = Left(dataField, dataField.Length - 1)
    '              Catch
    '                ' I got this error occuring by having a single quote between two commas.
    '                dataField = ""
    '              End Try

    '              index = index + 1

    '              Exit Do

    '            Else
    '              ' find the next comma.
    '              s = c + 1
    '            End If

    '          Loop

    '        Else
    '          ' ERROR.
    '        End If

    '      Else

    '        If quotationBegin <> 0 Then
    '          ' Quote mark is FOUND AFTER the commaPosition meaning the
    '          ' data to the commaPosition is ok to use as a full field.
    '          ' Data ends at the commaPosition.
    '          dataField = Left(expression, commaPosition - 1)
    '          expression = Right(expression, expression.Length - (dataField.Length + 1)).Trim
    '          index = index + 1
    '        Else
    '          ' There is NO quotation Found.
    '          dataField = Left(expression, commaPosition - 1)
    '          expression = Right(expression, expression.Length - commaPosition).Trim
    '          index = index + 1
    '        End If

    '      End If

    '      ReDim Preserve aryData(index)
    '      aryData(index) = dataField
    '      commaPosition = InStr(expression, commaMark)

    '    Loop

    '    index = index + 1

    '    ' The leftover field.
    '    ReDim Preserve aryData(index)
    '    If Left(expression, 1) = ChrW(34) Then
    '      expression = Right(expression, expression.Length - 1)
    '    End If
    '    If Right(expression, 1) = ChrW(34) Then
    '      expression = Left(expression, expression.Length - 1)
    '    End If
    '    aryData(index) = expression

    '  Else
    '  End If

    '  Return aryData

    'End Function

    Friend Shared Function CsvSplit(text As String) As String()

      If text.IndexOf(ChrW(34)) > -1 Then

        text = text.Trim

        ' Contains quoted value(s)...

        Dim result As New List(Of String)

        Dim value As String = ""
        Dim index As Integer = 0
        Dim inQuotation As Boolean = False
        Dim pending As Boolean = False
        Dim quoted As Boolean = False

        Do

          If Not inQuotation AndAlso text(index) = ","c Then
            If Not quoted Then
              result.Add(value.Trim)
            Else
              result.Add(value)
            End If
            value = ""
            pending = False
            quoted = False
          ElseIf Not inQuotation AndAlso text(index) = ChrW(34) Then
            ' Begin of quoted section...
            inQuotation = True
            quoted = True
          ElseIf inQuotation AndAlso text(index) = ChrW(34) Then
            inQuotation = False
          Else
            value &= text(index)
            pending = True
          End If

          index += 1
          If index > text.Length - 1 Then
            Exit Do
          End If

        Loop

        If pending Then
          If Not quoted Then
            result.Add(value.Trim)
            quoted = False
          Else
            result.Add(value)
          End If
        End If

        If quoted Then
        End If

        Return result.ToArray

      Else
        ' Simple split.
        Dim result = text.Split(","c)
        For index = 0 To result.Length - 1
          result(index) = result(index).Trim
        Next
        Return result
      End If

    End Function

    'Private Shared Function InStr(text As String, value As String) As Integer
    '  If text.IndexOf(value) > -1 Then
    '    Return text.IndexOf(value) + 1
    '  Else
    '    Return 0
    '  End If
    'End Function

    'Private Shared Function Left(text As String, length As Integer) As String
    '  If text.Length > length Then
    '    Return text.Substring(0, length)
    '  Else
    '    Return text
    '  End If
    'End Function

    'Private Shared Function Right(text As String, length As Integer) As String
    '  If text.Length = 0 Then
    '    Return ""
    '  ElseIf text.Length - length < 0 Then
    '    Return ""
    '  Else
    '    Return text.Substring(text.Length - length)
    '  End If
    'End Function

  End Class

End Namespace
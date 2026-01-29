' ============================================================================'
' Preprocessor: Convert GW-BASIC line numbers to LabelStatements
' Converts "100 PRINT" → "Label100: PRINT" before SyntaxTree parsing
' ============================================================================'

Imports System
Imports System.Text.RegularExpressions

Namespace Global.QB.CodeAnalysis.Syntax

  ''' <summary>
  ''' Pre-processes GW-BASIC source to convert line numbers to LabelStatements.
  ''' This allows the SyntaxTree to properly represent them as nodes.
  ''' </summary>
  Public Class GwBasicPreprocessor
    
    ''' <summary>
    ''' Converts line numbers like "100 PRINT" to "Label100: PRINT" format.
    ''' </summary>
    Public Shared Function PreprocessLineNumbers(sourceCode As String) As String
      If String.IsNullOrWhiteSpace(sourceCode) Then Return sourceCode
      
      Dim lines = sourceCode.Split({vbCrLf, vbLf}, StringSplitOptions.None)
      Dim result = New List(Of String)()
      
      For Each line In lines
        If StartsWithLineNumber(line) Then
          ' Extract line number and convert to LabelXXX: format
          Dim spacePos = line.IndexOf(" "c)
          If spacePos > 0 Then
            Dim lineNumber = line.Substring(0, spacePos)
            Dim code = line.Substring(spacePos).TrimStart()
            result.Add("Label" & lineNumber & ": " & code)
          Else
            result.Add(line)
          End If
        Else
          result.Add(line)
        End If
      Next
      
      Return String.Join(vbCrLf, result)
    End Function
    
    Private Shared Function StartsWithLineNumber(line As String) As Boolean
      If String.IsNullOrEmpty(line) Then Return False
      
      ' Check for digits at start
      Dim i = 0
      While i < line.Length AndAlso Char.IsDigit(line(i))
        i += 1
      End While
      
      Return i > 0 AndAlso i < line.Length AndAlso line(i) = " "c
    End Function
  End Class

End Namespace
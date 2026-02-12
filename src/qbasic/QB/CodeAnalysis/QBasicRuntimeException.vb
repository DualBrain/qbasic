Namespace Global.QB.CodeAnalysis

  ''' <summary>
  ''' Custom exception for QBasic runtime errors.
  ''' </summary>
  Public Class QBasicRuntimeException
    Inherits Exception

    Public ReadOnly Property ErrorCode As ErrorCode

    Public Sub New(errorCode As ErrorCode)
      MyBase.New($"Error {CInt(errorCode)}: {GetErrorMessage(errorCode)}")
      Me.ErrorCode = errorCode
    End Sub

    Public Sub New(errorCode As ErrorCode, message As String)
      MyBase.New(message)
      Me.ErrorCode = errorCode
    End Sub

    Private Shared Function GetErrorMessage(errorCode As ErrorCode) As String
      Select Case errorCode
        Case ErrorCode.None : Return "No error"
        Case ErrorCode.Syntax : Return "Syntax error"
        Case ErrorCode.Overflow : Return "Overflow"
        Case ErrorCode.OutOfMemory : Return "Out of memory"
        Case ErrorCode.DivisionByZero : Return "Division by zero"
        Case ErrorCode.TypeMismatch : Return "Type mismatch"
        Case ErrorCode.IllegalFunctionCall : Return "Illegal function call"
        Case ErrorCode.SubscriptOutOfRange : Return "Subscript out of range"
        Case ErrorCode.FileNotFound : Return "File not found"
        Case ErrorCode.BadFileMode : Return "Bad file mode"
        Case ErrorCode.BadFileName : Return "Bad file name"
        Case ErrorCode.BadFileNumber : Return "Bad file number"
        Case ErrorCode.TooManyFiles : Return "Too many files"
        Case ErrorCode.FileAlreadyOpen : Return "File already open"
        Case ErrorCode.InputPastEnd : Return "Input past end"
        Case ErrorCode.PermissionDenied : Return "Permission denied"
        Case ErrorCode.OutOfData : Return "Out of DATA"
        Case ErrorCode.UndefinedLineNumber : Return "Undefined line number"
        Case ErrorCode.ResumeWithoutError : Return "RESUME without error"
        Case ErrorCode.ReturnWithoutGosub : Return "RETURN without GOSUB"
        Case ErrorCode.AdvancedFeature : Return "Advanced feature"
        Case ErrorCode.Internal : Return "Internal error"
        Case Else : Return "Unknown error"
      End Select
    End Function

  End Class

End Namespace
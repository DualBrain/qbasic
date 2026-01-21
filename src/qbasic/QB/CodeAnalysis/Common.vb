Namespace Global.QB.CodeAnalysis

  Module Common

    Private Function GetErrorMessage(errorCode As Integer) As String
      Return GetErrorMessage(CType(errorCode, ErrorCode))
    End Function

    Friend Function GetErrorMessage(errorCode As ErrorCode) As String
      Select Case errorCode
        Case ErrorCode.None : Return "Undefined"
        Case ErrorCode.NextWithoutFor : Return "NEXT without FOR"
        Case ErrorCode.Syntax : Return "Syntax error"
        Case ErrorCode.ReturnWithoutGosub : Return "RETURN without GOSUB"
        Case ErrorCode.OutOfData : Return "Out of DATA"
        Case ErrorCode.IllegalFunctionCall : Return "Illegal function call"
        Case ErrorCode.Overflow : Return "Overflow"
        Case ErrorCode.OutOfMemory : Return "Out of memory"
        Case ErrorCode.UndefinedLineNumber : Return "Undefined line number"
        Case ErrorCode.SubscriptOutOfRange : Return "Subscript out of range"
        Case ErrorCode.DuplicateDefinition : Return "Duplicate Definition"
        Case ErrorCode.DivisionByZero : Return "Division by zero"
        Case ErrorCode.IllegalDirect : Return "Illegal direct"
        Case ErrorCode.TypeMismatch : Return "Type mismatch"
        Case ErrorCode.OutOfStringSpace : Return "Out of string space"
        Case ErrorCode.StringTooLong : Return "String too long"
        Case ErrorCode.StringFormulaTooComplex : Return "String formula too complex"
        Case ErrorCode.CanNotContinue : Return "Can't continue"
        Case ErrorCode.UndefinedUserFunction : Return "Undefined user function"
        Case ErrorCode.NoResume : Return "No RESUME"
        Case ErrorCode.ResumeWithoutError : Return "RESUME without error"
        Case ErrorCode.MissingOperand : Return "Missing operand"
        Case ErrorCode.LineBufferOverflow : Return "Line buffer overflow"
        Case ErrorCode.DeviceTimeout : Return "Device Timeout"
        Case ErrorCode.DeviceFault : Return "Device Fault"
        Case ErrorCode.ForWithoutNext : Return "FOR Without NEXT"
        Case ErrorCode.OutOfPaper : Return "Out of Paper"
        Case ErrorCode.WhileWithoutWend : Return "WHILE without WEND"
        Case ErrorCode.WendWithoutWHILE : Return "WEND without WHILE"
        Case ErrorCode.FieldOverflow : Return "FIELD overflow"
        Case ErrorCode.Internal : Return "Internal error"
        Case ErrorCode.BadFileNumber : Return "Bad file number"
        Case ErrorCode.FileNotFound : Return "File not found"
        Case ErrorCode.BadFileMode : Return "Bad file mode"
        Case ErrorCode.FileAlreadyOpen : Return "File already open"
        Case ErrorCode.DeviceIO : Return "Device I/O Error"
        Case ErrorCode.FileAlreadyExists : Return "File already exists"
        Case ErrorCode.DiskFull : Return "Disk full"
        Case ErrorCode.InputPastEnd : Return "Input past end"
        Case ErrorCode.BadRecordNumber : Return "Bad record number"
        Case ErrorCode.BadFilename : Return "Bad filename"
        Case ErrorCode.DirectStatementInFile : Return "Direct statement in file"
        Case ErrorCode.TooManyFiles : Return "Too many files"
        Case ErrorCode.DeviceUnavailable : Return "Device Unavailable"
        Case ErrorCode.CommunicationBufferOverflow : Return "Communication buffer overflow"
        Case ErrorCode.PermissionDenied : Return "Permission Denied"
        Case ErrorCode.DiskNotReady : Return "Disk not Ready"
        Case ErrorCode.DiskMedia : Return "Disk media error"
        Case ErrorCode.AdvancedFeature : Return "Advanced Feature"
        Case ErrorCode.RenameAcrossDisks : Return "Rename across disks"
        Case ErrorCode.PathFileAccess : Return "Path/File Access Error"
        Case ErrorCode.PathNotFound : Return "Path not found"
        Case Else : Return $"Unprintable error {errorCode}"
      End Select
    End Function


  End Module

End Namespace
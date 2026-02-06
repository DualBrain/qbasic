Namespace Global.QB.CodeAnalysis

  Public Enum ErrorCode
    None = 0
    NextWithoutFor = 1
    Syntax = 2
    ReturnWithoutGosub = 3
    OutOfData = 4
    IllegalFunctionCall = 5
    Overflow = 6
    OutOfMemory = 7
    UndefinedLineNumber = 8
    SubscriptOutOfRange = 9
    DuplicateDefinition = 10
    DivisionByZero = 11
    IllegalDirect = 12
    TypeMismatch = 13
    OutOfStringSpace = 14
    StringTooLong = 15
    StringFormulaTooComplex = 16
    CanNotContinue = 17
    UndefinedUserFunction = 18
    NoResume = 19
    ResumeWithoutError = 20
    Unprintable21 = 21
    MissingOperand = 22
    LineBufferOverflow = 23
    DeviceTimeout = 24
    DeviceFault = 25
    ForWithoutNext = 26
    OutOfPaper = 27
    Unprintable28 = 28
    WhileWithoutWend = 29
    WendWithoutWHILE = 30
    Unprintable31 = 31
    Unprintable32 = 32
    Unprintable33 = 33
    Unprintable34 = 34
    Unprintable35 = 35
    Unprintable36 = 36
    Unprintable37 = 37
    Unprintable38 = 38
    Unprintable39 = 39
    Unprintable40 = 40
    Unprintable41 = 41
    Unprintable42 = 42
    Unprintable43 = 43
    Unprintable44 = 44
    Unprintable45 = 45
    Unprintable46 = 46
    Unprintable47 = 47
    Unprintable48 = 48
    Unprintable49 = 49
    FieldOverflow = 50
    Internal = 51
    BadFileNumber = 52
    FileNotFound = 53
    BadFileMode = 54
    FileAlreadyOpen = 55
    Unprintable56 = 56
    DeviceIO = 57
    FileAlreadyExists = 58
    Unprintable59 = 59
    Unprintable60 = 60
    DiskFull = 61
    InputPastEnd = 62
    BadRecordNumber = 63
    BadFilename = 64
    Unprintable65 = 65
    DirectStatementInFile = 66
    TooManyFiles = 67
    DeviceUnavailable = 68
    CommunicationBufferOverflow = 69
    PermissionDenied = 70
    DiskNotReady = 71
    DiskMedia = 72
    AdvancedFeature = 73
    RenameAcrossDisks = 74
    PathFileAccess = 75
    PathNotFound = 76
  End Enum

  Friend Class QBasicBuildException
    Inherits Exception

    Public ReadOnly Property ErrorCode As ErrorCode

    Public Sub New(errorCode As ErrorCode)
      MyBase.New($"Error {CInt(errorCode)}: {GetErrorMessage(errorCode)}")
      Me.ErrorCode = errorCode
    End Sub

  End Class

End Namespace
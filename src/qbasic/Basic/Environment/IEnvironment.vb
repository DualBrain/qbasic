Namespace Global.Basic.Environment

  Public Interface IEnvironment

    ReadOnly Property FreeMemory As Long
    ReadOnly Property IsDebugBuild As Boolean

    'Property Environ As String

    Function Shell(command As String, environ As Dictionary(Of String, String)) As Boolean

  End Interface

End Namespace
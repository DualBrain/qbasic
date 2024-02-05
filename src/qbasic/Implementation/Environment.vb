'Imports SSDD.Basic.Environment

'Public Class Environment
'  Implements IEnvironment

'  Public ReadOnly Property FreeMemory As Long Implements IEnvironment.FreeMemory
'    Get
'      Return GC.GetTotalMemory(True)
'    End Get
'  End Property

'  Public Function Shell(command As String, environ As Dictionary(Of String, String)) As Boolean Implements IEnvironment.Shell
'    Throw New NotImplementedException
'  End Function

'  Public ReadOnly Property IsDebugBuild As Boolean Implements IEnvironment.IsDebugBuild
'    Get
'      'Dim assm As Reflection.Assembly = Reflection.Assembly.LoadFrom(Application.ExecutablePath)
'      'Dim found As Boolean = assm.GetCustomAttributes(GetType(DebuggableAttribute), False).Length > 0
'      'Return found
'      ''Me.Text = "Assembly is " & IIf(found, "debug", "release")
'      Return False
'    End Get
'  End Property

'  'Public Property Environ As String Implements IEnvironment.Environ
'  '  Get
'  '    Throw New NotImplementedException
'  '  End Get
'  '  Set(value As String)
'  '    Throw New NotImplementedException
'  '  End Set
'  'End Property

'End Class

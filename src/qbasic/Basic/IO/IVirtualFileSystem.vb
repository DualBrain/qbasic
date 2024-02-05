Namespace Global.Basic.IO

  Public Interface IVirtualFileSystem

    Function ChDir(pathname As String) As String
    Function Files(pathname As String) As String
    Function Kill(filename As String) As String
    Function Load(filename As String) As String
    Function MkDir(pathname As String) As String
    Function Name(oldname As String, newname As String) As String
    Function RmDir(pathname As String) As String
    Function Save(filename As String, content As String) As String

    Function Open(filename As String, mode As Integer, lock As Integer) As String
    Function Read(filename As String, position As Integer, length As Integer) As String
    Function Input(filename As String, position As Integer, count As Integer, lineInput As Boolean) As String
    Function Write(filename As String, text As String, position As Integer, random As Boolean) As String
    Function Lock(filename As String, min As Integer?, max As Integer?) As String
    Function Unlock(filename As String, min As Integer?, max As Integer?) As String
    Function Close(filenames As List(Of String)) As String

    ReadOnly Property InitialPath As String
    ReadOnly Property IsEightDotThree As Boolean

  End Interface

End Namespace
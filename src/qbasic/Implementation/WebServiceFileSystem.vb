'Imports Basic.Interpreter

'Public Class WebServiceFileSystem
'  Implements IVirtualFileSystemAsync

'  Public Event ChDirCompleted(sender As Object, e As VirtualFileSystemEventArgsAsync) Implements IVirtualFileSystemAsync.ChDirCompleted
'  Public Event FilesCompleted(sender As Object, e As VirtualFileSystemEventArgsAsync) Implements IVirtualFileSystemAsync.FilesCompleted
'  Public Event KillCompleted(sender As Object, e As VirtualFileSystemEventArgsAsync) Implements IVirtualFileSystemAsync.KillCompleted
'  Public Event LoadCompleted(sender As Object, e As VirtualFileSystemEventArgsAsync) Implements IVirtualFileSystemAsync.LoadCompleted
'  Public Event MkDirCompleted(sender As Object, e As VirtualFileSystemEventArgsAsync) Implements IVirtualFileSystemAsync.MkDirCompleted
'  Public Event NameCompleted(sender As Object, e As VirtualFileSystemEventArgsAsync) Implements IVirtualFileSystemAsync.NameCompleted
'  Public Event RmDirCompleted(sender As Object, e As VirtualFileSystemEventArgsAsync) Implements IVirtualFileSystemAsync.RmDirCompleted
'  Public Event SaveCompleted(sender As Object, e As VirtualFileSystemEventArgsAsync) Implements IVirtualFileSystemAsync.SaveCompleted

'  Public Event OpenCompleted(sender As Object, e As Basic.Interpreter.VirtualFileSystemEventArgsAsync) Implements Basic.Interpreter.IVirtualFileSystemAsync.OpenCompleted
'  Public Event CloseCompleted(sender As Object, e As Basic.Interpreter.VirtualFileSystemEventArgsAsync) Implements Basic.Interpreter.IVirtualFileSystemAsync.CloseCompleted
'  Public Event ReadCompleted(sender As Object, e As Basic.Interpreter.VirtualFileSystemEventArgsAsync) Implements Basic.Interpreter.IVirtualFileSystemAsync.ReadCompleted
'  Public Event InputCompleted(sender As Object, e As Basic.Interpreter.VirtualFileSystemEventArgsAsync) Implements Basic.Interpreter.IVirtualFileSystemAsync.InputCompleted
'  Public Event WriteCompleted(sender As Object, e As Basic.Interpreter.VirtualFileSystemEventArgsAsync) Implements Basic.Interpreter.IVirtualFileSystemAsync.WriteCompleted
'  Public Event LockCompleted(sender As Object, e As Basic.Interpreter.VirtualFileSystemEventArgsAsync) Implements Basic.Interpreter.IVirtualFileSystemAsync.LockCompleted
'  Public Event UnlockCompleted(sender As Object, e As Basic.Interpreter.VirtualFileSystemEventArgsAsync) Implements Basic.Interpreter.IVirtualFileSystemAsync.UnlockCompleted

'  Public Function ChDirAsync(userToken As System.Guid, pathname As String, userState As Object) As Boolean Implements IVirtualFileSystemAsync.ChDirAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.ChDirCompleted, AddressOf ExecuteChDirCompleted
'    ws.ChDirAsync(userToken, pathname, userState)

'    Return True

'  End Function

'  Private Sub ExecuteChDirCompleted(sender As Object, e As NoNameBasicServiceReference.ChDirCompletedEventArgs)
'    RaiseEvent ChDirCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function FilesAsync(userToken As System.Guid, pathname As String, userState As Object) As Boolean Implements IVirtualFileSystemAsync.FilesAsync
'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.FilesCompleted, AddressOf ExecuteFilesCompleted
'    Try
'      ws.FilesAsync(userToken, pathname, userState)
'    Catch ex As Exception
'      Stop
'    End Try

'    Return True

'  End Function

'  Private Sub ExecuteFilesCompleted(sender As Object, e As NoNameBasicServiceReference.FilesCompletedEventArgs)
'    RaiseEvent FilesCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function KillAsync(userToken As System.Guid, filename As String, userState As Object) As Boolean Implements IVirtualFileSystemAsync.KillAsync
'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.KillCompleted, AddressOf ExecuteKillCompleted
'    ws.KillAsync(userToken, filename, userState)

'    Return True

'  End Function

'  Private Sub ExecuteKillCompleted(sender As Object, e As NoNameBasicServiceReference.KillCompletedEventArgs)
'    RaiseEvent KillCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function LoadAsync(userToken As System.Guid, filename As String, userState As Object) As Boolean Implements IVirtualFileSystemAsync.LoadAsync
'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.LoadCompleted, AddressOf ExecuteLoadCompleted
'    ws.LoadAsync(userToken, filename, userState)

'    Return True

'  End Function

'  Private Sub ExecuteLoadCompleted(sender As Object, e As NoNameBasicServiceReference.LoadCompletedEventArgs)
'    RaiseEvent LoadCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function MkDirAsync(userToken As System.Guid, pathname As String, userState As Object) As Boolean Implements IVirtualFileSystemAsync.MkDirAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.MkDirCompleted, AddressOf ExecuteMkDirCompleted
'    ws.MkDirAsync(userToken, pathname, userState)

'    Return True

'  End Function

'  Private Sub ExecuteMkDirCompleted(sender As Object, e As NoNameBasicServiceReference.MkDirCompletedEventArgs)
'    RaiseEvent MkDirCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function NameAsync(userToken As System.Guid, oldname As String, newname As String, userState As Object) As Boolean Implements IVirtualFileSystemAsync.NameAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.NameCompleted, AddressOf ExecuteNameCompleted
'    ws.NameAsync(userToken, oldname, newname, userState)

'    Return True

'  End Function

'  Private Sub ExecuteNameCompleted(sender As Object, e As NoNameBasicServiceReference.NameCompletedEventArgs)
'    RaiseEvent NameCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function RmDirAsync(userToken As System.Guid, pathname As String, userState As Object) As Boolean Implements IVirtualFileSystemAsync.RmDirAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.RmDirCompleted, AddressOf ExecuteRmDirCompleted
'    ws.RmDirAsync(userToken, pathname, userState)

'    Return True

'  End Function

'  Private Sub ExecuteRmDirCompleted(sender As Object, e As NoNameBasicServiceReference.RmDirCompletedEventArgs)
'    RaiseEvent RmDirCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function SaveAsync(userToken As System.Guid, filename As String, content As String, userState As Object) As Boolean Implements IVirtualFileSystemAsync.SaveAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.SaveCompleted, AddressOf ExecuteSaveCompleted
'    ws.SaveAsync(userToken, filename, content, userState)

'    Return True

'  End Function

'  Private Sub ExecuteSaveCompleted(sender As Object, e As NoNameBasicServiceReference.SaveCompletedEventArgs)
'    RaiseEvent SaveCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function OpenAsync(userToken As System.Guid, ByVal filename As String, ByVal mode As Integer, ByVal lock As Integer, ByVal userState As Object) As Boolean Implements Basic.Interpreter.IVirtualFileSystemAsync.OpenAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.OpenCompleted, AddressOf ExecuteOpenCompleted
'    ws.OpenAsync(userToken, filename, mode, lock, userState)

'    Return True

'  End Function

'  Private Sub ExecuteOpenCompleted(sender As Object, e As NoNameBasicServiceReference.OpenCompletedEventArgs)
'    RaiseEvent OpenCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function CloseAsync(userToken As System.Guid, filenames As List(Of String), userState As Object) As Boolean Implements Basic.Interpreter.IVirtualFileSystemAsync.CloseAsync

'    Dim a As New NoNameBasicServiceReference.ArrayOfString

'    For Each filename In filenames
'      a.Add(filename)
'    Next

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.CloseCompleted, AddressOf ExecuteCloseCompleted
'    ws.CloseAsync(userToken, a, userState)

'    Return True

'  End Function

'  Private Sub ExecuteCloseCompleted(sender As Object, e As NoNameBasicServiceReference.CloseCompletedEventArgs)
'    RaiseEvent CloseCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function ReadAsync(userToken As System.Guid, filename As String, position As Integer, length As Integer, ByVal userState As Object) As Boolean Implements Basic.Interpreter.IVirtualFileSystemAsync.ReadAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.ReadCompleted, AddressOf ExecuteReadCompleted
'    ws.ReadAsync(userToken, filename, position, length, userState)

'    Return True

'  End Function

'  Private Sub ExecuteReadCompleted(sender As Object, e As NoNameBasicServiceReference.ReadCompletedEventArgs)
'    RaiseEvent ReadCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function InputAsync(userToken As System.Guid, filename As String, position As Integer, count As Integer, ByVal userState As Object) As Boolean Implements Basic.Interpreter.IVirtualFileSystemAsync.InputAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.InputCompleted, AddressOf ExecuteInputCompleted
'    ws.InputAsync(userToken, filename, position, count, userState)

'    Return True

'  End Function

'  Private Sub ExecuteInputCompleted(sender As Object, e As NoNameBasicServiceReference.InputCompletedEventArgs)
'    RaiseEvent InputCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function WriteAsync(userToken As System.Guid, filename As String, text As String, position As Integer, ByVal userState As Object) As Boolean Implements Basic.Interpreter.IVirtualFileSystemAsync.WriteAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.WriteCompleted, AddressOf ExecuteWriteCompleted
'    ws.WriteAsync(userToken, filename, text, position, userState)

'    Return True

'  End Function

'  Private Sub ExecuteWriteCompleted(sender As Object, e As NoNameBasicServiceReference.WriteCompletedEventArgs)
'    RaiseEvent WriteCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function LockAsync(usertoken As System.Guid, filename As String, min As Integer?, max As Integer?, ByVal userState As Object) As Boolean Implements Basic.Interpreter.IVirtualFileSystemAsync.LockAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.LockCompleted, AddressOf ExecuteLockCompleted
'    ws.LockAsync(usertoken, filename, min, max, userState)

'    Return True

'  End Function

'  Private Sub ExecuteLockCompleted(sender As Object, e As NoNameBasicServiceReference.LockCompletedEventArgs)
'    RaiseEvent LockCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'  Public Function UnlockAsync(usertoken As System.Guid, filename As String, min As Integer?, max As Integer?, ByVal userState As Object) As Boolean Implements Basic.Interpreter.IVirtualFileSystemAsync.UnlockAsync

'    Dim ws As New NoNameBasicServiceReference.NoNameBasicSoapClient()
'    AddHandler ws.UnlockCompleted, AddressOf ExecuteUnlockCompleted
'    ws.UnlockAsync(usertoken, filename, min, max, userState)

'    Return True

'  End Function

'  Private Sub ExecuteUnlockCompleted(sender As Object, e As NoNameBasicServiceReference.UnlockCompletedEventArgs)
'    RaiseEvent UnlockCompleted(sender, New VirtualFileSystemEventArgsAsync(e.UserState, e.Result))
'  End Sub

'End Class
Imports Basic.IO

Public Class WindowsFileSystem
  Implements IVirtualFileSystem

  Private Class FileHandle
    Public Property Position As Long
    Public Property Mode As Integer
    Public Property Lock As Integer
  End Class

  Public ReadOnly Property InitialPath As String Implements IVirtualFileSystem.InitialPath
    Get
      Return "C:\"
    End Get
  End Property

  ' Implementation notes: 
  ' * If a file is "open", it is included in the dictionary.
  ' * If a file is "closed", it is not included in the dictionary.
  Private ReadOnly m_fileHandleList As New Dictionary(Of String, FileHandle)

  Private ReadOnly m_basePath As String = IO.Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "gw-basic")

  Public Sub New()
    If Not IO.Directory.Exists(m_basePath) Then
      IO.Directory.CreateDirectory(m_basePath)
    End If
  End Sub

  Private Function ToRealPath(ByVal path As String) As String

    If path.StartsWith("C:\") Then
      path = path.Substring(2)
    Else
      Return Nothing
    End If

    If path = "\" Then ' Root
      path = m_basePath
    Else
      If path.StartsWith("\") Then
        path = path.Substring(1)
      End If
      If path.EndsWith("\") Then
        path = path.Substring(0, path.Length - 1)
      End If
      path = IO.Path.Combine(m_basePath, path)
    End If

    Return path

  End Function

  Public Function ChDir(pathname As String) As String Implements IVirtualFileSystem.ChDir

    Dim path As String = EnforceActualPath(pathname)

    If path Is Nothing Then
      Return "Error: Bad file name"
    ElseIf path.Length > 63 Then
      Return "Error: Path is longer than 63 characters."
    End If

    path = ToRealPath(path)

    If path Is Nothing Then
      Return "Error: Device Unavailable"
    End If

    If IO.Directory.Exists(path) Then
      Return pathname
    ElseIf IO.Directory.Exists(path & ".package") Then
      Return pathname.Substring(0, pathname.Length - 1) & ".package\"
    Else
      Return "Error: Path not found"
    End If

  End Function

  Public Function Close(filenames As System.Collections.Generic.List(Of String)) As String Implements IVirtualFileSystem.Close

    For index As Integer = 0 To filenames.Count - 1

      Dim filename = EnforceActualPath(filenames(index))

      If Not filename.StartsWith("C:\") Then
        Return Nothing '"Error: 68" ' Device Unavailable
      End If

      If filename Is Nothing Then
        Return Nothing '"Error: 64" ' Bad filename
      ElseIf filename.Length > 63 Then
        Return Nothing ' "Error: 64" ' Bad filename
      End If

      ' Remove entry from the file state.
      For file As Integer = m_fileHandleList.Count - 1 To 0 Step -1
        m_fileHandleList.Remove(filename)
      Next

    Next

    Return Nothing

  End Function

  Public Function Files(pathname As String) As String Implements IVirtualFileSystem.Files

    pathname = EnforceActualPath(pathname)

    Dim driveSize As Long, bytesUsed As Long
    Dim result As String = pathname & vbCrLf

    Try

      Dim path = ToRealPath(pathname)

      If path Is Nothing Then
        Return "Error: 68" ' Device Unavailable
      End If

      'Dim fileSystem = System.Runtime.InteropServices.Automation.AutomationFactory.CreateObject("Scripting.FileSystemObject"))
      'Dim drives = fileSystem.Drives

      'For Each drive In drives
      '  Dim volumeName = drive.VolumeName
      '  Dim driveLetter = drive.DriveLetter
      '  Dim totalSpace = drive.TotalSize
      '  Dim freeSpace = drive.FreeSpace
      'Next

      driveSize = 1000000
      bytesUsed = 0

      Dim di As New IO.DirectoryInfo(m_basePath)

      For Each folder In IO.Directory.EnumerateDirectories(path)
        folder = folder.Substring(folder.LastIndexOf("\") + 1)
        Dim ok As Boolean = True
        If folder = "Shared" Then ' Make a "hidden" folder on Windows Phone by filtering it out of the result set.
          ok = False
        ElseIf folder.IndexOf(".") > -1 Then
          Dim parts() = folder.Split("."c)
          If parts.Count <> 2 Then
            ok = False
          Else
            If parts(0).Length > 8 OrElse
               parts(1).Length > 3 Then
              ok = False
            End If
          End If
        Else
          If folder.Length > 8 Then
            ok = False
          End If
        End If
        If ok Then
          result &= String.Format("{0}|{1}{2}", folder, 1, vbCrLf)
        End If
      Next
      For Each file In IO.Directory.EnumerateFiles(path)
        Dim ok As Boolean = True
        file = file.Substring(file.LastIndexOf("\") + 1)
        If file.IndexOf(".") > -1 Then
          Dim parts() = file.Split("."c)
          If parts.Count <> 2 Then
            ok = False
          Else
            If parts(0).Length > 8 OrElse
               parts(1).Length > 3 Then
              ok = False
            End If
          End If
        Else
          If file.Length > 8 Then
            ok = False
          End If
        End If
        If ok Then
          result &= String.Format("{0}|{1}{2}", file, 0, vbCrLf)
        End If
      Next

    Catch ex As Exception
      Return ex.Message
    End Try

    result &= driveSize - bytesUsed & " BYTES FREE"

    Return result

  End Function

  Public Function Input(filename As String, position As Integer, count As Integer, lineInput As Boolean) As String Implements IVirtualFileSystem.Input

    filename = EnforceActualPath(filename)

    If filename Is Nothing Then
      Return "Error: 64" ' Bad filename
    ElseIf filename.Length > 63 Then
      Return "Error: 64" ' Bad filename
    End If

    Dim workingFilename As String = ToRealPath(filename)

    Dim content As String = Nothing

    Try
      If IO.File.Exists(workingFilename) Then
        Using s = IO.File.Open(workingFilename, IO.FileMode.Open)
          Using reader As New IO.StreamReader(s)
            content = reader.ReadToEnd
          End Using
        End Using
      Else
        Return "Error: 53"
      End If
    Catch ex As Exception
      'Return "Error: 75" ' Path/File Access Error
      Return String.Format("Error: {0}", ex.Message)
    End Try

    ' Determine position to end of line.

    Dim output As String = ""

    If position > content.Length Then
      Return "Error: 62" ' Input past end.
    Else

      If lineInput Then

        Dim cr As Integer = content.IndexOf(vbCr, position)

        If cr = -1 Then
          Try
            output = content.Substring(position)
          Catch ex As Exception
            Return "Error: (a) " & ex.Message
          End Try
        Else
          Try
            Dim length As Integer = ((cr - position) + 1)
            If content.IndexOf(vbCrLf, position) = cr Then
              length += 1
            End If
            output = content.Substring(position, length)
          Catch ex As Exception
            Return "Error: (b) " & ex.Message
          End Try
        End If

      Else

        ' Starting at whatever position in the file...
        ' Parse for field(s) and build a return string containing
        ' found field(s).

        ' -1 "Ignore white space", 
        '  0  Numeric
        '  1  Unquoted String
        '  2  Quoted String
        Dim mode As Integer = -1
        Dim field As Integer = 1
        Dim offset As Integer = 0

        Do

          ' past end of file...
          If position + offset + 1 > content.Length Then
            Exit Do
          End If

          Dim ch = content(position + offset)

          Select Case mode
            Case -1 ' Ignore white space
              If ch = ChrW(32) OrElse
                 ch = ChrW(10) Then
                ' "ignore"...
                output &= ch
              ElseIf ch = ChrW(13) Then
                ' ignore???
                output &= ch
                If field = count Then
                  Exit Do
                Else
                  field += 1
                End If
              Else
                ' Otherwise, need to begin the process of parsing a field;
                ' determine what kind of parsing will be used.
                output &= ch
                Select Case ch
                  Case "-"c, "."c, "0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c, "0"c
                    mode = 0
                  Case ChrW(34)
                    mode = 2
                  Case Else
                    mode = 1
                End Select
              End If
            Case 0 ' Numeric (or appears to be numeric, could be a string starting with a numeric character; however, the rules are the same.)
              ' Terminates on a space, carriage return, line feed, comma or eof.
              output &= ch
              Select Case ch
                Case ChrW(32) ' Space
                  ' See if we have nothing but spaces to the CR/LF.
                  Dim jump As Integer = -1
                  For scan = position + offset + 1 To content.Length - 1
                    ch = content(position + offset + 1)
                    Select Case ch
                      Case ChrW(32)
                        ' Ignore...
                      Case ChrW(13)
                        jump = scan
                        'If content(jump) = ChrW(13) AndAlso
                        '   jump < content.Length - 1 AndAlso
                        '   content(jump + 1) = ChrW(10) Then
                        '  jump += 1
                        'End If
                        Exit For
                      Case ChrW(10)
                        jump = scan
                        Exit For
                      Case Else
                        jump = scan - 1
                        Exit For
                    End Select
                  Next
                  If jump > -1 Then
                    Dim l = jump - position - offset
                    If l > 0 Then
                      output &= content.Substring(position + offset + 1, l)
                      offset += l
                    End If
                  End If
                  If field = count Then
                    Exit Do
                  Else
                    field += 1
                    mode = -1
                  End If
                Case ChrW(13), ChrW(10), ","c
                  ' Done... (CR/LF combination handled outside of scan loop.)
                  If field = count Then
                    Exit Do
                  Else
                    field += 1
                    mode = -1
                  End If
                Case Else
              End Select
            Case 1 ' Unquoted string
              ' Terminates on a comma, cr, lf, 255 characters or eof.
              output &= ch
              Select Case ch
                Case ","c, ChrW(13), ChrW(10)
                  If field = count Then
                    Exit Do
                  Else
                    field += 1
                    mode = -1
                  End If
                Case Else
              End Select
            Case 2 ' Quoted String
              ' Terminates on a quote, comma, cr, lf, 255 characters or eof.
              output &= ch
              Select Case ch
                Case ChrW(34)
                  If field = count Then
                    Exit Do
                  Else
                    field += 1
                    mode = -1
                  End If
                Case ChrW(13), ChrW(10)
                  If field = count Then
                    Exit Do
                  Else
                    field += 1
                    mode = -1
                  End If
                Case Else
              End Select
            Case Else
          End Select

          offset += 1

        Loop

        'If output.EndsWith(" "c) AndAlso
        '   (content(position + offset) = ChrW(13) OrElse
        '    content(position + offset) = ChrW(10)) Then
        '  output &= content(position + offset)
        '  offset += 1
        'End If
        If output.EndsWith(ChrW(13)) AndAlso
           position + offset < content.Length - 1 AndAlso
           content(position + offset + 1) = ChrW(10) Then
          output &= content(position + offset)
          offset += 1
        End If

      End If

    End If

    Return output

  End Function

  Private Function CsvSplit(ByVal text As String) As String()

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

  Public Function Kill(filename As String) As String Implements IVirtualFileSystem.Kill

    Dim path = EnforceActualPath(filename)

    Dim workingPath = ToRealPath(filename)

    If workingPath Is Nothing Then
      Return "Error: 68" ' Device Unavailable
    End If

    If IO.File.Exists(workingPath) Then
      ' Check to see if the file is open.
      If m_fileHandleList.ContainsKey(path) Then
        'Return "Error: 70" ' Permission Denied
        Return "Error: 55" ' File already open (according to manual and testing)
      End If
      Try
        IO.File.Delete(workingPath)
        Return path
      Catch ex As Exception
        Return String.Format("Error: {0}", ex.Message)
      End Try
    Else
      Return "Error: 53" ' File not found
    End If

  End Function

  Public Function Load(filename As String) As String Implements IVirtualFileSystem.Load

    filename = EnforceActualPath(filename)

    Try

      Dim path = ToRealPath(filename)

      If path Is Nothing Then
        Return "Error: Device Unavailable"
      End If

      If IO.File.Exists(path) Then
        Using s = IO.File.Open(path, IO.FileMode.Open)
          ' Modified to read bytes and processed through the tokenizer.
          Dim b(CInt(s.Length - 1)) As Byte
          Dim data = s.Read(b, 0, b.Length)
          Return Basic.Tokenizer.Decode.Process(b)
          ' The previous incarnation would just read the ascii text.
          'Using reader As New IO.StreamReader(s)
          '  Return reader.ReadToEnd
          'End Using
        End Using
      Else
        Return "Error: File not found"
      End If

    Catch ex As Exception
      Return String.Format("Error: {0}", ex.Message)
    End Try

  End Function

  Public Function Lock(filename As String, min As Integer?, max As Integer?) As String Implements IVirtualFileSystem.Lock
    Return Nothing
  End Function

  Public Function MkDir(pathname As String) As String Implements IVirtualFileSystem.MkDir

    Dim path As String = EnforceActualPath(pathname)

    If path Is Nothing Then
      Return "Error: Bad file name"
    ElseIf path.Length > 63 Then
      Return "Error: Path is longer than 63 characters."
    End If

    If Not path.EndsWith("\") Then
      path &= "\"
    End If

    Dim lastBackslash As Integer = path.LastIndexOf("\")
    Dim nextBackslash As Integer = path.LastIndexOf("\", lastBackslash - 1)
    Dim parentPath As String = path.Substring(0, nextBackslash + 1)
    Dim name As String = path.Substring(nextBackslash + 1, lastBackslash - nextBackslash - 1)

    parentPath = ToRealPath(parentPath)

    If parentPath Is Nothing Then
      Return "Error: Device Unavailable"
    End If

    If Not IO.Directory.Exists(parentPath) Then
      Return "Error: Path not found"
    End If

    Dim workingPath = ToRealPath(path)

    If IO.File.Exists(workingPath) Then
      Return "Error: Path/File Access Error"
    End If

    If IO.Directory.Exists(workingPath & ".package") Then
      Return "Error: Path/File Access Error"
    End If

    Try
      IO.Directory.CreateDirectory(workingPath)
      Return path
    Catch ex As Exception
      Return String.Format("Error: {0}", ex.Message)
    End Try

  End Function

  Public Function Name(oldname As String, newname As String) As String Implements IVirtualFileSystem.Name

    Dim oldPath = EnforceActualPath(oldname)
    Dim newPath = EnforceActualPath(newname)

    Dim workingOldPath = ToRealPath(oldPath)
    Dim workingNewPath = ToRealPath(newPath)

    If workingOldPath Is Nothing OrElse
       workingNewPath Is Nothing Then
      Return "Error: Device Unavailable"
    End If

    If m_fileHandleList.ContainsKey(oldPath) Then
      Return "Error: Permission Denied"
    End If

    If Not IO.File.Exists(workingOldPath) Then
      Return "Error: File not found"
    End If

    If IO.Directory.Exists(workingNewPath) OrElse
       IO.File.Exists(workingNewPath) Then
      Return "Error: Path/File Access Error"
    End If

    Try
      IO.File.Move(workingOldPath, workingNewPath)
      Return newPath
    Catch ex As Exception
      Return String.Format("Error: {0}", ex.Message)
    End Try

  End Function

  Public Function Open(filename As String, mode As Integer, lock As Integer) As String Implements IVirtualFileSystem.Open

    filename = EnforceActualPath(filename)

    Dim path = IO.Path.GetDirectoryName(filename)

    Dim workingPath As String = ToRealPath(path)

    If workingPath Is Nothing Then
      Return "Error: 68" ' Device Unavailable
    End If

    If Not IO.Directory.Exists(workingPath) Then
      Return "Error: 76" ' Path not found
    End If

    Dim workingFilename As String = ToRealPath(filename)

    If workingFilename Is Nothing Then
      Return "Error: 68" ' Device Unavailable
    End If

    If m_fileHandleList.ContainsKey(filename) Then
      Return "Error: 70" ' Permission Denied
    End If

    ' mode
    '  0 = Input ' Position to the beginning of a the file.  A "File not found" error is given if the file does not exist.
    '  1 = Output ' Position to the beginning of the file.  If the file does not exist, one is created.
    '  2 = Append ' Position to the end of the file.  If the file does not exist, one is created.
    '  3 = RandomAccessReadWrite
    '  4 = RandomAccessRead
    '  5 = RandomAccessWrite

    ' lock
    '  0 = [Default]
    '  1 = [Shared]
    '  2 = LockRead
    '  3 = LockWrite
    '  4 = LockReadWrite

    Try

      m_fileHandleList.Add(filename, New FileHandle() With {.Position = 0,
                                                            .Mode = mode,
                                                            .Lock = lock})

      If mode = 0 Then
        ' Input
        If Not IO.File.Exists(workingFilename) Then
          Return "Error: 53" ' File not found
        End If
      ElseIf mode = 1 Then 'OrElse mode = 2 Then
        ' Output 'or Append
        If IO.File.Exists(workingFilename) Then
          IO.File.Delete(workingFilename)
        End If
      End If

      Using s = IO.File.Open(workingFilename, IO.FileMode.OpenOrCreate)
        Return s.Length.ToString
      End Using

    Catch ex As Exception
      Return "Error: " & ex.Message
    End Try

  End Function

  Public Function Read(filename As String, position As Integer, length As Integer) As String Implements IVirtualFileSystem.Read

    filename = EnforceActualPath(filename)

    If filename Is Nothing Then
      Return "Error: 64" ' Bad filename
    ElseIf filename.Length > 63 Then
      Return "Error: 64" ' Bad filename
    End If

    Dim workingFilename As String = ToRealPath(filename)

    If workingFilename Is Nothing Then
      Return "Error: 68" ' Device Unavailable
    End If

    If IO.File.Exists(workingFilename) Then
      Dim content As String = Nothing
      Using s = IO.File.Open(workingFilename, IO.FileMode.Open)
        Using reader As New IO.StreamReader(s)
          content = reader.ReadToEnd
        End Using
      End Using
      Try
        Dim output As String = content.Substring(position, length)
        Return output
      Catch ex As Exception
        Return "Error: 62" ' Input past end
        'Return "Error: 63" ' Bad record number.
      End Try
    Else
      Return "Error: 53" ' File not found
    End If

  End Function

  Public Function RmDir(pathname As String) As String Implements IVirtualFileSystem.RmDir

    Dim path As String = EnforceActualPath(pathname)

    If path Is Nothing Then
      Return "Error: Bad file name"
    ElseIf path.Length > 63 Then
      Return "Error: Path is longer than 63 characters."
    End If

    Dim workingPath As String = ToRealPath(path)

    If workingPath Is Nothing Then
      Return "Error: Device Unavailable"
    End If

    If Not IO.Directory.Exists(workingPath) Then
      Return "Error: Path not found"
    End If

    ' Check to see if any folders/files exist in the path to be removed.

    If IO.Directory.EnumerateDirectories(workingPath).Count > 0 OrElse
       IO.Directory.EnumerateFiles(workingPath).Count > 0 Then
      Return "Error: Path/File Access Error"
    End If

    Try
      IO.Directory.Delete(workingPath)
      Return path
    Catch ex As Exception
      Return String.Format("Error: {0}", ex.Message)
    End Try

  End Function

  Public Function Save(filename As String, content As String) As String Implements IVirtualFileSystem.Save

    filename = EnforceActualPath(filename)
    Dim path = IO.Path.GetDirectoryName(filename)

    Dim workingPath As String = ToRealPath(path)

    If workingPath Is Nothing Then
      Return "Error: Device Unavailable"
    End If

    If Not IO.Directory.Exists(workingPath) Then
      Return "Error: Path not found"
    End If

    workingPath = ToRealPath(filename)

    If workingPath Is Nothing Then
      Return "Error: Device Unavailable"
    End If

    If IO.Directory.Exists(workingPath) Then
      Return "Error: Path/File Access Error"
    End If

    Try
      Using s = IO.File.Open(workingPath, IO.FileMode.Create)
        Using writer As New IO.StreamWriter(s)
          writer.Write(content)
          Return Nothing
        End Using
      End Using
    Catch ex As Exception
      Return String.Format("Error: {0}", ex.Message)
    End Try

  End Function

  Public Function Unlock(filename As String, min As Integer?, max As Integer?) As String Implements IVirtualFileSystem.Unlock
    Return Nothing
  End Function

  Public Function Write(filename As String, text As String, position As Integer, random As Boolean) As String Implements IVirtualFileSystem.Write

    filename = EnforceActualPath(filename)

    If filename Is Nothing Then
      Return "Error: 64" ' Bad filename
    ElseIf filename.Length > 63 Then
      Return "Error: 64" ' Bad filename
    End If

    Dim workingFilename As String = ToRealPath(filename)

    If workingFilename Is Nothing Then
      Return "Error: 68" ' Device Unavailable
    End If

    If Not IO.File.Exists(workingFilename) Then
      Return "Error: 53"
    End If

    Dim content As String = Nothing
    Try
      Using s = IO.File.Open(workingFilename, IO.FileMode.Open)
        Using reader As New IO.StreamReader(s)
          content = reader.ReadToEnd
        End Using
      End Using
    Catch ex As Exception
      Return String.Format("Error: {0}", ex.Message)
    End Try

    If random Then
      If content.Length < position + text.Length Then
        ' Need to expand the file to accomidate this record.
        content = content.PadRight(position + text.Length)
      End If
      If content.Length = position + text.Length Then
        content = content.Substring(0, position) & text
      Else
        content = content.Substring(0, position) & text & content.Substring(position + text.Length)
      End If
    Else
      If position = 0 Then
        content = text
      Else
        content &= text
      End If
    End If

    Try
      Using s = IO.File.Open(workingFilename, IO.FileMode.Create)
        Using writer As New IO.StreamWriter(s)
          writer.Write(content)
          Return Nothing
        End Using
      End Using
    Catch ex As Exception
      Return String.Format("Error: {0}", ex.Message)
    End Try

  End Function

  Private Function EnforceActualPath(ByVal pathname As String) As String

    ' If blank, empty, less than 3 characters (the minimum full path) or not letter-colon-backslash 
    ' pattern for the first three letters or contains an asterisk or question mark.,
    ' set to null to force an invalid path situation.

    If String.IsNullOrEmpty(pathname) OrElse
       pathname.Length < 3 OrElse
       Not ("ABCDEFGHIJKLMNOPQRSTUVWXYZ".Contains(pathname(0)) AndAlso
            pathname(1) = ":" AndAlso
            pathname(2) = "\") OrElse
       pathname.IndexOf("*") > -1 OrElse
       pathname.IndexOf("?") > -1 Then
      Return Nothing
    End If

    ' Otherwise, parse through items accordingly.

    Dim result As String = ""

    Dim files() As String = pathname.Split("\"c) 'Split(pathname, "\")
    For Each file In files

      Dim parts() = file.Split("."c) 'Split(file, ".")

      If parts(0).Length > 8 Then
        parts(0) = parts(0).Substring(0, 8)
      End If

      If parts.Count > 1 AndAlso parts(1).Length > 3 Then
        If parts(1) = "PACKAGE" OrElse parts(1) = "package" Then
          ' leave as is.
        Else
          parts(1) = parts(1).Substring(0, 3)
        End If
      End If

      If result <> "" Then
        result &= "\"
      End If

      result &= parts(0).Trim.ToUpper
      If parts.Count > 1 Then
        result &= "." & parts(1).Trim.ToUpper
      End If

    Next

    Return result

  End Function

  'Public Function ConvertFileToPackage(filename As String) As String Implements Basic.Interpreter.IVirtualFileSystem.ConvertFileToPackage

  '  ' Determine the new folder name...
  '  ' If folder name exists, raise exception.
  '  ' If folder does not exist, create new folder.
  '  ' Move file into the newly created folder.
  '  ' Create a package.properties file with default settings.
  '  ' Return folder path.

  '  Return "Error: Not implemented."

  'End Function

  'Public Function ConvertFolderToPackage(pathname As String) As String Implements Basic.Interpreter.IVirtualFileSystem.ConvertFolderToPackage

  '  ' Determine the new folder name...
  '  ' If folder name exists, raise exception.
  '  ' If folder does not exist, rename pathname into new folder name.
  '  ' Create a package.properties file with default settings.
  '  ' Return folder path.

  '  Return "Error: Not implemented."

  'End Function

  'Public Function ConvertPackageToFolder(pathname As String) As String Implements Basic.Interpreter.IVirtualFileSystem.ConvertPackageToFolder

  '  ' Determine the new folder name...
  '  ' If folder name exists, raise exception.
  '  ' If folder does not exist, rename pathname into new folder name.
  '  ' Delete package.properties file.
  '  ' Delete package.publication file (if exists).
  '  ' Return folder path.

  '  Return "Error: Not implemented."

  'End Function

  'Public Function SetPackageStartup(filename As String) As String Implements Basic.Interpreter.IVirtualFileSystem.SetPackageStartup

  '  ' Update package.properties to reflect proper startup script.

  '  Return "Error: Not implemented."

  'End Function

  Public ReadOnly Property IsEightDotThree As Boolean Implements IVirtualFileSystem.IsEightDotThree
    Get
      Return True
    End Get
  End Property

End Class
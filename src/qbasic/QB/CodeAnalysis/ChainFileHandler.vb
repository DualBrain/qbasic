Imports System.IO

Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis

  ''' <summary>
  ''' Handles file loading and validation for CHAIN operations.
  ''' Resolves paths, validates file existence, and prepares target files for chaining.
  ''' </summary>
  Friend NotInheritable Class ChainFileHandler

    ''' <summary>
    ''' Resolves and validates a CHAIN target filename.
    ''' </summary>
    ''' <param name="filename">The filename from CHAIN statement</param>
    ''' <param name="currentDirectory">Current working directory for relative paths</param>
    ''' <returns>Validated absolute file path</returns>
    Friend Shared Function ResolveAndValidateChainFile(filename As String, currentDirectory As String) As String
      If String.IsNullOrWhiteSpace(filename) Then
        Throw New QBasicRuntimeException(ErrorCode.BadFileName)
      End If

      ' Remove quotes if present
      filename = filename.Trim().Trim(""""c)

      ' Add .BAS extension if not present
      If Not Path.HasExtension(filename) Then
        filename &= ".BAS"
      End If

      ' Get absolute path
      Dim absolutePath As String
      If Path.IsPathRooted(filename) Then
        absolutePath = filename
      Else
        absolutePath = Path.Combine(currentDirectory, filename)
      End If

      ' Normalize path
      absolutePath = Path.GetFullPath(absolutePath)

      ' Validate file exists
      If Not File.Exists(absolutePath) Then
        Throw New QBasicRuntimeException(ErrorCode.FileNotFound)
      End If

      ' Validate it's a .BAS file
      Dim extension = Path.GetExtension(absolutePath).ToUpper()
      If extension <> ".BAS" Then
        Throw New QBasicRuntimeException(ErrorCode.BadFileMode)
      End If

      Return absolutePath
    End Function

    ''' <summary>
    ''' Loads source code from a chain target file.
    ''' </summary>
    ''' <param name="filePath">Absolute path to the file</param>
    ''' <returns>File contents as string</returns>
    Friend Shared Function LoadChainFile(filePath As String) As String
      Try
        Return File.ReadAllText(filePath)
      Catch ex As Exception
        Throw New QBasicRuntimeException(ErrorCode.FileNotFound)
      End Try
    End Function

    ''' <summary>
    ''' Validates that a line number exists in the target file's syntax tree.
    ''' </summary>
    ''' <param name="tree">The parsed syntax tree of the target file</param>
    ''' <param name="lineNumber">The line number to validate</param>
    ''' <returns>True if line exists, False otherwise</returns>
    Friend Shared Function ValidateLineNumber(tree As SyntaxTree, lineNumber As Integer) As Boolean
      If lineNumber <= 0 Then Return False

      ' This is simplified - in full implementation we'd need to map line numbers
      ' to actual syntax nodes. For now, just basic validation.
      Return lineNumber <= tree.Root.Members.Count
    End Function

    ''' <summary>
    ''' Gets the directory of the current loaded file for resolving relative paths.
    ''' </summary>
    ''' <param name="currentFilePath">Current file path (can be null)</param>
    ''' <returns>Directory path</returns>
    Friend Shared Function GetCurrentDirectory(currentFilePath As String) As String
      If String.IsNullOrEmpty(currentFilePath) Then
        Return Environment.CurrentDirectory
      Else
        Return Path.GetDirectoryName(Path.GetFullPath(currentFilePath))
      End If
    End Function

  End Class

End Namespace
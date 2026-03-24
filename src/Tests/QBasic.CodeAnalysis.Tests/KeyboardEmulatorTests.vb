Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

Imports QBLib

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class KeyboardEmulatorTests

    Private Shared Function Evaluate(text As String, Optional commandLineArgs() As String = Nothing) As (Result As EvaluationResult, Variables As Dictionary(Of String, Object))

      Dim variables = New Dictionary(Of String, Object)
      Dim syntaxTree As SyntaxTree = SyntaxTree.Parse(text)
      Dim compilation As Compilation = Compilation.Create(syntaxTree)
      Dim result As EvaluationResult = compilation.Evaluate(variables, commandLineArgs)
      Return (result, variables)

    End Function

    <Fact>
    Public Sub KeyboardEmulator_Initialize_SetsNumLockOn()

      KeyboardEmulator.Reset()
      Assert.Equal(32, KeyboardEmulator.GetKeyboardFlags(1047))

    End Sub

    <Fact>
    Public Sub PeekBiosAddress1047WithDefSeg0_ReturnsNumLockOn()

      KeyboardEmulator.Reset()

      Dim code = "DEF SEG = 0
result% = PEEK(1047)
DEF SEG
"
      Dim eval = Evaluate(code)
      Assert.Equal("32", $"{eval.Variables("result%")}")

    End Sub

    <Fact>
    Public Sub PokeBiosAddress1047WithDefSeg0_UpdatesKeyboardFlags()

      KeyboardEmulator.Reset()

      Dim code = "DEF SEG = 0
POKE 1047, 0
DEF SEG
result% = PEEK(1047)
"
      Dim eval = Evaluate(code)
      Assert.Equal("0", $"{eval.Variables("result%")}")

      KeyboardEmulator.Reset()

    End Sub

    <Fact>
    Public Sub PeekWithoutDefSeg0_UsesPeekDictionary()

      KeyboardEmulator.Reset()

      Dim code = "
POKE 1000, 65
result% = PEEK(1000)
"
      Dim eval = Evaluate(code)
      Assert.Equal("65", $"{eval.Variables("result%")}")

    End Sub

    <Fact>
    Public Sub IsKnownBiosAddress_ReturnsCorrectValues()

      Assert.True(KeyboardEmulator.IsKnownBiosAddress(0))
      Assert.True(KeyboardEmulator.IsKnownBiosAddress(100))
      Assert.True(KeyboardEmulator.IsKnownBiosAddress(255))
      Assert.True(KeyboardEmulator.IsKnownBiosAddress(1047))
      Assert.False(KeyboardEmulator.IsKnownBiosAddress(256))
      Assert.False(KeyboardEmulator.IsKnownBiosAddress(1000))
      Assert.False(KeyboardEmulator.IsKnownBiosAddress(65535))

    End Sub

    <Fact>
    Public Sub IsImplementedBiosAddress_ReturnsTrueOnlyFor1047()

      Assert.False(KeyboardEmulator.IsImplementedBiosAddress(0))
      Assert.True(KeyboardEmulator.IsImplementedBiosAddress(1047))
      Assert.False(KeyboardEmulator.IsImplementedBiosAddress(100))

    End Sub

    <Fact>
    Public Sub KeyboardEmulatorHelpers_ReturnCorrectStates()

      KeyboardEmulator.Reset()

      Assert.True(KeyboardEmulator.IsNumLockOn())
      Assert.False(KeyboardEmulator.IsCapsLockOn())
      Assert.False(KeyboardEmulator.IsScrollLockOn())

      KeyboardEmulator.SetKeyboardFlags(1047, 0)
      Assert.False(KeyboardEmulator.IsNumLockOn())
      Assert.False(KeyboardEmulator.IsCapsLockOn())
      Assert.False(KeyboardEmulator.IsScrollLockOn())

      KeyboardEmulator.SetKeyboardFlags(1047, &H70)
      Assert.True(KeyboardEmulator.IsNumLockOn())
      Assert.True(KeyboardEmulator.IsCapsLockOn())
      Assert.True(KeyboardEmulator.IsScrollLockOn())

      KeyboardEmulator.Reset()

    End Sub

  End Class

End Namespace

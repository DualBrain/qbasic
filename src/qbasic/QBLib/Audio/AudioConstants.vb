Namespace Global.QBLib.Audio

  Public Module AudioConstants

    Public Const MIN_FREQUENCY As Integer = 37
    Public Const MAX_FREQUENCY As Integer = 32767
    Public Const DEFAULT_BEEP_FREQUENCY As Integer = 800
    Public Const DEFAULT_BEEP_DURATION_MS As Integer = 250
    Public Const DEFAULT_SOUND_DURATION_TICKS As Integer = 18

    Public Const SAMPLE_RATE As Integer = 44100
    Public Const BITS_PER_SAMPLE As Integer = 16
    Public Const CHANNELS As Integer = 1

    Public Const DEFAULT_PLAY_OCTAVE As Integer = 6 ' should be 4 (based on frequency tables)??? however, in testing/comparing with MSQB there appears to be a 2 octave difference?????

  End Module

End Namespace
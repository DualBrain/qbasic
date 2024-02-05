Namespace Global.Basic.Audio

  Public Interface ISound

    ReadOnly Property IsPlaying As Boolean
    ReadOnly Property SampleRate As Integer

    Function Sound(frequency As Single, duration As Single) As Integer

  End Interface

End Namespace
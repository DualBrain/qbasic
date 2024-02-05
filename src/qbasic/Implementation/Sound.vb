'Imports SSDD.Basic.Audio

'Public Class Sound
'  Implements ISound

'  ''Private WithEvents m_mediaElement As MediaElement

'  'Private m_soundEffectInstance As Microsoft.Xna.Framework.Audio.SoundEffectInstance

'  Public Sub New()
'  End Sub

'  ''Public Sub New(ByVal mediaElement As MediaElement)
'  ''  m_mediaElement = mediaElement
'  ''End Sub

'  ''Public Property Meinteger
'  ''    Return m_mediaElement
'  ''  End Get
'  ''  Set(value As MediaElement)
'  ''    m_mediaElement = value
'  ''  End Set
'  ''End Property

'  Public ReadOnly Property IsPlaying As Boolean Implements ISound.IsPlaying
'    Get
'      Return False
'      'Return m_soundEffectInstance IsNot Nothing AndAlso
'      '       m_soundEffectInstance.State = Microsoft.Xna.Framework.Audio.SoundState.Playing
'    End Get
'  End Property

'  Public Function Sound(frequency As Single, duration As Single) As Integer Implements ISound.Sound

'    '''TODO: need to figure out a way to block the a beep until the currently active beep is completed.
'    ''If m_mediaElement IsNot Nothing AndAlso
'    ''   Not m_mediaElement.CurrentState = MediaElementState.Playing Then
'    ''  m_mediaElement.AutoPlay = True
'    ''  m_mediaElement.SetSource(New MyMediaStreamSource(frequency, duration))
'    ''  'm_mediaElement.Play()
'    ''End If

'    ''Using stream = Basic.Interpreter 'Microsoft.Xna.Framework.TitleContainer.OpenStream(Sound)
'    ''  Dim effect = Microsoft.Xna.Framework.Audio.SoundEffect.FromStream(stream)
'    ''  'Microsoft.Xna.Framework.FrameworkDispatcher.Update()
'    ''  effect.Play()
'    ''  'Thread.Sleep(effect.Duration)
'    ''End Using

'    'If m_soundEffectInstance IsNot Nothing AndAlso
'    '   m_soundEffectInstance.State = Microsoft.Xna.Framework.Audio.SoundState.Playing Then
'    '  If duration = 0 Then
'    '    ' stop the currently playing sound.
'    '    m_soundEffectInstance.Stop()
'    '    Return 0 ' No error.
'    '  Else
'    '    ' Wait until the sound is done playing.
'    '    ' Since we can, use Thread.Sleep - this is less CPU intensive.
'    '    Do Until m_soundEffectInstance.State = Microsoft.Xna.Framework.Audio.SoundState.Stopped
'    '      System.Threading.Thread.Sleep(55)
'    '    Loop
'    '    ' If Thread.Sleep isn't available, throw Error 5 and the 
'    '    ' interpreter will keep retrying the current call until the currently
'    '    ' running sound is finished playing.
'    '    'Return 5 ' Illegal function call.
'    '  End If
'    'End If

'    'Try
'    '  Using gs As New SoundGenerator(Me.SampleRate, frequency, duration)
'    '    'Dim effect = Microsoft.Xna.Framework.Audio.SoundEffect.FromStream(gs.Stream)
'    '    m_soundEffectInstance = Microsoft.Xna.Framework.Audio.SoundEffect.FromStream(gs.Stream).CreateInstance
'    '    m_soundEffectInstance.Play()
'    '    'effect.Play()
'    '    'System.Threading.Thread.Sleep(effect.Duration)
'    '  End Using
'    '  Return 0 ' No error.
'    'Catch ex As OutOfMemoryException
'    '  Return 7 ' Out of memory
'    'End Try

'    Return 0

'  End Function

'  Public ReadOnly Property SampleRate As Integer Implements ISound.SampleRate
'    Get
'      Return 44100
'    End Get
'  End Property

'End Class
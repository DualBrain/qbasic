''Imports System.Net
''Imports System.Windows
''Imports System.Windows.Controls
''Imports System.Windows.Documents
''Imports System.Windows.Ink
''Imports System.Windows.Input
''Imports System.Windows.Media
''Imports System.Windows.Media.Animation
''Imports System.Windows.Shapes
''Imports System.Collections.Generic
'Imports System.IO

'Imports Basic.Interpreter

'Public Class MyMediaStreamSource
'  Inherits MediaStreamSource

'  Private _waveFormat As WaveFormatEx
'  Private _audioDesc As MediaStreamDescription
'  Private _currentPosition As Long
'  Private _startPosition As Long
'  Private _currentTimeStamp As Long

'  Private Const SampleRate As Integer = 44100
'  Private Const ChannelCount As Integer = 1
'  Private Const BitsPerSample As Integer = 16
'  Private Const ByteRate As Integer = SampleRate * ChannelCount * BitsPerSample / 8

'  Private _stream As MemoryStream

'  ' you only need sample attributes for video
'  Private _emptySampleDict As New Dictionary(Of MediaSampleAttributeKeys, String)()

'  Private m_frequency As Single
'  Private m_duration As Single ' Measured in ticks (18.2 times per second or 55 milliseconds.)

'  Public Sub New(ByVal frequency As Single, ByVal duration As Single)

'    m_frequency = frequency
'    m_duration = duration

'    _waveFormat = New WaveFormatEx()
'    _waveFormat.BitsPerSample = 16
'    _waveFormat.AvgBytesPerSec = ByteRate
'    _waveFormat.Channels = ChannelCount
'    _waveFormat.BlockAlign = ChannelCount * (BitsPerSample \ 8)
'    _waveFormat.ext = Nothing
'    _waveFormat.FormatTag = WaveFormatEx.FormatPCM
'    _waveFormat.SamplesPerSec = SampleRate
'    _waveFormat.Size = 0 ' must be zero
'    _waveFormat.ValidateWaveFormat()

'    _stream = New System.IO.MemoryStream()

'  End Sub

'  Protected Overrides Sub OpenMediaAsync()

'    System.Diagnostics.Debug.WriteLine("Started OpenMediaAsync")

'    _startPosition = 0
'    _currentPosition = 0
'    _currentTimeStamp = 0

'    ' Init
'    Dim streamAttributes As New Dictionary(Of MediaStreamAttributeKeys, String)()
'    Dim sourceAttributes As New Dictionary(Of MediaSourceAttributesKeys, String)()
'    Dim availableStreams As New List(Of MediaStreamDescription)()

'    ' Stream Description and WaveFormatEx
'    streamAttributes(MediaStreamAttributeKeys.CodecPrivateData) = _waveFormat.ToHexString()

'    ' wfx
'    Dim msd As New MediaStreamDescription(MediaStreamType.Audio, streamAttributes)
'    _audioDesc = msd

'    ' next, add the description so that Silverlight will actually request samples for it
'    availableStreams.Add(_audioDesc)

'    ' Tell silverlight we have an endless stream
'    sourceAttributes(MediaSourceAttributesKeys.Duration) = TimeSpan.FromMinutes(0).Ticks.ToString(System.Globalization.CultureInfo.InvariantCulture)

'    ' we don't support seeking on our stream
'    sourceAttributes(MediaSourceAttributesKeys.CanSeek) = False.ToString()

'    ' tell Silverlight we're done opening our media
'    ReportOpenMediaCompleted(sourceAttributes, availableStreams)

'    'System.Diagnostics.Debug.WriteLine("Completed OpenMediaAsync");

'  End Sub

'  Protected Overrides Sub CloseMedia()
'    ' Close the stream
'    System.Diagnostics.Debug.WriteLine("CloseMedia")
'    _startPosition = 0
'    _currentPosition = 0
'    _currentTimeStamp = 0
'    _audioDesc = Nothing
'  End Sub

'  Protected Overrides Sub GetDiagnosticAsync(diagnosticKind As MediaStreamSourceDiagnosticKind)
'    Throw New NotImplementedException()
'  End Sub

'  'Protected Overrides Sub GetSampleAsync(mediaStreamType As MediaStreamType)

'  '  'Const middleC As Double = 261.626
'  '  'Const standardA As Double = 440
'  '  'Const beep As Double = 800

'  '  'Const frequency As Double = beep 'standardA

'  '  'Dim numSamples As Short = SampleRate 'ChannelCount * 256
'  '  'Dim numSamples As Short = ChannelCount * 256
'  '  Dim numSamples As Short = (SampleRate * 2)
'  '  Dim bufferByteCount As Short = (BitsPerSample \ 8) * numSamples

'  '  If _currentTimeStamp > ((m_duration * 55) * 10000) Then
'  '    ReportGetSampleCompleted(New MediaStreamSample(_audioDesc, Nothing, _currentPosition, bufferByteCount, _currentTimeStamp, _emptySampleDict))
'  '    Return
'  '  End If

'  '  ' fill the stream with noise
'  '  For i As Short = 0 To numSamples - 1
'  '    Dim t As Double = i / (SampleRate * ChannelCount) ' Time of this sample in seconds
'  '    ' Dim s As Short = CShort(Math.Floor(Math.Sin(t * 2 * Math.PI * frequency) * Short.MaxValue))
'  '    Dim s As Short = CShort(Short.MaxValue * Math.Sin(t * (m_frequency * (Math.PI * 2))))
'  '    _stream.Write(BitConverter.GetBytes(s), 0, 2)

'  '  Next

'  '  ' Send out the next sample
'  '  Dim msSamp As New MediaStreamSample(_audioDesc, _stream, _currentPosition, bufferByteCount, _currentTimeStamp, _emptySampleDict)

'  '  ' Move our timestamp and position forward
'  '  _currentTimeStamp += _waveFormat.AudioDurationFromBufferSize(CUInt(bufferByteCount))
'  '  _currentPosition += bufferByteCount

'  '  ReportGetSampleCompleted(msSamp)

'  'End Sub

'  Protected Overrides Sub GetSampleAsync(mediaStreamType As MediaStreamType)

'    'Dim frequency As Single = m_frequency '(m_frequency \ 40) * 40
'    'Dim frequency As Short = (m_frequency \ 20) * 20
'    Dim frequency As Single = CShort(m_frequency / 5) * 5  ' Seems to work fine for every 5 frequencies; so forcing to nearest 5 mark.

'    If _currentTimeStamp > ((m_duration * 55) * 10000) Then
'      ReportGetSampleCompleted(New MediaStreamSample(_audioDesc, Nothing, 0, 0, 0, _emptySampleDict))
'      Return
'    End If

'    Dim numSamples As Short = SampleRate \ 5 '2560 * ChannelCount
'    Dim bufferByteCount As Short = (BitsPerSample \ 8) * numSamples

'    ' fill the stream with noise
'    For i As Short = 0 To numSamples - 1

'      Dim t As Double = i / (SampleRate * ChannelCount) ' Time of this sample in seconds
'      ' Dim s As Short = CShort(Math.Floor(Math.Sin(t * 2 * Math.PI * frequency) * Short.MaxValue))
'      Dim s As Short = CShort(Short.MaxValue * Math.Sin(t * (frequency * (Math.PI * 2)))) ' * 2))))
'      _stream.Write(BitConverter.GetBytes(s), 0, 2)

'      'amplitude = CShort(Fix(Short.MaxValue * Math.Sin(CULng(2 * Math.PI * wholePhaseAngle) \ UShort.MaxValue)))
'      'amplitude = If(wholePhaseAngle < CUShort(Short.MaxValue), Short.MinValue, Short.MaxValue)

'    Next

'    ' Send out the next sample
'    Dim msSamp As New MediaStreamSample(_audioDesc, _stream, _currentPosition, bufferByteCount, _currentTimeStamp, _emptySampleDict)

'    ' Move our timestamp and position forward
'    _currentTimeStamp += _waveFormat.AudioDurationFromBufferSize(CUInt(bufferByteCount))
'    _currentPosition += bufferByteCount

'    'For i As Short = 0 To _waveFormat.SamplesPerSec - 1
'    '  Dim value = 65535 * Math.Sin(frequency * i / _waveFormat.SamplesPerSec)
'    '  If value > Short.MaxValue Then
'    '    value = Short.MaxValue
'    '  ElseIf value < Short.MinValue Then
'    '    value = Short.MinValue
'    '  End If
'    '  Dim s = CShort(value)
'    '  _stream.Write(BitConverter.GetBytes(s), 0, 2)
'    'Next

'    '' Send out the next sample
'    'Dim msSamp As New MediaStreamSample(_audioDesc, _stream, _currentPosition, (_waveFormat.SamplesPerSec \ 8) * _waveFormat.Channels, _currentTimeStamp, _emptySampleDict)

'    '' Move our timestamp and position forward
'    '_currentTimeStamp += _waveFormat.AudioDurationFromBufferSize(CUInt((_waveFormat.SamplesPerSec \ 8) * _waveFormat.Channels))
'    '_currentPosition += (_waveFormat.SamplesPerSec \ 8) * _waveFormat.Channels

'    ReportGetSampleCompleted(msSamp)

'  End Sub

'  Protected Overrides Sub SeekAsync(seekToTime As Long)
'    ReportSeekCompleted(seekToTime)
'  End Sub

'  Protected Overrides Sub SwitchMediaStreamAsync(mediaStreamDescription As MediaStreamDescription)
'    Throw New NotImplementedException()
'  End Sub

'  Private Function CreateSineData16Bit(ByVal format As Basic.Interpreter.WaveFormatEx,
'                                       ByVal frequency As Single) As Byte()

'    Dim samplesPerCycle As Short = format.SamplesPerSec \ frequency
'    Dim buffer(format.BlockAlign * samplesPerCycle - 1) As Byte ' 1 sine wave cycle only! might be too small
'    Dim theta As Double = 0
'    Dim thetaStep As Double = (frequency * (Math.PI * 2)) / format.SamplesPerSec
'    For i As Short = 0 To buffer.Length - 1 Step format.BlockAlign

'      Dim value As Short = CType(Short.MaxValue * Math.Sin(theta), Short)
'      theta += thetaStep

'      Dim bytes() As Byte = BitConverter.GetBytes(value)

'      For channel As Short = 0 To format.Channels - 1
'        System.Buffer.BlockCopy(bytes, 0, buffer, (channel * 2) + i, bytes.Count)
'      Next

'    Next
'    Return buffer
'  End Function

'End Class
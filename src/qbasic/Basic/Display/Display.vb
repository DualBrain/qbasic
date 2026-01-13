Namespace Global.Basic.Display

  'Imports System.Windows.Media.Imaging

  Public MustInherit Class Display

    Private ReadOnly m_idirect As IDirect = Nothing

    'NOTE: Modified so that syncContext is now checked and Paint now happens on the "display thread".

    Private ReadOnly m_syncContext As Threading.SynchronizationContext

    'Private WithEvents InternalTimer As System.Timers.Timer 'Timers.ITimer

    'Private m_canvas As Canvas

    Private m_screenMode As Integer = -1
    Private m_screenWidth As Integer = 640
    Private m_screenHeight As Integer = 400
    Private m_screenColorCount As Integer = 16
    Private m_activePage As Short = 0
    Private m_visualPage As Short = 0
    Private m_colorSwitch As Boolean = False
    Private m_characterWidth As Integer = 8
    Private m_characterHeight As Integer = 16

    Private ReadOnly m_defaultForegroundColor As Short = 7
    Private ReadOnly m_defaultBackgroundColor As Short = 0

    Private ReadOnly m_columnCount As New List(Of Short) From {80,
                                                      40,
                                                      80,
                                                      20,
                                                      40,
                                                      40,
                                                      80,
                                                      40,
                                                      80,
                                                      80,
                                                      80,
                                                      80,
                                                      80,
                                                      40}

    Private ReadOnly m_page As New Dictionary(Of Integer, InternalPage)

    Private m_invalidated As Boolean = True

    Private m_rowCount As Short = 25 ' How many text rows are on the screen.

    'Private m_page(m_activePage).CharacterBuffer() As Byte 'Character ' Represents the screen character buffer.
    'Private m_segments As New List(Of Segment)

    Private m_viewActive As Boolean = False
    Private m_topLine As Short = 1
    Private m_bottomLine As Short = 24 ' 25

    Private m_backColor As Short
    Private m_foreColor As Short
    'Private m_borderColor As Short

    'Private m_screenMode As Short ' 0 = text, 1 = allow for pallete parameter, 2 - Color is illegal, 7-10 border is illegal.

    ' "State Machine"
    Private m_location As Short = 0 ' Represents the current location on the screen (CsrLin, Loc(0)).
    Private m_cursorVisible As Boolean
    Private m_cursorToggle As Boolean ' Used in Paint() to toggle cursor (flash).
    Private m_cursorLocation As Short = -1 ' If cursor is currently flashing on, where is it (used during Paint() to "turn off" cursor).
    Private m_cursorThick As Boolean
    'Private m_clearScreened As Boolean ' Flags whether or not CLS was recently executed (used during Paint()).
    Private m_keyOn As Boolean ' Whether or not the function key line is visible.
    Private ReadOnly m_keys As New List(Of String) From {"LIST ",
                                                "RUN" & ChrW(27),
                                                "LOAD""",
                                                "SAVE""",
                                                "CONT" & ChrW(27),
                                                ",""LPT1:""" & ChrW(27),
                                                "TRON" & ChrW(27),
                                                "TROFF" & ChrW(27),
                                                "KEY ",
                                                "SCREEN 0, 0, 0" & ChrW(27)}

    Private m_insert As Boolean
    Private m_capsLock As Boolean
    Private m_shiftDown As Boolean
    Private m_controlDown As Boolean
    Private m_altDown As Boolean

    'Private m_showVisualQueues As Boolean = True

    Friend Class PaintParams
      Public Property PaintAttribute As Integer?
      Public Property BorderAttribute As Integer?
      Public Property Paint As String
      Public Property Border As String
      Public Property Background As String
    End Class

    Public ReadOnly Property IsInvalidated As Boolean
      Get
        Dim invalidated As Boolean = m_invalidated
        m_invalidated = False
        Return invalidated
      End Get
    End Property

    Public Class InternalPage

      Private ReadOnly m_mode As Integer
      Private ReadOnly m_width As Integer
      Private ReadOnly m_height As Integer

      ' BASIC color attribute; legal values depends on screen mode.
      ' In other words:
      '   Screen 0: 0-31
      '   Screen 1: 0-3
      '   Screen 2: 0-1
      Private ReadOnly m_colorAttributes As Byte()

      ' Size depends on number of colors for the current screen mode,
      ' where each index holds a translation reference to the EGA palette.
      Private m_attributeEgaPalette() As Integer

      ' Using the BASIC color -> EGA palette -> ARGB (16-bit) color.

      ' Raw RGB pixel array.
      Private ReadOnly m_pixels As Integer()

      ' Holds the EGA (64 color) palette to ARGB (16-bit) palette cached translation values.
      Private Shared ReadOnly m_egaPalette(63) As Integer

      Private Shared ReadOnly m_vgaPalette(255) As Integer

      Private m_cgaAlternatePalette As Boolean

      Private m_characterBuffer() As Byte 'Character ' Represents the screen character buffer.
      Private m_characterAttribute() As Byte 'Character ' Represents the screen attribute buffer.
      Private ReadOnly m_segments As New List(Of Segment)

      Private ReadOnly m_rowCount As Integer '= 25
      Private ReadOnly m_columnCount As Integer '= 80

      Public Sub New(mode As Integer, columns As Integer, rows As Integer)

        InitializeEgaPalette()
        InitializeVgaPalette()

        Select Case mode
          Case 0
            If columns = 40 Then ' 40 column mode
              m_width = 320
              m_height = 400
              m_columnCount = columns
              m_rowCount = rows
            Else ' 80 column mode
              m_width = 640
              m_height = 400
              m_columnCount = columns
              m_rowCount = rows
            End If
          Case 1
            m_width = 320
            m_height = 200
            m_columnCount = 40
            m_rowCount = 25
          Case 2
            m_width = 640
            m_height = 200
            m_columnCount = 80
            m_rowCount = 25
          Case 3 ' Tandy 1000
            m_width = 160
            m_height = 200
            m_columnCount = 20
            m_rowCount = 25
          Case 4 ' Tandy 1000
            m_width = 320
            m_height = 200
            m_columnCount = 40
            m_rowCount = 25
          Case 5 ' Tandy 1000
            m_width = 320
            m_height = 200
            m_columnCount = 40
            m_rowCount = 25
          Case 6 ' Tandy 1000
            m_width = 640
            m_height = 200
            m_columnCount = 80
            m_rowCount = 25
          Case 7
            m_width = 320
            m_height = 200
            m_columnCount = 40
            m_rowCount = 25
          Case 8
            m_width = 640
            m_height = 200
            m_columnCount = 80
            m_rowCount = 25
          Case 9
            m_width = 640
            m_height = 350
            m_columnCount = 80
            m_rowCount = rows
          Case 10
            m_width = 640
            m_height = 350
            m_columnCount = 80
            m_rowCount = rows
          Case 11
            m_width = 640
            m_height = 480
            m_columnCount = 80
            m_rowCount = rows
          Case 12
            m_width = 640
            m_height = 480
            m_columnCount = 80
            m_rowCount = rows
          Case 13
            m_width = 320
            m_height = 200
            m_columnCount = 40
            m_rowCount = 25
          Case Else
            Stop
        End Select

        ReDim m_characterBuffer((m_rowCount * m_columnCount) - 1)
        ReDim m_characterAttribute((m_rowCount * m_columnCount) - 1)

        ReDim m_colorAttributes(m_width * m_height - 1)
        ReDim m_pixels(m_width * m_height - 1)

        'm_attributeEgaPalette = attributeEgaPalette

        m_mode = mode
        SetPalette()

        For index As Integer = 0 To m_width * m_height - 1
          m_colorAttributes(index) = 0 ' default all entries to black.
          If mode = 11 OrElse mode = 12 OrElse mode = 13 Then
            m_pixels(index) = m_vgaPalette(m_attributeEgaPalette(0))
          Else
            m_pixels(index) = m_egaPalette(m_attributeEgaPalette(0))
          End If
        Next

      End Sub

      Friend Sub SetCgaAlternatePalette(enabled As Boolean)
        If enabled Then
          m_attributeEgaPalette(1) = 2
          m_attributeEgaPalette(2) = 4
          m_attributeEgaPalette(3) = 20
        Else
          m_attributeEgaPalette(1) = 3
          m_attributeEgaPalette(2) = 5
          m_attributeEgaPalette(3) = 63
        End If
        For index As Integer = 0 To m_colorAttributes.Length - 1
          Select Case m_colorAttributes(index)
            Case 0
            Case 1 : m_pixels(index) = m_egaPalette(m_attributeEgaPalette(1))
            Case 2 : m_pixels(index) = m_egaPalette(m_attributeEgaPalette(2))
            Case 3 : m_pixels(index) = m_egaPalette(m_attributeEgaPalette(3))
            Case Else
              Stop
          End Select
        Next
        m_cgaAlternatePalette = enabled
      End Sub

      Friend Sub SetPalette()

        Select Case m_mode
          Case 0

            ReDim m_attributeEgaPalette(31)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 1
            m_attributeEgaPalette(2) = 2
            m_attributeEgaPalette(3) = 3
            m_attributeEgaPalette(4) = 4
            m_attributeEgaPalette(5) = 5
            m_attributeEgaPalette(6) = 20
            m_attributeEgaPalette(7) = 7
            m_attributeEgaPalette(8) = 56
            m_attributeEgaPalette(9) = 57
            m_attributeEgaPalette(10) = 58
            m_attributeEgaPalette(11) = 59
            m_attributeEgaPalette(12) = 60
            m_attributeEgaPalette(13) = 61
            m_attributeEgaPalette(14) = 62
            m_attributeEgaPalette(15) = 63

            For index As Integer = 16 To 31
              m_attributeEgaPalette(index) = m_attributeEgaPalette(index - 16)
            Next

          Case 1

            ReDim m_attributeEgaPalette(3)

            If m_cgaAlternatePalette Then
              m_attributeEgaPalette(0) = 0
              m_attributeEgaPalette(1) = 2
              m_attributeEgaPalette(2) = 4
              m_attributeEgaPalette(3) = 20
            Else
              m_attributeEgaPalette(0) = 0
              m_attributeEgaPalette(1) = 3
              m_attributeEgaPalette(2) = 5
              m_attributeEgaPalette(3) = 63
            End If
            'For index As Integer = 4 To 31
            '  m_attributeEgaPalette(index) = -1
            'Next

          Case 2

            ReDim m_attributeEgaPalette(1)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 63
            'For index As Integer = 2 To 31
            '  m_attributeEgaPalette(index) = -1
            'Next

          Case 3 ' TANDY 1000 160x200 16 colors

            ReDim m_attributeEgaPalette(15)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 1
            m_attributeEgaPalette(2) = 2
            m_attributeEgaPalette(3) = 3
            m_attributeEgaPalette(4) = 4
            m_attributeEgaPalette(5) = 5
            m_attributeEgaPalette(6) = 20
            m_attributeEgaPalette(7) = 7
            m_attributeEgaPalette(8) = 56
            m_attributeEgaPalette(9) = 57
            m_attributeEgaPalette(10) = 58
            m_attributeEgaPalette(11) = 59
            m_attributeEgaPalette(12) = 60
            m_attributeEgaPalette(13) = 61
            m_attributeEgaPalette(14) = 62
            m_attributeEgaPalette(15) = 63

          Case 4 ' TANDY 1000 320x200 4 colors

            ReDim m_attributeEgaPalette(3)

            If m_cgaAlternatePalette Then
              m_attributeEgaPalette(0) = 0
              m_attributeEgaPalette(1) = 2
              m_attributeEgaPalette(2) = 4
              m_attributeEgaPalette(3) = 20
            Else
              m_attributeEgaPalette(0) = 0
              m_attributeEgaPalette(1) = 3
              m_attributeEgaPalette(2) = 5
              m_attributeEgaPalette(3) = 63
            End If

          Case 5 ' TANDY 1000 320x200 16 colors

            ReDim m_attributeEgaPalette(15)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 1
            m_attributeEgaPalette(2) = 2
            m_attributeEgaPalette(3) = 3
            m_attributeEgaPalette(4) = 4
            m_attributeEgaPalette(5) = 5
            m_attributeEgaPalette(6) = 20
            m_attributeEgaPalette(7) = 7
            m_attributeEgaPalette(8) = 56
            m_attributeEgaPalette(9) = 57
            m_attributeEgaPalette(10) = 58
            m_attributeEgaPalette(11) = 59
            m_attributeEgaPalette(12) = 60
            m_attributeEgaPalette(13) = 61
            m_attributeEgaPalette(14) = 62
            m_attributeEgaPalette(15) = 63

            'For index As Integer = 16 To 31
            '  m_attributeEgaPalette(index) = -1
            'Next

          Case 6 ' TANDY 1000 640x200 4 colors

            ReDim m_attributeEgaPalette(3)

            If m_cgaAlternatePalette Then
              m_attributeEgaPalette(0) = 0
              m_attributeEgaPalette(1) = 2
              m_attributeEgaPalette(2) = 4
              m_attributeEgaPalette(3) = 20
            Else
              m_attributeEgaPalette(0) = 0
              m_attributeEgaPalette(1) = 3
              m_attributeEgaPalette(2) = 5
              m_attributeEgaPalette(3) = 63
            End If

          Case 7

            ReDim m_attributeEgaPalette(15)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 1
            m_attributeEgaPalette(2) = 2
            m_attributeEgaPalette(3) = 3
            m_attributeEgaPalette(4) = 4
            m_attributeEgaPalette(5) = 5
            m_attributeEgaPalette(6) = 20
            m_attributeEgaPalette(7) = 7
            m_attributeEgaPalette(8) = 56
            m_attributeEgaPalette(9) = 57
            m_attributeEgaPalette(10) = 58
            m_attributeEgaPalette(11) = 59
            m_attributeEgaPalette(12) = 60
            m_attributeEgaPalette(13) = 61
            m_attributeEgaPalette(14) = 62
            m_attributeEgaPalette(15) = 63

            'For index As Integer = 16 To 31
            '  m_attributeEgaPalette(index) = -1
            'Next

          Case 8

            ReDim m_attributeEgaPalette(15)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 1
            m_attributeEgaPalette(2) = 2
            m_attributeEgaPalette(3) = 3
            m_attributeEgaPalette(4) = 4
            m_attributeEgaPalette(5) = 5
            m_attributeEgaPalette(6) = 20
            m_attributeEgaPalette(7) = 7
            m_attributeEgaPalette(8) = 56
            m_attributeEgaPalette(9) = 57
            m_attributeEgaPalette(10) = 58
            m_attributeEgaPalette(11) = 59
            m_attributeEgaPalette(12) = 60
            m_attributeEgaPalette(13) = 61
            m_attributeEgaPalette(14) = 62
            m_attributeEgaPalette(15) = 63

            'For index As Integer = 16 To 31
            '  m_attributeEgaPalette(index) = -1
            'Next

          Case 9

            ReDim m_attributeEgaPalette(15)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 1
            m_attributeEgaPalette(2) = 2
            m_attributeEgaPalette(3) = 3
            m_attributeEgaPalette(4) = 4
            m_attributeEgaPalette(5) = 5
            m_attributeEgaPalette(6) = 20
            m_attributeEgaPalette(7) = 7
            m_attributeEgaPalette(8) = 56
            m_attributeEgaPalette(9) = 57
            m_attributeEgaPalette(10) = 58
            m_attributeEgaPalette(11) = 59
            m_attributeEgaPalette(12) = 60
            m_attributeEgaPalette(13) = 61
            m_attributeEgaPalette(14) = 62
            m_attributeEgaPalette(15) = 63

            'For index As Integer = 16 To 31
            '  m_attributeEgaPalette(index) = -1
            'Next

          Case 10

            ReDim m_attributeEgaPalette(3)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 3
            m_attributeEgaPalette(2) = 5
            m_attributeEgaPalette(3) = 63

          Case 11

            ReDim m_attributeEgaPalette(1)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 15

          Case 12

            ReDim m_attributeEgaPalette(15)

            m_attributeEgaPalette(0) = 0
            m_attributeEgaPalette(1) = 1
            m_attributeEgaPalette(2) = 2
            m_attributeEgaPalette(3) = 3
            m_attributeEgaPalette(4) = 4
            m_attributeEgaPalette(5) = 5
            m_attributeEgaPalette(6) = 6
            m_attributeEgaPalette(7) = 7
            m_attributeEgaPalette(8) = 8
            m_attributeEgaPalette(9) = 9
            m_attributeEgaPalette(10) = 10
            m_attributeEgaPalette(11) = 11
            m_attributeEgaPalette(12) = 12
            m_attributeEgaPalette(13) = 13
            m_attributeEgaPalette(14) = 14
            m_attributeEgaPalette(15) = 15

          Case 13

            ReDim m_attributeEgaPalette(255)

            For index = 0 To 255
              m_attributeEgaPalette(index) = index
            Next

            '' how to calculate the color using RGB.
            'Dim blue = 63 ' 0 to 63
            'Dim green = 63 ' 0 to 63
            'Dim red = 63 ' 0 to 63
            'Dim value = (65536 * blue) + (256 * green) + red

          Case Else
            Stop

        End Select

      End Sub

      Friend Sub SetPalette(attribute As Integer, color As Integer)

        ' Translate MODE color to EGA palette index and update attributeEgaPalette lookup table.

        Dim value As Integer

        Select Case m_mode
          Case 0

            Select Case color
              Case 0 : value = 0
              Case 1 : value = 1
              Case 2 : value = 2
              Case 3 : value = 3
              Case 4 : value = 4
              Case 5 : value = 5
              Case 6 : value = 20
              Case 7 : value = 7
              Case 8 : value = 56
              Case 9 : value = 57
              Case 10 : value = 58
              Case 11 : value = 59
              Case 12 : value = 60
              Case 13 : value = 61
              Case 14 : value = 62
              Case 15 : value = 63
              Case 16 : value = 0
              Case 17 : value = 1
              Case 18 : value = 2
              Case 19 : value = 3
              Case 20 : value = 4
              Case 21 : value = 5
              Case 22 : value = 20
              Case 23 : value = 7
              Case 24 : value = 56
              Case 25 : value = 57
              Case 26 : value = 58
              Case 27 : value = 59
              Case 28 : value = 60
              Case 29 : value = 61
              Case 30 : value = 62
              Case 31 : value = 63
              Case Else
                Stop
            End Select

          Case 1

            If attribute = 0 Then

              Select Case color
                Case 0 : value = 0
                Case 1 : value = 1
                Case 2 : value = 2
                Case 3 : value = 3
                Case 4 : value = 4
                Case 5 : value = 5
                Case 6 : value = 20
                Case 7 : value = 7
                Case 8 : value = 56
                Case 9 : value = 57
                Case 10 : value = 58
                Case 11 : value = 59
                Case 12 : value = 60
                Case 13 : value = 61
                Case 14 : value = 62
                Case 15 : value = 63
                Case 16 : value = 0
                Case 17 : value = 1
                Case 18 : value = 2
                Case 19 : value = 3
                Case 20 : value = 4
                Case 21 : value = 5
                Case 22 : value = 20
                Case 23 : value = 7
                Case 24 : value = 56
                Case 25 : value = 57
                Case 26 : value = 58
                Case 27 : value = 59
                Case 28 : value = 60
                Case 29 : value = 61
                Case 30 : value = 62
                Case 31 : value = 63
                Case Else
                  Stop
              End Select

            Else

              If m_cgaAlternatePalette Then
                Select Case color
                  Case 0 : value = 0
                  Case 1 : value = 2
                  Case 2 : value = 4
                  Case 3 : value = 20
                  Case Else
                    Stop
                End Select
              Else
                Select Case color
                  Case 0 : value = 0
                  Case 1 : value = 3
                  Case 2 : value = 5
                  Case 3 : value = 63
                  Case Else
                    value = color
                End Select
              End If

            End If

          Case 2

            Select Case color
              Case 0 : value = 0
              Case 1 : value = 63
              Case Else
                Stop
            End Select

          Case 3 ' TANDY 1000 160x200 16 colors

            Select Case color
              Case 0 : value = 0
              Case 1 : value = 1
              Case 2 : value = 2
              Case 3 : value = 3
              Case 4 : value = 4
              Case 5 : value = 5
              Case 6 : value = 20
              Case 7 : value = 7
              Case 8 : value = 56
              Case 9 : value = 57
              Case 10 : value = 58
              Case 11 : value = 59
              Case 12 : value = 60
              Case 13 : value = 61
              Case 14 : value = 62
              Case 15 : value = 63
              Case Else
                value = color
            End Select

          Case 4 ' TANDY 1000 320x200 4 colors

            Select Case color
              Case 0 : value = 0
              Case 1 : value = 3
              Case 2 : value = 5
              Case 3 : value = 63
              Case Else
                value = color
            End Select

          Case 5 ' TANDY 1000 320x200 16 colors

            Select Case color
              Case 0 : value = 0
              Case 1 : value = 1
              Case 2 : value = 2
              Case 3 : value = 3
              Case 4 : value = 4
              Case 5 : value = 5
              Case 6 : value = 20
              Case 7 : value = 7
              Case 8 : value = 56
              Case 9 : value = 57
              Case 10 : value = 58
              Case 11 : value = 59
              Case 12 : value = 60
              Case 13 : value = 61
              Case 14 : value = 62
              Case 15 : value = 63
              Case Else
                value = color
            End Select

          Case 6 ' TANDY 1000 640x200 4 colors

            Select Case color
              Case 0 : value = 0
              Case 1 : value = 3
              Case 2 : value = 5
              Case 3 : value = 63
              Case Else
                value = color
            End Select

          Case 7

            Select Case color
              Case 0 : value = 0
              Case 1 : value = 1
              Case 2 : value = 2
              Case 3 : value = 3
              Case 4 : value = 4
              Case 5 : value = 5
              Case 6 : value = 20
              Case 7 : value = 7
              Case 8 : value = 56
              Case 9 : value = 57
              Case 10 : value = 58
              Case 11 : value = 59
              Case 12 : value = 60
              Case 13 : value = 61
              Case 14 : value = 62
              Case 15 : value = 63
              Case Else
                value = color
            End Select

          Case 8

            Select Case color
              Case 0 : value = 0
              Case 1 : value = 1
              Case 2 : value = 2
              Case 3 : value = 3
              Case 4 : value = 4
              Case 5 : value = 5
              Case 6 : value = 20
              Case 7 : value = 7
              Case 8 : value = 56
              Case 9 : value = 57
              Case 10 : value = 58
              Case 11 : value = 59
              Case 12 : value = 60
              Case 13 : value = 61
              Case 14 : value = 62
              Case 15 : value = 63
              Case Else
                value = color
            End Select

          Case 9

            Select Case color
              Case 0 : value = 0
              Case 1 : value = 1
              Case 2 : value = 2
              Case 3 : value = 3
              Case 4 : value = 4
              Case 5 : value = 5
              Case 6 : value = 20
              Case 7 : value = 7
              Case 8 : value = 56
              Case 9 : value = 57
              Case 10 : value = 58
              Case 11 : value = 59
              Case 12 : value = 60
              Case 13 : value = 61
              Case 14 : value = 62
              Case 15 : value = 63
              Case Else
                value = color
            End Select

          Case Else
            Stop
        End Select

        m_attributeEgaPalette(attribute) = value

        For index As Integer = 0 To m_colorAttributes.Length - 1
          If m_colorAttributes(index) = attribute Then
            If m_mode = 11 OrElse m_mode = 12 OrElse m_mode = 13 Then
              m_pixels(index) = m_vgaPalette(m_attributeEgaPalette(attribute))
            Else
              m_pixels(index) = m_egaPalette(m_attributeEgaPalette(attribute))
            End If
          End If
        Next

      End Sub

      Friend ReadOnly Property RowCount As Integer
        Get
          Return m_rowCount
        End Get
      End Property

      Friend ReadOnly Property ColumnCount As Integer
        Get
          Return m_columnCount
        End Get
      End Property

      Private Shared Sub InitializeVgaPalette()

        m_vgaPalette(0) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_vgaPalette(1) = ConvertToARGB32(255, &H0, &H0, &HAA)
        m_vgaPalette(2) = ConvertToARGB32(255, &H0, &HAA, &H0)
        m_vgaPalette(3) = ConvertToARGB32(255, &H0, &HAA, &HAA)
        m_vgaPalette(4) = ConvertToARGB32(255, &HAA, &H0, &H0)
        m_vgaPalette(5) = ConvertToARGB32(255, &HAA, &H0, &HAA)
        m_vgaPalette(6) = ConvertToARGB32(255, &HAA, &H55, &H0)
        m_vgaPalette(7) = ConvertToARGB32(255, &HAA, &HAA, &HAA)
        m_vgaPalette(8) = ConvertToARGB32(255, &H55, &H55, &H55)
        m_vgaPalette(9) = ConvertToARGB32(255, &H55, &H55, &HFF)
        m_vgaPalette(10) = ConvertToARGB32(255, &H55, &HFF, &H55)
        m_vgaPalette(11) = ConvertToARGB32(255, &H55, &HFF, &HFF)
        m_vgaPalette(12) = ConvertToARGB32(255, &HFF, &H55, &H55)
        m_vgaPalette(13) = ConvertToARGB32(255, &HFF, &H55, &HFF)
        m_vgaPalette(14) = ConvertToARGB32(255, &HFF, &HFF, &H55)
        m_vgaPalette(15) = ConvertToARGB32(255, &HFF, &HFF, &HFF)
        m_vgaPalette(16) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_vgaPalette(17) = ConvertToARGB32(255, &H14, &H14, &H14)
        m_vgaPalette(18) = ConvertToARGB32(255, &H20, &H20, &H20)
        m_vgaPalette(19) = ConvertToARGB32(255, &H2C, &H2C, &H2C)
        m_vgaPalette(20) = ConvertToARGB32(255, &H38, &H38, &H38)
        m_vgaPalette(21) = ConvertToARGB32(255, &H45, &H45, &H45)
        m_vgaPalette(22) = ConvertToARGB32(255, &H51, &H51, &H51)
        m_vgaPalette(23) = ConvertToARGB32(255, &H61, &H61, &H61)
        m_vgaPalette(24) = ConvertToARGB32(255, &H71, &H71, &H71)
        m_vgaPalette(25) = ConvertToARGB32(255, &H82, &H82, &H82)
        m_vgaPalette(26) = ConvertToARGB32(255, &H92, &H92, &H92)
        m_vgaPalette(27) = ConvertToARGB32(255, &HA2, &HA2, &HA2)
        m_vgaPalette(28) = ConvertToARGB32(255, &HB6, &HB6, &HB6)
        m_vgaPalette(29) = ConvertToARGB32(255, &HCB, &HCB, &HCB)
        m_vgaPalette(30) = ConvertToARGB32(255, &HE3, &HE3, &HE3)
        m_vgaPalette(31) = ConvertToARGB32(255, &HFF, &HFF, &HFF)
        m_vgaPalette(32) = ConvertToARGB32(255, &H0, &H0, &HFF)
        m_vgaPalette(33) = ConvertToARGB32(255, &H41, &H0, &HFF)
        m_vgaPalette(34) = ConvertToARGB32(255, &H7D, &H0, &HFF)
        m_vgaPalette(35) = ConvertToARGB32(255, &HBE, &H0, &HFF)
        m_vgaPalette(36) = ConvertToARGB32(255, &HFF, &H0, &HFF)
        m_vgaPalette(37) = ConvertToARGB32(255, &HFF, &H0, &HBE)
        m_vgaPalette(38) = ConvertToARGB32(255, &HFF, &H0, &H7D)
        m_vgaPalette(39) = ConvertToARGB32(255, &HFF, &H0, &H41)
        m_vgaPalette(40) = ConvertToARGB32(255, &HFF, &H0, &H0)
        m_vgaPalette(41) = ConvertToARGB32(255, &HFF, &H41, &H0)
        m_vgaPalette(42) = ConvertToARGB32(255, &HFF, &H7D, &H0)
        m_vgaPalette(43) = ConvertToARGB32(255, &HFF, &HBE, &H0)
        m_vgaPalette(44) = ConvertToARGB32(255, &HFF, &HFF, &H0)
        m_vgaPalette(45) = ConvertToARGB32(255, &HBE, &HFF, &H0)
        m_vgaPalette(46) = ConvertToARGB32(255, &H7D, &HFF, &H0)
        m_vgaPalette(47) = ConvertToARGB32(255, &H41, &HFF, &H0)
        m_vgaPalette(48) = ConvertToARGB32(255, &H0, &HFF, &H0)
        m_vgaPalette(49) = ConvertToARGB32(255, &H0, &HFF, &H41)
        m_vgaPalette(50) = ConvertToARGB32(255, &H0, &HFF, &H7D)
        m_vgaPalette(51) = ConvertToARGB32(255, &H0, &HFF, &HBE)
        m_vgaPalette(52) = ConvertToARGB32(255, &H0, &HFF, &HFF)
        m_vgaPalette(53) = ConvertToARGB32(255, &H0, &HBE, &HFF)
        m_vgaPalette(54) = ConvertToARGB32(255, &H0, &H7D, &HFF)
        m_vgaPalette(55) = ConvertToARGB32(255, &H0, &H41, &HFF)
        m_vgaPalette(56) = ConvertToARGB32(255, &H7D, &H7D, &HFF)
        m_vgaPalette(57) = ConvertToARGB32(255, &H9E, &H7D, &HFF)
        m_vgaPalette(58) = ConvertToARGB32(255, &HBE, &H7D, &HFF)
        m_vgaPalette(59) = ConvertToARGB32(255, &HDF, &H7D, &HFF)
        m_vgaPalette(60) = ConvertToARGB32(255, &HFF, &H7D, &HFF)
        m_vgaPalette(61) = ConvertToARGB32(255, &HFF, &H7D, &HDF)
        m_vgaPalette(62) = ConvertToARGB32(255, &HFF, &H7D, &HBE)
        m_vgaPalette(63) = ConvertToARGB32(255, &HFF, &H7D, &H9E)
        m_vgaPalette(64) = ConvertToARGB32(255, &HFF, &H7D, &H7D)
        m_vgaPalette(65) = ConvertToARGB32(255, &HFF, &H9E, &H7D)
        m_vgaPalette(66) = ConvertToARGB32(255, &HFF, &HBE, &H7D)
        m_vgaPalette(67) = ConvertToARGB32(255, &HFF, &HDF, &H7D)
        m_vgaPalette(68) = ConvertToARGB32(255, &HFF, &HFF, &H7D)
        m_vgaPalette(69) = ConvertToARGB32(255, &HDF, &HFF, &H7D)
        m_vgaPalette(70) = ConvertToARGB32(255, &HBE, &HFF, &H7D)
        m_vgaPalette(71) = ConvertToARGB32(255, &H9E, &HFF, &H7D)
        m_vgaPalette(72) = ConvertToARGB32(255, &H7D, &HFF, &H7D)
        m_vgaPalette(73) = ConvertToARGB32(255, &H7D, &HFF, &H9E)
        m_vgaPalette(74) = ConvertToARGB32(255, &H7D, &HFF, &HBE)
        m_vgaPalette(75) = ConvertToARGB32(255, &H7D, &HFF, &HDF)
        m_vgaPalette(76) = ConvertToARGB32(255, &H7D, &HFF, &HFF)
        m_vgaPalette(77) = ConvertToARGB32(255, &H7D, &HDF, &HFF)
        m_vgaPalette(78) = ConvertToARGB32(255, &H7D, &HBE, &HFF)
        m_vgaPalette(79) = ConvertToARGB32(255, &H7D, &H9E, &HFF)
        m_vgaPalette(80) = ConvertToARGB32(255, &HB6, &HB6, &HFF)
        m_vgaPalette(81) = ConvertToARGB32(255, &HC7, &HB6, &HFF)
        m_vgaPalette(82) = ConvertToARGB32(255, &HDB, &HB6, &HFF)
        m_vgaPalette(83) = ConvertToARGB32(255, &HEB, &HB6, &HFF)
        m_vgaPalette(84) = ConvertToARGB32(255, &HFF, &HB6, &HFF)
        m_vgaPalette(85) = ConvertToARGB32(255, &HFF, &HB6, &HEB)
        m_vgaPalette(86) = ConvertToARGB32(255, &HFF, &HB6, &HDB)
        m_vgaPalette(87) = ConvertToARGB32(255, &HFF, &HB6, &HC7)
        m_vgaPalette(88) = ConvertToARGB32(255, &HFF, &HB6, &HB6)
        m_vgaPalette(89) = ConvertToARGB32(255, &HFF, &HC7, &HB6)
        m_vgaPalette(90) = ConvertToARGB32(255, &HFF, &HDB, &HB6)
        m_vgaPalette(91) = ConvertToARGB32(255, &HFF, &HEB, &HB6)
        m_vgaPalette(92) = ConvertToARGB32(255, &HFF, &HFF, &HB6)
        m_vgaPalette(93) = ConvertToARGB32(255, &HEB, &HFF, &HB6)
        m_vgaPalette(94) = ConvertToARGB32(255, &HDB, &HFF, &HB6)
        m_vgaPalette(95) = ConvertToARGB32(255, &HC7, &HFF, &HB6)
        m_vgaPalette(96) = ConvertToARGB32(255, &HB6, &HFF, &HB6)
        m_vgaPalette(97) = ConvertToARGB32(255, &HB6, &HFF, &HC7)
        m_vgaPalette(98) = ConvertToARGB32(255, &HB6, &HFF, &HDB)
        m_vgaPalette(99) = ConvertToARGB32(255, &HB6, &HFF, &HEB)
        m_vgaPalette(100) = ConvertToARGB32(255, &HB6, &HFF, &HFF)
        m_vgaPalette(101) = ConvertToARGB32(255, &HB6, &HEB, &HFF)
        m_vgaPalette(102) = ConvertToARGB32(255, &HB6, &HDB, &HFF)
        m_vgaPalette(103) = ConvertToARGB32(255, &HB6, &HC7, &HFF)
        m_vgaPalette(104) = ConvertToARGB32(255, &H0, &H0, &H71)
        m_vgaPalette(105) = ConvertToARGB32(255, &H1C, &H0, &H71)
        m_vgaPalette(106) = ConvertToARGB32(255, &H38, &H0, &H71)
        m_vgaPalette(107) = ConvertToARGB32(255, &H55, &H0, &H71)
        m_vgaPalette(108) = ConvertToARGB32(255, &H71, &H0, &H71)
        m_vgaPalette(109) = ConvertToARGB32(255, &H71, &H0, &H55)
        m_vgaPalette(110) = ConvertToARGB32(255, &H71, &H0, &H38)
        m_vgaPalette(111) = ConvertToARGB32(255, &H71, &H0, &H1C)
        m_vgaPalette(112) = ConvertToARGB32(255, &H71, &H0, &H0)
        m_vgaPalette(113) = ConvertToARGB32(255, &H71, &H1C, &H0)
        m_vgaPalette(114) = ConvertToARGB32(255, &H71, &H38, &H0)
        m_vgaPalette(115) = ConvertToARGB32(255, &H71, &H55, &H0)
        m_vgaPalette(116) = ConvertToARGB32(255, &H71, &H71, &H0)
        m_vgaPalette(117) = ConvertToARGB32(255, &H55, &H71, &H0)
        m_vgaPalette(118) = ConvertToARGB32(255, &H38, &H71, &H0)
        m_vgaPalette(119) = ConvertToARGB32(255, &H1C, &H71, &H0)
        m_vgaPalette(120) = ConvertToARGB32(255, &H0, &H71, &H0)
        m_vgaPalette(121) = ConvertToARGB32(255, &H0, &H71, &H1C)
        m_vgaPalette(122) = ConvertToARGB32(255, &H0, &H71, &H38)
        m_vgaPalette(123) = ConvertToARGB32(255, &H0, &H71, &H55)
        m_vgaPalette(124) = ConvertToARGB32(255, &H0, &H71, &H71)
        m_vgaPalette(125) = ConvertToARGB32(255, &H0, &H55, &H71)
        m_vgaPalette(126) = ConvertToARGB32(255, &H0, &H38, &H71)
        m_vgaPalette(127) = ConvertToARGB32(255, &H0, &H1C, &H71)
        m_vgaPalette(128) = ConvertToARGB32(255, &H38, &H38, &H71)
        m_vgaPalette(129) = ConvertToARGB32(255, &H45, &H38, &H71)
        m_vgaPalette(130) = ConvertToARGB32(255, &H55, &H38, &H71)
        m_vgaPalette(131) = ConvertToARGB32(255, &H61, &H38, &H71)
        m_vgaPalette(132) = ConvertToARGB32(255, &H71, &H38, &H71)
        m_vgaPalette(133) = ConvertToARGB32(255, &H71, &H38, &H61)
        m_vgaPalette(134) = ConvertToARGB32(255, &H71, &H38, &H55)
        m_vgaPalette(135) = ConvertToARGB32(255, &H71, &H38, &H45)
        m_vgaPalette(136) = ConvertToARGB32(255, &H71, &H38, &H38)
        m_vgaPalette(137) = ConvertToARGB32(255, &H71, &H45, &H38)
        m_vgaPalette(138) = ConvertToARGB32(255, &H71, &H55, &H38)
        m_vgaPalette(139) = ConvertToARGB32(255, &H71, &H61, &H38)
        m_vgaPalette(140) = ConvertToARGB32(255, &H71, &H71, &H38)
        m_vgaPalette(141) = ConvertToARGB32(255, &H61, &H71, &H38)
        m_vgaPalette(142) = ConvertToARGB32(255, &H55, &H71, &H38)
        m_vgaPalette(143) = ConvertToARGB32(255, &H45, &H71, &H38)
        m_vgaPalette(144) = ConvertToARGB32(255, &H38, &H71, &H38)
        m_vgaPalette(145) = ConvertToARGB32(255, &H38, &H71, &H45)
        m_vgaPalette(146) = ConvertToARGB32(255, &H38, &H71, &H55)
        m_vgaPalette(147) = ConvertToARGB32(255, &H38, &H71, &H61)
        m_vgaPalette(148) = ConvertToARGB32(255, &H38, &H71, &H71)
        m_vgaPalette(149) = ConvertToARGB32(255, &H38, &H61, &H71)
        m_vgaPalette(150) = ConvertToARGB32(255, &H38, &H55, &H71)
        m_vgaPalette(151) = ConvertToARGB32(255, &H38, &H45, &H71)
        m_vgaPalette(152) = ConvertToARGB32(255, &H51, &H51, &H71)
        m_vgaPalette(153) = ConvertToARGB32(255, &H59, &H51, &H71)
        m_vgaPalette(154) = ConvertToARGB32(255, &H61, &H51, &H71)
        m_vgaPalette(155) = ConvertToARGB32(255, &H69, &H51, &H71)
        m_vgaPalette(156) = ConvertToARGB32(255, &H71, &H51, &H71)
        m_vgaPalette(157) = ConvertToARGB32(255, &H71, &H51, &H69)
        m_vgaPalette(158) = ConvertToARGB32(255, &H71, &H51, &H61)
        m_vgaPalette(159) = ConvertToARGB32(255, &H71, &H51, &H59)
        m_vgaPalette(160) = ConvertToARGB32(255, &H71, &H51, &H51)
        m_vgaPalette(161) = ConvertToARGB32(255, &H71, &H59, &H51)
        m_vgaPalette(162) = ConvertToARGB32(255, &H71, &H61, &H51)
        m_vgaPalette(163) = ConvertToARGB32(255, &H71, &H69, &H51)
        m_vgaPalette(164) = ConvertToARGB32(255, &H71, &H71, &H51)
        m_vgaPalette(165) = ConvertToARGB32(255, &H69, &H71, &H51)
        m_vgaPalette(166) = ConvertToARGB32(255, &H61, &H71, &H51)
        m_vgaPalette(167) = ConvertToARGB32(255, &H59, &H71, &H51)
        m_vgaPalette(168) = ConvertToARGB32(255, &H51, &H71, &H51)
        m_vgaPalette(169) = ConvertToARGB32(255, &H51, &H71, &H59)
        m_vgaPalette(170) = ConvertToARGB32(255, &H51, &H71, &H61)
        m_vgaPalette(171) = ConvertToARGB32(255, &H51, &H71, &H69)
        m_vgaPalette(172) = ConvertToARGB32(255, &H51, &H71, &H71)
        m_vgaPalette(173) = ConvertToARGB32(255, &H51, &H69, &H71)
        m_vgaPalette(174) = ConvertToARGB32(255, &H51, &H61, &H71)
        m_vgaPalette(175) = ConvertToARGB32(255, &H51, &H59, &H71)
        m_vgaPalette(176) = ConvertToARGB32(255, &H0, &H0, &H41)
        m_vgaPalette(177) = ConvertToARGB32(255, &H10, &H0, &H41)
        m_vgaPalette(178) = ConvertToARGB32(255, &H20, &H0, &H41)
        m_vgaPalette(179) = ConvertToARGB32(255, &H30, &H0, &H41)
        m_vgaPalette(180) = ConvertToARGB32(255, &H41, &H0, &H41)
        m_vgaPalette(181) = ConvertToARGB32(255, &H41, &H0, &H30)
        m_vgaPalette(182) = ConvertToARGB32(255, &H41, &H0, &H20)
        m_vgaPalette(183) = ConvertToARGB32(255, &H41, &H0, &H10)
        m_vgaPalette(184) = ConvertToARGB32(255, &H41, &H0, &H0)
        m_vgaPalette(185) = ConvertToARGB32(255, &H41, &H10, &H0)
        m_vgaPalette(186) = ConvertToARGB32(255, &H41, &H20, &H0)
        m_vgaPalette(187) = ConvertToARGB32(255, &H41, &H30, &H0)
        m_vgaPalette(188) = ConvertToARGB32(255, &H41, &H41, &H0)
        m_vgaPalette(189) = ConvertToARGB32(255, &H30, &H41, &H0)
        m_vgaPalette(190) = ConvertToARGB32(255, &H20, &H41, &H0)
        m_vgaPalette(191) = ConvertToARGB32(255, &H10, &H41, &H0)
        m_vgaPalette(192) = ConvertToARGB32(255, &H0, &H41, &H0)
        m_vgaPalette(193) = ConvertToARGB32(255, &H0, &H41, &H10)
        m_vgaPalette(194) = ConvertToARGB32(255, &H0, &H41, &H20)
        m_vgaPalette(195) = ConvertToARGB32(255, &H0, &H41, &H30)
        m_vgaPalette(196) = ConvertToARGB32(255, &H0, &H41, &H41)
        m_vgaPalette(197) = ConvertToARGB32(255, &H0, &H30, &H41)
        m_vgaPalette(198) = ConvertToARGB32(255, &H0, &H20, &H41)
        m_vgaPalette(199) = ConvertToARGB32(255, &H0, &H10, &H41)
        m_vgaPalette(200) = ConvertToARGB32(255, &H20, &H20, &H41)
        m_vgaPalette(201) = ConvertToARGB32(255, &H28, &H20, &H41)
        m_vgaPalette(202) = ConvertToARGB32(255, &H30, &H20, &H41)
        m_vgaPalette(203) = ConvertToARGB32(255, &H38, &H20, &H41)
        m_vgaPalette(204) = ConvertToARGB32(255, &H41, &H20, &H41)
        m_vgaPalette(205) = ConvertToARGB32(255, &H41, &H20, &H38)
        m_vgaPalette(206) = ConvertToARGB32(255, &H41, &H20, &H30)
        m_vgaPalette(207) = ConvertToARGB32(255, &H41, &H20, &H28)
        m_vgaPalette(208) = ConvertToARGB32(255, &H41, &H20, &H20)
        m_vgaPalette(209) = ConvertToARGB32(255, &H41, &H28, &H20)
        m_vgaPalette(210) = ConvertToARGB32(255, &H41, &H30, &H20)
        m_vgaPalette(211) = ConvertToARGB32(255, &H41, &H38, &H20)
        m_vgaPalette(212) = ConvertToARGB32(255, &H41, &H41, &H20)
        m_vgaPalette(213) = ConvertToARGB32(255, &H38, &H41, &H20)
        m_vgaPalette(214) = ConvertToARGB32(255, &H30, &H41, &H20)
        m_vgaPalette(215) = ConvertToARGB32(255, &H28, &H41, &H20)
        m_vgaPalette(216) = ConvertToARGB32(255, &H20, &H41, &H20)
        m_vgaPalette(217) = ConvertToARGB32(255, &H20, &H41, &H28)
        m_vgaPalette(218) = ConvertToARGB32(255, &H20, &H41, &H30)
        m_vgaPalette(219) = ConvertToARGB32(255, &H20, &H41, &H38)
        m_vgaPalette(220) = ConvertToARGB32(255, &H20, &H41, &H41)
        m_vgaPalette(221) = ConvertToARGB32(255, &H20, &H38, &H41)
        m_vgaPalette(222) = ConvertToARGB32(255, &H20, &H30, &H41)
        m_vgaPalette(223) = ConvertToARGB32(255, &H20, &H28, &H41)
        m_vgaPalette(224) = ConvertToARGB32(255, &H2C, &H2C, &H41)
        m_vgaPalette(225) = ConvertToARGB32(255, &H30, &H2C, &H41)
        m_vgaPalette(226) = ConvertToARGB32(255, &H34, &H2C, &H41)
        m_vgaPalette(227) = ConvertToARGB32(255, &H3C, &H2C, &H41)
        m_vgaPalette(228) = ConvertToARGB32(255, &H41, &H2C, &H41)
        m_vgaPalette(229) = ConvertToARGB32(255, &H41, &H2C, &H3C)
        m_vgaPalette(230) = ConvertToARGB32(255, &H41, &H2C, &H34)
        m_vgaPalette(231) = ConvertToARGB32(255, &H41, &H2C, &H30)
        m_vgaPalette(232) = ConvertToARGB32(255, &H41, &H2C, &H2C)
        m_vgaPalette(233) = ConvertToARGB32(255, &H41, &H30, &H2C)
        m_vgaPalette(234) = ConvertToARGB32(255, &H41, &H34, &H2C)
        m_vgaPalette(235) = ConvertToARGB32(255, &H41, &H3C, &H2C)
        m_vgaPalette(236) = ConvertToARGB32(255, &H41, &H41, &H2C)
        m_vgaPalette(237) = ConvertToARGB32(255, &H3C, &H41, &H2C)
        m_vgaPalette(238) = ConvertToARGB32(255, &H34, &H41, &H2C)
        m_vgaPalette(239) = ConvertToARGB32(255, &H30, &H41, &H2C)
        m_vgaPalette(240) = ConvertToARGB32(255, &H2C, &H41, &H2C)
        m_vgaPalette(241) = ConvertToARGB32(255, &H2C, &H41, &H30)
        m_vgaPalette(242) = ConvertToARGB32(255, &H2C, &H41, &H34)
        m_vgaPalette(243) = ConvertToARGB32(255, &H2C, &H41, &H3C)
        m_vgaPalette(244) = ConvertToARGB32(255, &H2C, &H41, &H41)
        m_vgaPalette(245) = ConvertToARGB32(255, &H2C, &H3C, &H41)
        m_vgaPalette(246) = ConvertToARGB32(255, &H2C, &H34, &H41)
        m_vgaPalette(247) = ConvertToARGB32(255, &H2C, &H30, &H41)
        m_vgaPalette(248) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_vgaPalette(249) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_vgaPalette(250) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_vgaPalette(251) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_vgaPalette(252) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_vgaPalette(253) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_vgaPalette(254) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_vgaPalette(255) = ConvertToARGB32(255, &H0, &H0, &H0)

      End Sub

      Private Shared Sub InitializeEgaPalette()

        m_egaPalette(0) = ConvertToARGB32(255, &H0, &H0, &H0)
        m_egaPalette(1) = ConvertToARGB32(255, &H0, &H0, &HAA)
        m_egaPalette(2) = ConvertToARGB32(255, &H0, &HAA, &H0)
        m_egaPalette(3) = ConvertToARGB32(255, &H0, &HAA, &HAA)
        m_egaPalette(4) = ConvertToARGB32(255, &HAA, &H0, &H0)
        m_egaPalette(5) = ConvertToARGB32(255, &HAA, &H0, &HAA)
        m_egaPalette(6) = ConvertToARGB32(255, &HAA, &HAA, &H0)
        m_egaPalette(7) = ConvertToARGB32(255, &HAA, &HAA, &HAA)
        m_egaPalette(8) = ConvertToARGB32(255, &H0, &H0, &H55)
        m_egaPalette(9) = ConvertToARGB32(255, &H0, &H0, &HFF)
        m_egaPalette(10) = ConvertToARGB32(255, &H0, &HAA, &H55)
        m_egaPalette(11) = ConvertToARGB32(255, &H0, &HAA, &HFF)
        m_egaPalette(12) = ConvertToARGB32(255, &HAA, &H0, &H55)
        m_egaPalette(13) = ConvertToARGB32(255, &HAA, &H0, &HFF)
        m_egaPalette(14) = ConvertToARGB32(255, &HAA, &HAA, &H55)
        m_egaPalette(15) = ConvertToARGB32(255, &HAA, &HAA, &HFF)
        m_egaPalette(16) = ConvertToARGB32(255, &H0, &H55, &H0)
        m_egaPalette(17) = ConvertToARGB32(255, &H0, &H55, &HAA)
        m_egaPalette(18) = ConvertToARGB32(255, &H0, &HFF, &H0)
        m_egaPalette(19) = ConvertToARGB32(255, &H0, &HFF, &HAA)
        m_egaPalette(20) = ConvertToARGB32(255, &HAA, &H55, &H0)
        m_egaPalette(21) = ConvertToARGB32(255, &HAA, &H55, &HAA)
        m_egaPalette(22) = ConvertToARGB32(255, &HAA, &HFF, &H0)
        m_egaPalette(23) = ConvertToARGB32(255, &HAA, &HFF, &HAA)
        m_egaPalette(24) = ConvertToARGB32(255, &H0, &H55, &H55)
        m_egaPalette(25) = ConvertToARGB32(255, &H0, &H55, &HFF)
        m_egaPalette(26) = ConvertToARGB32(255, &H0, &HFF, &H55)
        m_egaPalette(27) = ConvertToARGB32(255, &H0, &HFF, &HFF)
        m_egaPalette(28) = ConvertToARGB32(255, &HAA, &H55, &H55)
        m_egaPalette(29) = ConvertToARGB32(255, &HAA, &H55, &HFF)
        m_egaPalette(30) = ConvertToARGB32(255, &HAA, &HFF, &H55)
        m_egaPalette(31) = ConvertToARGB32(255, &HAA, &HFF, &HFF)
        m_egaPalette(32) = ConvertToARGB32(255, &H55, &H0, &H0)
        m_egaPalette(33) = ConvertToARGB32(255, &H55, &H0, &HAA)
        m_egaPalette(34) = ConvertToARGB32(255, &H55, &HAA, &H0)
        m_egaPalette(35) = ConvertToARGB32(255, &H55, &HAA, &HAA)
        m_egaPalette(36) = ConvertToARGB32(255, &HFF, &H0, &H0)
        m_egaPalette(37) = ConvertToARGB32(255, &HFF, &H0, &HAA)
        m_egaPalette(38) = ConvertToARGB32(255, &HFF, &HAA, &H0)
        m_egaPalette(39) = ConvertToARGB32(255, &HFF, &HAA, &HAA)
        m_egaPalette(40) = ConvertToARGB32(255, &H55, &H0, &H55)
        m_egaPalette(41) = ConvertToARGB32(255, &H55, &H0, &HFF)
        m_egaPalette(42) = ConvertToARGB32(255, &H55, &HAA, &H55)
        m_egaPalette(43) = ConvertToARGB32(255, &H55, &HAA, &HFF)
        m_egaPalette(44) = ConvertToARGB32(255, &HFF, &H0, &H55)
        m_egaPalette(45) = ConvertToARGB32(255, &HFF, &H0, &HFF)
        m_egaPalette(46) = ConvertToARGB32(255, &HFF, &HAA, &H55)
        m_egaPalette(47) = ConvertToARGB32(255, &HFF, &HAA, &HFF)
        m_egaPalette(48) = ConvertToARGB32(255, &H55, &H55, &H0)
        m_egaPalette(49) = ConvertToARGB32(255, &H55, &H55, &HAA)
        m_egaPalette(50) = ConvertToARGB32(255, &H55, &HFF, &H0)
        m_egaPalette(51) = ConvertToARGB32(255, &H55, &HFF, &HAA)
        m_egaPalette(52) = ConvertToARGB32(255, &HFF, &H55, &H0)
        m_egaPalette(53) = ConvertToARGB32(255, &HFF, &H55, &HAA)
        m_egaPalette(54) = ConvertToARGB32(255, &HFF, &HFF, &H0)
        m_egaPalette(55) = ConvertToARGB32(255, &HFF, &HFF, &HAA)
        m_egaPalette(56) = ConvertToARGB32(255, &H55, &H55, &H55)
        m_egaPalette(57) = ConvertToARGB32(255, &H55, &H55, &HFF)
        m_egaPalette(58) = ConvertToARGB32(255, &H55, &HFF, &H55)
        m_egaPalette(59) = ConvertToARGB32(255, &H55, &HFF, &HFF)
        m_egaPalette(60) = ConvertToARGB32(255, &HFF, &H55, &H55)
        m_egaPalette(61) = ConvertToARGB32(255, &HFF, &H55, &HFF)
        m_egaPalette(62) = ConvertToARGB32(255, &HFF, &HFF, &H55)
        m_egaPalette(63) = ConvertToARGB32(255, &HFF, &HFF, &HFF)

      End Sub

      Public Property CharacterBuffer As Byte()
        Get
          Return m_characterBuffer
        End Get
        Set(value As Byte())
          m_characterBuffer = value
        End Set
      End Property

      Friend Property CharacterAttribute As Byte()
        Get
          Return m_characterAttribute
        End Get
        Set(value As Byte())
          m_characterAttribute = value
        End Set
      End Property

      Friend ReadOnly Property Segments As List(Of Segment)
        Get
          Return m_segments
        End Get
      End Property

      Friend ReadOnly Property Pixels() As Integer()
        Get
          Return m_pixels
        End Get
      End Property

      Friend ReadOnly Property Colors() As Byte()
        Get
          Return m_colorAttributes
        End Get
      End Property

      'Public ReadOnly Property Width As Integer
      '  Get
      '    Return m_width
      '  End Get
      'End Property

      'Public ReadOnly Property Height As Integer
      '  Get
      '    Return m_height
      '  End Get
      'End Property

      Friend Property Point(index As Integer) As Byte
        Get
          Return m_colorAttributes(index)
        End Get
        Set(value As Byte)
          m_colorAttributes(index) = value
          If m_mode = 11 OrElse m_mode = 12 OrElse m_mode = 13 Then
            m_pixels(index) = m_vgaPalette(m_attributeEgaPalette(value))
          Else
            m_pixels(index) = m_egaPalette(m_attributeEgaPalette(value))
          End If
        End Set
      End Property

      Private Shared Function ConvertToARGB32(a As Integer, r As Integer, g As Integer, b As Integer) As Integer
        Return (a << 24) Or (r << 16) Or (g << 8) Or b
      End Function

    End Class

#Region "Upstream Implementation (Paint)"

    'Public ReadOnly Property Palette() As Integer()
    '  Get
    '    Return m_palette
    '  End Get
    'End Property

    Public Function Screen(row As Short, col As Short, z As Boolean) As Short

      Dim position As Short = GetLocation(row, col)

      If z Then
        Return m_page(m_activePage).CharacterAttribute(position)
      Else
        Return m_page(m_activePage).CharacterBuffer(position)
      End If

    End Function

    'Public ReadOnly Property Page(ByVal index As Integer) As List(Of Byte)
    '  Get
    '    Return m_page(index)
    '  End Get
    'End Property

    Public ReadOnly Property Page(index As Integer) As InternalPage
      Get
        Return m_page(index)
      End Get
    End Property

    'Public ReadOnly Property Colors(ByVal page As Integer) As Byte()
    '  Get
    '    Return m_page(page).Bytes
    '  End Get
    'End Property

    Public ReadOnly Property Pixels(page As Integer) As Integer()
      Get
        Return m_page(page).Pixels
      End Get
    End Property

    Public ReadOnly Property VisualPage As Integer
      Get
        Return m_visualPage
      End Get
    End Property

    Public ReadOnly Property ScreenWidth As Integer
      Get
        Return m_screenWidth
      End Get
    End Property

    Public ReadOnly Property ScreenHeight As Integer
      Get
        Return m_screenHeight
      End Get
    End Property

    '  Friend Function PaintX(x As Double, y As Double, fillStr As String, borderCol As UInt32, backgroundStr As String, passed As Int32)

    '    ' uses 2 buffers, a and b, and swaps between them for reading and creating
    '    Dim fillCol As UInt32 = 0 ' stub
    '    Dim a_n As UInt32 = 0
    '    'static uint16 *a_x=(uint16*)malloc(2*65536),*a_y=(uint16*)malloc(2*65536);
    '    Dim a_x(2 * 65535) As UInt16
    '    Dim a_y(2 * 65535) As UInt16
    '    'static uint8 *a_t=(uint8*)malloc(65536);
    '    Dim a_t(65536) As Byte
    '    Dim b_n As UInt32 = 0
    '    'static uint16 *b_x=(uint16*)malloc(2*65536),*b_y=(uint16*)malloc(2*65536);
    '    Dim b_x(2 * 65536) As UInt16
    '    Dim b_y(2 * 65536) As UInt16
    '    'static uint8 *b_t=(uint8*)malloc(65536);
    '    Dim b_t(65536) As Byte
    '    'static uint8 *done=(uint8*)calloc(640*480,1);
    '    Dim done(640 * 480, 1) As Byte
    '    Dim ix, iy, i, t, x2, y2 As Int32
    '    Dim offset As UInt32
    '    Dim cp As Byte
    '    Dim sp As UInt16
    '    Dim backgroundCol As UInt32

    '    If Me.ScreenMode = 0 Then Return 5
    '    'if (qbg_text_only){error(5); return;}
    '    'if ((passed&2)==0){error(5); return;}//must be called with this parameter!
    '    If (passed And 2) = 0 Then Return 5 ' must be called with this parameter!

    '    ' STEP 1: create the tile in a buffer (tile) using the source string

    '    Dim tilestr(256) As Byte
    '    Dim tile(8, 64) As Byte
    '    Dim sx, sy As Int32
    '    Dim bytesPerRow As Int32
    '    Dim row2Offset As Int32
    '    Dim row3Offset As Int32
    '    Dim row4Offset As Int32
    '    Dim [byte] As Int32
    '    Dim bitValue As Int32
    '    Dim c As Int32
    '    If fillStr.Length = 0 Then Return 5
    '    Dim bitsPerPixel = 1
    '    If bitsPerPixel = 4 Then
    '      If fillStr.Length > 256 Then Return 5
    '    Else
    '      If fillStr.Length > 64 Then Return 5
    '    End If
    '    'memset(&tilestr[0],0,256);
    '    'memcpy(&tilestr[0],fillstr->chr,fillstr->len);
    '    ReDim tilestr(256)

    '    sx = 8 : sy = fillStr.Length ' defaults
    '    If bitsPerPixel = 8 Then sx = 1
    '    'if (qbg_bits_per_pixel==8) sx=1;
    '    If bitsPerPixel = 4 Then
    '      If fillStr.Length Mod 3 Then
    '        sy = (fillStr.Length - (fillStr.Length Mod 3) + 4) >> 2
    '      Else
    '        sy = fillStr.Length >> 2
    '      End If
    '      bytesPerRow = sx >> 3 : If (sx Mod 7) Then bytesPerRow += 1
    '      row2Offset = bytesPerRow
    '      row3Offset = bytesPerRow * 2
    '      row4Offset = bytesPerRow * 3
    '    End If
    '    If bitsPerPixel = 2 Then sx = 4
    '    ' use modified "PUT" routine to create the tile
    '    'cp=&tilestr[0];
    '    Dim cpIndex As Integer = 0
    '    cp = tilestr(cpIndex)
    '    ' layer
    '    '  for (y=0;y<sy;y++)
    '    For y = 0 To sy - 1
    '      If bitsPerPixel = 4 Then
    '        bitValue = 128
    '        [byte] = 0
    '      End If
    '      For x = 0 To sx - 1
    '        ' get color
    '        If bitsPerPixel = 8 Then
    '          c = cp
    '          cpIndex += 1
    '        End If
    '        If bitsPerPixel = 4 Then
    '          [byte] = x >> 3
    '          c = 0
    '          If (tilestr([byte]) And bitValue) = bitValue Then c = c Or 1
    '          If (tilestr(row2Offset + [byte]) And bitValue) = bitValue Then c = c Or 2
    '          If (tilestr(row3Offset + [byte]) And bitValue) = bitValue Then c = c Or 4
    '          If (tilestr(row4Offset + [byte]) And bitValue) = bitValue Then c = c Or 8
    '          bitValue >>= 1 : If (bitValue = 0) Then bitValue = 128
    '        End If
    '        If bitsPerPixel = 1 Then
    '          If Not x And 7 Then
    '            [byte] = tilestr(cpIndex)
    '            cp += 1
    '          End If
    '          c = ([byte] & 128) >> 7 : [byte] <<= 1
    '        End If
    '        If bitsPerPixel = 2 Then
    '          If Not x And 3 Then
    '            [byte] = tilestr(cpIndex)
    '            cp += 1
    '          End If
    '          c = ([byte] And 192) >> 6 : [byte] <<= 2
    '        End If
    '        ' "pset" color
    '        tile(x, y) = c
    '      Next 'x
    '      If bitsPerPixel = 4 Then cp += (bytesPerRow * 4)
    '      If bitsPerPixel = 1 Then
    '        If sx And 7 Then cp += 1
    '      End If
    '      If bitsPerPixel = 2 Then
    '        If sx And 3 Then cp += 1
    '      End If
    '    Next 'y
    '    ' unlayer
    '    ' tile created!

    '    ' STEP 2: establish border and background colors

    '    'if ((passed&4)==0) bordercol=qbg_color;
    '    If (passed And 4) = 0 Then borderCol = Me.m_foreColor
    '    borderCol = m_borderColor And qbg_pixel_mask

    '    backgroundCol = 0 ' default
    '    'if (passed&8)
    '    '{
    '    If (passed And 8) = 8 Then
    '      If backgroundStr.Length = 0 Then Return 5
    '      If backgroundStr.Length > 255 Then Return 5
    '      If bitsPerPixel = 1 Then
    '        c = AscW(backgroundStr(0))
    '        If ((c > 0) AndAlso (c < 255)) Then backgroundCol = -1 ' unclear definition
    '        If c = 255 Then backgroundCol = 1
    '      End If
    '      If bitsPerPixel = 1 Then
    '        backgroundCol = -1 ' unclear definition
    '        x2 = AscW(backgroundStr(0))
    '        y2 = x2 And 3
    '        x2 >>= 2 : If ((x2 And 3) <> y2) Then GoTo uncleardef
    '        x2 >>= 2 : If ((x2 And 3) <> y2) Then GoTo uncleardef
    '        x2 >>= 2 : If ((x2 & 3) <> y2) Then GoTo uncleardef
    '        backgroundCol = y2
    '      End If
    '      If bitsPerPixel = 4 Then
    '        backgroundCol = -1 ' unclear definition
    '        y2 = 0
    '        x2 = 4 : If (backgroundStr.Length < 4) Then x2 = backgroundStr.Length
    '        'c=0; memcpy(&c,backgroundstr->chr,x2);
    '        x2 = c And 255 : c >>= 8 : If ((x2 <> 0) AndAlso (x2 <> 255)) Then GoTo uncleardef
    '        y2 = y2 Or (x2 And 1)
    '        x2 = c And 255 : c >>= 8 : If ((x2 <> 0) AndAlso (x2 <> 255)) Then GoTo uncleardef
    '        y2 = y2 Or ((x2 And 1) << 1)
    '        x2 = c And 255 : c >>= 8 : If ((x2 <> 0) AndAlso (x2 <> 255)) Then GoTo uncleardef
    '        y2 = y2 Or ((x2 And 1) << 2)
    '        x2=c and 255: c>>=8: if ((x2<>)andalso(x2<>255)) then goto uncleardef
    '        y2 = y2 Or ((x2 And 1) << 3)
    '        backgroundCol = y2
    '      End If
    '      If bitsPerPixel = 8 Then
    '        backgroundCol = AscW(backgroundStr(0))
    '      End If
    '    End If

    'uncleardef:

    '    ' STEP 3: perform tile'd fill

    '    If (passed And 1) = 1 Then
    '      qbg_x += x
    '      qbg_y += y
    '    Else
    '      qbg_x = x
    '      qbg_y = y
    '    End If

    '    If qbg_clipping_or_scaling Then
    '      If (qbg_clipping_or_scaling = 2) Then
    '        ix = CLng(qbg_x * qbg_scaling_x + qbg_scaling_offset_x) + qbg_view_offset_x
    '        iy = CLng(qbg_y * qbg_scaling_y + qbg_scaling_offset_y) + qbg_view_offset_y
    '      Else
    '        ix = CLng(qbg_x) + qbg_view_offset_x : iy = CLng(qbg_y) + qbg_view_offset_y
    '      End If
    '    Else
    '      ix = CLng(qbg_x) : iy = CLng(qbg_y)
    '    End If

    '    ' return if offscreen
    '    If ((ix < qbg_view_x1) OrElse (iy < qbg_view_y1) OrElse (ix > qbg_view_x2) OrElse (iy > qbg_view_y2)) Then
    '      Return True
    '    End If

    '    offset = iy * qbg_width + ix

    '    ' return if first point is the bordercolor
    '    If (m_page(m_activePage).Colors(offset) = borderCol) Then Return True

    '    ' return if first point is the same as the tile color used and is not the background color
    '    fillCol = tile(ix Mod sx, iy Mod sy)
    '    If ((fillCol = m_page(m_activePage).Colors(offset)) AndAlso (fillCol <> backgroundCol)) Then Return True
    '    m_page(m_activePage).Colors(offset) = fillCol

    '    ' create first node
    '    a_x(0) = ix : a_y(0) = iy
    '    a_t(0) = 15
    '    ' types:
    '    ' &1=check left
    '    ' &2=check right
    '    ' &4=check above
    '    ' &8=check below

    '    a_n = 1
    '    m_page(m_activePage).colors(iy * qbg_width + ix) = fillCol
    '    done(iy * qbg_width + ix) = 1

    'nextpass:
    '    b_n = 0
    '    For i = 0 To a_n - 1
    '      t = a_t(i) : ix = a_x(i) : iy = a_y(i)

    '      ' left
    '      if (t and 1) = 1 then 
    '        x2 = ix - 1 : y2 = iy
    '        If (x2 >= qbg_view_x1) Then
    '          offset = y2 * qbg_width + x2
    '          If (Not done(offset)) Then
    '            done([offset] = 1)
    '            If (m_page(m_activePage).Colors(offset) <> borderCol) Then
    '              fillCol = tile(x2 Mod sx, y2 Mod sy)
    '              ' no tile check required when moving horizontally!
    '              m_page(m_activePage).Colors(offset) = fillCol
    '              b_t(b_n) = 13 : b_x(b_n) = x2 : b_y(b_n) = y2 : b_n += 1 ' add new node
    '            End If
    '          End If
    '        End If
    '      End If

    '      ' right
    '      If (t And 2) = 2 Then
    '        x2 = ix + 1 : y2 = iy
    '        If (x2 <= qbg_view_x2) Then
    '          offset = y2 * qbg_width + x2
    '          If (Not done(offset)) Then
    '            done(offset) = 1
    '            If (m_page(m_activePage).Colors(offset) <> borderCol) Then
    '              fillCol = tile(x2 Mod sx, y2 Mod sy)
    '              ' no tile check required when moving horizontally!
    '              m_page(m_activePage).Colors(offset) = fillCol
    '              b_t(b_n) = 14 : b_x(b_n) = x2 : b_y(b_n) = y2 : b_n += 1 ' add new node
    '            End If
    '          End If
    '        End If
    '      End If

    '      ' above
    '      If (t And 4) = 4 Then
    '        x2 = ix : y2 = iy - 1
    '        If (y2 >= qbg_view_y1) Then
    '          offset = y2 * qbg_width + x2
    '          If (Not done(offset)) Then
    '            done(offset) = 1
    '            If (m_page(m_activePage).Colors(offset) <> borderCol) Then
    '              fillCol = tile(x2 Mod sx, y2 Mod sy)
    '              If ((fillCol <> m_page(m_activePage).Colors(offset)) OrElse (fillCol = backgroundCol)) Then
    '                m_page(m_activePage).Colors(offset) = fillCol
    '                b_t(b_n) = 7 : b_x(b_n) = x2 : b_y(b_n) = y2 : b_n += 1  ' add new node
    '              End If
    '            End If
    '          End If
    '        End If
    '      End If

    '      ' below
    '      If (t And 8) = 8 Then
    '        x2 = ix : y2 = iy + 1
    '        If (y2 <= qbg_view_y2) Then
    '          offset = y2 * qbg_width + x2
    '          If (Not done(offset)) Then
    '            done(offset) = 1
    '            If (m_page(m_activePage).Colors(offset) <> borderCol) Then
    '              fillCol = tile(x2 Mod sx, y2 Mod sy)
    '              If ((fillCol <> m_page(m_activePage).Colors(offset)) OrElse (fillCol = backgroundCol)) Then
    '                m_page(m_activePage).Colors(offset) = fillCol
    '                b_t(b_n) = 11 : b_x(b_n) = x2 : b_y(b_n) = y2 : b_n += 1 ' add new node
    '              End If
    '            End If
    '          End If
    '        End If
    '      End If

    '    Next 'i

    '    ' no new nodes?
    '    If (b_n = 0) Then
    '      'memset(done,0,qbg_width*qbg_height); 'cleanup
    '      Return True ' finished!
    '    End If

    '    ' swap a & b arrays
    '    sp = a_x : a_x = b_x : b_x = sp
    '    sp = a_y : a_y = b_y : b_y = sp
    '    cp = a_t : a_t = b_t : b_t = cp
    '    a_n = b_n

    '    GoTo nextpass

    '  End Function

    Friend Function Paint(x As Integer,
                          y As Integer,
                          paintParams As PaintParams) As Boolean

      If paintParams.BorderAttribute Is Nothing AndAlso
         paintParams.PaintAttribute IsNot Nothing Then
        paintParams.BorderAttribute = paintParams.PaintAttribute
      End If

      If paintParams.Border Is Nothing AndAlso
         paintParams.Paint IsNot Nothing Then
        paintParams.Border = paintParams.Paint
      End If

      Dim c As Short = CShort(If(paintParams.PaintAttribute, m_foreColor))
      Dim b As Short = CShort(If(paintParams.BorderAttribute, m_foreColor))

      Dim pattern(,) As Byte = Nothing ' Maximum 64 rows x 8 columns; depends on screen mode

      If paintParams.Paint IsNot Nothing Then

        ' Build a paint pattern.
        Select Case m_screenMode
          Case 1 ' Every two bits represents a color attribute.
            ReDim pattern(paintParams.Paint.Length - 1, 3) ' Every byte is 4 pixels, pattern is maximum 4 pixels wide.
            For row As Integer = 0 To paintParams.Paint.Length - 1
              For col As Integer = 0 To 3
                Dim check As Byte = CByte(3 << (col * 2))
                Dim attribute As Byte = CByte((AscW(paintParams.Paint(row)) And check) >> (col * 2))
                pattern(row, col) = attribute
              Next
            Next
          Case 2 ' Every bit represents a color attribute.
            ReDim pattern(paintParams.Paint.Length - 1, 7)
            For row As Integer = 0 To paintParams.Paint.Length - 1
              For col As Integer = 0 To 7
                Dim check As Byte = CByte(1 << (7 - col))
                Dim attribute As Byte = CByte(If((AscW(paintParams.Paint(row)) And check) = check, 1, 0))
                pattern(row, col) = attribute
              Next
            Next
          Case 7, 8, 9 ' Every bit represents 1/4 of a color attribute; spans across 4 bytes, one for each bit plane.

            If paintParams.Paint.Length Mod 4 > 0 Then
              ' If each row does not contain all 4 bytes, the missing bytes are treated as CHR$(0)
              paintParams.Paint &= "".PadRight(4 - (paintParams.Paint.Length Mod 4), ChrW(0))
            End If

            Dim rows = (paintParams.Paint.Length - 1) \ 4
            ReDim pattern(rows, 7)

            For row As Integer = 0 To rows
              For col As Integer = 0 To 7
                Dim check As Byte = CByte(1 << (7 - col))
                Dim bb As Integer = If((AscW(paintParams.Paint(row * 4)) And check) = check, 1, 0)
                Dim gg As Integer = If((AscW(paintParams.Paint((row * 4) + 1)) And check) = check, 1, 0)
                Dim rr As Integer = If((AscW(paintParams.Paint((row * 4) + 2)) And check) = check, 1, 0)
                Dim aa As Integer = If((AscW(paintParams.Paint((row * 4) + 3)) And check) = check, 1, 0)
                Dim attribute As Byte = CByte((bb * 1) + (gg * 2) + (rr * 4) + (aa * 8))
                pattern(row, col) = attribute
              Next
            Next

          Case Else
            ' Not implemented.
        End Select

      End If

      If Point(x, y) <> b Then
        If IsPointLegal(x, y) Then

          Dim done(m_screenHeight - 1, m_screenWidth - 1) As Boolean

          ' Start with the original point in the stack.
          Dim pts As New Generic.Stack(Of Drawing.Point)
          pts.Push(New Drawing.Point(x, y))

          Dim clr As Short '= c
          If pattern IsNot Nothing Then
            Dim rr As Integer = y
            Dim cc As Integer = x
            If rr > pattern.GetLength(0) - 1 Then
              rr = rr Mod pattern.GetLength(0)
            End If
            If cc > pattern.GetLength(1) - 1 Then
              cc = cc Mod pattern.GetLength(1)
            End If
            clr = pattern(rr, cc)
          Else
            clr = c
          End If
          Pset(x, y, clr) : done(y, x) = True

          ' While the stack is not empty, process a point.

          Do While pts.Count > 0
            Dim pt = pts.Pop
            If pt.X > 0 Then CheckPointBoundary(pts, pt.X - 1, pt.Y, c, b, pattern, done)
            If pt.Y > 0 Then CheckPointBoundary(pts, pt.X, pt.Y - 1, c, b, pattern, done)
            If pt.X < m_screenWidth - 1 Then CheckPointBoundary(pts, pt.X + 1, pt.Y, c, b, pattern, done)
            If pt.Y < m_screenHeight - 1 Then CheckPointBoundary(pts, pt.X, pt.Y + 1, c, b, pattern, done)
          Loop

        End If

      End If

      Return True

    End Function

    'Public Function Paint(ByVal x As Integer,
    '                      ByVal y As Integer,
    '                      ByVal c As Integer,
    '                      Optional ByVal b As Integer = -1) As Boolean

    '  If c = -1 Then
    '    c = m_foreColor
    '  End If

    '  If b = -1 Then

    '    ' Get the old and new colors components.

    '    'Dim index As Integer = (y * m_screenWidth) + x
    '    Dim oldColor As Integer = Me.Point(x, y)

    '    ' Start with the original point in the stack.
    '    Dim pts As New Generic.Stack(Of Point)
    '    pts.Push(New Point(x, y))
    '    Me.Pset(x, y, c)

    '    ' While the stack is not empty, process a point.

    '    Dim pixelCount As Integer = 1

    '    ' The CheckPoint version of this is a "flood fill" where it will fill the current point color with 
    '    ' the new color; stopping at any other colors.
    '    ' As it turns out, GW-BASIC does not work in this manner, it only stops at a "border color"; 
    '    ' overwriting any other color until encountered.
    '    Do While pts.Count > 0
    '      Dim pt = pts.Pop
    '      If pt.X > 0 Then CheckPoint(pts, pt.X - 1, pt.Y, oldColor, c)
    '      If pt.Y > 0 Then CheckPoint(pts, pt.X, pt.Y - 1, oldColor, c)
    '      If pt.X < m_screenWidth - 1 Then CheckPoint(pts, pt.X + 1, pt.Y, oldColor, c)
    '      If pt.Y < m_screenHeight - 1 Then CheckPoint(pts, pt.X, pt.Y + 1, oldColor, c)
    '    Loop

    '  Else

    '    ' Fill to boundary color...
    '    ' Moved this and built a new version of Paint that works more like GW-BASIC.

    '    If Me.Point(x, y) <> b Then

    '      Dim done As New List(Of Point)
    '      Dim pts As New Generic.Stack(Of Point)
    '      pts.Push(New Point(x, y))
    '      Me.Pset(x, y, c) : done.Add(New Point(x, y))

    '      Do While pts.Count > 0

    '        Dim pt = pts.Pop

    '        If pt.X > 0 Then CheckPointBoundary(pts, pt.X - 1, pt.Y, c, b)
    '        If pt.Y > 0 Then CheckPointBoundary(pts, pt.X, pt.Y - 1, c, b)
    '        If pt.X < m_screenWidth - 1 Then CheckPointBoundary(pts, pt.X + 1, pt.Y, c, b)
    '        If pt.Y < m_screenHeight - 1 Then CheckPointBoundary(pts, pt.X, pt.Y + 1, c, b)

    '      Loop

    '    End If

    '  End If

    '  Return True

    'End Function

    ' See if this point should be added to the stack.

    Private Function IsPointLegal(x As Integer, y As Integer) As Boolean

      If Not m_graphicView.IsScreen Then
        x += m_graphicView.Left
        y += m_graphicView.Top
      End If

      Return x.Between(m_graphicView.Left, m_graphicView.Right) AndAlso
             y.Between(m_graphicView.Top, m_graphicView.Bottom)

    End Function

    Private Sub CheckPointBoundary(ByRef pts As Generic.Stack(Of Drawing.Point),
x As Integer,
y As Integer,
newColor As Short,
boundaryColor As Short,
pattern As Byte(,),
done As Boolean(,))

      If Not done(y, x) AndAlso IsPointLegal(x, y) Then
        Dim c = Point(x, y)
        If c <> boundaryColor AndAlso
           c <> newColor Then
          pts.Push(New Drawing.Point(x, y))
          If pattern IsNot Nothing Then
            Dim rr As Integer = y
            Dim cc As Integer = x
            If rr > pattern.GetLength(0) - 1 Then
              rr = rr Mod pattern.GetLength(0)
            End If
            If cc > pattern.GetLength(1) - 1 Then
              cc = cc Mod pattern.GetLength(1)
            End If
            c = pattern(rr, cc)
          Else
            c = newColor
          End If
          Pset(x, y, c)
          done(y, x) = True
        End If
      End If

      'Dim found As Boolean = False
      'For index As Integer = 0 To done.Count - 1
      '  If done(index).X = x AndAlso done(index).Y = y Then
      '    found = True
      '    Exit For
      '  End If
      'Next

      'If Not found AndAlso Me.Point(x, y) <> boundaryColor Then
      '  pts.Push(New Point(x, y))
      '  Me.Pset(x, y, newColor)
      '  done.Add(New Point(x, y))
      'End If

    End Sub

    'Private Sub CheckPoint(ByRef pts As Generic.Stack(Of Drawing.Point),
    '                       x As Integer,
    '                       y As Integer,
    '                       oldColor As Short,
    '                       newColor As Short)

    '  If Point(x, y) = oldColor Then
    '    pts.Push(New Drawing.Point(x, y))
    '    Pset(x, y, newColor)
    '  End If

    'End Sub

    Friend Sub Pcopy(sourcePage As Integer, destinationPage As Integer)

      Array.Copy(m_page(sourcePage).Pixels, m_page(destinationPage).Pixels, m_page(sourcePage).Pixels.Length)
      m_invalidated = True

    End Sub

    Friend Function Point(x As Integer, y As Integer) As Short

      If Not m_graphicView.IsScreen Then
        ' Need to translate x/y to view...
        x += m_graphicView.Left
        y += m_graphicView.Top
      End If

      ' On the physical screen and within the active view port...
      If x.Between(0, m_screenWidth - 1) AndAlso
         y.Between(0, m_screenHeight - 1) AndAlso
         x.Between(m_graphicView.Left, m_graphicView.Right) AndAlso
         y.Between(m_graphicView.Top, m_graphicView.Bottom) Then

        Dim index As Integer = (y * m_screenWidth) + x
        Return m_page(m_activePage).Point(index)

      End If

      Return -1

    End Function

    Public ReadOnly Property ActivePage As Integer
      Get
        Return m_activePage
      End Get
    End Property

    'Public Function Circle(ByVal xcenter As Integer, ByVal ycenter As Integer, ByVal radius As Double, ByVal attribute As Integer, ByVal start As Double, ByVal [end] As Double, ByVal aspect As Double) As Boolean

    '  Dim pi As Double = 3.141593

    '  Dim ra As Double = 1 / aspect

    '  For i = xcenter - radius To xcenter + radius
    '    Dim h = i
    '    Dim v = ycenter + Math.Sqrt((radius ^ 2) - (h - xcenter) ^ 2) * aspect
    '    If Not Me.Pset(h, v) Then Return False
    '    h = i
    '    v = ycenter - Math.Sqrt((radius ^ 2) - (h - xcenter) ^ 2) * aspect
    '    If Not Me.Pset(h, v) Then Return False
    '  Next

    '  For i = ycenter - radius * aspect To ycenter + radius * aspect
    '    Dim v = i
    '    Dim h = xcenter + Math.Sqrt(Math.Abs((radius ^ 2) - (aspect * (v - ycenter)) ^ 2))
    '    If Not Me.Pset(h, v) Then Return False
    '    'v = i
    '    'h = xcenter - Math.Sqrt(Math.Abs((radius ^ 2) - (aspect * (v - ycenter)) ^ 2))
    '    'If Not Me.Pset(h, v) Then Return False
    '  Next

    '  Return True

    'End Function

    Friend Function Circle(xcenter As Integer, ycenter As Integer, radius As Double, attribute As Short, start As Double, [end] As Double, aspect As Double) As Boolean

      '                                     1                                  2               4                 8               16
      ' passed is used to notify the routine whether or not the optional values are actually provided or not.
      ' it's a bit field.

      Select Case m_screenMode
        'Case 0
        '????
        Case 1
          If Not attribute.Between(0, 3) Then attribute = 3
        Case 2
          If Not attribute.Between(0, 1) Then attribute = 1
        Case 7
          If Not attribute.Between(0, 15) Then attribute = 15
        Case 8
          If Not attribute.Between(0, 15) Then attribute = 15
        Case 9
          If Not attribute.Between(0, 15) Then attribute = 15
        Case 10
          If Not attribute.Between(0, 3) Then attribute = 3
        Case Else
          Stop
      End Select

      Static pi As Double = 3.1415926535897931
      Static pi2 As Double = 6.2831853071795862
      Static line_to_start, line_from_end As Integer
      Static ix, iy As Integer ' integer screen co-ordinates of circle's centre
      Static xspan, yspan As Double
      Static c As Double ' circumference
      Static px, py As Double
      Static sinb, cosb As Double ' second angle used in double-angle-formula
      Static pixels As Integer
      Static tmp As Double
      Static tmpi As Integer
      Static i As Integer
      Static exclusive As Integer
      Static arc1, arc2, arc3, arc4, arcinc As Double
      Static px2 As Double ', py2 As Double
      Static x2, y2 As Integer
      Static lastplotted_x2, lastplotted_y2 As Integer
      Static lastchecked_x2, lastchecked_y2 As Integer

      'If m_writePage.Text Then Throw New InvalidOperationException

      ' lines to & from centre
      'If (Not ((passed And 4) = 4)) Then start = 0
      'If (Not ((passed And 8) = 8)) Then [end] = pi2
      line_to_start = 0 : If (start < 0) Then line_to_start = 1 : start = -start
      line_from_end = 0 : If ([end] < 0) Then line_from_end = 1 : [end] = -[end]

      ' error checking
      If (start > pi2) Then Throw New InvalidOperationException
      If ([end] > pi2) Then Throw New InvalidOperationException

      ' when end<start, the arc of the circle that wouldn't have been drawn if start & end 
      ' were swapped is drawn
      exclusive = 0
      If [end] < start Then
        tmp = start : start = [end] : [end] = tmp
        tmpi = line_to_start : line_to_start = line_from_end : line_from_end = tmpi
        exclusive = 1
      End If

      ' calc. centre
      'If (passed And 1) = 1 Then x = m_writePage.X + x : y = m_writePage.Y + y
      'm_writePage.X = x : m_writePage.Y = y ' set graphics cursor position to circle's centre

      Dim r As Double = radius
      Dim x As Integer = xcenter
      Dim y As Integer = ycenter

      r = x + r ' the differece between x & x+r in pixels will be the radius in pixels
      ' resolve coordinates (but keep as floats)
      'If m_writePage.ClippingOrScaling <> 0 Then
      '  If m_writePage.ClippingOrScaling = 2 Then
      '    x = x * m_writePage.ScalingX + m_writePage.ScalingOffsetX + m_writePage.ViewOffsetX
      '    y = y * m_writePage.ScalingY + m_writePage.ScalingOffsetY + m_writePage.ViewOffsetY
      '    r = r * m_writePage.ScalingX + m_writePage.ScalingOffsetX + m_writePage.ViewOffsetX
      '  Else
      '    x = x + m_writePage.ViewOffsetX
      '    y = y + m_writePage.ViewOffsetY
      '    r = r + m_writePage.ViewOffsetX
      '  End If
      'End If
      'If x < 0 Then ix = CInt(x - 0.05) Else ix = CInt(x + 0.5)
      'If y < 0 Then iy = CInt(y - 0.05) Else iy = CInt(y + 0.05)
      If x < 0 Then ix = CInt(x) Else ix = CInt(x)
      If y < 0 Then iy = CInt(y) Else iy = CInt(y)
      r = Math.Abs(r - x) ' r is now a radius in pixels

      ' adjust vertical and horizontal span of the circle based on aspect ratio
      xspan = r : yspan = r
      'If Not ((passed And 16) = 16) Then
      '  aspect = 1 ' Note: default aspect ratio is 1:1 for QB64 specific modes (256/32)
      '  If (m_writePage.CompatibleMode = 1) Then aspect = 4.0 * (200.0 / 320.0) / 3.0
      '  If (m_writePage.CompatibleMode = 2) Then aspect = 4.0 * (200.0 / 640.0) / 3.0
      '  If (m_writePage.CompatibleMode = 7) Then aspect = 4.0 * (200.0 / 320.0) / 3.0
      '  If (m_writePage.CompatibleMode = 8) Then aspect = 4.0 * (200.0 / 640.0) / 3.0
      '  If (m_writePage.CompatibleMode = 9) Then aspect = 4.0 * (350.0 / 640.0) / 3.0
      '  If (m_writePage.CompatibleMode = 10) Then aspect = 4.0 * (350.0 / 640.0) / 3.0
      '  If (m_writePage.CompatibleMode = 11) Then aspect = 4.0 * (480.0 / 640.0) / 3.0
      '  If (m_writePage.CompatibleMode = 12) Then aspect = 4.0 * (480.0 / 640.0) / 3.0
      '  If (m_writePage.CompatibleMode = 13) Then aspect = 4.0 * (200.0 / 320.0) / 3.0
      '  ' Old method: aspect = 4.0 * (m_writePage.Height / m_writePage.width) / 3.0
      'End If
      If aspect >= 0 Then
        If aspect < 1 Then
          ' aspect: 0 to 1
          yspan *= aspect
        End If
        If aspect > 1 Then
          ' aspect: 1 to infinity
          xspan /= aspect
        End If
      Else
        If (aspect > -1) Then
          ' aspect: -1 to 0
          yspan *= (1 + aspect)
        End If
        ' if aspect<-1 no change is required
      End If

      ' skip everything if none of the circle is inside current viwport
      'If ((x + xspan + 0.5) < m_writePage.ViewX1) Then Return
      'If ((y + yspan + 0.5) < m_writePage.ViewY1) Then Return
      'If ((x - xspan - 0.5) > m_writePage.ViewX2) Then Return
      'If ((y - yspan - 0.5) > m_writePage.ViewY2) Then Return

      'If Not ((passed And 2) = 2) Then col = m_writePage.Color
      'm_writePage.DrawColor = col

      ' pre-set/pre-calcualate values
      c = pi2 * r
      pixels = CInt(c / 4.0) ' + 0.5)
      arc1 = 0
      arc2 = pi
      arc3 = pi
      arc4 = pi2
      arcinc = (pi / 2) / CDbl(pixels)
      sinb = Math.Sin(arcinc)
      cosb = Math.Cos(arcinc)
      lastplotted_x2 = -1
      lastchecked_x2 = -1
      i = 0

      If CBool(line_to_start) Then
        px = Math.Cos(start) : py = Math.Sin(start)
        x2 = CInt(px * xspan + 0.5) : y2 = CInt(py * yspan - 0.5)
        'FastLine(ix, iy, ix + x2, iy - y2, col)
        Line(ix, iy, ix + x2, iy - y2, attribute, False, False, New Short?)
      End If

      px = 1
      py = 0

drawcircle:
      x2 = CInt(px * xspan) ' + 0.5)
      y2 = CInt(py * yspan) ' - 0.5)

      If (i = 0) Then lastchecked_x2 = x2 : lastchecked_y2 = y2 : GoTo plot

      If ((Math.Abs(x2 - lastplotted_x2) >= 2) OrElse (Math.Abs(y2 - lastplotted_y2) >= 2)) Then
plot:
        If CBool(exclusive) Then
          If ((arc1 <= start) OrElse (arc1 >= [end])) Then Pset(ix + lastchecked_x2, iy + lastchecked_y2, attribute)
          If ((arc2 <= start) OrElse (arc2 >= [end])) Then Pset(ix - lastchecked_x2, iy + lastchecked_y2, attribute)
          If ((arc3 <= start) OrElse (arc3 >= [end])) Then Pset(ix - lastchecked_x2, iy - lastchecked_y2, attribute)
          If ((arc4 <= start) OrElse (arc4 >= [end])) Then Pset(ix + lastchecked_x2, iy - lastchecked_y2, attribute)
        Else ' inclusive
          If ((arc1 >= start) AndAlso (arc1 <= [end])) Then Pset(ix + lastchecked_x2, iy + lastchecked_y2, attribute)
          If ((arc2 >= start) AndAlso (arc2 <= [end])) Then Pset(ix - lastchecked_x2, iy + lastchecked_y2, attribute)
          If ((arc3 >= start) AndAlso (arc3 <= [end])) Then Pset(ix - lastchecked_x2, iy - lastchecked_y2, attribute)
          If ((arc4 >= start) AndAlso (arc4 <= [end])) Then Pset(ix + lastchecked_x2, iy - lastchecked_y2, attribute)
        End If
        If (i > pixels) Then GoTo allplotted
        lastplotted_x2 = lastchecked_x2 : lastplotted_y2 = lastchecked_y2
      End If
      lastchecked_x2 = x2 : lastchecked_y2 = y2

      If (i <= pixels) Then
        i += 1
        If (i > pixels) Then GoTo plot
        px2 = px * cosb + py * sinb
        py = py * cosb - px * sinb
        px = px2
        If CBool(i) Then arc1 += arcinc : arc2 -= arcinc : arc3 += arcinc : arc4 -= arcinc
        GoTo drawcircle
      End If

allplotted:

      If CBool(line_from_end) Then
        px = Math.Cos([end]) : py = Math.Sin([end])
        x2 = CInt(px * xspan + 0.5) : y2 = CInt(py * yspan - 0.5)
        'FastLine(ix, iy, ix + x2, iy - y2, col)
        Line(ix, iy, ix + x2, iy - y2, attribute, False, False, New Short?)
      End If

      Return True

    End Function

    Friend Function Line(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer, attribute As Short, box As Boolean, fill As Boolean, style As Short?, Optional ByRef styleBit As Integer = 0) As Boolean

      If attribute = -1 Then
        attribute = m_foreColor
      End If

      If box Then

        ' Box

        If fill Then

          For y As Integer = y1 To y2
            Line(x1, y, x2, y, attribute, False, False, style)
          Next

        Else

          ' Top of box.
          Line(x1, y1, x2, y1, attribute, False, False, style, styleBit)
          ' Left side of box.
          Line(x1, y1, x1, y2, attribute, False, False, style, styleBit)
          ' Right side of box.
          Line(x2, y1, x2, y2, attribute, False, False, style, styleBit)
          ' Bottom of box.
          Line(x1, y2, x2, y2, attribute, False, False, style, styleBit)

        End If

      Else

        ' Line

        If x1 <> x2 Then

          Dim m = (y1 - y2) / (x1 - x2)

          If Math.Abs(m) <= 1 Then

            For i = 0 To (x2 - x1) Step Math.Sign(x2 - x1)

              Dim x = x1 + i
              Dim y = CInt(VisualBasic.Conversion.Int(y1 + i * m + 0.5))

              Dim checkBit As UShort = CUShort(1 << styleBit) : styleBit += 1 : If styleBit > 15 Then styleBit = 0
              Dim doIt As Boolean = style Is Nothing
              If Not doIt Then
                doIt = CBool((style And checkBit) = checkBit)
              End If

              If doIt Then
                Pset(x, y, attribute)
              End If

            Next

          Else

            For i = 0 To (y2 - y1) Step Math.Sign(y2 - y1)

              Dim y = y1 + i
              Dim x = CInt(VisualBasic.Conversion.Int(x1 + i / m + 0.5))

              Dim checkBit As UShort = CUShort(1 << styleBit) : styleBit += 1 : If styleBit > 15 Then styleBit = 0
              Dim doIt As Boolean = style Is Nothing
              If Not doIt Then
                doIt = CBool((style And checkBit) = checkBit)
              End If

              If doIt Then
                Pset(x, y, attribute)
              End If

            Next

          End If

        Else

          If y2 = y1 Then
            Pset(x1, y2, attribute)
          Else
            For i = 0 To (y2 - y1) Step Math.Sign(y2 - y1)

              Dim x = x1
              Dim y = (y1 + i)

              Dim checkBit As UShort = CUShort(1 << styleBit) : styleBit += 1 : If styleBit > 15 Then styleBit = 0
              Dim doIt As Boolean = style Is Nothing
              If Not doIt Then
                doIt = CBool((style And checkBit) = checkBit)
              End If

              If doIt Then
                Pset(x, y, attribute)
              End If

            Next
          End If

        End If

      End If

      Return True

    End Function

    Friend Function Pset(x As Integer, y As Integer, c As Short) As Boolean

      If Not m_graphicView.IsScreen Then
        ' Need to translate x/y to view...
        x += m_graphicView.Left
        y += m_graphicView.Top
      End If

      ' On the physical screen and within the active view port...
      If x.Between(0, m_screenWidth - 1) AndAlso
         y.Between(0, m_screenHeight - 1) AndAlso
         x.Between(m_graphicView.Left, m_graphicView.Right) AndAlso
         y.Between(m_graphicView.Top, m_graphicView.Bottom) Then

        Select Case m_screenMode

          Case 0 : If Not c.Between(0, 31) Then c = 15
          Case 1 : If Not c.Between(0, 3) Then c = 3
          Case 2 : If Not c.Between(0, 1) Then c = 1

          Case 3 : If Not c.Between(0, 15) Then c = 15 ' TANDY 1000 160x200 16 colors
          Case 4 : If Not c.Between(0, 3) Then c = 3 ' TANDY 1000 320x200 4 colors
          Case 5 : If Not c.Between(0, 15) Then c = 15 ' TANDY 1000 320x200 16 colors
          Case 6 : If Not c.Between(0, 3) Then c = 3 ' TANDY 1000 640x200 4 colors

          Case 7 : If Not c.Between(0, 15) Then c = 15
          Case 8 : If Not c.Between(0, 15) Then c = 15
          Case 9 : If Not c.Between(0, 15) Then c = 15
          Case 10 : Stop : If Not c.Between(0, 15) Then c = 15

          Case 11 : If Not c.Between(0, 3) Then c = 3
          Case 12 : If Not c.Between(0, 15) Then c = 15
          Case 13 : If Not c.Between(0, 255) Then c = 255

          Case Else
            Stop
        End Select

        Dim index As Integer = (y * m_screenWidth) + x
        m_page(m_activePage).Point(index) = CByte(c)
        m_foreColor = c

        m_invalidated = True

        'If m_activePage = m_visualPage AndAlso
        '   Me.DirectPixels IsNot Nothing Then
        '  Me.DirectPixels(index) = m_palette(c)
        'End If

      End If

      Return True

    End Function

    Friend Function Pset(x As Integer, y As Integer) As Boolean

      If Not m_graphicView.IsScreen Then
        ' Need to translate x/y to view...
        x += m_graphicView.Left
        y += m_graphicView.Top
      End If

      If x.Between(m_graphicView.Left, m_graphicView.Right) AndAlso
         y.Between(m_graphicView.Top, m_graphicView.Bottom) Then

        'If x.Between(0, m_screenWidth - 1) AndAlso
        '   y.Between(0, m_screenHeight - 1) Then

        Dim index As Integer = (y * m_screenWidth) + x
        m_page(m_activePage).Point(index) = CByte(m_foreColor)

        'If m_activePage = m_visualPage AndAlso
        '   Me.DirectPixels IsNot Nothing Then
        '  Me.DirectPixels(index) = m_palette(m_foreColor)
        'End If

        m_invalidated = True

      End If

      Return True

    End Function

    Friend Function Preset(x As Integer, y As Integer, c As Short) As Boolean

      If Not m_graphicView.IsScreen Then
        ' Need to translate x/y to view...
        x += m_graphicView.Left
        y += m_graphicView.Top
      End If

      ' On the physical screen and within the active view port...
      If x.Between(0, m_screenWidth - 1) AndAlso
         y.Between(0, m_screenHeight - 1) AndAlso
         x.Between(m_graphicView.Left, m_graphicView.Right) AndAlso
         y.Between(m_graphicView.Top, m_graphicView.Bottom) Then

        Dim index As Integer = (y * m_screenWidth) + x

        m_page(m_activePage).Point(index) = CByte(c)

        'If m_activePage = m_visualPage AndAlso
        '   Me.DirectPixels IsNot Nothing Then
        '  Me.DirectPixels(index) = m_palette(c)
        'End If

        m_invalidated = True

      End If

      Return True

    End Function

    Friend Function Preset(x As Integer, y As Integer) As Boolean

      If Not m_graphicView.IsScreen Then
        ' Need to translate x/y to view...
        x += m_graphicView.Left
        y += m_graphicView.Top
      End If

      ' On the physical screen and within the active view port...
      If x.Between(0, m_screenWidth - 1) AndAlso
         y.Between(0, m_screenHeight - 1) AndAlso
         x.Between(m_graphicView.Left, m_graphicView.Right) AndAlso
         y.Between(m_graphicView.Top, m_graphicView.Bottom) Then

        Dim index As Integer = (y * m_screenWidth) + x
        m_page(m_activePage).Point(index) = CByte(m_backColor)

        'If m_activePage = m_visualPage AndAlso
        '   Me.DirectPixels IsNot Nothing Then
        '  Me.DirectPixels(index) = m_palette(m_backColor)
        'End If

        m_invalidated = True

      End If

      Return True

    End Function

    Friend ReadOnly Property ScreenMode As Integer
      Get
        Return m_screenMode
      End Get
    End Property

    Public ReadOnly Property CharacterWidth As Integer
      Get
        Return m_characterWidth
      End Get
    End Property

    Public ReadOnly Property CharacterHeight As Integer
      Get
        Return m_characterHeight
      End Get
    End Property

    Friend ReadOnly Property IsKeyOn As Boolean
      Get
        Return m_keyOn
      End Get
    End Property

    Friend ReadOnly Property TopLine As Short
      Get
        Return m_topLine
      End Get
    End Property

    Friend ReadOnly Property BottomLine As Short
      Get
        If m_viewActive Then
          Return m_bottomLine
        Else
          Return m_rowCount '25
        End If
      End Get
    End Property

    'Public ReadOnly Property CharacterBuffer() As Character()
    '  Get
    '    Return m_characterBuffer
    '  End Get
    'End Property

    Public Property CursorVisible As Boolean
      Get
        Return m_cursorVisible
      End Get
      Set(value As Boolean)
        m_cursorVisible = value
      End Set
    End Property

    Public Property CursorLocation As Short
      Get
        Return m_cursorLocation
      End Get
      Set(value As Short)
        m_cursorLocation = value
      End Set
    End Property

    Public Property CursorThick As Boolean
      Get
        Return m_cursorThick
      End Get
      Set(value As Boolean)
        m_cursorThick = value
      End Set
    End Property

    Public Property CursorToggle As Boolean
      Get
        Return m_cursorToggle
      End Get
      Set(value As Boolean)
        m_cursorToggle = value
      End Set
    End Property

    Public ReadOnly Property Insert As Boolean
      Get
        Return m_insert
      End Get
    End Property

    Public ReadOnly Property Location As Short
      Get
        Return m_location
      End Get
    End Property

    Public ReadOnly Property Segments As List(Of Segment)
      Get
        Return m_page(m_activePage).Segments
      End Get
    End Property

    'Public Property ShowVisualQueues As Boolean
    '  Get
    '    Return m_showVisualQueues
    '  End Get
    '  Set(value As Boolean)
    '    m_showVisualQueues = value
    '  End Set
    'End Property

    Public Property RowCount As Short
      Set(value As Short)
        If value = m_rowCount Then
          Return
        Else

          Select Case m_screenMode
            Case 0
              Select Case value
                Case 25
                  If m_screenHeight = 400 Then
                    Dim keyOn As Boolean = m_keyOn
                    If keyOn Then KeyOff()
                    m_characterHeight = 16
                    m_rowCount = 25
                    Dim pages As Integer = m_page.Count - 1
                    m_page.Clear()
                    For p As Integer = 0 To pages
                      m_page.Add(p, New InternalPage(m_screenMode, m_columnCount(m_screenMode), m_rowCount))
                    Next
                    ViewPrint()
                    Cls(-1)
                    If keyOn Then Me.KeyOn()
                  Else
                    Screen(0, m_colorSwitch, m_activePage, m_visualPage, True)
                  End If
                Case 43
                  If m_screenHeight <> 350 Then
                    Screen(9, m_colorSwitch, m_activePage, m_visualPage, True)
                  End If
                  Dim keyOn As Boolean = m_keyOn
                  If keyOn Then KeyOff()
                  m_characterHeight = 8
                  m_rowCount = 43
                  Dim pages As Integer = m_page.Count - 1
                  m_page.Clear()
                  For p As Integer = 0 To pages
                    m_page.Add(p, New InternalPage(m_screenMode, m_columnCount(m_screenMode), m_rowCount))
                  Next
                  ViewPrint()
                  Cls(-1)
                  If keyOn Then Me.KeyOn()
                Case 50
                  If m_screenHeight <> 400 Then
                    Screen(0, m_colorSwitch, m_activePage, m_visualPage, True)
                  End If
                  Dim keyOn As Boolean = m_keyOn
                  If keyOn Then KeyOff()
                  m_characterHeight = 8
                  m_rowCount = 50
                  Dim pages As Integer = m_page.Count - 1
                  m_page.Clear()
                  For p As Integer = 0 To pages
                    m_page.Add(p, New InternalPage(m_screenMode, m_columnCount(m_screenMode), m_rowCount))
                  Next
                  ViewPrint()
                  Cls(-1)
                  If keyOn Then Me.KeyOn()
                Case Else
                  Throw New ArgumentException("rowCount invalid.")
              End Select
            Case 1, 2, 3, 4, 5, 6, 7, 8, 13
              'NOTE: Should never get here....
              If value = 25 Then
                Return
              Else
                Throw New ArgumentException("rowCount invalid.")
              End If
            Case 9, 10
              Select Case value
                Case 25
                  Dim keyOn As Boolean = m_keyOn
                  If keyOn Then KeyOff()
                  m_characterHeight = 14
                  m_rowCount = 25
                  Dim pages As Integer = m_page.Count - 1
                  m_page.Clear()
                  For p As Integer = 0 To pages
                    m_page.Add(p, New InternalPage(m_screenMode, m_columnCount(m_screenMode), m_rowCount))
                  Next
                  ViewPrint()
                  Cls(-1)
                  If keyOn Then Me.KeyOn()
                Case 43
                  Dim keyOn As Boolean = m_keyOn
                  If keyOn Then KeyOff()
                  m_characterHeight = 8
                  m_rowCount = 43
                  Dim pages As Integer = m_page.Count - 1
                  m_page.Clear()
                  For p As Integer = 0 To pages
                    m_page.Add(p, New InternalPage(m_screenMode, m_columnCount(m_screenMode), m_rowCount))
                  Next
                  ViewPrint()
                  Cls(-1)
                  If keyOn Then Me.KeyOn()
                Case Else
                  Throw New ArgumentException("rowCount invalid.")
              End Select
            Case 11, 12
              Select Case value
                Case 30
                  Dim keyOn As Boolean = m_keyOn
                  If keyOn Then KeyOff()
                  m_characterHeight = 16
                  m_rowCount = 30
                  Dim pages As Integer = m_page.Count - 1
                  m_page.Clear()
                  For p As Integer = 0 To pages
                    m_page.Add(p, New InternalPage(m_screenMode, m_columnCount(m_screenMode), m_rowCount))
                  Next
                  Cls(-1)
                  If keyOn Then Me.KeyOn()
                Case 60
                  Dim keyOn As Boolean = m_keyOn
                  If keyOn Then KeyOff()
                  m_characterHeight = 8
                  m_rowCount = 60
                  Dim pages As Integer = m_page.Count - 1
                  m_page.Clear()
                  For p As Integer = 0 To pages
                    m_page.Add(p, New InternalPage(m_screenMode, m_columnCount(m_screenMode), m_rowCount))
                  Next
                  ViewPrint()
                  Cls(-1)
                  If keyOn Then Me.KeyOn()
                Case Else
                  Throw New ArgumentException("rowCount invalid.")
              End Select
            Case Else
              Throw New ArgumentException("screenMode invalid.")
          End Select
        End If
      End Set
      Get
        Return m_rowCount '25
      End Get
    End Property

    Public Property ColumnCount As Short
      Get
        Return m_columnCount(m_screenMode)
      End Get
      Set(value As Short)

        If m_columnCount(m_screenMode) = value Then Return

        Dim keyOn As Boolean = m_keyOn
        If keyOn Then KeyOff()

        Select Case m_screenMode
          Case 0
            If m_columnCount(0) <> value Then
              If value = 40 OrElse value = 80 Then
                m_columnCount(m_screenMode) = value
                Screen(0, m_colorSwitch, m_activePage, m_visualPage, True)
                'm_borderColor = 0
              ElseIf value = 20 Then
                Screen(3, m_colorSwitch, m_activePage, m_visualPage, True)
              Else
                Throw New ArgumentException("columnCount invalid.")
              End If
            Else
              Return
            End If
          Case 1
            If value = 80 Then
              Screen(2, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 40 Then
              Return
            ElseIf value = 20 Then
              Screen(3, m_colorSwitch, m_activePage, m_visualPage, True)
            Else
              Throw New ArgumentException("columnCount invalid.")
            End If
          Case 2
            If value = 80 Then
              Return
            ElseIf value = 40 Then
              Screen(1, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 20 Then
              Screen(3, m_colorSwitch, m_activePage, m_visualPage, True)
            Else
              Throw New ArgumentException("columnCount invalid.")
            End If
          Case 3 ' TANDY 1000 160x200 16 colors
            If value = 80 Then
              Screen(2, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 40 Then
              Screen(1, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 20 Then
              Return
            Else
              Throw New ArgumentException("columnCount invalid.")
            End If
          Case 4 ' TANDY 1000 320x200 4 colors
            If value = 80 Then
              Screen(2, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 40 Then
              Return
            ElseIf value = 20 Then
              Screen(3, m_colorSwitch, m_activePage, m_visualPage, True)
            Else
              Throw New ArgumentException("columnCount invalid.")
            End If
          Case 5 ' TANDY 1000 320x200 16 colors
            If value = 80 Then
              Screen(6, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 40 Then
              Return
            ElseIf value = 20 Then
              Screen(3, m_colorSwitch, m_activePage, m_visualPage, True)
            Else
              Throw New ArgumentException("columnCount invalid.")
            End If
          Case 6 ' TANDY 1000 640x200 4 colors
            If value = 80 Then
              Return
            ElseIf value = 40 Then
              Screen(5, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 20 Then
              Screen(3, m_colorSwitch, m_activePage, m_visualPage, True)
            Else
              Throw New ArgumentException("columnCount invalid.")
            End If
          Case 7
            If value = 80 Then
              Screen(8, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 40 Then
              Return
            ElseIf value = 20 Then
              Screen(3, m_colorSwitch, m_activePage, m_visualPage, True)
            Else
              Throw New ArgumentException("columnCount invalid.")
            End If
          Case 8
            If value = 40 Then
              Screen(7, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 80 Then
              Return
            ElseIf value = 20 Then
              Screen(3, m_colorSwitch, m_activePage, m_visualPage, True)
            Else
              Throw New ArgumentException("columnCount invalid.")
            End If
          Case 9
            If value = 40 Then
              Screen(7, m_colorSwitch, m_activePage, m_visualPage, True)
            ElseIf value = 80 Then
              Return
            ElseIf value = 20 Then
              Screen(3, m_colorSwitch, m_activePage, m_visualPage, True)
            Else
              Throw New ArgumentException("columnCount invalid.")
            End If
          Case Else
            Throw New ArgumentException("screenMode invalid.")
        End Select

        Cls(-1)

        If keyOn Then Me.KeyOn()

      End Set
    End Property

    Friend Function Screen(mode As Short, colorSwitch As Boolean, activePage As Short, visualPage As Short, Optional forced As Boolean = False) As Boolean

      If m_colorSwitch <> colorSwitch Then
        m_colorSwitch = colorSwitch
      End If

      If m_screenMode <> mode OrElse forced Then

        Dim keyOn As Boolean = m_keyOn
        If keyOn Then KeyOff()

        m_screenMode = mode

        Dim pages As Integer

        Select Case mode

          Case -3 ' Hercules 720x348 80x25 Rows=25 1 pages

            'NOTE: Set to -3 since conflicts with Tandy Mode 3 and Hercules is most likely less used.

            m_screenWidth = 720
            m_screenHeight = 350 '348
            m_screenColorCount = 2
            m_characterWidth = 8
            m_characterHeight = 14

            ' Bright Green on black
            m_backColor = 0
            m_foreColor = 1

            m_rowCount = 25

            pages = 1

          Case -4 ' AT&T ? ? Rows=? ? pages

            'NOTE: Set to -4 since conflicts with Tandy Mode 4 and AT&T is event more likely less used.

            m_screenWidth = 640
            m_screenHeight = 400
            m_screenColorCount = 16
            m_characterWidth = 8
            m_characterHeight = 16

            ' Orange on black
            m_backColor = 0
            m_foreColor = 6

            m_rowCount = 25

            pages = 1

          Case 0 ' TEXT MODE Rows=25,43,50 8 pages

            ' 25 rows = 400 pixels / 16 char height
            ' 43 rows = 350 pixels / 8 char height
            ' 50 rows = 400 pixels / 8 char height

            If Me.m_columnCount(0) = 40 Then
              m_screenWidth = 320
              m_screenHeight = 400
            Else
              m_screenWidth = 640
              m_screenHeight = 400
            End If
            m_screenColorCount = 16
            m_characterWidth = 8
            m_characterHeight = 16

            m_backColor = m_defaultBackgroundColor '0
            m_foreColor = m_defaultForegroundColor '10 '7  Bright Green on black

            m_rowCount = 25

            pages = 8

          Case 1 ' CGA 320x200 4 colors Rows=25 8 pages

            m_screenWidth = 320
            m_screenHeight = 200
            m_screenColorCount = 4
            m_characterWidth = 8
            m_characterHeight = 8

            m_backColor = 0
            m_foreColor = 3

            m_rowCount = 25

            pages = 8

          Case 2 ' CGA 640x200 2 colors Rows=25 8 pages

            m_screenWidth = 640
            m_screenHeight = 200
            m_screenColorCount = 2
            m_characterWidth = 8
            m_characterHeight = 8

            m_backColor = 0
            m_foreColor = 1

            m_rowCount = 25

            pages = 8

          Case 3 ' TANDY 1000 160x200 16 colors Rows=25 8 pages

            m_screenWidth = 160
            m_screenHeight = 200
            m_screenColorCount = 16
            m_characterWidth = 8
            m_characterHeight = 8

            m_backColor = 0
            m_foreColor = 15

            m_rowCount = 25

            pages = 8

          Case 4 ' TANDY 1000 320x200 4 colors Rows=25 8 pages

            m_screenWidth = 320
            m_screenHeight = 200
            m_screenColorCount = 4
            m_characterWidth = 8
            m_characterHeight = 8

            m_backColor = 0
            m_foreColor = 3

            m_rowCount = 25

            pages = 8

          Case 5 ' TANDY 1000 320x200 16 colors Rows=25 8 pages

            m_screenWidth = 320
            m_screenHeight = 200
            m_screenColorCount = 16
            m_characterWidth = 8
            m_characterHeight = 8

            m_backColor = 0
            m_foreColor = 15

            m_rowCount = 25

            pages = 8

          Case 6 ' TANDY 1000 640x200 4 colors Rows=25 8 pages

            m_screenWidth = 640
            m_screenHeight = 200
            m_screenColorCount = 4
            m_characterWidth = 8
            m_characterHeight = 8

            m_backColor = 0
            m_foreColor = 3

            m_rowCount = 25

            pages = 8

          Case 7 ' EGA 320x200 16 colors 8 pages

            m_screenWidth = 320
            m_screenHeight = 200
            m_screenColorCount = 16
            m_characterWidth = 8
            m_characterHeight = 8

            m_backColor = 0
            m_foreColor = 15

            m_rowCount = 25

            pages = 8

          Case 8 ' EGA 640x200 16 colors 4 pages

            m_screenWidth = 640
            m_screenHeight = 200
            m_screenColorCount = 16
            m_characterWidth = 8
            m_characterHeight = 8

            m_backColor = 0
            m_foreColor = 15

            m_rowCount = 25

            pages = 4

          Case 9 ' EGA 640x350 16 colors 2 pages

            m_screenWidth = 640
            m_screenHeight = 350
            m_screenColorCount = 16
            m_characterWidth = 8
            m_characterHeight = 14 ' default to 25 lines (use 8 for 43 lines)

            m_backColor = 0
            m_foreColor = 15

            m_rowCount = 25

            pages = 2

          Case 10 ' EGA 640x350 4 psuedo colors 2 pages (monochrome)

            m_screenWidth = 640
            m_screenHeight = 350
            m_screenColorCount = 4
            m_characterWidth = 8
            m_characterHeight = 14 ' default to 25 lines (use 8 for 43 lines)

            m_backColor = 0
            m_foreColor = 3

            m_rowCount = 25

            pages = 4

          Case 11 ' VGA 640x480 80x30 Rows=30 1 pages

            m_screenWidth = 640
            m_screenHeight = 480
            m_screenColorCount = 2
            m_characterWidth = 8
            m_characterHeight = 16

            m_backColor = 0
            m_foreColor = 1

            m_rowCount = 30

            pages = 1

          Case 12 ' VGA 640x480 80x30 Rows=30 1 pages

            m_screenWidth = 640
            m_screenHeight = 480
            m_screenColorCount = 16
            m_characterWidth = 8
            m_characterHeight = 16

            m_backColor = 0
            m_foreColor = 15

            m_rowCount = 30

            pages = 1

          Case 13 ' VGA 320x200 256 colors 1 page

            m_screenWidth = 320
            m_screenHeight = 200
            m_screenColorCount = 256
            m_characterWidth = 8
            m_characterHeight = 8

            m_backColor = 0
            m_foreColor = 15

            m_rowCount = 25

            pages = 1

            'Case 720 ' 720p 16:9 1280x720 ARGB colors 1 page

            '  Return False

            'Case 1080 ' 1080p 16:9 1920x1080 ARGB colors 1 page

            '  Return False

          Case Else

            Return False

        End Select

        If m_screenColorCount = 0 Then
          ' Added this to remove a warning.
        End If

        m_page.Clear()
        For p As Integer = 0 To pages - 1
          m_page.Add(p, New InternalPage(mode, m_columnCount(mode), m_rowCount))
        Next

        m_activePage = If(activePage.Between(0, CShort(m_page.Count - 1)), activePage, 0S)
        m_visualPage = If(visualPage.Between(0, CShort(m_page.Count - 1)), visualPage, 0S)

        GraphicViewReset()

        If keyOn Then m_keyOn = True

        Cls(-1)

        'If keyOn Then Me.KeyOn()

      Else

        m_activePage = If(activePage.Between(0, CShort(m_page.Count - 1)), activePage, 0S)
        m_visualPage = If(visualPage.Between(0, CShort(m_page.Count - 1)), visualPage, 0S)

      End If

      Return True

    End Function

    Friend Sub ViewPrint()
      m_viewActive = False
      m_topLine = 1
      m_bottomLine = m_rowCount - 1S '24
      Locate(m_topLine, 1)
    End Sub

    Friend Sub ViewPrint(topLine As Short, bottomLine As Short)
      m_viewActive = True
      m_topLine = topLine
      m_bottomLine = bottomLine
      Locate(m_topLine, 1)
    End Sub

#End Region

    Public Class Segment

      Private ReadOnly m_columnCount As Short
      Private m_locationBegin As Short
      Private m_locationEnd As Short

      Private ReadOnly m_minimum As Short
      Private ReadOnly m_maximum As Short

      Public ReadOnly Property LocationBegin As Short
        Get
          Return m_locationBegin
        End Get
      End Property

      Public ReadOnly Property LocationEnd As Short
        Get
          Return m_locationEnd
        End Get
      End Property

      Friend ReadOnly Property Length As Short
        Get
          Return CShort(m_locationEnd - m_locationBegin + 1) ' Always at least one character.
        End Get
      End Property

      Friend ReadOnly Property Line As Short ' The starting line number.
        Get
          Return (m_locationBegin \ m_columnCount)
        End Get
      End Property

      Friend Function IncludedLine(line As Short) As Boolean
        Dim location = line * m_columnCount
        Return location.Between(m_locationBegin, m_locationEnd)
      End Function

      Friend ReadOnly Property LineCount As Short ' How many lines (always at least one.)
        Get
          Return (Length \ m_columnCount)
        End Get
      End Property

      Public Sub New(locationBegin As Short, locationEnd As Short, columnCount As Short, min As Short, max As Short)
        m_locationBegin = locationBegin
        m_locationEnd = locationEnd
        m_columnCount = columnCount
        m_minimum = min
        m_maximum = max
      End Sub

      Public Sub New(line As Short, columnCount As Short, max As Short, min As Short)
        m_locationBegin = line * columnCount
        m_locationEnd = m_locationBegin + columnCount - 1S
        m_columnCount = columnCount
        m_minimum = min
        m_maximum = max
      End Sub

      Friend Sub Clear()
        m_locationEnd = m_locationBegin + m_columnCount - 1S
      End Sub

      Friend Sub Shorten()
        m_locationEnd -= m_columnCount
      End Sub

      Friend Sub Expand()
        m_locationEnd += m_columnCount
      End Sub

      Friend Sub ShiftUp()

        m_locationBegin -= m_columnCount
        m_locationEnd -= m_columnCount

        If Not m_locationBegin.Between(m_minimum, m_maximum) Then
          m_locationBegin = m_minimum
        End If

      End Sub

      Friend Sub ShiftDown()

        m_locationBegin += m_columnCount
        m_locationEnd += m_columnCount

        If Not m_locationEnd.Between(m_minimum, m_maximum) Then
          m_locationEnd = m_maximum
        End If

      End Sub

    End Class

    'Public Class Character

    '  Public [Char] As Char

    '  Public ForeColor As Short
    '  Public BackColor As Short

    '  'Public LineBegin As Boolean

    '  Public Updated As Boolean

    '  Public Sub Reset(ByVal foreColor As Short, ByVal backColor As Short)
    '    Me.Char = ChrW(0)
    '    Me.ForeColor = foreColor
    '    Me.BackColor = backColor
    '    'Me.LineBegin = False
    '    Me.Updated = True
    '  End Sub

    '  Public Sub Clone(ByVal ch As Character)
    '    Me.Char = ch.Char
    '    Me.ForeColor = ch.ForeColor
    '    Me.BackColor = ch.BackColor
    '    'Me.LineBegin = ch.LineBegin
    '    Me.Updated = True
    '  End Sub

    'End Class

    Public Sub New() 'timer As System.Timers.Timer) 'Timers.ITimer)

      m_idirect = TryCast(Me, IDirect)

      m_syncContext = Threading.SynchronizationContext.Current

      'InitializeEgaPalette()

      'InitializeDirectPixels()

      'm_canvas = canvas

      'm_screenMode = 0
      Color(7, 0)
      'Me.Cls()
      Screen(0, False, 0, 0)
      'Me.KeyOn()

      'InternalTimer = Timer
      'Try
      '  InternalTimer.Interval = 1 '200 'was 100 'New TimeSpan(0, 0, 0, 0, 100)
      'Catch ex As System.ArgumentOutOfRangeException ' Windows Timer
      '  InternalTimer.Interval = 1
      'End Try
      ''AddHandler m_timer.Tick, AddressOf Me.PaintTimer
      'InternalTimer.Start()

    End Sub

    Public Sub New(timer As System.Timers.Timer, 'Timers.ITimer,
                   defaultForegroundColor As Short,
                   defaultBackgroundColor As Short,
                   isKeyOn As Boolean)

      m_idirect = TryCast(Me, IDirect)

      m_syncContext = Threading.SynchronizationContext.Current

      m_defaultForegroundColor = defaultForegroundColor
      m_defaultBackgroundColor = defaultBackgroundColor

      'InitializeEgaPalette()

      'InitializeDirectPixels()

      'm_canvas = canvas

      'm_screenMode = 0
      Color(m_defaultForegroundColor, m_defaultBackgroundColor)
      'Me.Cls()
      Screen(0, False, 0, 0)

      If isKeyOn Then
        KeyOn()
      End If

      'InternalTimer = timer
      'Try
      '  InternalTimer.Interval = 0 '200 'was 100 'New TimeSpan(0, 0, 0, 0, 100)
      'Catch ex As System.ArgumentOutOfRangeException ' Windows Timer
      '  InternalTimer.Interval = 1
      'End Try
      ''AddHandler m_timer.Tick, AddressOf Me.PaintTimer
      'InternalTimer.Start()

    End Sub

    Public Sub New(defaultForegroundColor As Short,
                   defaultBackgroundColor As Short,
                   isKeyOn As Boolean)

      m_idirect = TryCast(Me, IDirect)

      m_syncContext = Threading.SynchronizationContext.Current

      m_defaultForegroundColor = defaultForegroundColor
      m_defaultBackgroundColor = defaultBackgroundColor

      Color(m_defaultForegroundColor, m_defaultBackgroundColor)
      Screen(0, False, 0, 0)

      If isKeyOn Then
        KeyOn()
      End If

    End Sub


#Region "KeyOn/KeyOff"

    Friend Function ExecuteKeyOn(index As Short) As Boolean
      If m_keys(index - 1).IndexOf(ChrW(27)) > -1 Then
        Dim value As String = m_keys(index - 1).Substring(0, m_keys(index - 1).IndexOf(ChrW(27)))
        Print(value, False)
        Return True
      Else
        Print(m_keys(index - 1), False)
        Return False
      End If
    End Function

    Friend Sub KeyOn()

      If m_idirect IsNot Nothing Then
        m_idirect.KeyOn()
        Return
      End If

      m_keyOn = True

      BuildKeyOn()

    End Sub

    Friend Sub KeyOff()

      m_keyOn = False

      ClearKeyOn()

    End Sub

    Private Sub ResetCharacter(position As Integer)

      ' Store the character in buffer so we can determine the character using another keyword.

      m_page(m_activePage).CharacterBuffer(position) = 0
      m_page(m_activePage).CharacterAttribute(position) = CByte((m_foreColor Mod 16S) + ((m_backColor Mod 8S) * 16S) + If(m_foreColor > 15, 128S, 0S))

      ' Draw the character to the page.

      Dim r As Integer = position \ m_columnCount(m_screenMode)
      Dim c As Integer = position Mod m_columnCount(m_screenMode)

      Dim x As Integer = c * m_characterWidth
      Dim y As Integer = r * m_characterHeight

      For cr As Integer = 0 To m_characterHeight - 1

        m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 0)) = CByte(m_backColor)
        m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 1)) = CByte(m_backColor)
        m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 2)) = CByte(m_backColor)
        m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 3)) = CByte(m_backColor)
        m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 4)) = CByte(m_backColor)
        m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 5)) = CByte(m_backColor)
        m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 6)) = CByte(m_backColor)
        m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 7)) = CByte(m_backColor)

        'If m_visualPage = m_activePage AndAlso
        '   Me.DirectPixels IsNot Nothing Then

        '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 0)) = m_egaPalette(m_backColor)
        '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 1)) = m_egaPalette(m_backColor)
        '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 2)) = m_egaPalette(m_backColor)
        '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 3)) = m_egaPalette(m_backColor)
        '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 4)) = m_egaPalette(m_backColor)
        '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 5)) = m_egaPalette(m_backColor)
        '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 6)) = m_egaPalette(m_backColor)
        '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 7)) = m_egaPalette(m_backColor)

        'End If

      Next

      m_invalidated = True

      'If m_visualPage = m_activePage AndAlso
      '   Me.DirectPixels IsNot Nothing Then
      '  Me.DirectInvalidate()
      'End If

    End Sub

    Private Sub CloneCharacter(oldPosition As Integer, newPosition As Integer)

      m_page(m_activePage).CharacterBuffer(newPosition) = m_page(m_activePage).CharacterBuffer(oldPosition)
      m_page(m_activePage).CharacterAttribute(newPosition) = m_page(m_activePage).CharacterAttribute(oldPosition)

      Dim oldRow As Integer = oldPosition \ m_columnCount(m_screenMode)
      Dim oldColumn As Integer = oldPosition Mod m_columnCount(m_screenMode)

      Dim oldX As Integer = oldColumn * m_characterWidth
      Dim oldY As Integer = oldRow * m_characterHeight

      Dim newRow As Integer = newPosition \ m_columnCount(m_screenMode)
      Dim newColumn As Integer = newPosition Mod m_columnCount(m_screenMode)

      Dim newX As Integer = newColumn * m_characterWidth
      Dim newY As Integer = newRow * m_characterHeight

      For y As Integer = 0 To m_characterHeight - 1
        Dim si As Integer = ((oldY + y) * m_screenWidth) + oldX
        Dim di As Integer = ((newY + y) * m_screenWidth) + newX
        Array.Copy(m_page(m_activePage).Colors, si, m_page(m_activePage).Colors, di, m_characterWidth)
        Array.Copy(m_page(m_activePage).Pixels, si, m_page(m_activePage).Pixels, di, m_characterWidth)
      Next

      m_invalidated = True

      'For x As Integer = 0 To m_characterWidth - 1
      '  For y As Integer = 0 To m_characterHeight - 1
      '    m_page(m_activePage).Point(((newY + y) * m_screenWidth) + (newX + x)) = m_page(m_activePage).Point(((oldY + y) * m_screenWidth) + (oldX + x))
      '  Next
      'Next

      'For x As Integer = 0 To m_characterWidth - 1
      '  For y As Integer = 0 To m_characterHeight - 1

      '    m_page(m_activePage).Point(((newY + y) * m_screenWidth) + (newX + x)) = m_page(m_activePage).Point(((oldY + y) * m_screenWidth) + (oldX + x))

      '    'If m_visualPage = m_activePage AndAlso
      '    '   Me.DirectPixels IsNot Nothing Then
      '    '  Me.DirectPixels(((newY + y) * m_screenWidth) + (newX + x)) = Me.DirectPixels(((oldY + y) * m_screenWidth) + (oldX + x))
      '    'End If

      '  Next
      'Next

      'If m_visualPage = m_activePage AndAlso
      '   Me.DirectPixels IsNot Nothing Then
      '  Me.DirectInvalidate()
      'End If

    End Sub

    Private Sub WriteCharacter(ascii As Byte, position As Integer)

      ' Store the character in buffer so we can determine the character using another keyword.

      m_page(m_activePage).CharacterBuffer(position) = ascii
      m_page(m_activePage).CharacterAttribute(position) = CByte((m_foreColor Mod 16) + ((m_backColor Mod 8) * 16) + If(m_foreColor > 15, 128, 0))

      ' Draw the character to the page.

      'Dim map As Integer() = CharMap.CharMap(m_screenMode, ChrW(ascii))
      Dim map As Integer() = CharMap.CharMap(m_characterHeight, ChrW(ascii))

      Dim r As Integer = position \ m_columnCount(m_screenMode)
      Dim c As Integer = position Mod m_columnCount(m_screenMode)

      If m_idirect IsNot Nothing Then
        Dim ch = ChrW(ascii)
        m_idirect.WriteCharacter(ch, r, c)
      Else

        Dim x As Integer = c * m_characterWidth
        Dim y As Integer = r * m_characterHeight

        For cr As Integer = 0 To m_characterHeight - 1

          m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 0)) = CByte(If((map(cr) And 1) > 0, m_foreColor, m_backColor))
          m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 1)) = CByte(If((map(cr) And 2) > 0, m_foreColor, m_backColor))
          m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 2)) = CByte(If((map(cr) And 4) > 0, m_foreColor, m_backColor))
          m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 3)) = CByte(If((map(cr) And 8) > 0, m_foreColor, m_backColor))
          m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 4)) = CByte(If((map(cr) And 16) > 0, m_foreColor, m_backColor))
          m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 5)) = CByte(If((map(cr) And 32) > 0, m_foreColor, m_backColor))
          m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 6)) = CByte(If((map(cr) And 64) > 0, m_foreColor, m_backColor))
          m_page(m_activePage).Point(((y + cr) * m_screenWidth) + (x + 7)) = CByte(If((map(cr) And 128) > 0, m_foreColor, m_backColor))

          'If m_visualPage = m_activePage AndAlso
          '   Me.DirectPixels IsNot Nothing Then

          '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 0)) = m_egaPalette(If((map(cr) And 1) > 0, m_foreColor, m_backColor))
          '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 1)) = m_egaPalette(If((map(cr) And 2) > 0, m_foreColor, m_backColor))
          '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 2)) = m_egaPalette(If((map(cr) And 4) > 0, m_foreColor, m_backColor))
          '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 3)) = m_egaPalette(If((map(cr) And 8) > 0, m_foreColor, m_backColor))
          '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 4)) = m_egaPalette(If((map(cr) And 16) > 0, m_foreColor, m_backColor))
          '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 5)) = m_egaPalette(If((map(cr) And 32) > 0, m_foreColor, m_backColor))
          '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 6)) = m_egaPalette(If((map(cr) And 64) > 0, m_foreColor, m_backColor))
          '  Me.DirectPixels(((y + cr) * m_screenWidth) + (x + 7)) = m_egaPalette(If((map(cr) And 128) > 0, m_foreColor, m_backColor))

          'End If

        Next

      End If


      m_invalidated = True

      'If m_visualPage = m_activePage AndAlso
      '   Me.DirectPixels IsNot Nothing Then
      '  Me.DirectInvalidate()
      'End If

    End Sub

    Friend Sub SetKeyLabel(index As Integer, text As String)
      m_keys(index) = text
    End Sub

    Private Sub BuildKeyOn()

      ClearKeyOn()

      For index As Short = 0 To CShort(m_keys.Count - 1)

        Dim position = (m_page(m_activePage).CharacterBuffer.Length - m_columnCount(m_screenMode)) + (index * 8)

        If position > m_page(m_activePage).CharacterBuffer.Length - 1 Then
          Exit For
        End If

        If index = 9 Then
          WriteCharacter(AscW("0"c), position)
          'm_page(m_activePage).CharacterBuffer(position).Char = "0"
        Else
          WriteCharacter(CByte(AscW((index + 1).ToString)), position)
          'm_page(m_activePage).CharacterBuffer(position).Char = (index + 1).ToString
        End If

        Dim backColor As Short = m_backColor
        Dim foreColor As Short = m_foreColor

        Select Case m_screenMode
          Case 0 : m_backColor = 7 : m_foreColor = 0
          Case 1 : m_backColor = 0 : m_foreColor = 3
          Case 2 : m_backColor = 0 : m_foreColor = 1
          Case 7 : m_backColor = 0 : m_foreColor = 1
          Case 8 : m_backColor = 0 : m_foreColor = 1
          Case 9 : m_backColor = 0 : m_foreColor = 1
        End Select

        For ch = 0 To If(m_keys(index).Length < 6, m_keys(index).Length - 1, 5)

          If position + 1 + ch > m_page(m_activePage).CharacterBuffer.Length - 1 Then
            Exit For
          End If

          WriteCharacter(CByte(AscW(m_keys(index)(ch))), position + 1 + ch)

          'm_page(m_activePage).CharacterBuffer(position + 1 + ch).Char = m_keys(index)(ch)
          'm_page(m_activePage).CharacterBuffer(position + 1 + ch).ForeColor = foreColor
          'm_page(m_activePage).CharacterBuffer(position + 1 + ch).BackColor = backColor
        Next

        m_backColor = backColor
        m_foreColor = foreColor

        ' Need to add the last line segment, removing any other last line if necessary.

      Next

    End Sub

    Private Sub ClearKeyOn()

      Dim backColor = m_backColor
      Dim foreColor = m_foreColor

      Select Case m_screenMode
        Case 0 : m_backColor = 0 : m_foreColor = 7
        Case 1 : m_backColor = 0 : m_foreColor = 3
        Case 2 : m_backColor = 0 : m_foreColor = 1
        Case 7 : m_backColor = 0 : m_foreColor = 1
        Case 8 : m_backColor = 0 : m_foreColor = 1
        Case 9 : m_backColor = 0 : m_foreColor = 1
      End Select

      For index As Short = CShort(m_page(m_activePage).CharacterBuffer.Length - m_columnCount(m_screenMode)) To CShort(m_page(m_activePage).CharacterBuffer.Length - 1)
        If index > m_page(m_activePage).CharacterBuffer.Length - 1 Then
          Exit For
        End If
        WriteCharacter(0, index)
        'm_page(m_activePage).CharacterBuffer(index).Char = ChrW(0)
        'm_page(m_activePage).CharacterBuffer(index).ForeColor = 7 'Colors.LightGray
        'm_page(m_activePage).CharacterBuffer(index).BackColor = 0 'Colors.Black
        ''m_screen(index).LineBegin = False
        'm_page(m_activePage).CharacterBuffer(index).Updated = True
      Next

      m_backColor = backColor
      m_foreColor = foreColor

      ' Todo, need to remove the last line segment.

    End Sub

#End Region

    Friend Function GetCurrentLineText() As String

      Dim result As String = Nothing

      Dim f = FindBeginOfLine()
      Dim e = FindEndOfLine()

      For index As Short = f To e

        'If m_page(m_activePage).CharacterBuffer(index).Char = ChrW(0) Then
        '  Exit For
        'End If

        If Me.m_page(Me.m_activePage).CharacterBuffer(index) = 0 Then
          Exit For
        End If

        'If result Is Nothing Then
        '  result = m_page(m_activePage).CharacterBuffer(index).Char
        'Else
        '  result &= m_page(m_activePage).CharacterBuffer(index).Char
        'End If

        If result Is Nothing Then
          result = ChrW(m_page(m_activePage).CharacterBuffer(index))
        Else
          result &= ChrW(m_page(m_activePage).CharacterBuffer(index))
        End If

      Next

      Return result

    End Function

    Private Function GetCurrentLineTextLength() As Short

      Dim result As Short = 0

      Dim f = FindBeginOfLine()
      Dim e = FindEndOfLine()

      For index As Short = f To e

        'If m_page(m_activePage).CharacterBuffer(index).Char = ChrW(0) Then
        '  Exit For
        'End If

        If Me.m_page(Me.m_activePage).CharacterBuffer(index) = 0 Then
          Exit For
        End If

        result += 1S

      Next

      Return result

    End Function

    Private Function FindBeginOfLine() As Short

      Dim segments = From p In m_page(m_activePage).Segments
                     Where m_location.Between(p.LocationBegin, p.LocationEnd)

      If segments.Any Then
        Return segments(0).LocationBegin
      Else
        Return -1
      End If

    End Function

    Private Function FindEndOfLine() As Short

      Dim segments = From p In m_page(m_activePage).Segments
                     Where m_location.Between(p.LocationBegin, p.LocationEnd)

      If segments.Any Then
        Return segments(0).LocationEnd
      Else
        Return -1
      End If

    End Function

    'Private Shared Function ViewPortBegin() As Short
    '  Return 0
    'End Function

    Private Function ViewPortEnd() As Short
      Return CShort(If(m_keyOn, m_page(m_activePage).CharacterBuffer.Length - m_columnCount(m_screenMode) - 1, m_page(m_activePage).CharacterBuffer.Length - 1))
    End Function

    Private Sub ExpandLine(line As Short)

      ' Shift everything starting at the line specified down one line.

      Dim locationStart As Short = (line + 1S) * m_columnCount(m_screenMode)
      Dim locationEnd As Short = ViewPortEnd()

      For index = locationEnd To locationStart + m_columnCount(m_screenMode) Step -1
        'm_page(m_activePage).CharacterBuffer(index).Clone(m_page(m_activePage).CharacterBuffer(index - m_columnCount(m_screenMode)))
        CloneCharacter(index - m_columnCount(m_screenMode), index)
      Next

      ' Clear out current line.

      For index = locationStart To locationStart + m_columnCount(m_screenMode) - 1
        'm_page(m_activePage).CharacterBuffer(index).Reset(m_foreColor, m_backColor)
        ResetCharacter(index)
      Next

      ' All segments => current line need to be modified.

      For index As Short = CShort(m_page(m_activePage).Segments.Count - 1) To 0 Step -1
        If m_page(m_activePage).Segments(index).IncludedLine(line) Then
          m_page(m_activePage).Segments(index).Expand()
        ElseIf m_page(m_activePage).Segments(index).Line > line Then
          m_page(m_activePage).Segments(index).ShiftDown()
        End If
      Next

    End Sub

    Private Sub ReduceLine(line As Short)

      ' Shift everything starting at the line specified up one line.

      Dim locationStart As Short = line * m_columnCount(m_screenMode)
      Dim locationEnd As Short = ViewPortEnd()

      For index = locationStart To locationEnd - m_columnCount(m_screenMode)
        'm_page(m_activePage).CharacterBuffer(index).Clone(m_page(m_activePage).CharacterBuffer(index + m_columnCount(m_screenMode)))
        CloneCharacter(index + m_columnCount(m_screenMode), index)
      Next

      ' Clear out last line.

      For index = locationEnd - m_columnCount(m_screenMode) To locationEnd
        'm_page(m_activePage).CharacterBuffer(index).Reset(m_foreColor, m_backColor)
        ResetCharacter(index)
      Next

      ' All segments => current line need to be modified.

      For index As Short = CShort(m_page(m_activePage).Segments.Count - 1) To 0 Step -1
        If m_page(m_activePage).Segments(index).IncludedLine(line) Then
          m_page(m_activePage).Segments(index).Shorten()
        ElseIf m_page(m_activePage).Segments(index).Line > line Then
          m_page(m_activePage).Segments(index).ShiftUp()
        End If
      Next

      'Dim beginOfLine As Short = line * m_columnCount

      'For index = beginOfLine + m_columnCount To ViewPortEnd() - m_columnCount
      '  m_screen(index).Clone(m_screen(index + (m_columnCount)))
      'Next

      '' All segments => current line need to be modified.

      'For index As Short = m_segments.Count - 1 To 0 Step -1
      '  If m_segments(index).Line = line Then
      '    m_segments(index).Shorten()
      '  ElseIf m_segments(index).Line > line Then
      '    m_segments(index).ShiftUp()
      '  End If
      'Next

    End Sub

#Region "BASIC Commands/Keywords/Functions"

    Friend Sub Cls(argument As Integer)

      Dim topLine As Short
      Dim bottomLine As Short

      If m_viewActive Then
        topLine = m_topLine
        bottomLine = m_bottomLine
      Else
        topLine = 1
        bottomLine = m_rowCount '25
      End If

      If m_page(m_activePage).CharacterBuffer Is Nothing OrElse m_page(m_activePage).CharacterBuffer.Length <> (m_rowCount * m_columnCount(m_screenMode)) Then

        ReDim m_page(m_activePage).CharacterBuffer((m_rowCount * m_columnCount(m_screenMode)) - 1)
        ReDim m_page(m_activePage).CharacterAttribute((m_rowCount * m_columnCount(m_screenMode)) - 1)

        Dim max As Short = CShort(m_page(m_activePage).CharacterBuffer.Length - 1)
        If m_keyOn Then max -= m_columnCount(m_screenMode)

        For index As Short = 0 To max
          WriteCharacter(0, index)
        Next

      Else

        For index As Short = GetLocation(topLine, 1) To GetLocation(bottomLine, m_columnCount(m_screenMode))
          WriteCharacter(0, index)
        Next

      End If

      Locate(m_topLine, 1)

      'TODO: May have to do some work with segments...

      m_page(m_activePage).Segments.Clear()

      If m_keyOn Then
        ' Split into two segments (24 lines and the key on line).
        'm_segments.Add(New Segment With {.Begin = 0, .End = 0})
        'm_segments.Add(New Segment With {.Begin = max + 1, .End = m_screen.Count - 1})
        'm_segments.Add(New Segment(m_columnCount) With {.LocationBegin = 0, .LocationEnd = m_columnCount - 1})
        'm_segments.Add(New Segment(m_columnCount) With {.LocationBegin = max + 1, .LocationEnd = m_screen.Count - 1})

        m_page(m_activePage).Segments.Add(New Segment(0, m_columnCount(m_screenMode), 0, m_columnCount(m_screenMode) * m_rowCount - 1S))
        m_page(m_activePage).Segments.Add(New Segment(m_rowCount - 1S, m_columnCount(m_screenMode), 0, m_columnCount(m_screenMode) * m_rowCount - 1S))

        BuildKeyOn()

      Else
        'm_segments.Add(New Segment(m_columnCount) With {.LocationBegin = 0, .LocationEnd = m_columnCount - 1})
        m_page(m_activePage).Segments.Add(New Segment(0, m_columnCount(m_screenMode), 0, m_columnCount(m_screenMode) * m_rowCount - 1S))
      End If

      If m_idirect IsNot Nothing Then
        m_idirect.Cls(argument)
      End If

    End Sub

    Friend ReadOnly Property CsrLin() As Short
      Get
        'If m_idirect IsNot Nothing Then
        '  Return m_idirect.CsrLin()
        'Else
        Return (m_location \ m_columnCount(m_screenMode)) + 1S
        'End If
      End Get
    End Property

    Friend ReadOnly Property Pos(dummy As Short) As Short
      Get
        'If m_idirect IsNot Nothing Then
        '  Return m_idirect.Pos(dummy)
        'Else
        Return (m_location Mod m_columnCount(m_screenMode)) + 1S
        'End If
      End Get
    End Property

    Private Function GetLocation(row As Short, col As Short) As Short
      Return ((row - 1S) * m_columnCount(m_screenMode)) + (col - 1S)
    End Function

    Friend Sub Locate(row As Short, col As Short)

      'TODO: need to check constraints against m_topRow and m_bottomRow (VIEW PRINT)

      If row = -1 Then
        row = CsrLin
      End If

      If col = -1 Then
        col = Pos(0)
      End If

      If m_viewActive Then
        If row < m_topLine OrElse row > m_bottomLine Then 'If(m_keyOn, m_rowCount - 1, m_rowCount) Then
          Throw New ArgumentException("row invalid.")
        End If
      Else
        If row < 1 OrElse row > m_rowCount Then '25 Then
          Throw New ArgumentException("row invalid.")
        End If
      End If

      If col < 1 OrElse col > m_columnCount(m_screenMode) Then
        Throw New ArgumentException("col invalid.")
      End If

      m_location = GetLocation(row, col) '((row - 1) * m_columnCount) + (col - 1)

      m_invalidated = True

      If m_idirect IsNot Nothing Then
        m_idirect.Locate(row, col)
        Return
      End If

    End Sub

    Private Sub Color()

      Select Case m_screenMode
        Case 0
          'Color(7, 0)
          Color(10, 0) ' bright green on black

          If m_idirect IsNot Nothing Then
            m_idirect.Color()
          End If

        Case 2
          Throw New ArgumentException("screenmode")

        Case Else
          ' Do nothing?

      End Select

    End Sub

    Private Sub Color(background As Short)

      Select Case m_screenMode
        Case 0

          If Not background.Between(0, 15) Then
            Throw New ArgumentException("background invalid.")
          End If

          If background > 7 Then
            background -= 8S
          End If

          m_backColor = background

          If m_idirect IsNot Nothing Then
            m_idirect.Color(background)
          End If

        Case 1

          ' Doesn't actually change the background color,
          ' just update what the background palette index represents.
          'SetPalette(0, m_palette(background))
          SetPalette(0, background)

        Case 2

          Throw New ArgumentException("screenmode invalid.")

        Case 7

          If Not background.Between(0, 15) Then
            Throw New ArgumentException("background invalid.")
          End If

          m_backColor = background

        Case 8

          If Not background.Between(0, 15) Then
            Throw New ArgumentException("background invalid.")
          End If

          m_backColor = background

        Case 9

          background = background Mod 16S

          If Not background.Between(0, 15) Then
            Throw New ArgumentException("background invalid.")
          End If

          m_backColor = background

        Case 10

          Stop

          If Not background.Between(0, 15) Then
            Throw New ArgumentException("background invalid.")
          End If

          m_backColor = background

        Case Else

          ' Do nothing?

      End Select

    End Sub

    Private Sub Color(foreground As Short, background As Short)

      Select Case m_screenMode

        Case -3
          Stop

        Case -4
          Stop

        Case 0

          If Not foreground.Between(0, 31) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          If background > -1 Then
            Color(background)
          End If

          m_foreColor = foreground

          If m_idirect IsNot Nothing Then
            m_idirect.Color(foreground, background)
          End If

        Case 1

          If foreground > -1 Then 'Actually, background.
            ' set the background color...
            Color(foreground)
          End If

          If background > -1 Then ' Actually, palette.

            Dim odd As Boolean = (background Mod 2) > 0

            If odd Then
              SetPalette(1, 3)
              SetPalette(2, 5)
              SetPalette(3, 15)
            Else
              SetPalette(1, 2)
              SetPalette(2, 4)
              SetPalette(3, 20)
            End If

          End If

        Case 2
          Stop

        Case 3

          If Not foreground.Between(0, 15) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          If background > -1 Then
            Color(background)
          End If

          m_foreColor = foreground

        Case 4

          If Not foreground.Between(0, 3) Then '15) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          If background > -1 Then
            Color(background)
          End If

          m_foreColor = foreground

        Case 5

          If Not foreground.Between(0, 15) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          If background > -1 Then
            Color(background)
          End If

          m_foreColor = foreground

        Case 6

          If Not foreground.Between(0, 3) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          If background > -1 Then
            Color(background)
          End If

          m_foreColor = foreground

        Case 7

          If Not foreground.Between(0, 15) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          If background > -1 Then
            Color(background)
          End If

          m_foreColor = foreground

        Case 8

          If Not foreground.Between(0, 15) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          If background > -1 Then
            Color(background)
          End If

          m_foreColor = foreground

        Case 9

          foreground = foreground Mod 16S

          If Not foreground.Between(0, 15) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          If background > -1 Then
            Color(background)
          End If

          m_foreColor = foreground

        Case 10

          'If background > -1 Then
          '  Color(background)
          'End If

          'If Not foreground.Between(0, 15) Then
          '  Throw New ArgumentException("foreground")
          'End If

          'm_foreColor = foreground

        Case 11
          Stop

        Case 12

          If Not foreground.Between(0, 255) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          m_foreColor = foreground

        Case 13

          If Not foreground.Between(0, 255) Then
            Throw New ArgumentException("foreground invalid.")
          End If

          m_foreColor = foreground

        Case Else
          ' Do nothing
      End Select

    End Sub

    ' Color for SCREEN 0
    Friend Sub Color0(foreground As Short, background As Short, border As Short)

      If Not border.Between(-1, 15) Then
        Throw New ArgumentException("border invalid.")
      End If

      If foreground = -1 AndAlso background = -1 Then
        Color()
      ElseIf foreground = -1 Then
        Color(background)
      Else
        Color(foreground, background)
      End If

    End Sub

    ' Color for SCREEN 1
    Friend Sub Color1(background As Short, palette As Short)

      If Not palette.Between(-1, 255) Then
        Throw New ArgumentException("palette invalid.")
      End If

      If background > -1 Then
        ' set the background color...
        Color(background)
      End If

      If palette > -1 Then

        Dim odd As Boolean = (palette Mod 2) > 0

        For index As Integer = 0 To m_page.Count - 1
          m_page(index).SetCgaAlternatePalette(Not odd)
        Next

        m_invalidated = True

        'If odd Then
        '  SetPalette(1, 3)
        '  SetPalette(2, 5)
        '  SetPalette(3, 63)
        'Else
        '  SetPalette(1, 2)
        '  SetPalette(2, 4)
        '  SetPalette(3, 20)
        'End If

      End If

    End Sub

    ' Color for SCREEN 7, 8, 9, 10
    Friend Sub ColorEga(foreground As Short, background As Short)

      If foreground = -1 AndAlso background = -1 Then
        Color()
      ElseIf foreground = -1 Then
        Color(background)
      Else
        Color(foreground, background)
      End If

    End Sub

    ' Color for SCREEN 4, 12, 13
    Friend Sub ColorVga(foreground As Short)
      If foreground = -1 Then
        Color()
      Else
        Color(foreground, m_backColor)
      End If
    End Sub

    'Friend Sub Color(ByVal param1 As Short, ByVal param2 As Short, ByVal param3 As Short)

    '  Select Case m_screenMode
    '    Case 0

    '      If Not param3.Between(-1, 15) Then
    '        Throw New ArgumentException("border")
    '      End If
    '      'TODO: Handle "border".

    '      If param1 = -1 AndAlso param2 = -1 Then
    '        Me.Color()
    '      ElseIf param1 = -1 Then
    '        Me.Color(param2)
    '      Else
    '        Me.Color(param1, param2)
    '      End If

    '    Case 1

    '      If param3 <> -1 Then
    '        Throw New ArgumentException("border")
    '      End If

    '      Me.Color(param1, param2)

    '    Case 2

    '    Case Else
    '      ' Do nothing...

    '  End Select

    'End Sub

    Friend Sub SetPalette()

      For index As Integer = 0 To m_page.Count - 1
        m_page(index).SetPalette()
      Next
      m_invalidated = True

      If m_idirect IsNot Nothing Then
        m_idirect.SetPalette()
      End If

    End Sub

    'Friend Sub SetPalette()

    '  Select Case m_screenMode
    '    Case 0

    '      m_palette(0) = 0
    '      m_palette(1) = 1
    '      m_palette(2) = 2
    '      m_palette(3) = 3
    '      m_palette(4) = 4
    '      m_palette(5) = 5
    '      m_palette(6) = 20
    '      m_palette(7) = 7
    '      m_palette(8) = 56
    '      m_palette(9) = 57
    '      m_palette(10) = 58
    '      m_palette(11) = 59
    '      m_palette(12) = 60
    '      m_palette(13) = 61
    '      m_palette(14) = 62
    '      m_palette(15) = 63

    '      For index As Integer = 16 To 31
    '        m_palette(index) = m_palette(index - 16)
    '      Next

    '    Case 1

    '      m_palette(0) = 0
    '      m_palette(1) = 3
    '      m_palette(2) = 5
    '      m_palette(3) = 63
    '      For index As Integer = 4 To 31
    '        m_palette(index) = -1
    '      Next

    '    Case 2

    '      m_palette(0) = 0
    '      m_palette(1) = 63
    '      For index As Integer = 2 To 31
    '        m_palette(index) = -1
    '      Next

    '      'Case 3

    '      'Case 4
    '      'Case 5
    '      'Case 6

    '    Case 7

    '      m_palette(0) = 0
    '      m_palette(1) = 1
    '      m_palette(2) = 2
    '      m_palette(3) = 3
    '      m_palette(4) = 4
    '      m_palette(5) = 5
    '      m_palette(6) = 20
    '      m_palette(7) = 7
    '      m_palette(8) = 56
    '      m_palette(9) = 57
    '      m_palette(10) = 58
    '      m_palette(11) = 59
    '      m_palette(12) = 60
    '      m_palette(13) = 61
    '      m_palette(14) = 62
    '      m_palette(15) = 63

    '      For index As Integer = 16 To 31
    '        m_palette(index) = -1
    '      Next

    '    Case 8

    '      m_palette(0) = 0
    '      m_palette(1) = 1
    '      m_palette(2) = 2
    '      m_palette(3) = 3
    '      m_palette(4) = 4
    '      m_palette(5) = 5
    '      m_palette(6) = 20
    '      m_palette(7) = 7
    '      m_palette(8) = 56
    '      m_palette(9) = 57
    '      m_palette(10) = 58
    '      m_palette(11) = 59
    '      m_palette(12) = 60
    '      m_palette(13) = 61
    '      m_palette(14) = 62
    '      m_palette(15) = 63

    '      For index As Integer = 16 To 31
    '        m_palette(index) = -1
    '      Next

    '    Case 9

    '      m_palette(0) = 0
    '      m_palette(1) = 1
    '      m_palette(2) = 2
    '      m_palette(3) = 3
    '      m_palette(4) = 4
    '      m_palette(5) = 5
    '      m_palette(6) = 20
    '      m_palette(7) = 7
    '      m_palette(8) = 56
    '      m_palette(9) = 57
    '      m_palette(10) = 58
    '      m_palette(11) = 59
    '      m_palette(12) = 60
    '      m_palette(13) = 61
    '      m_palette(14) = 62
    '      m_palette(15) = 63

    '      For index As Integer = 16 To 31
    '        m_palette(index) = -1
    '      Next

    '      'Case 10
    '      'Case 11
    '      'Case 12
    '      'Case 13
    '    Case Else

    '  End Select

    'End Sub

    Friend Sub SetPalette(attribute As Integer, color As Integer)

      Select Case m_screenMode
        Case 0
          If Not attribute.Between(0, 31) Then
            Throw New ArgumentException("attribute invalid.")
          End If
          If Not color.Between(0, 63) Then
            Throw New ArgumentException("color invalid.")
          End If

          If m_idirect IsNot Nothing Then
            m_idirect.SetPalette(attribute, color)
          End If

        Case 1
          If Not attribute.Between(0, 3) Then
            Throw New ArgumentException("attribute invalid.")
          End If
          'If Not color.Between(0, 15) Then
          '  Throw New ArgumentException("color")
          'End If
          If Not color.Between(0, 63) Then
            Throw New ArgumentException("color invalid.")
          End If

        Case 2
          If Not attribute.Between(0, 1) Then
            Throw New ArgumentException("attribute invalid.")
          End If
          If Not color.Between(0, 15) Then
            Throw New ArgumentException("color invalid.")
          End If

        Case 7
          If Not attribute.Between(0, 15) Then
            Throw New ArgumentException("attribute invalid.")
          End If
          If Not color.Between(0, 15) Then
            Throw New ArgumentException("color invalid.")
          End If

        Case 8
          If Not attribute.Between(0, 15) Then
            Throw New ArgumentException("attribute invalid.")
          End If
          If Not color.Between(0, 15) Then
            Throw New ArgumentException("color invalid.")
          End If

        Case 9
          If Not attribute.Between(0, 15) Then
            Throw New ArgumentException("attribute invalid.")
          End If
          If Not color.Between(0, 63) Then
            Throw New ArgumentException("color invalid.")
          End If

      End Select

      For index As Integer = 0 To m_page.Count - 1
        m_page(index).SetPalette(attribute, color)
      Next
      'm_palette(attribute) = color
      m_invalidated = True

    End Sub

    Friend Sub Print()

      If m_idirect IsNot Nothing Then
        m_idirect.Locate(m_idirect.CsrLin + 1S, m_idirect.Pos(-1))
      End If

      ' Determine the current line number based on the current cursor location.
      Dim line As Integer = 1 + m_location \ m_columnCount(m_screenMode)

      ' Handle special line 25 rules
      If line = m_rowCount Then '25 Then

        ' If the line is currently 25 and a CR is requested...
        ' place cursor at beginning of line 24.
        ShiftViewUp()
        m_location = (m_bottomLine - 1S) * m_columnCount(m_screenMode)

      Else

        If line = m_bottomLine Then
          ShiftViewUp()
        Else

          If Me.m_location Mod Me.m_columnCount(Me.m_screenMode) = 0 Then
            m_location += m_columnCount(m_screenMode)
          Else
            m_location += m_columnCount(m_screenMode) - (m_location Mod m_columnCount(m_screenMode))
          End If

          line = m_location \ m_columnCount(m_screenMode)

          If line = m_bottomLine Then
            ShiftViewUp()
          End If

        End If

        Dim found = (Aggregate a In m_page(m_activePage).Segments Where m_location.Between(a.LocationBegin, a.LocationEnd) Into Count()) > 0

        If Not found Then
          m_page(m_activePage).Segments.Add(New Segment(m_location, m_location + m_columnCount(m_screenMode) - 1S, m_columnCount(m_screenMode), 0, m_columnCount(m_screenMode) * m_rowCount - 1S))
        End If

      End If

    End Sub

    Private Function LocationToLine(location As Short) As Short
      Return 1S + location \ m_columnCount(m_screenMode)
    End Function

    Public Sub Print(text As String, lineFeed As Boolean, Optional isInPrintLoop As Boolean = False)

      InternalPrint(text, lineFeed, isInPrintLoop)

    End Sub

    Private Sub InternalPrint(text As String, lineFeed As Boolean, Optional isInPrintLoop As Boolean = False)

      Dim segments = From p In m_page(m_activePage).Segments
                     Where m_location.Between(p.LocationBegin, p.LocationEnd)

      If Not segments.Any Then

        ' need to either merge with an existing segment (same line) or
        ' create new segment.

        Dim beginOfLine = (m_location \ m_columnCount(m_screenMode)) * m_columnCount(m_screenMode)

        segments = From p In m_page(m_activePage).Segments
                   Where beginOfLine = p.LocationBegin

        If Not segments.Any Then
          ' New line?
          'm_segments.Add(New Segment() With {.Begin = beginOfLine, .End = beginOfLine})
          m_page(m_activePage).Segments.Add(New Segment(beginOfLine, beginOfLine + m_columnCount(m_screenMode) - 1S, m_columnCount(m_screenMode), 0, m_columnCount(m_screenMode) * m_rowCount - 1S))
          segments = From p In m_page(m_activePage).Segments
                     Where beginOfLine = p.LocationBegin
        Else
          ' Expand to include this location.
          'segments(0).LocationEnd = m_location
          Stop
        End If
        'Else
        '  ' Do we need to expand the current selection?
        '  If segments(0).Length < text.Length Then

        '  End If
      End If

      Dim segmentCount As Integer = m_page(m_activePage).Segments.Count
      Dim segmentIndex As Integer = m_page(m_activePage).Segments.Count - 1

      For index As Integer = 0 To m_page(m_activePage).Segments.Count - 1
        If m_location.Between(m_page(m_activePage).Segments(index).LocationBegin, m_page(m_activePage).Segments(index).LocationEnd) Then
          segmentIndex = index
          Exit For
        End If
      Next

      ' Line 25 is a special line.
      If LocationToLine(m_location) = m_rowCount Then '25 Then

        Dim max As Short = GetLocation(m_rowCount, m_columnCount(m_screenMode)) '25, m_columnCount(m_screenMode))

        ' For each character to be written to the display...
        For index As Short = 0 To CShort(text.Length - 1)

          ' Write a character to the display...
          WriteCharacter(CByte(AscW(text(index))), m_location)

          If m_location < max Then
            m_location += 1S
          End If

        Next

        If Not isInPrintLoop Then
          m_location = (m_bottomLine - 1S) * m_columnCount(m_screenMode)
          If lineFeed Then
            Print()
          End If
        End If

      Else

        Dim max As Short = GetLocation(m_bottomLine, m_columnCount(m_screenMode))

        ' For each character to be written to the display...
        For index As Short = 0 To CShort(text.Length - 1)

          '' Have we reached the end of this display segment?
          'If m_location = m_segments(segmentIndex).LocationEnd Then
          '  ' If so, expand segment.
          '  m_segments(segmentIndex).Expand()
          'End If

          ' Write a character to the display...
          WriteCharacter(CByte(AscW(text(index))), m_location)
          m_location += 1S

          ' Determine if the cursor has gone past the view size...
          If m_location > max Then

            ' If so, shift everything up...

            ' Because of the line 25 special handling, need to move the cursor back to the editing line...
            m_location -= 1S
            ' Cause a print line to occur which will shift the screen up.
            Print()

            ' Now need to find the current segment index again since we've shifted things.
            For search As Integer = 0 To m_page(m_activePage).Segments.Count - 1
              If m_location.Between(m_page(m_activePage).Segments(search).LocationBegin, m_page(m_activePage).Segments(search).LocationEnd) Then
                segmentIndex = search
                Exit For
              End If
            Next

            ' Now need to handle the fact that, for some reason,
            ' the previously written character is "lost" (reverting to a NULL value); 
            ' so we will move the cursor back, redraw the character and then continue moving 
            ' forward as previously intended.
            m_location -= 1S
            WriteCharacter(CByte(AscW(text(index))), m_location)
            m_location += 1S

          End If

          ' Have we reached the end of this display segment?
          If m_location = m_page(m_activePage).Segments(segmentIndex).LocationEnd Then
            ' If so, expand segment.
            m_page(m_activePage).Segments(segmentIndex).Expand()
          End If

        Next

        If lineFeed Then
          Print()
        End If

      End If

    End Sub

#End Region

#Region "Paint"

    Public MustOverride Sub Paint()

    'Dim bitmap As WriteableBitmap

    'If TypeOf m_canvas.Background Is ImageBrush Then
    '  bitmap = New WriteableBitmap(DirectCast(m_canvas.Background, ImageBrush).ImageSource)
    'Else
    '  bitmap = New WriteableBitmap(640, 400)
    'End If

    ''If m_clearScreened Then

    ''  Dim cls As New Rectangle()
    ''  cls.Width = 640
    ''  cls.Height = 400
    ''  cls.Fill = New SolidColorBrush(m_backColor)

    ''  bitmap.Render(cls, New MatrixTransform())

    ''  bitmap.Invalidate()

    ''  m_clearScreened = False

    ''End If

    'Dim charWidth As Short = 8
    'Dim charHeight As Short = 16

    'For index As Short = 0 To m_screen.Count - 1

    '  Dim r As Short = index \ m_columnCount
    '  Dim c As Short = index Mod m_columnCount

    '  Dim x As Short = c * charWidth
    '  Dim y As Short = r * charHeight

    '  If m_cursorLocation > -1 AndAlso m_location <> m_cursorLocation Then

    '    Dim cursorX As Short = (m_cursorLocation Mod m_columnCount) * charWidth
    '    Dim cursorY As Short = (m_cursorLocation \ m_columnCount) * charHeight

    '    Dim cursorColor = ConvertToARGB32(Colors.Black)
    '    For h = 0 To If(m_cursorThick, 4, 0)
    '      For w As Short = 0 To charWidth - 1
    '        bitmap.Pixels(((cursorY + charHeight - 1 - h) * bitmap.PixelWidth) + (cursorX + w)) = cursorColor
    '      Next
    '    Next

    '    m_screen(m_cursorLocation).Updated = True

    '    m_cursorLocation = -1
    '    m_cursorThick = False

    '  End If

    '  If index = m_location AndAlso Not m_cursorToggle Then

    '    Dim cursorColor = ConvertToARGB32(Colors.Black)
    '    For h = 0 To If(m_cursorThick, 4, 0)
    '      For w As Short = 0 To charWidth - 1
    '        bitmap.Pixels(((y + charHeight - 1 - h) * bitmap.PixelWidth) + (x + w)) = cursorColor
    '      Next
    '    Next

    '    m_screen(index).Updated = True

    '    m_cursorLocation = -1
    '    m_cursorThick = False

    '  End If

    '  If m_screen(index).Updated Then

    '    Dim foreColor = ConvertToARGB32(m_screen(index).ForeColor)
    '    Dim backColor = ConvertToARGB32(m_screen(index).BackColor)
    '    Dim map As Short() = CharMap.CharMap(m_screen(index).Char)

    '    For cr As Short = 0 To charHeight - 1
    '      For cc As Short = 0 To charWidth - 1
    '        If map((cr * charWidth) + cc) = 1 Then
    '          bitmap.Pixels(((y + cr) * bitmap.PixelWidth) + (x + cc)) = foreColor
    '        Else
    '          bitmap.Pixels(((y + cr) * bitmap.PixelWidth) + (x + cc)) = backColor
    '        End If
    '      Next
    '    Next

    '    m_screen(index).Updated = False

    '  End If

    '  If index = m_location AndAlso m_cursorToggle Then
    '    Dim cursorColor = ConvertToARGB32(Colors.Gray)
    '    m_cursorThick = m_insert
    '    For h = 0 To If(m_cursorThick, 4, 0)
    '      For w As Short = 0 To charWidth - 1
    '        bitmap.Pixels(((y + charHeight - 1 - h) * bitmap.PixelWidth) + (x + w)) = cursorColor
    '      Next
    '    Next
    '    m_cursorLocation = m_location
    '  End If

    '  If index = m_location Then
    '    m_cursorToggle = Not m_cursorToggle
    '  End If

    '  ' Debug visual queues...

    '  If m_showVisualQueues Then

    '    Dim position As Short = index
    '    Dim startOfSegment = (Aggregate a In m_segments Where a.LocationBegin = position Into Count()) > 0
    '    Dim endOfSegment = (Aggregate a In m_segments Where a.LocationEnd = position Into Count()) > 0

    '    Dim cursorColor = If(startOfSegment, ConvertToARGB32(Colors.Green), ConvertToARGB32(Colors.Black))
    '    For w As Short = 0 To 1
    '      bitmap.Pixels(((y + (charHeight \ 2)) * bitmap.PixelWidth) + (x + w)) = cursorColor
    '    Next
    '    cursorColor = If(endOfSegment, ConvertToARGB32(Colors.Red), ConvertToARGB32(Colors.Black))
    '    For w As Short = charWidth - 2 To charWidth - 1
    '      bitmap.Pixels(((y + (charHeight \ 2)) * bitmap.PixelWidth) + (x + w)) = cursorColor
    '    Next

    '    'If m_screen(index).Char = ChrW(0) Then
    '    '  Dim color As Windows.Media.Color = Windows.Media.Color.FromArgb(0, 255, 255, 0)
    '    '  Dim cursorColor = ConvertToARGB32(color)
    '    '  bitmap.Pixels(((y + (charHeight \ 2)) * bitmap.PixelWidth) + (x + (charWidth \ 2))) = cursorColor
    '    'End If

    '  End If

    'Next

    'bitmap.Invalidate()

    'If TypeOf m_canvas.Background Is ImageBrush Then
    '  DirectCast(m_canvas.Background, ImageBrush).ImageSource = bitmap
    'Else

    '  Dim brush As New ImageBrush

    '  brush.ImageSource = bitmap
    '  brush.Stretch = Stretch.Fill
    '  m_canvas.Background = brush

    'End If

    'End Sub

    Private Sub InternalTimer_Tick() 'Handles InternalTimer.Elapsed
      'If m_canvas.Visibility = Visibility.Collapsed Then
      '  m_timer.Stop()
      '  m_timer = Nothing
      'Else
      If m_syncContext IsNot Nothing Then
        m_syncContext.Post(AddressOf HandleTimer, Nothing)
      Else
        HandleTimer(Nothing)
      End If
      'End If
    End Sub

    Private Sub HandleTimer(state As Object)
      Paint()
    End Sub

    'Private Function ConvertToARGB32(ByVal color As Color) As Short
    '  Return (CShort(color.A) << 24) Or (CShort(color.R) << 16) Or (CShort(color.G) << 8) Or (CShort(color.B))
    'End Function

    'Private Sub ShiftScreenUp()

    '  Dim max As Short = (m_screen.Count - m_columnCount)
    '  If m_keyOn Then max -= m_columnCount

    '  For index As Short = 0 To max - 1
    '    m_screen(index) = m_screen(index + m_columnCount)
    '    m_screen(index).Updated = True
    '  Next

    '  For index As Short = max To max + m_columnCount - 1
    '    m_screen(index) = New Character() With {.Char = ChrW(0),
    '                                            .ForeColor = m_foreColor,
    '                                            .BackColor = m_backColor,
    '                                            .Updated = True}
    '  Next

    '  ' Adjust segments since we scrolled up a  line.
    '  For index As Short = m_segments.Count - 1 To 0 Step -1
    '    m_segments(index).ShiftUp()
    '    If m_segments(index).LocationEnd < 0 Then
    '      m_segments.RemoveAt(index)
    '    End If
    '  Next

    '  m_location = max

    'End Sub

    Private Sub ShiftViewUp()

      ' Shift view up by one row.

      Dim min As Short = GetLocation(m_topLine, 1)
      Dim max As Short = GetLocation(m_bottomLine, 1) - 1S

      '----------------------------------------
      ' New Way
      '----------------------------------------

      Dim oldPosition As Integer = min + m_columnCount(m_screenMode)
      Dim oldRow As Integer = oldPosition \ m_columnCount(m_screenMode)
      Dim oldColumn As Integer = oldPosition Mod m_columnCount(m_screenMode)
      Dim oldX As Integer = oldColumn * m_characterWidth
      Dim oldY As Integer = oldRow * m_characterHeight

      Dim newPosition As Integer = min
      Dim newRow As Integer = newPosition \ m_columnCount(m_screenMode)
      Dim newColumn As Integer = newPosition Mod m_columnCount(m_screenMode)
      Dim newX As Integer = newColumn * m_characterWidth
      Dim newY As Integer = newRow * m_characterHeight

      Dim si As Integer = (oldY * m_screenWidth) + oldX
      Dim di As Integer = (newY * m_screenWidth) + newX
      Dim c As Integer = (max - min) * (m_characterWidth * m_characterHeight)
      Array.Copy(m_page(m_activePage).Colors, si, m_page(m_activePage).Colors, di, c)
      Array.Copy(m_page(m_activePage).Pixels, si, m_page(m_activePage).Pixels, di, c)

      Array.Copy(m_page(m_activePage).CharacterBuffer, min + m_columnCount(m_screenMode), m_page(m_activePage).CharacterBuffer, min, max - min)
      Array.Copy(m_page(m_activePage).CharacterAttribute, min + m_columnCount(m_screenMode), m_page(m_activePage).CharacterAttribute, min, max - min)

      'For index As Short = min To max
      '  m_page(m_activePage).CharacterBuffer(index) = m_page(m_activePage).CharacterBuffer(index + m_columnCount(m_screenMode))
      'Next

      '----------------------------------------
      ' Old Way
      '----------------------------------------

      'For index As Short = min To max
      '  'm_page(m_activePage).CharacterBuffer(index) = m_page(m_activePage).CharacterBuffer(index + m_columnCount(m_screenMode))
      '  'm_page(m_activePage).CharacterBuffer(index).Updated = True
      '  CloneCharacter(index + m_columnCount(m_screenMode), index)
      'Next

      '----------------------------------------

      ' Clear the last row of the view.

      For index As Short = max + 1S To max + m_columnCount(m_screenMode)
        'm_page(m_activePage).CharacterBuffer(index) = New Character() With {.Char = ChrW(0),
        '                                        .ForeColor = m_foreColor,
        '                                        .BackColor = m_backColor,
        '                                        .Updated = True}
        ResetCharacter(index)
      Next

      'TODO: Need to only adjust segments that are in the view range.

      ' Adjust segments since we scrolled up a line.
      For index As Short = CShort(m_page(m_activePage).Segments.Count - 1) To 0 Step -1
        m_page(m_activePage).Segments(index).ShiftUp()
        If m_page(m_activePage).Segments(index).LocationEnd < 0 Then
          m_page(m_activePage).Segments.RemoveAt(index)
        End If
      Next

      m_location = max + 1S

      m_invalidated = True

    End Sub

#End Region

#Region "Editor controls"

    Friend ReadOnly Property CapsLock As Boolean
      Get
        Return m_capsLock
      End Get
    End Property

    Friend ReadOnly Property ShiftDown As Boolean
      Get
        Return m_shiftDown
      End Get
    End Property

    Friend ReadOnly Property ControlDown As Boolean
      Get
        Return m_controlDown
      End Get
    End Property

    Friend ReadOnly Property AltDown As Boolean
      Get
        Return m_altDown
      End Get
    End Property

    Friend Sub EditInsert()
      m_insert = Not m_insert
    End Sub

    Friend Sub EditCapsLock()
      m_capsLock = Not m_capsLock
    End Sub

    Friend Sub EditShiftDown()
      m_shiftDown = True
    End Sub

    Friend Sub EditShiftUp()
      m_shiftDown = False
    End Sub

    Friend Sub EditControlDown()
      m_controlDown = True
    End Sub

    Friend Sub EditControlUp()
      m_controlDown = False
    End Sub

    Friend Sub EditAltDown()
      m_altDown = True
    End Sub

    Friend Sub EditAltUp()
      m_altDown = False
    End Sub

    Friend Sub EditKeyPress(ch As Char)

      If m_insert Then

        ' Determine active segment.
        Dim segmentIndex As Integer = -1
        For index As Integer = 0 To m_page(m_activePage).Segments.Count - 1
          If m_location.Between(m_page(m_activePage).Segments(index).LocationBegin, m_page(m_activePage).Segments(index).LocationEnd) Then
            segmentIndex = index
            Exit For
          End If
        Next

        ' Determine the number of characters in the current segment.

        Dim length As Short = 0 'GetCurrentLineTextLength()

        For index As Short = m_page(m_activePage).Segments(segmentIndex).LocationBegin To m_page(m_activePage).Segments(segmentIndex).LocationEnd
          If Me.m_page(Me.m_activePage).CharacterBuffer(index) = 0 Then
            Exit For
          End If
          length += 1S
        Next

        ' If increasing the length of the string would exceed the number of characters
        ' that can fit in the current segment...
        If length > m_page(m_activePage).Segments(segmentIndex).Length - 1 Then
          'Dim line = (m_location \ m_columnCount(m_screenMode))
          Dim line = (m_page(m_activePage).Segments(segmentIndex).LocationEnd \ m_columnCount(m_screenMode))
          If line = 24 Then
            Dim location As Short = m_location
            ShiftViewUp()
            m_location = location - m_columnCount(m_screenMode)
            line = 23
          End If
          ExpandLine(line)
          For index = m_page(m_activePage).Segments.Count - 1 To 0
            If m_page(m_activePage).Segments(index).LocationBegin > (m_screenHeight * m_screenWidth) - 1 Then
              m_page(m_activePage).Segments.RemoveAt(index)
            ElseIf m_page(m_activePage).Segments(index).LocationEnd > (m_screenHeight * m_screenWidth) - 1 Then
              m_page(m_activePage).Segments(index).Shorten()
            End If
          Next
        End If

        ''If m_location > FindBeginOfLine() AndAlso
        ''   length Mod m_columnCount(m_screenMode) = 0 Then

        'If m_location Mod m_columnCount(m_screenMode) = 0 Then

        '  ' We've increased the line length.

        '  Dim line = (m_location \ m_columnCount(m_screenMode))
        '  ExpandLine(line)

        'End If

        Dim sections = From p In m_page(m_activePage).Segments
                       Where m_location.Between(p.LocationBegin, p.LocationEnd)

        If sections.Any Then

          For index As Short = sections(0).LocationEnd To m_location + 1S Step -1
            'm_page(m_activePage).CharacterBuffer(index).Clone(m_page(m_activePage).CharacterBuffer(index - 1))
            CloneCharacter(index - 1, index)
          Next

          Print(ch, False)

        End If

      Else
        Print(ch, False)
      End If

    End Sub

    Friend Sub EditReturn()

      Dim beginOfLine = FindBeginOfLine()
      Dim endofline = FindEndOfLine()

      If beginOfLine > -1 AndAlso endofline > -1 Then

        Print()

      Else

        Stop

      End If

    End Sub

    Friend Sub EditArrowUp()
      'If m_location > m_columnCount - 1 Then m_location -= m_columnCount
      If m_location > GetLocation(m_topLine + 1S, 1) Then m_location -= m_columnCount(m_screenMode)
    End Sub

    Friend Sub EditArrowDown()
      'Dim max As Short = If(m_keyOn, m_screen.Length - m_columnCount - m_columnCount, m_screen.Length - m_columnCount)
      Dim max As Short = GetLocation(m_bottomLine - 1S, m_columnCount(m_screenMode))
      If m_location < max Then m_location += m_columnCount(m_screenMode)
    End Sub

    Friend Sub EditArrowLeft()
      'If m_location > 0 Then m_location -= 1
      If m_location > GetLocation(m_topLine, 1) Then m_location -= 1S
    End Sub

    Friend Sub EditArrowRight()
      'Dim max As Short = If(m_keyOn, m_screen.Length - m_columnCount - 1, m_screen.Length - 1)
      Dim max As Short = GetLocation(m_bottomLine, m_columnCount(m_screenMode))
      If m_location < max Then m_location += 1S
    End Sub

    Friend Sub EditHome()
      'm_location = 0
      m_location = GetLocation(m_topLine, 1)
      m_invalidated = True
    End Sub

    Friend Sub EditEnd()

      ' Need to look at the characters on the line to determine where to seek.

      Dim beginOfLine = FindBeginOfLine()
      Dim endOfLine = FindEndOfLine()

      For seek As Short = endOfLine To beginOfLine Step -1
        'If m_page(m_activePage).CharacterBuffer(seek).Char <> ChrW(0) Then
        If m_page(m_activePage).CharacterBuffer(seek) <> 0 Then
          m_location = seek + 1S
          Exit For
        End If
      Next

      m_invalidated = True

    End Sub

    Friend Sub EditBackspace()

      Dim beginOfLine = FindBeginOfLine()
      Dim endOfLine = FindEndOfLine()

      If beginOfLine > -1 AndAlso beginOfLine < endOfLine Then

        If m_location = beginOfLine Then
          EditDelete()
        Else

          Dim length As Short = GetCurrentLineTextLength()

          For index As Short = m_location To endOfLine
            'm_page(m_activePage).CharacterBuffer(index - 1).Clone(m_page(m_activePage).CharacterBuffer(index))
            CloneCharacter(index, index - 1)
          Next

          'm_page(m_activePage).CharacterBuffer(endOfLine).Reset(m_foreColor, m_backColor)
          ResetCharacter(endOfLine)

          m_location -= 1S

          Dim segments = From p In m_page(m_activePage).Segments
                         Where m_location.Between(p.LocationBegin, p.LocationEnd)

          If length = m_columnCount(m_screenMode) AndAlso
             (length - 1) Mod m_columnCount(m_screenMode) = (m_columnCount(m_screenMode) - 1) Then

            ' We've reduced the line count, need to move lines up.

            Dim line = (m_location \ m_columnCount(m_screenMode))
            ReduceLine(line + 1S)

          End If

        End If

      End If

      m_invalidated = True

    End Sub

    Friend Sub EditDelete()

      Dim beginOfLine = FindBeginOfLine()
      Dim endOfLine = FindEndOfLine()

      If beginOfLine > -1 AndAlso beginOfLine < endOfLine AndAlso m_location <= endOfLine Then

        Dim length As Short = GetCurrentLineTextLength()

        For index As Short = m_location To endOfLine - 1S
          'm_page(m_activePage).CharacterBuffer(index).Clone(m_page(m_activePage).CharacterBuffer(index + 1))
          CloneCharacter(index + 1, index)
        Next

        'm_page(m_activePage).CharacterBuffer(endOfLine).Reset(m_foreColor, m_backColor)
        ResetCharacter(endOfLine)

        Dim segments = From p In m_page(m_activePage).Segments
                       Where m_location.Between(p.LocationBegin, p.LocationEnd)

        If length = m_columnCount(m_screenMode) AndAlso
           (length - 1) Mod m_columnCount(m_screenMode) = (m_columnCount(m_screenMode) - 1) Then

          ' We've reduced the line count, need to move lines up.

          Dim line = (m_location \ m_columnCount(m_screenMode))
          ReduceLine(line + 1S)

        End If

      End If

    End Sub

    Friend Sub EditEscape()

      Dim segments = From p In m_page(m_activePage).Segments
                     Where m_location.Between(p.LocationBegin, p.LocationEnd)

      If segments.Any Then

        For seek As Short = segments(0).LocationEnd To segments(0).LocationBegin Step -1
          'm_page(m_activePage).CharacterBuffer(seek).Char = ChrW(0)
          'm_page(m_activePage).CharacterBuffer(seek).ForeColor = m_foreColor
          'm_page(m_activePage).CharacterBuffer(seek).BackColor = m_backColor
          'm_page(m_activePage).CharacterBuffer(seek).Updated = True
          WriteCharacter(0, seek)
        Next

        m_location = segments(0).LocationBegin

        If segments(0).LineCount > 1 Then
          For index As Short = 2 To segments(0).LineCount
            ReduceLine(segments(0).Line)
          Next
        End If

      End If

      m_invalidated = True

    End Sub

    Friend Sub EditTab()
      For seek As Short = 0 To 7
        EditKeyPress(" "c)
      Next
    End Sub

    'Friend Sub EditToggleQueues()
    '  m_showVisualQueues = Not m_showVisualQueues
    'End Sub

#End Region

    Private Class GraphicView
      Public Property IsScreen As Boolean
      Public Property UpperLeft As Drawing.Point
      Public Property LowerRight As Drawing.Point
      Public ReadOnly Property Left As Integer
        Get
          Return UpperLeft.X
        End Get
      End Property
      Public ReadOnly Property Right As Integer
        Get
          Return LowerRight.X
        End Get
      End Property
      Public ReadOnly Property Top As Integer
        Get
          Return UpperLeft.Y
        End Get
      End Property
      Public ReadOnly Property Bottom As Integer
        Get
          Return LowerRight.Y
        End Get
      End Property
    End Class

    Private ReadOnly m_graphicView As New GraphicView With {.IsScreen = True,
                                                   .UpperLeft = New Drawing.Point(0, 0),
                                                   .LowerRight = New Drawing.Point(m_screenWidth - 1, m_screenHeight - 1)}

    Friend Sub GraphicViewReset()
      m_graphicView.IsScreen = True
      m_graphicView.UpperLeft = New Drawing.Point(0, 0)
      m_graphicView.LowerRight = New Drawing.Point(m_screenWidth - 1, m_screenHeight - 1)
    End Sub

    Friend Function View(isScreen As Boolean?, x1 As Integer?, y1 As Integer?, x2 As Integer?, y2 As Integer?, fill As Integer?, border As Integer?) As Boolean

      If m_idirect IsNot Nothing Then
        Return m_idirect.View(isScreen, x1, y1, x2, y2, fill, border)
      End If

      ' isScreen... True: 0,0 screen position is the physical screen 0,0; False (Default): 0,0 is the upper left hand portion of the defined view port.
      ' fill....... If provided, the color attribute to "CLS" the view port to the defined background color.
      ' border..... If provided, the color attribute to draw a border around the view port (if on the physical screen).

      ' Reset...
      GraphicViewReset()

      If isScreen Is Nothing AndAlso
         x1 Is Nothing AndAlso
         y1 Is Nothing AndAlso
         x2 Is Nothing AndAlso
         y2 Is Nothing AndAlso
         fill Is Nothing AndAlso
         border Is Nothing Then
        ' Return...
        Return True
      End If

      If fill IsNot Nothing Then
        Line(CInt(x1), CInt(y1), CInt(x2), CInt(y2), CShort(fill), True, True, Nothing)
      End If

      If border IsNot Nothing Then
        Line(CInt(x1), CInt(y1), CInt(x2), CInt(y2), CShort(border), True, False, Nothing)
      End If

      m_graphicView.IsScreen = If(isScreen, False)
      m_graphicView.UpperLeft = New Drawing.Point(CInt(x1), CInt(y1))
      m_graphicView.LowerRight = New Drawing.Point(CInt(x2), CInt(y2))

      Return True

    End Function

  End Class

End Namespace
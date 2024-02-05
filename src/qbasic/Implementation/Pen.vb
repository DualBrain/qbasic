'Imports SSDD.Basic.Input

'Public Class Pen
'  Implements IPen

'  Public Event PositionChanged(sender As Object, e As PositionChangedEventArgs) Implements IPen.PositionChanged
'  Public Event SwitchChanged(sender As Object, e As SwitchChangedEventArgs) Implements IPen.SwitchChanged

'  Private WithEvents m_image As System.Windows.Controls.Image

'  'Private m_useTouch As Boolean
'  'Private m_useTapBehavior As Boolean ' Determines if we are working like a mouse (false) or more touch-centric (true).

'  Public Sub New(ByVal image As System.Windows.Controls.Image) 'useTapBehavior As Boolean,
'    'useTouch As Boolean)

'    m_image = image

'    'm_useTapBehavior = useTapBehavior
'    'm_useTouch = useTouch

'    'If useTouch Then
'    '  AddHandler Touch.FrameReported, AddressOf OnTouchFrameReported
'    'End If

'  End Sub

'  'Private Sub OnTouchFrameReported(sender As Object, e As TouchFrameEventArgs)

'  '  Dim touchPoint = e.GetPrimaryTouchPoint(Nothing)

'  '  Dim x As Integer = touchPoint.Position.X
'  '  Dim y As Integer = touchPoint.Position.Y

'  '  If touchPoint.Action = TouchAction.Down Then
'  '    RaiseEvent SwitchChanged(Me, New Basic.Interpreter.Input.SwitchChangedEventArgs(True))
'  '  ElseIf touchPoint.Action = TouchAction.Up Then
'  '    RaiseEvent SwitchChanged(Me, New Basic.Interpreter.Input.SwitchChangedEventArgs(False))
'  '  End If

'  '  RaiseEvent PositionChanged(Me, New Basic.Interpreter.Input.PositionChangedEventArgs(x, y))

'  'End Sub

'  'Public Property Image As System.Windows.Controls.Image
'  '  Get
'  '    Return m_image
'  '  End Get
'  '  Set(value As System.Windows.Controls.Image)
'  '    m_image = value
'  '  End Set
'  'End Property

'  Private Sub m_image_MouseLeftButtonDown(sender As Object, e As System.Windows.Input.MouseButtonEventArgs) Handles m_image.MouseLeftButtonDown
'    'If Not m_useTouch AndAlso Not m_useTapBehavior Then
'    RaiseEvent SwitchChanged(Me, New SwitchChangedEventArgs(True))
'    'End If
'  End Sub

'  Private Sub m_image_MouseLeftButtonUp(sender As Object, e As System.Windows.Input.MouseButtonEventArgs) Handles m_image.MouseLeftButtonUp
'    'If Not m_useTouch AndAlso Not m_useTapBehavior Then
'    RaiseEvent SwitchChanged(Me, New SwitchChangedEventArgs(False))
'    'End If
'  End Sub

'  Private Sub m_image_MouseMove(sender As Object, e As System.Windows.Input.MouseEventArgs) Handles m_image.MouseMove
'    'If Not m_useTouch Then
'    Dim x As Integer = e.GetPosition(m_image).X
'    Dim y As Integer = e.GetPosition(m_image).Y
'    RaiseEvent PositionChanged(Me, New PositionChangedEventArgs(x, y))
'    'End If
'  End Sub

'  'Private Sub m_image_Tap(sender As Object, e As System.Windows.Input.GestureEventArgs) Handles m_image.Tap
'  '  If m_useTapBehavior Then
'  '    Dim position = e.GetPosition(m_image)
'  '    Dim x As Integer = e.GetPosition(m_image).X
'  '    Dim y As Integer = e.GetPosition(m_image).Y
'  '    RaiseEvent PositionChanged(Me, New Basic.Interpreter.Input.PositionChangedEventArgs(x, y))
'  '    RaiseEvent SwitchChanged(Me, New Basic.Interpreter.Input.SwitchChangedEventArgs(True))
'  '    RaiseEvent SwitchChanged(Me, New Basic.Interpreter.Input.SwitchChangedEventArgs(False))
'  '  End If
'  'End Sub

'  Public ReadOnly Property SurfaceHeight As Integer Implements IPen.SurfaceHeight
'    Get
'      Return m_image.ActualHeight
'    End Get
'  End Property

'  Public ReadOnly Property SurfaceWidth As Integer Implements IPen.SurfaceWidth
'    Get
'      Return m_image.ActualWidth
'    End Get
'  End Property

'End Class
'Imports SSDD.Basic.Timers

'Public Class Timer
'  Inherits System.Windows.Threading.DispatcherTimer
'  Implements ITimer

'  Private m_interval As Short = 0

'  Public Sub New()
'    MyBase.New()

'    AddHandler MyBase.Tick, AddressOf Me_Tick

'  End Sub

'  Public Shadows Property Interval As Integer Implements ITimer.Interval
'    Get
'      Return m_interval
'    End Get
'    Set(value As Integer)
'      m_interval = value
'      MyBase.Interval = New TimeSpan(0, 0, 0, 0, m_interval)
'    End Set
'  End Property

'  Public Shadows Sub Start() Implements ITimer.Start
'    MyBase.Start()
'  End Sub

'  Public Shadows Sub [Stop]() Implements ITimer.Stop
'    MyBase.Stop()
'  End Sub

'  Public Shadows Event Tick(sender As Object, e As System.EventArgs) Implements ITimer.Tick

'  Private Sub Me_Tick(sender As Object, e As EventArgs)
'    RaiseEvent Tick(Me, e)
'  End Sub

'End Class
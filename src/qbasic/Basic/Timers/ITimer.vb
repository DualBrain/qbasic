Namespace Global.Basic.Timers

  Public Interface ITimer

    Event Tick(sender As Object, e As System.EventArgs)

    Property Interval As Integer

    Sub Start()
    Sub [Stop]()

  End Interface

End Namespace
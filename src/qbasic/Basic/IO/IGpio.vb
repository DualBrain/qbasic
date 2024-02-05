Namespace Global.Basic.IO

  Public Interface IGpio

    Function IsAvailable() As Boolean ' True = GPIO.  False = No GPIO available.

    Function PinMode(pin As Integer, value As Integer) As Boolean

    Function DigitalWrite(pin As Integer, value As Integer) As Boolean

    Function DigitalRead(pin As Integer) As Integer

  End Interface

End Namespace

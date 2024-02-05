Namespace Global.Basic.VisualBasic

  <System.ComponentModel.EditorBrowsable(ComponentModel.EditorBrowsableState.Never)>
  Friend NotInheritable Class DoubleType

    Private Sub New()
    End Sub

    ' FromObject(Value) -> FromObject(Value, Nothing)
    Friend Shared Function FromObject(Value As Object) As Double
      'implicit casting of Nothing returns 0
      If Value Is Nothing Then
        Return 0
      End If
      Return DoubleType.FromObject(Value, Nothing)
    End Function

    Friend Shared Function FromObject(Value As Object, NumberFormat As System.Globalization.NumberFormatInfo) As Double

      'implicit casting of Nothing returns 0

      If Value Is Nothing Then
        Return 0
      End If
      If TypeOf Value Is Boolean Then
        Return ((-1D) * (Convert.ToDouble(Value)))
      End If

      If TypeOf Value Is String Then
        Return FromString(DirectCast(Value, String), NumberFormat)
      End If

      Return Convert.ToDouble(Value)

    End Function

    Friend Shared Function FromString(Value As String) As Double
      Return DoubleType.FromString(Value, Nothing)
    End Function

    Friend Shared Function FromString(Value As String, NumberFormat As System.Globalization.NumberFormatInfo) As Double
      Try
        If NumberFormat Is Nothing Then NumberFormat = Threading.Thread.CurrentThread.CurrentCulture.NumberFormat
        Return DoubleType.Parse(Value, NumberFormat)
      Catch ex As Exception
        Throw New InvalidCastException("String to Double", ex)
      End Try
    End Function

    Friend Shared Function Parse(Value As String) As Double
      Return DoubleType.Parse(Value, Nothing)
    End Function

    Friend Shared Function Parse(Value As String, NumberFormat As System.Globalization.NumberFormatInfo) As Double
      'implicit casting of Nothing to Double returns 0
      If (Value Is Nothing) Then
        Return 0
      End If
      Return Double.Parse(Value, NumberFormat)
    End Function

    Friend Shared Function TryParse(value As String, <System.Runtime.InteropServices.OutAttribute()> ByRef result As Double) As Boolean
      Return Double.TryParse(value, result)
    End Function

  End Class

End Namespace
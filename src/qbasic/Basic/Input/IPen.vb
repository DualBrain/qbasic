Namespace Global.Basic.Input

  Public Interface IPen

    'Property Position As Point
    'Property ScreenMode As Integer
    'Property ColumnCount As Integer

    ReadOnly Property SurfaceWidth As Integer
    ReadOnly Property SurfaceHeight As Integer

    Event PositionChanged As EventHandler(Of PositionChangedEventArgs)
    Event SwitchChanged As EventHandler(Of SwitchChangedEventArgs)

  End Interface

End Namespace
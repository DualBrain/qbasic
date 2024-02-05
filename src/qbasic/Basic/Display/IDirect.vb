Namespace Global.Basic.Display

  ' The general premise is to create a direct write mechanism
  ' so that hardware accelerated support for screen output.

  ' The approach is for the platform implementation of Display to
  ' optionally implement this interface if, and only if, the
  ' target platform implementation is handling all screen output
  ' directly.

  ' If not implementing this interface, all that must be done
  ' on the target platform implementation is the Paint() event.
  ' When this (IDirect) interface is implemented, the Core will
  ' not call Paint() directly.

  Public Interface IDirect

    Function Screen(row As Short, col As Short, z As Boolean) As Short

    ReadOnly Property VisualPage As Integer

    ReadOnly Property ScreenWidth As Integer

    ReadOnly Property ScreenHeight As Integer

    Sub Pcopy(sourcePage As Integer, destinationPage As Integer)

    Function Point(x As Integer, y As Integer) As Short

    ReadOnly Property ActivePage As Integer

    Function Circle(xcenter As Integer, ycenter As Integer, radius As Double, attribute As Integer, start As Double, [end] As Double, aspect As Double) As Boolean

    Function Line(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer, attribute As Integer, box As Boolean, fill As Boolean, style As Short?, Optional ByRef styleBit As Integer = 0) As Boolean

    Function Pset(x As Integer, y As Integer, c As Short) As Boolean

    Function Pset(x As Integer, y As Integer) As Boolean

    Function Preset(x As Integer, y As Integer, c As Short) As Boolean

    Function Preset(x As Integer, y As Integer) As Boolean

    ReadOnly Property ScreenMode As Integer

    ReadOnly Property IsKeyOn As Boolean

    Function Screen(mode As Short, colorSwitch As Boolean, activePage As Short, visualPage As Short, Optional forced As Boolean = False) As Integer

    Sub ViewPrint()

    Sub ViewPrint(topLine As Integer, bottomLine As Integer)

    Sub KeyOn()

    Sub Cls(argument As Integer)

    ReadOnly Property CsrLin() As Short

    ReadOnly Property Pos(dummy As Short) As Short

    Sub Locate(row As Short, col As Short)

    Sub Color()

    Sub Color(background As Short)

    Sub Color(foreground As Short, background As Short)

    Sub SetPalette()

    Sub SetPalette(attribute As Integer, color As Integer)

    'Sub Print()

    'Sub Print(ByVal text As String, ByVal lineFeed As Boolean, Optional ByVal isInPrintLoop As Boolean = False)

    'Sub InternalPrint(ByVal text As String, ByVal lineFeed As Boolean, Optional ByVal isInPrintLoop As Boolean = False)

    Sub WriteCharacter(ch As Char, r As Integer, c As Integer)

    Function View(isScreen As Boolean?, x1 As Integer?, y1 As Integer?, x2 As Integer?, y2 As Integer?, fill As Integer?, border As Integer?) As Boolean

  End Interface

End Namespace

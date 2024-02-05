Namespace Global.Basic.Input

  <Flags()>
  Public Enum ModifierKeys
    None = 0
    Alt = 1
    Control = 2
    Shift = 4
    Apple = 8
#Disable Warning CA1069 ' Enums values should not be duplicated
    Windows = 8
#Enable Warning CA1069 ' Enums values should not be duplicated
  End Enum

End Namespace
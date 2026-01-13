Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundConstant

    Public Sub New(value As Object)
      Me.Value = value
    End Sub

    Public ReadOnly Property Value As Object

  End Class

End Namespace
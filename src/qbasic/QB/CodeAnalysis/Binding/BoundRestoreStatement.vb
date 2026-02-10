Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundRestoreStatement
    Inherits BoundStatement

    Sub New(target As BoundLabel)
      Me.Target = target
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.RestoreStatement
    Public ReadOnly Property Target As BoundLabel

    Public ReadOnly Property HasTarget As Boolean
      Get
        Return Target IsNot Nothing
      End Get
    End Property

  End Class

End Namespace


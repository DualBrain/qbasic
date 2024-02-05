Namespace Global.Basic.VisualBasic

  Friend NotInheritable Class VBMath

    Private Shared m_seed As Integer = 0
    Private Shared m_rnd As New Random(m_seed)
    Private Shared m_last As Single = CType(m_rnd.NextDouble(), Single)

    'Public Shared Property Seed As Short
    '  Get
    '    Return m_seed
    '  End Get
    '  Set(value As Short)
    '    m_seed = value
    '  End Set
    'End Property

    Friend Shared Function Rnd() As Single
      m_last = CType(m_rnd.NextDouble(), Single)
      Return m_last
    End Function

    Friend Shared Function Rnd(number As Single) As Single

      If number = 0.0 Then
        Return m_last
      ElseIf number < 0.0 Then
        'fd: What does this mean?
        'fd: ms-help://MS.VSCC/MS.MSDNVS/script56/html/vsstmRandomize
        'fd: ms-help://MS.VSCC/MS.MSDNVS/script56/html/vsfctrnd.htm
        Randomize(CInt(number))
      End If

      Return Rnd()

    End Function

    Friend Shared Sub Randomize()
      m_seed = 0
      m_rnd = New Random(m_seed)
    End Sub

    Friend Shared Sub Randomize(seed As Integer)
      m_seed = seed
      m_rnd = New Random(m_seed)
    End Sub

    'Public Shared Sub Randomize(ByVal Number As Double)
    '  m_rnd = New Random(CType(Number, Integer))
    'End Sub

  End Class

End Namespace
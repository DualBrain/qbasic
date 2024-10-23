'Imports System.Collections.Generic
'Imports System.Diagnostics
''Imports Roslyn.Utilities

'Namespace Global.Basic.CodeAnalysis.Syntax.InternalSyntax

'  Friend Class SyntaxListPool
'    Private _freeList As ArrayElement(Of SyntaxListBuilder)() = New ArrayElement(Of SyntaxListBuilder)(9) {}
'    Private _freeIndex As Integer

'#If DEBUG Then

'    Private ReadOnly List<SyntaxListBuilder> _allocated = New List<SyntaxListBuilder>();

'#End If

'        Friend Sub New()
'    End Sub
'    Friend Function Allocate() As SyntaxListBuilder
'      Dim item As SyntaxListBuilder
'      If _freeIndex > 0 Then
'        _freeIndex -= 1
'        item = _freeList(_freeIndex).Value
'        _freeList(_freeIndex).Value = Nothing
'      Else
'        item = New SyntaxListBuilder(10)
'      End If

'#If DEBUG Then

'      Debug.Assert(!_allocated.Contains(item));
'            _allocated.Add(item);

'#End If

'            Return item
'    End Function
'    Friend Function Allocate(Of TNode As GreenNode)() As SyntaxListBuilder(Of TNode)
'      Return New SyntaxListBuilder(Of TNode)(Me.Allocate())
'    End Function
'    Friend Function AllocateSeparated(Of TNode As GreenNode
')() As SeparatedSyntaxListBuilder(Of TNode)
'      Return New SeparatedSyntaxListBuilder(Of TNode)(Me.Allocate())
'    End Function
'    Friend Sub Free(Of TNode As GreenNode
')(item As SeparatedSyntaxListBuilder(Of TNode))
'      Free(item.UnderlyingBuilder)
'    End Sub
'    Friend Sub Free(item As SyntaxListBuilder)
'      If item Is Nothing Then
'        Return
'      End If

'      item.Clear()
'      If _freeIndex >= _freeList.Length Then
'        Me.Grow()
'      End If
'#If DEBUG Then

'      Debug.Assert(_allocated.Contains(item));

'            _allocated.Remove(item);

'#End If

'            _freeList(_freeIndex).Value = item
'      _freeIndex += 1
'    End Sub
'    Private Sub Grow()
'      Dim tmp As Microsoft.CodeAnalysis.ArrayElement(Of Microsoft.CodeAnalysis.Syntax.InternalSyntax.SyntaxListBuilder)() = New ArrayElement(Of SyntaxListBuilder)(_freeList.Length * 2 - 1) {}
'      Array.Copy(_freeList, tmp, _freeList.Length)
'      _freeList = tmp
'    End Sub
'    Public Function ToListAndFree(Of TNode As GreenNode
')(item As SyntaxListBuilder(Of TNode)) As SyntaxList(Of TNode)
'      If item.IsNull Then
'        Return CType(Nothing, SyntaxList(Of TNode))
'      End If

'      Dim list As Microsoft.CodeAnalysis.Syntax.InternalSyntax.SyntaxList(Of TNode) = item.ToList()
'      Free(item)
'      Return list
'    End Function
'    Public Function ToListAndFree(Of TNode As GreenNode)(item As SeparatedSyntaxListBuilder(Of TNode)) As SeparatedSyntaxList(Of TNode)
'      Dim list As Microsoft.CodeAnalysis.Syntax.InternalSyntax.SeparatedSyntaxList(Of TNode) = item.ToList()
'      Free(item)
'      Return list
'    End Function
'  End Class
'End Namespace

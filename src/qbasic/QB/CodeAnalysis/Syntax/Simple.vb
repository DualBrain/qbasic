' ============================================================================' & vbCrLf & _
' GW-BASIC to QBasic Transformer' & vbCrLf & _
' Transforms line-numbered GW-BASIC code to structured QBasic' & vbCrLf & _
' Completely uses SyntaxTree - no text scanning' & vbCrLf & _
' ============================================================================' & vbCrLf & _
'' & vbCrLf & _
'Imports System' & vbCrLf & _
'Imports System.Collections.Generic' & vbCrLf & _
'Imports System.Linq' & vbCrLf & _
'' & vbCrLf & _
'Namespace Global.QB.CodeAnalysis.Syntax' & vbCrLf & _
'' & vbCrLf & _
'  ''' <summary>' & vbCrLf & _
'  ''' Transforms GW-BASIC line-numbered code to modern QBasic structured code.' & vbCrLf & _
'  ''' </summary>' & vbCrLf & _
'  Public Class GwBasicToQBasicRewriter' & vbCrLf & _
'    Inherits SyntaxTreeRewriter' & vbCrLf & _
'' & vbCrLf & _
'    Private m_analysisCollected As Boolean = False' & vbCrLf & _
'' & vbCrLf & _
'    Public ReadOnly Property Analysis As New TransformationResult()' & vbCrLf & _
'' & vbCrLf & _
'    Public Sub CollectTargetsOnce(node As SyntaxNode)' & vbCrLf & _
'      If m_analysisCollected Then Return' & vbCrLf & _
'' & vbCrLf & _
'      m_analysisCollected = True' & vbCrLf & _
'' & vbCrLf & _
'      ' Walk entire SyntaxTree to find all line numbers and GOTO/GOSUB targets' & vbCrLf & _
'      WalkAllNodes(node)' & vbCrLf & _
'' & vbCrLf & _
'    End Sub' & vbCrLf & _
'' & vbCrLf & _
'  End Class' & vbCrLf & _
'' & vbCrLf & _
'End Namespace
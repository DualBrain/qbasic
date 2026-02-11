Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundInputStatement
    Inherits BoundStatement

    Public Sub New(suppressCr As Boolean, suppressQuestionMark As Boolean, prompt As BoundExpression, variables As ImmutableArray(Of VariableSymbol))
      Me.SuppressCr = suppressCr
      Me.SuppressQuestionMark = suppressQuestionMark
      Me.PromptExpression = prompt
      Me.Variables = variables
      Me.IsFileInput = False
      Me.FileNumber = Nothing
    End Sub

    Public Sub New(fileNumber As BoundExpression, variables As ImmutableArray(Of VariableSymbol))
      Me.SuppressCr = False
      Me.SuppressQuestionMark = False
      Me.PromptExpression = Nothing
      Me.Variables = variables
      Me.IsFileInput = True
      Me.FileNumber = fileNumber
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.InputStatement
    Public ReadOnly Property SuppressCr As Boolean
    Public ReadOnly Property SuppressQuestionMark As Boolean
    Public ReadOnly Property PromptExpression As BoundExpression
    Public ReadOnly Property Variables As ImmutableArray(Of VariableSymbol)
    Public ReadOnly Property IsFileInput As Boolean
    Public ReadOnly Property FileNumber As BoundExpression

  End Class

End Namespace
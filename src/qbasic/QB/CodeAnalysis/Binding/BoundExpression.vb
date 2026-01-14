Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

   Friend MustInherit Class BoundExpression
     Inherits BoundNode

     Private m_syntax As Syntax.ExpressionSyntax

     Public MustOverride ReadOnly Property Type As TypeSymbol

     Public Overridable ReadOnly Property ConstantValue As BoundConstant

     Public ReadOnly Property Syntax As Syntax.ExpressionSyntax
       Get
         Return m_syntax
       End Get
     End Property

     Protected Sub New()
     End Sub

     Protected Sub New(syntax As Syntax.ExpressionSyntax)
       m_syntax = syntax
     End Sub

   End Class

End Namespace
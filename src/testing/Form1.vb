Imports Microsoft.CodeAnalysis.VisualBasic

Public Class Form1

  Private WithEvents QbSyntaxTreeViewer As SyntaxTreeForm
  Private WithEvents VbSyntaxTreeViewer As SyntaxTreeForm

  Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
    If QbSyntaxTreeViewer Is Nothing Then
      QbSyntaxTreeViewer = New SyntaxTreeForm(TextBox1.Text)
      QbSyntaxTreeViewer.Show(Me)
    Else
      QbSyntaxTreeViewer.Reload(TextBox1.Text)
    End If
  End Sub

  Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
    If VbSyntaxTreeViewer Is Nothing Then
      VbSyntaxTreeViewer = New SyntaxTreeForm(TextBox1.Text, True)
      VbSyntaxTreeViewer.Show(Me)
    Else
      VbSyntaxTreeViewer.Reload(TextBox1.Text)
    End If
  End Sub

  Private Sub QbSyntaxTreeViewer_FormClosed(sender As Object, e As FormClosedEventArgs) Handles QbSyntaxTreeViewer.FormClosed
    QbSyntaxTreeViewer = Nothing
  End Sub

  Private Sub VbSyntaxTreeViewer_FormClosed(sender As Object, e As FormClosedEventArgs) Handles VbSyntaxTreeViewer.FormClosed
    VbSyntaxTreeViewer = Nothing
  End Sub

  Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
    TextBox1.Text = "10 PRINT ""HELLO WORLD!"""
  End Sub

End Class

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form1
  Inherits System.Windows.Forms.Form

  'Form overrides dispose to clean up the component list.
  <System.Diagnostics.DebuggerNonUserCode()>
  Protected Overrides Sub Dispose(ByVal disposing As Boolean)
    Try
      If disposing AndAlso components IsNot Nothing Then
        components.Dispose()
      End If
    Finally
      MyBase.Dispose(disposing)
    End Try
  End Sub

  'Required by the Windows Form Designer
  Private components As System.ComponentModel.IContainer

  'NOTE: The following procedure is required by the Windows Form Designer
  'It can be modified using the Windows Form Designer.  
  'Do not modify it using the code editor.
  <System.Diagnostics.DebuggerStepThrough()>
  Private Sub InitializeComponent()
    TextBox1 = New TextBox()
    SplitContainer1 = New SplitContainer()
    Button2 = New Button()
    Button1 = New Button()
    CType(SplitContainer1, ComponentModel.ISupportInitialize).BeginInit()
    SplitContainer1.Panel1.SuspendLayout()
    SplitContainer1.Panel2.SuspendLayout()
    SplitContainer1.SuspendLayout()
    SuspendLayout()
    ' 
    ' TextBox1
    ' 
    TextBox1.BackColor = Color.Black
    TextBox1.Dock = DockStyle.Fill
    TextBox1.ForeColor = Color.LightGray
    TextBox1.Location = New Point(0, 0)
    TextBox1.Margin = New Padding(1, 1, 1, 1)
    TextBox1.Multiline = True
    TextBox1.Name = "TextBox1"
    TextBox1.Size = New Size(410, 238)
    TextBox1.TabIndex = 0
    ' 
    ' SplitContainer1
    ' 
    SplitContainer1.Dock = DockStyle.Fill
    SplitContainer1.Location = New Point(0, 0)
    SplitContainer1.Margin = New Padding(1, 1, 1, 1)
    SplitContainer1.Name = "SplitContainer1"
    ' 
    ' SplitContainer1.Panel1
    ' 
    SplitContainer1.Panel1.Controls.Add(TextBox1)
    ' 
    ' SplitContainer1.Panel2
    ' 
    SplitContainer1.Panel2.BackColor = Color.DarkGray
    SplitContainer1.Panel2.Controls.Add(Button2)
    SplitContainer1.Panel2.Controls.Add(Button1)
    SplitContainer1.Size = New Size(607, 238)
    SplitContainer1.SplitterDistance = 410
    SplitContainer1.SplitterWidth = 1
    SplitContainer1.TabIndex = 1
    ' 
    ' Button2
    ' 
    Button2.Location = New Point(14, 37)
    Button2.Margin = New Padding(1, 1, 1, 1)
    Button2.Name = "Button2"
    Button2.Size = New Size(164, 22)
    Button2.TabIndex = 1
    Button2.Text = "Generate Syntax Tree (VB)"
    Button2.UseVisualStyleBackColor = True
    ' 
    ' Button1
    ' 
    Button1.Location = New Point(14, 7)
    Button1.Margin = New Padding(1, 1, 1, 1)
    Button1.Name = "Button1"
    Button1.Size = New Size(164, 22)
    Button1.TabIndex = 0
    Button1.Text = "Generate Syntax Tree (QB)"
    Button1.UseVisualStyleBackColor = True
    ' 
    ' Form1
    ' 
    AutoScaleDimensions = New SizeF(7F, 15F)
    AutoScaleMode = AutoScaleMode.Font
    ClientSize = New Size(607, 238)
    Controls.Add(SplitContainer1)
    Margin = New Padding(1, 1, 1, 1)
    Name = "Form1"
    ShowIcon = False
    Text = "QBasic Testing"
    SplitContainer1.Panel1.ResumeLayout(False)
    SplitContainer1.Panel1.PerformLayout()
    SplitContainer1.Panel2.ResumeLayout(False)
    CType(SplitContainer1, ComponentModel.ISupportInitialize).EndInit()
    SplitContainer1.ResumeLayout(False)
    ResumeLayout(False)
  End Sub

  Friend WithEvents TextBox1 As TextBox
  Friend WithEvents SplitContainer1 As SplitContainer
  Friend WithEvents Button1 As Button
  Friend WithEvents Button2 As Button

End Class

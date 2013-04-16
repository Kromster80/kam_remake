object Form1: TForm1
  Left = 282
  Top = 194
  Caption = 'Form1'
  ClientHeight = 529
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 152
    Top = 8
    Width = 512
    Height = 512
    OnMouseDown = Image1MouseDown
    OnMouseUp = Image1MouseUp
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 33
    Caption = 'New maze'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 40
    Width = 137
    Height = 33
    Caption = '100 Random | routes'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 104
    Width = 129
    Height = 17
    Caption = 'A* old'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 128
    Width = 129
    Height = 17
    Caption = 'A* new'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 152
    Width = 129
    Height = 17
    Caption = 'A* JPS'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    State = cbChecked
    TabOrder = 4
  end
  object Button3: TButton
    Left = 8
    Top = 184
    Width = 137
    Height = 33
    Caption = 'Repeat'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = Button3Click
  end
end

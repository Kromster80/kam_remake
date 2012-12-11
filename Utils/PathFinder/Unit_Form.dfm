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
  object Label1: TLabel
    Left = 64
    Top = 112
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 64
    Top = 128
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'New maze'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 40
    Width = 137
    Height = 25
    Caption = 'Test 100 random routes'
    TabOrder = 1
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 112
    Width = 41
    Height = 17
    Caption = 'JPS'
    TabOrder = 2
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 128
    Width = 33
    Height = 17
    Caption = 'A*'
    TabOrder = 3
  end
end

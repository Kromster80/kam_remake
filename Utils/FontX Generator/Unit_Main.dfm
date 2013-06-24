object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 329
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    545
    329)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 64
    Width = 92
    Height = 16
    Caption = 'Used characters'
  end
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 61
    Height = 16
    Caption = 'Font name'
  end
  object Label3: TLabel
    Left = 144
    Top = 16
    Width = 51
    Height = 16
    Caption = 'Font size'
  end
  object Label4: TLabel
    Left = 280
    Top = 16
    Width = 45
    Height = 16
    Caption = 'Preview'
  end
  object Image1: TImage
    Left = 280
    Top = 32
    Width = 256
    Height = 288
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitHeight = 256
  end
  object Label5: TLabel
    Left = 8
    Top = 216
    Width = 45
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Padding'
  end
  object Button1: TButton
    Left = 8
    Top = 264
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Generate font'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 80
    Width = 265
    Height = 129
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 8
    Top = 32
    Width = 129
    Height = 24
    TabOrder = 2
    Text = 'Arial MS Uni'
  end
  object SpinEdit1: TSpinEdit
    Left = 144
    Top = 32
    Width = 49
    Height = 26
    MaxValue = 24
    MinValue = 6
    TabOrder = 3
    Value = 11
  end
  object Button2: TButton
    Left = 8
    Top = 296
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save font ...'
    TabOrder = 4
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 200
    Top = 32
    Width = 41
    Height = 17
    Caption = 'Bold'
    TabOrder = 5
  end
  object CheckBox2: TCheckBox
    Left = 200
    Top = 48
    Width = 49
    Height = 17
    Caption = 'Italic'
    TabOrder = 6
  end
  object SpinEdit2: TSpinEdit
    Left = 8
    Top = 232
    Width = 49
    Height = 26
    Anchors = [akLeft, akBottom]
    MaxValue = 8
    MinValue = 0
    TabOrder = 7
    Value = 1
  end
  object Button3: TButton
    Left = 144
    Top = 264
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export texture ...'
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 152
    Top = 272
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Import texture ...'
    TabOrder = 9
    OnClick = Button4Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'fntx'
    Left = 288
    Top = 40
  end
  object OpenDialog1: TOpenDialog
    Left = 352
    Top = 40
  end
end

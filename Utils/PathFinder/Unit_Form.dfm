object Form1: TForm1
  Left = 282
  Top = 194
  Caption = 'Form1'
  ClientHeight = 529
  ClientWidth = 689
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
    Left = 8
    Top = 160
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 8
    Top = 176
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
    Caption = '10 routes'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 72
    Width = 137
    Height = 25
    Caption = '10 routes'
    TabOrder = 2
    OnClick = Button3Click
  end
end

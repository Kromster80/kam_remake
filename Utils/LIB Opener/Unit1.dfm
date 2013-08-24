object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 216
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Decode A'
    TabOrder = 0
    OnClick = BtnDecodeAClick
  end
  object Button2: TButton
    Left = 216
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Decode U'
    TabOrder = 1
    OnClick = BtnDecodeUClick
  end
end

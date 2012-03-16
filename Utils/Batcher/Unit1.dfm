object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 293
  ClientWidth = 426
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
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object Button2: TButton
    Left = 24
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
  end
  object Button3: TButton
    Left = 24
    Top = 80
    Width = 185
    Height = 25
    Caption = 'Export Campaign Tests to EVT'
    TabOrder = 2
    OnClick = Button3Click
  end
end

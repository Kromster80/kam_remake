object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Form1'
  ClientHeight = 254
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 88
    Top = 24
    Width = 161
    Height = 25
    Caption = 'Resample 640 > 512'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 48
    Width = 161
    Height = 25
    Caption = 'Exclude Common From Set'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 88
    Top = 88
    Width = 161
    Height = 25
    Caption = 'Compress file ZLibEx'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 88
    Top = 112
    Width = 161
    Height = 25
    Caption = 'Decompress file ZLibEx'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 88
    Top = 152
    Width = 161
    Height = 25
    Caption = 'Split'
    TabOrder = 4
    OnClick = Button5Click
  end
  object OpenDialog1: TOpenDialog
    Left = 24
    Top = 24
  end
end

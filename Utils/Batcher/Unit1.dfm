object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 289
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button3: TButton
    Left = 24
    Top = 24
    Width = 185
    Height = 25
    Caption = 'Export Campaign Texts to EVT'
    TabOrder = 0
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 24
    Top = 72
    Width = 185
    Height = 25
    Caption = 'Mass rename'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 120
    Width = 185
    Height = 25
    Caption = 'Check goals count'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 224
    Top = 24
    Width = 241
    Height = 241
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    TabOrder = 3
  end
end

object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 313
  ClientWidth = 665
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    665
    313)
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
    Left = 248
    Top = 24
    Width = 393
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 3
    ExplicitHeight = 257
  end
  object Button4: TButton
    Left = 24
    Top = 168
    Width = 121
    Height = 25
    Caption = 'Rip time goals'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 24
    Top = 240
    Width = 121
    Height = 25
    Caption = 'unXOR all maps'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 24
    Top = 264
    Width = 121
    Height = 25
    Caption = 'XOR all maps'
    TabOrder = 6
    OnClick = Button5Click
  end
  object Button7: TButton
    Left = 24
    Top = 192
    Width = 121
    Height = 25
    Caption = 'Show message goals'
    TabOrder = 7
    OnClick = Button7Click
  end
end

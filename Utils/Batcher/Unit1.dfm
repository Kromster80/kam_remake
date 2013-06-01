object Form1: TForm1
  Left = 0
  Top = 0
  Width = 888
  Height = 477
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    849
    430)
  PixelsPerInch = 120
  TextHeight = 17
  object Button3: TButton
    Left = 31
    Top = 31
    Width = 242
    Height = 33
    Caption = 'Export Campaign Texts to EVT'
    TabOrder = 0
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 31
    Top = 84
    Width = 242
    Height = 32
    Caption = 'Mass rename'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 31
    Top = 136
    Width = 159
    Height = 33
    Caption = 'Check goals count'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 324
    Top = 31
    Width = 514
    Height = 368
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button4: TButton
    Left = 31
    Top = 167
    Width = 159
    Height = 33
    Caption = 'Rip time goals'
    TabOrder = 4
    OnClick = Button4Click
  end
  object btnUnXorAll: TButton
    Left = 31
    Top = 345
    Width = 159
    Height = 33
    Caption = 'unXOR all maps'
    TabOrder = 5
    OnClick = btnXorAllClick
  end
  object btnXorAll: TButton
    Left = 31
    Top = 377
    Width = 159
    Height = 32
    Caption = 'XOR all maps'
    TabOrder = 6
    OnClick = btnXorAllClick
  end
  object Button7: TButton
    Left = 31
    Top = 199
    Width = 159
    Height = 32
    Caption = 'Show message goals'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 31
    Top = 230
    Width = 242
    Height = 33
    Caption = 'Append user players to MP maps'
    TabOrder = 8
    OnClick = Button8Click
  end
  object Button5: TButton
    Left = 31
    Top = 262
    Width = 242
    Height = 32
    Caption = 'Add AI players to MP maps'
    TabOrder = 9
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 31
    Top = 293
    Width = 242
    Height = 33
    Caption = 'Add AI defence'
    TabOrder = 10
    OnClick = Button6Click
  end
  object Button9: TButton
    Left = 41
    Top = 425
    Width = 316
    Height = 43
    Caption = 'Fix script text'
    TabOrder = 11
    OnClick = Button9Click
  end
end

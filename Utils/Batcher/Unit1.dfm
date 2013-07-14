object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 439
  ClientWidth = 872
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    872
    439)
  PixelsPerInch = 96
  TextHeight = 13
  object Button3: TButton
    Left = 24
    Top = 24
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Export Campaign Texts to EVT'
    TabOrder = 0
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 24
    Top = 64
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Mass rename'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 104
    Width = 121
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Check goals count'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 248
    Top = 24
    Width = 393
    Height = 281
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
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
  end
  object Button4: TButton
    Left = 24
    Top = 128
    Width = 121
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Rip time goals'
    TabOrder = 4
    OnClick = Button4Click
  end
  object btnUnXorAll: TButton
    Left = 24
    Top = 264
    Width = 121
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'unXOR all maps'
    TabOrder = 5
    OnClick = btnXorAllClick
  end
  object btnXorAll: TButton
    Left = 24
    Top = 288
    Width = 121
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'XOR all maps'
    TabOrder = 6
    OnClick = btnXorAllClick
  end
  object Button7: TButton
    Left = 24
    Top = 152
    Width = 121
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Show message goals'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 24
    Top = 176
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Append user players to MP maps'
    TabOrder = 8
    OnClick = Button8Click
  end
  object Button5: TButton
    Left = 24
    Top = 200
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Add AI players to MP maps'
    TabOrder = 9
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 24
    Top = 224
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Add AI defence'
    TabOrder = 10
    OnClick = Button6Click
  end
  object Button9: TButton
    Left = 31
    Top = 325
    Width = 242
    Height = 33
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Fix script text'
    TabOrder = 11
    OnClick = Button9Click
  end
end

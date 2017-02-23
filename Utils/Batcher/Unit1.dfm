object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 414
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
    414)
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
    Caption = 'Export campaign texts to EVT'
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
    Caption = 'Mass rename sprites'
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
    Left = 288
    Top = 24
    Width = 465
    Height = 256
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
    Left = 288
    Top = 336
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
    Left = 288
    Top = 365
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
    Caption = 'Add AI auto config to MP maps'
    TabOrder = 10
    OnClick = Button6Click
  end
  object Button9: TButton
    Left = 423
    Top = 336
    Width = 154
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Fix script text'
    TabOrder = 11
    OnClick = Button9Click
  end
  object btnCheckColor: TButton
    Left = 7
    Top = 281
    Width = 258
    Height = 25
    Hint = 
      'Check if there is color specified in *.dat file for every player' +
      ' by either SET_MAP_COLOR or SET_RGB_COLOR command'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Check all maps set color explicitly'
    TabOrder = 12
    OnClick = btnCheckColorClick
  end
  object btnSetDefColor: TButton
    Left = 7
    Top = 310
    Width = 258
    Height = 25
    Hint = 
      'Add SET_RGB_COLOR command for players without color commands in ' +
      '*.dat for all maps'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Set all maps missing colors explicitly with defaults'
    TabOrder = 13
    OnClick = btnCheckColorClick
  end
  object btnRemoveNewRemap: TButton
    Left = 7
    Top = 339
    Width = 258
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Remove SET_NEW_REMAP from all maps'
    TabOrder = 14
    OnClick = btnRemoveNewRemapClick
  end
  object btnDeleteUnusedSetMapColor: TButton
    Left = 7
    Top = 368
    Width = 258
    Height = 25
    Hint = 
      'Some maps have both SET_MAP_COLOR and SET_RGB_COLOR, SET_MAP_COL' +
      'OR is always coming first, means SET_RGB_COLOR is always overrid' +
      'ing color previously set by SET_MAP_COLOR. 4th button finds all ' +
      'such maps and deletes SET_MAP_COLOR command.'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Delete unused SET_MAP_COLOR for all maps'
    TabOrder = 15
    OnClick = btnDeleteUnusedSetMapColorClick
  end
end

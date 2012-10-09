object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 449
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 320
    Top = 136
    Width = 185
    Height = 129
  end
  object Label1: TLabel
    Left = 16
    Top = 368
    Width = 31
    Height = 13
    Caption = 'Cycles'
  end
  object Button1: TButton
    Left = 16
    Top = 416
    Width = 177
    Height = 25
    Caption = 'Run selected test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 320
    Top = 272
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object SpinEdit1: TSpinEdit
    Left = 16
    Top = 384
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object ListBox1: TListBox
    Left = 16
    Top = 136
    Width = 297
    Height = 225
    ItemHeight = 13
    TabOrder = 3
  end
  object Memo2: TMemo
    Left = 16
    Top = 16
    Width = 337
    Height = 105
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'Runner'
      ''
      
        'Tool to run a game in pure simulation mode to test distribution ' +
        'of '
      'results and help catch bugs related to that.'
      ''
      'For example:'
      
        '  TestStone runs a stone mining that in theory should yeild same' +
        ' '
      'amount of stone each run')
    ReadOnly = True
    TabOrder = 4
  end
end

object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 449
  ClientWidth = 601
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    601
    449)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 296
    Top = 16
    Width = 289
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Label1: TLabel
    Left = 16
    Top = 136
    Width = 31
    Height = 13
    Caption = 'Cycles'
  end
  object Label2: TLabel
    Left = 132
    Top = 136
    Width = 3
    Height = 13
    Alignment = taRightJustify
  end
  object Button1: TButton
    Left = 144
    Top = 136
    Width = 121
    Height = 41
    Caption = 'Run selected test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 184
    Width = 249
    Height = 129
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object seCycles: TSpinEdit
    Left = 16
    Top = 152
    Width = 121
    Height = 22
    MaxValue = 1000000
    MinValue = 0
    TabOrder = 2
    Value = 10
  end
  object ListBox1: TListBox
    Left = 16
    Top = 16
    Width = 249
    Height = 113
    ItemHeight = 13
    TabOrder = 3
  end
  object Memo2: TMemo
    Left = 16
    Top = 312
    Width = 249
    Height = 121
    Anchors = [akLeft, akBottom]
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'Runner'
      ''
      'Tool to run a game in pure simulation mode to test '
      'distribution of '
      'results and help catch bugs related to that.'
      ''
      'For example:'
      '  TestStone runs a stone mining that in theory '
      'should yeild same '
      'amount of stone each run')
    ReadOnly = True
    TabOrder = 4
  end
  object RadioGroup1: TRadioGroup
    Left = 280
    Top = 288
    Width = 305
    Height = 41
    Anchors = [akLeft, akRight, akBottom]
    Caption = ' Display '
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Sequential'
      'Distribution'
      'Charts')
    TabOrder = 5
    OnClick = RadioGroup1Click
  end
end

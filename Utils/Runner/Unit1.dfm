object Form2: TForm2
  Left = 244
  Top = 169
  Caption = 'Form2'
  ClientHeight = 513
  ClientWidth = 705
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    705
    513)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 464
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Cycles'
  end
  object Label2: TLabel
    Left = 124
    Top = 464
    Width = 3
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
  end
  object Button1: TButton
    Left = 136
    Top = 464
    Width = 57
    Height = 41
    Anchors = [akLeft, akBottom]
    Caption = 'Run'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 200
    Top = 344
    Width = 249
    Height = 161
    Anchors = [akLeft, akBottom]
    TabOrder = 1
  end
  object seCycles: TSpinEdit
    Left = 8
    Top = 480
    Width = 121
    Height = 22
    Anchors = [akLeft, akBottom]
    MaxValue = 1000000
    MinValue = 0
    TabOrder = 2
    Value = 10
  end
  object ListBox1: TListBox
    Left = 8
    Top = 344
    Width = 185
    Height = 113
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object Memo2: TMemo
    Left = 456
    Top = 344
    Width = 241
    Height = 121
    Anchors = [akLeft, akBottom]
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'Runner'
      ''
      'Tool to run a game in pure simulation mode to '
      'test distribution of results and help catch bugs '
      'related to that.'
      ''
      'For example:'
      '  TestStone runs a stone mining that in theory '
      'should yeild same amount of stone each run')
    ReadOnly = True
    TabOrder = 4
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 689
    Height = 321
    ActivePage = TabSheet3
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'Results'
      OnResize = TabSheetResize
      ExplicitLeft = 20
      ExplicitTop = 64
      ExplicitWidth = 281
      ExplicitHeight = 165
      DesignSize = (
        681
        293)
      object Image1: TImage
        Left = 40
        Top = 0
        Width = 633
        Height = 273
        Anchors = [akLeft, akTop, akRight, akBottom]
        ExplicitHeight = 265
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Distribution'
      ImageIndex = 1
      OnResize = TabSheetResize
      ExplicitWidth = 281
      ExplicitHeight = 165
      DesignSize = (
        681
        293)
      object Image2: TImage
        Left = 40
        Top = 0
        Width = 633
        Height = 265
        Anchors = [akLeft, akTop, akRight, akBottom]
        ExplicitHeight = 257
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Times'
      ImageIndex = 2
      OnResize = TabSheetResize
      ExplicitWidth = 281
      ExplicitHeight = 165
      DesignSize = (
        681
        293)
      object Image3: TImage
        Left = 40
        Top = 0
        Width = 633
        Height = 273
        Anchors = [akLeft, akTop, akRight, akBottom]
        ExplicitHeight = 265
      end
      object Label3: TLabel
        Left = 528
        Top = 8
        Width = 47
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Threshold'
      end
      object TrackBar1: TTrackBar
        Left = 520
        Top = 24
        Width = 150
        Height = 33
        Anchors = [akTop, akRight]
        Max = 25
        Position = 10
        TabOrder = 0
        OnChange = TrackBar1Change
      end
    end
  end
end

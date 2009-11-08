object frmMain: TfrmMain
  Left = 192
  Top = 113
  Width = 649
  Height = 452
  Caption = 'KaM Font Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    641
    425)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 160
    Top = 40
    Width = 471
    Height = 345
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object btnExport: TBitBtn
    Left = 161
    Top = 394
    Width = 120
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export'#39'n'#39'Show BMP'
    TabOrder = 1
    OnClick = btnExportClick
  end
  object btnImport: TBitBtn
    Left = 283
    Top = 394
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Import BMP'
    Enabled = False
    TabOrder = 2
  end
  object ListBox1: TListBox
    Left = 8
    Top = 40
    Width = 145
    Height = 345
    ItemHeight = 13
    TabOrder = 3
    OnClick = ListBox1Click
  end
  object RefreshData: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 21
    Caption = 'Refresh'
    TabOrder = 4
    OnClick = RefreshDataClick
  end
  object BitBtn1: TBitBtn
    Left = 373
    Top = 394
    Width = 97
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save Font File'
    Enabled = False
    TabOrder = 0
  end
  object OpenDialog1: TOpenDialog
    Left = 568
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 608
    Top = 8
  end
end

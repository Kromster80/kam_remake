object frmMain: TfrmMain
  Left = 192
  Top = 113
  Width = 455
  Height = 464
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
    447
    430)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 40
    Width = 425
    Height = 347
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object btnLoadFont: TBitBtn
    Left = 8
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Open Font File'
    TabOrder = 0
    OnClick = btnLoadFontClick
  end
  object BitBtn1: TBitBtn
    Left = 120
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Save Font File'
    TabOrder = 1
  end
  object btnExport: TBitBtn
    Left = 9
    Top = 398
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export BMP'
    TabOrder = 2
  end
  object btnImport: TBitBtn
    Left = 89
    Top = 398
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Import BMP'
    TabOrder = 3
  end
  object OpenDialog1: TOpenDialog
    Left = 352
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 384
    Top = 8
  end
end

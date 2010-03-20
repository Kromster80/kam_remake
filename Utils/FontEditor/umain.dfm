object frmMain: TfrmMain
  Left = 190
  Top = 131
  Width = 825
  Height = 680
  Caption = 'KaM Font Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 160
    Top = 16
    Width = 512
    Height = 512
    OnMouseMove = Image1MouseMove
  end
  object Label4: TLabel
    Left = 8
    Top = 392
    Width = 32
    Height = 13
    Caption = 'Label4'
    Color = clBtnFace
    ParentColor = False
  end
  object Image3: TImage
    Left = 680
    Top = 16
    Width = 128
    Height = 512
  end
  object Image4: TImage
    Left = 160
    Top = 536
    Width = 649
    Height = 30
  end
  object Image5: TImage
    Left = 160
    Top = 565
    Width = 649
    Height = 60
  end
  object ListBox1: TListBox
    Left = 8
    Top = 40
    Width = 145
    Height = 345
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object RefreshData: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 21
    Caption = 'Refresh list'
    TabOrder = 2
    OnClick = RefreshDataClick
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 496
    Width = 145
    Height = 25
    Caption = 'Save Font File'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 633
    Width = 817
    Height = 20
    Panels = <
      item
        Text = 'Font'
        Width = 200
      end
      item
        Text = 'Coordinates'
        Width = 100
      end
      item
        Text = 'Font Hex Code'
        Width = 50
      end>
    SimplePanel = False
  end
  object Edit1: TEdit
    Left = 8
    Top = 536
    Width = 145
    Height = 21
    TabOrder = 4
    Text = 'Sample phrase'
    OnChange = Edit1Change
  end
  object CheckCells: TCheckBox
    Left = 8
    Top = 412
    Width = 69
    Height = 17
    Caption = 'Show cells'
    TabOrder = 6
    OnClick = CheckCellsClick
  end
  object btnExportBig: TBitBtn
    Left = 8
    Top = 432
    Width = 145
    Height = 25
    Caption = 'Export BMP'
    TabOrder = 7
    OnClick = btnExportBigClick
  end
  object btnImportBig: TBitBtn
    Left = 8
    Top = 456
    Width = 145
    Height = 25
    Caption = 'Import BMP'
    TabOrder = 5
    OnClick = btnImportBigClick
  end
  object OpenDialog1: TOpenDialog
    Left = 88
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 120
    Top = 8
  end
end

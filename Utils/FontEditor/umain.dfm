object frmMain: TfrmMain
  Left = 428
  Top = 135
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
  object Label3: TLabel
    Left = 64
    Top = 407
    Width = 83
    Height = 13
    Caption = 'BaseCharHeight?'
    Color = clBtnFace
    ParentColor = False
  end
  object Label4: TLabel
    Left = 64
    Top = 431
    Width = 72
    Height = 13
    Caption = 'Word spacing?'
    Color = clBtnFace
    ParentColor = False
  end
  object Label5: TLabel
    Left = 64
    Top = 455
    Width = 62
    Height = 13
    Caption = 'Char spacing'
    Color = clBtnFace
    ParentColor = False
  end
  object Label6: TLabel
    Left = 64
    Top = 479
    Width = 66
    Height = 13
    Caption = 'Line spacing?'
    Color = clBtnFace
    ParentColor = False
  end
  object Label1: TLabel
    Left = 64
    Top = 503
    Width = 75
    Height = 13
    Caption = 'NB space width'
    Color = clBtnFace
    ParentColor = False
  end
  object ListBox1: TListBox
    Left = 8
    Top = 40
    Width = 145
    Height = 233
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
    Top = 600
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
        Width = 240
      end
      item
        Text = 'Coordinates'
        Width = 120
      end
      item
        Text = 'Font Hex Code'
        Width = 50
      end>
    SimplePanel = False
  end
  object Edit1: TEdit
    Left = 8
    Top = 545
    Width = 145
    Height = 21
    TabOrder = 4
    Text = 'Sample phrase'
    OnChange = Edit1Change
  end
  object CheckCells: TCheckBox
    Left = 8
    Top = 524
    Width = 71
    Height = 17
    Caption = 'Show cells'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = CheckCellsClick
  end
  object btnExportBig: TBitBtn
    Left = 8
    Top = 572
    Width = 73
    Height = 25
    Caption = 'Export BMP'
    TabOrder = 7
    OnClick = btnExportBigClick
  end
  object btnImportBig: TBitBtn
    Left = 80
    Top = 572
    Width = 73
    Height = 25
    Caption = 'Import BMP'
    TabOrder = 5
    OnClick = btnImportBigClick
  end
  object RGPalette: TRadioGroup
    Left = 8
    Top = 280
    Width = 145
    Height = 112
    Caption = ' Preview palette  '
    Columns = 2
    Items.Strings = (
      'map.bbm'
      'pal0.bbm'
      'pal1.bbm'
      'pal2.bbm'
      'pal3.bbm'
      'pal4.bbm'
      'pal5.bbm'
      'setup.bbm'
      'setup2.bbm'
      'linear'
      'mapgold.lbm'
      'setup.lbm')
    TabOrder = 8
    OnClick = RGPaletteClick
  end
  object SpinEdit1: TSpinEdit
    Left = 8
    Top = 400
    Width = 50
    Height = 22
    MaxValue = 65535
    MinValue = -65535
    TabOrder = 9
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit2: TSpinEdit
    Left = 8
    Top = 424
    Width = 50
    Height = 22
    MaxValue = 65535
    MinValue = -65535
    TabOrder = 10
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit3: TSpinEdit
    Left = 8
    Top = 448
    Width = 50
    Height = 22
    MaxValue = 65535
    MinValue = -65535
    TabOrder = 11
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit4: TSpinEdit
    Left = 8
    Top = 472
    Width = 50
    Height = 22
    MaxValue = 65535
    MinValue = -65535
    TabOrder = 12
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit5: TSpinEdit
    Left = 8
    Top = 496
    Width = 50
    Height = 22
    MaxValue = 24
    MinValue = 0
    TabOrder = 13
    Value = 0
    OnChange = SpinEdit1Change
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

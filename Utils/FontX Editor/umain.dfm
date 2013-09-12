object frmMain: TfrmMain
  Left = 185
  Top = 94
  Caption = 'KaM Font Editor'
  ClientHeight = 650
  ClientWidth = 697
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 160
    Top = 16
    Width = 512
    Height = 512
    OnMouseMove = PaintBox1MouseMove
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
  end
  object Image4: TImage
    Left = 312
    Top = 536
    Width = 377
    Height = 30
  end
  object Image5: TImage
    Left = 160
    Top = 565
    Width = 529
    Height = 60
  end
  object Label3: TLabel
    Left = 62
    Top = 339
    Width = 83
    Height = 13
    Caption = 'BaseCharHeight?'
    Color = clBtnFace
    ParentColor = False
  end
  object Label4: TLabel
    Left = 62
    Top = 363
    Width = 72
    Height = 13
    Caption = 'Word spacing?'
    Color = clBtnFace
    ParentColor = False
  end
  object Label5: TLabel
    Left = 62
    Top = 387
    Width = 62
    Height = 13
    Caption = 'Char spacing'
    Color = clBtnFace
    ParentColor = False
  end
  object Label6: TLabel
    Left = 62
    Top = 411
    Width = 66
    Height = 13
    Caption = 'Line spacing?'
    Color = clBtnFace
    ParentColor = False
  end
  object Shape1: TShape
    Left = 160
    Top = 16
    Width = 34
    Height = 34
    Brush.Style = bsClear
    Pen.Color = clWhite
    Pen.Width = 2
  end
  object lbFonts: TListBox
    Left = 8
    Top = 40
    Width = 145
    Height = 257
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbFontsClick
  end
  object btnRefresh: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 21
    Caption = 'Refresh list'
    TabOrder = 2
    OnClick = btnRefreshClick
  end
  object btnSaveFont: TBitBtn
    Left = 8
    Top = 600
    Width = 145
    Height = 25
    Caption = 'Save Font File ...'
    TabOrder = 0
    OnClick = btnSaveFontClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 630
    Width = 697
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
  end
  object Edit1: TEdit
    Left = 160
    Top = 540
    Width = 145
    Height = 21
    TabOrder = 4
    Text = 'Sample phrase'
    OnChange = Edit1Change
  end
  object CheckCells: TCheckBox
    Left = 8
    Top = 309
    Width = 71
    Height = 17
    Caption = 'Show cells'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = CheckCellsClick
  end
  object btnExportBitmap: TBitBtn
    Left = 8
    Top = 572
    Width = 73
    Height = 25
    Caption = 'Export PNG ...'
    TabOrder = 7
    OnClick = btnExportPngClick
  end
  object btnImportBitmap: TBitBtn
    Left = 80
    Top = 572
    Width = 73
    Height = 25
    Caption = 'Import PNG ...'
    TabOrder = 5
    OnClick = btnImportPngClick
  end
  object SpinEdit1: TSpinEdit
    Left = 8
    Top = 336
    Width = 50
    Height = 22
    MaxValue = 65535
    MinValue = -65535
    TabOrder = 8
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit2: TSpinEdit
    Left = 8
    Top = 360
    Width = 50
    Height = 22
    MaxValue = 65535
    MinValue = -65535
    TabOrder = 9
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit3: TSpinEdit
    Left = 8
    Top = 384
    Width = 50
    Height = 22
    MaxValue = 65535
    MinValue = -65535
    TabOrder = 10
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit4: TSpinEdit
    Left = 8
    Top = 408
    Width = 50
    Height = 22
    MaxValue = 65535
    MinValue = -65535
    TabOrder = 11
    Value = 0
    OnChange = SpinEdit1Change
  end
  object ScrollBar1: TScrollBar
    Left = 672
    Top = 16
    Width = 17
    Height = 513
    Kind = sbVertical
    Max = 2000
    PageSize = 0
    TabOrder = 12
    OnChange = ScrollBar1Change
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 440
    Width = 145
    Height = 105
    Caption = ' Letter: '
    TabOrder = 13
    object Label7: TLabel
      Left = 62
      Top = 27
      Width = 36
      Height = 13
      Caption = 'Y offset'
      Color = clBtnFace
      ParentColor = False
    end
    object SpinEdit5: TSpinEdit
      Left = 8
      Top = 24
      Width = 50
      Height = 22
      MaxValue = 65535
      MinValue = -65535
      TabOrder = 0
      Value = 0
      OnChange = SpinEdit5Change
    end
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

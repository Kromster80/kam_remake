object frmMain: TfrmMain
  Left = 185
  Top = 94
  Caption = 'KaM Font Editor'
  ClientHeight = 690
  ClientWidth = 713
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
  OnResize = FormResize
  DesignSize = (
    713
    690)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 176
    Top = 24
    Width = 512
    Height = 544
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseMove = PaintBox1MouseMove
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
  end
  object Image4: TImage
    Left = 328
    Top = 576
    Width = 377
    Height = 30
    Anchors = [akLeft, akBottom]
  end
  object Image5: TImage
    Left = 176
    Top = 605
    Width = 529
    Height = 60
    Anchors = [akLeft, akBottom]
  end
  object Shape1: TShape
    Left = 176
    Top = 24
    Width = 34
    Height = 34
    Brush.Style = bsClear
    Pen.Color = clWhite
    Pen.Width = 2
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 72
    Height = 13
    Caption = 'Available fonts:'
  end
  object lbFonts: TListBox
    Left = 8
    Top = 24
    Width = 161
    Height = 233
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbFontsClick
  end
  object btnSaveFont: TBitBtn
    Left = 8
    Top = 640
    Width = 161
    Height = 25
    Caption = 'Save FontX File ...'
    TabOrder = 0
    OnClick = btnSaveFontClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 670
    Width = 713
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
    ExplicitTop = 630
    ExplicitWidth = 697
  end
  object Edit1: TEdit
    Left = 176
    Top = 580
    Width = 145
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Text = 'Sample phrase'
    OnChange = Edit1Change
  end
  object CheckCells: TCheckBox
    Left = 168
    Top = 5
    Width = 71
    Height = 17
    Caption = 'Show cells'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckCellsClick
  end
  object btnExportBitmap: TBitBtn
    Left = 8
    Top = 612
    Width = 81
    Height = 25
    Caption = 'Export PNG ...'
    TabOrder = 6
    OnClick = btnExportPngClick
  end
  object btnImportBitmap: TBitBtn
    Left = 88
    Top = 612
    Width = 81
    Height = 25
    Caption = 'Import PNG ...'
    TabOrder = 4
    OnClick = btnImportPngClick
  end
  object ScrollBar1: TScrollBar
    Left = 688
    Top = 16
    Width = 17
    Height = 553
    Anchors = [akTop, akRight, akBottom]
    Kind = sbVertical
    Max = 65535
    PageSize = 0
    TabOrder = 7
    OnChange = ScrollBar1Change
    ExplicitLeft = 672
    ExplicitHeight = 513
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 424
    Width = 161
    Height = 57
    Caption = ' Selected letter properties '
    TabOrder = 8
    object Label7: TLabel
      Left = 62
      Top = 27
      Width = 36
      Height = 13
      Caption = 'Y offset'
      Color = clBtnFace
      ParentColor = False
    end
    object seLetterY: TSpinEdit
      Left = 8
      Top = 24
      Width = 50
      Height = 22
      MaxValue = 32
      MinValue = -32
      TabOrder = 0
      Value = 0
      OnChange = seLetterYChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 264
    Width = 161
    Height = 153
    Caption = ' Font properties '
    TabOrder = 9
    object Label6: TLabel
      Left = 62
      Top = 99
      Width = 60
      Height = 13
      Caption = 'Line spacing'
      Color = clBtnFace
      ParentColor = False
    end
    object Label5: TLabel
      Left = 62
      Top = 75
      Width = 62
      Height = 13
      Caption = 'Char spacing'
      Color = clBtnFace
      ParentColor = False
    end
    object Label4: TLabel
      Left = 62
      Top = 51
      Width = 66
      Height = 13
      Caption = 'Word spacing'
      Color = clBtnFace
      ParentColor = False
    end
    object Label3: TLabel
      Left = 62
      Top = 27
      Width = 77
      Height = 13
      Caption = 'BaseCharHeight'
      Color = clBtnFace
      ParentColor = False
    end
    object Label8: TLabel
      Left = 62
      Top = 123
      Width = 81
      Height = 13
      Caption = 'All letters Y offset'
      Color = clBtnFace
      ParentColor = False
    end
    object seCharSpacing: TSpinEdit
      Left = 8
      Top = 72
      Width = 50
      Height = 22
      MaxValue = 65535
      MinValue = -65535
      TabOrder = 0
      Value = 0
      OnChange = seFontPropsChange
    end
    object seLineSpacing: TSpinEdit
      Left = 8
      Top = 96
      Width = 50
      Height = 22
      MaxValue = 65535
      MinValue = -65535
      TabOrder = 1
      Value = 0
      OnChange = seFontPropsChange
    end
    object seWordSpacing: TSpinEdit
      Left = 8
      Top = 48
      Width = 50
      Height = 22
      MaxValue = 65535
      MinValue = -65535
      TabOrder = 2
      Value = 0
      OnChange = seFontPropsChange
    end
    object seBaseHeight: TSpinEdit
      Left = 8
      Top = 24
      Width = 50
      Height = 22
      MaxValue = 65535
      MinValue = -65535
      TabOrder = 3
      Value = 0
      OnChange = seFontPropsChange
    end
    object seAllYOffset: TSpinEdit
      Left = 8
      Top = 120
      Width = 50
      Height = 22
      MaxValue = 32
      MinValue = -32
      TabOrder = 4
      Value = 0
      OnChange = seFontPropsChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 488
    Width = 161
    Height = 113
    Caption = ' Export settings '
    TabOrder = 10
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 39
      Height = 13
      Caption = 'Padding'
    end
    object sePadRight: TSpinEdit
      Left = 104
      Top = 56
      Width = 49
      Height = 22
      MaxValue = 16
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object sePadLeft: TSpinEdit
      Left = 8
      Top = 56
      Width = 49
      Height = 22
      MaxValue = 16
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object sePadBottom: TSpinEdit
      Left = 56
      Top = 80
      Width = 49
      Height = 22
      MaxValue = 16
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object sePadTop: TSpinEdit
      Left = 56
      Top = 32
      Width = 49
      Height = 22
      MaxValue = 16
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 32
    Top = 40
  end
  object SaveDialog1: TSaveDialog
    Left = 96
    Top = 40
  end
end

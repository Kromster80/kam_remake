object frmMain: TfrmMain
  Left = 190
  Top = 131
  Width = 689
  Height = 637
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
    681
    610)
  PixelsPerInch = 96
  TextHeight = 13
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
    Caption = 'Refresh'
    TabOrder = 2
    OnClick = RefreshDataClick
  end
  object BitBtn1: TBitBtn
    Left = 5
    Top = 491
    Width = 97
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save Font File'
    Enabled = False
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 160
    Top = 8
    Width = 521
    Height = 569
    ActivePage = TabSheet1
    TabIndex = 0
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'Preview'
      DesignSize = (
        513
        541)
      object Image1: TImage
        Left = 0
        Top = 28
        Width = 512
        Height = 512
        OnMouseMove = Image1MouseMove
        OnMouseUp = Image1MouseUp
      end
      object btnExportBig: TBitBtn
        Left = 120
        Top = 0
        Width = 120
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Export BMP'
        TabOrder = 0
        OnClick = btnExportBigClick
      end
      object CheckCells: TCheckBox
        Left = 4
        Top = 4
        Width = 69
        Height = 17
        Caption = 'Show cells'
        TabOrder = 1
        OnClick = CheckCellsClick
      end
      object btnImportBig: TBitBtn
        Left = 240
        Top = 0
        Width = 120
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Import BMP'
        TabOrder = 2
        OnClick = btnImportBigClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Letters'
      ImageIndex = 1
      object Image2: TImage
        Left = 168
        Top = 208
        Width = 161
        Height = 161
      end
      object Image3: TImage
        Left = 0
        Top = 56
        Width = 512
        Height = 128
        OnMouseDown = Image3MouseDown
        OnMouseMove = Image3MouseMove
      end
      object Label1: TLabel
        Left = 16
        Top = 40
        Width = 61
        Height = 13
        Caption = 'Work palette'
        Color = clBtnFace
        ParentColor = False
      end
      object Label2: TLabel
        Left = 176
        Top = 192
        Width = 27
        Height = 13
        Caption = 'Letter'
        Color = clBtnFace
        ParentColor = False
      end
      object Image4: TImage
        Left = 8
        Top = 376
        Width = 489
        Height = 40
      end
      object Image5: TImage
        Left = 8
        Top = 424
        Width = 489
        Height = 80
      end
      object imgColourSelected: TImage
        Left = 4
        Top = 208
        Width = 48
        Height = 48
      end
      object Label3: TLabel
        Left = 12
        Top = 192
        Width = 27
        Height = 13
        Caption = 'Brush'
        Color = clBtnFace
        ParentColor = False
      end
      object Shape1: TShape
        Left = 0
        Top = 56
        Width = 18
        Height = 18
        Brush.Style = bsClear
        Pen.Color = clLime
        Pen.Width = 2
      end
      object Edit1: TEdit
        Left = 8
        Top = 352
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Sample phrase'
        OnChange = Edit1Change
      end
      object RadioGroup1: TRadioGroup
        Left = 0
        Top = 0
        Width = 513
        Height = 33
        Caption = ' Palette  '
        Columns = 9
        Items.Strings = (
          'map'
          'pal0'
          'pal1'
          'pal2'
          'pal3'
          'pal4'
          'pal5'
          'setup'
          'setup2')
        TabOrder = 1
        OnClick = RadioGroup1Click
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 590
    Width = 681
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
  object OpenDialog1: TOpenDialog
    Left = 568
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 608
    Top = 8
  end
end

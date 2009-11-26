object frmMain: TfrmMain
  Left = 192
  Top = 113
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
    603)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExport: TBitBtn
    Left = 161
    Top = 555
    Width = 120
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export BMP'
    TabOrder = 1
    OnClick = btnExportClick
  end
  object btnImport: TBitBtn
    Left = 323
    Top = 555
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
    Top = 555
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
    Height = 541
    ActivePage = TabSheet2
    TabIndex = 1
    TabOrder = 5
    object TabSheet1: TTabSheet
      Caption = 'Preview'
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 512
        Height = 512
        OnMouseMove = Image1MouseMove
        OnMouseUp = Image1MouseUp
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Letters'
      ImageIndex = 1
      object Image2: TImage
        Left = 168
        Top = 176
        Width = 161
        Height = 161
      end
      object Image3: TImage
        Left = 0
        Top = 24
        Width = 512
        Height = 128
        OnMouseDown = Image3MouseDown
        OnMouseMove = Image3MouseMove
      end
      object Label1: TLabel
        Left = 16
        Top = 8
        Width = 33
        Height = 13
        Caption = 'Palette'
      end
      object Label2: TLabel
        Left = 176
        Top = 160
        Width = 27
        Height = 13
        Caption = 'Letter'
      end
      object Image4: TImage
        Left = 8
        Top = 352
        Width = 489
        Height = 25
      end
      object Image5: TImage
        Left = 8
        Top = 384
        Width = 489
        Height = 41
      end
      object imgColourSelected: TImage
        Left = 0
        Top = 160
        Width = 128
        Height = 128
      end
      object Edit1: TEdit
        Left = 8
        Top = 328
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Sample phrase'
        OnChange = Edit1Change
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 584
    Width = 681
    Height = 19
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

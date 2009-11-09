object frmMain: TfrmMain
  Left = 192
  Top = 113
  Width = 689
  Height = 620
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
    593)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExport: TBitBtn
    Left = 161
    Top = 562
    Width = 120
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export BMP'
    TabOrder = 1
    OnClick = btnExportClick
  end
  object btnImport: TBitBtn
    Left = 323
    Top = 562
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
    Top = 562
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
    ActivePage = TabSheet1
    TabIndex = 0
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
      object Shape1: TShape
        Left = 32
        Top = 32
        Width = 33
        Height = 33
        Brush.Style = bsClear
        Pen.Color = clAqua
        Pen.Width = 2
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Letters'
      ImageIndex = 1
      object Image2: TImage
        Left = 168
        Top = 136
        Width = 161
        Height = 161
      end
      object Image3: TImage
        Left = 8
        Top = 24
        Width = 489
        Height = 81
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
        Top = 120
        Width = 27
        Height = 13
        Caption = 'Letter'
      end
      object Shape2: TShape
        Left = 24
        Top = 40
        Width = 17
        Height = 17
        Brush.Style = bsClear
        Pen.Color = clAqua
      end
      object Image4: TImage
        Left = 8
        Top = 328
        Width = 489
        Height = 25
      end
      object Image5: TImage
        Left = 8
        Top = 360
        Width = 489
        Height = 41
      end
      object Edit1: TEdit
        Left = 8
        Top = 304
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Sample phrase'
      end
    end
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

object Form1: TForm1
  Left = 438
  Top = 169
  Caption = 'Form1'
  ClientHeight = 585
  ClientWidth = 801
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    801
    585)
  PixelsPerInch = 96
  TextHeight = 16
  object Label4: TLabel
    Left = 280
    Top = 8
    Width = 50
    Height = 16
    Caption = 'Preview:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Image1: TImage
    Left = 280
    Top = 24
    Width = 512
    Height = 552
    Anchors = [akLeft, akTop, akRight]
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 36
    Height = 16
    Caption = 'Fonts:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label2: TLabel
    Left = 152
    Top = 8
    Width = 52
    Height = 16
    Caption = 'Variants:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label6: TLabel
    Left = 8
    Top = 392
    Width = 62
    Height = 16
    Caption = 'Atlas index'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object btnSave: TButton
    Left = 144
    Top = 456
    Width = 129
    Height = 57
    Caption = 'Save font ...'
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object btnExportTex: TButton
    Left = 8
    Top = 456
    Width = 129
    Height = 25
    Caption = 'Export texture ...'
    TabOrder = 1
    OnClick = btnExportTexClick
  end
  object btnImportTex: TButton
    Left = 8
    Top = 488
    Width = 129
    Height = 25
    Caption = 'Import texture ...'
    TabOrder = 2
    OnClick = btnImportTexClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 280
    Width = 265
    Height = 105
    Caption = ' Texture atlas properties '
    TabOrder = 3
    object Label5: TLabel
      Left = 152
      Top = 16
      Width = 45
      Height = 16
      Caption = 'Padding'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object sePadding: TSpinEdit
      Left = 152
      Top = 32
      Width = 49
      Height = 26
      MaxValue = 8
      MinValue = 0
      TabOrder = 0
      Value = 1
    end
    object rgSizeX: TRadioGroup
      Left = 14
      Top = 16
      Width = 59
      Height = 81
      Caption = ' Size X '
      ItemIndex = 1
      Items.Strings = (
        '128'
        '256'
        '512'
        '1024')
      TabOrder = 1
    end
    object rgSizeY: TRadioGroup
      Left = 80
      Top = 16
      Width = 65
      Height = 81
      Caption = ' Size Y '
      ItemIndex = 2
      Items.Strings = (
        '128'
        '256'
        '512'
        '1024')
      TabOrder = 2
    end
    object cbCells: TCheckBox
      Left = 152
      Top = 64
      Width = 81
      Height = 20
      Caption = 'Show cells'
      TabOrder = 3
    end
  end
  object ListBox1: TListBox
    Left = 8
    Top = 24
    Width = 137
    Height = 241
    TabOrder = 4
    OnClick = ListBox1Click
  end
  object btnCollate: TButton
    Left = 152
    Top = 208
    Width = 121
    Height = 25
    Caption = 'Collate selected'
    TabOrder = 5
    OnClick = btnCollateClick
  end
  object ListBox2: TListBox
    Left = 152
    Top = 24
    Width = 121
    Height = 177
    MultiSelect = True
    TabOrder = 6
  end
  object tbAtlas: TTrackBar
    Left = 0
    Top = 408
    Width = 150
    Height = 33
    TabOrder = 7
    OnChange = tbAtlasChange
  end
  object dlgSave: TSaveDialog
    Left = 288
    Top = 32
  end
  object dlgOpen: TOpenDialog
    Left = 288
    Top = 80
  end
end

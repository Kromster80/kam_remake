object Form1: TForm1
  Left = 438
  Top = 169
  Caption = 'Form1'
  ClientHeight = 649
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
    649)
  PixelsPerInch = 96
  TextHeight = 16
  object Label4: TLabel
    Left = 280
    Top = 8
    Width = 45
    Height = 16
    Caption = 'Preview'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Image1: TImage
    Left = 280
    Top = 24
    Width = 512
    Height = 552
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitHeight = 512
  end
  object Label1: TLabel
    Left = 8
    Top = 120
    Width = 83
    Height = 16
    Caption = 'Available fonts'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label2: TLabel
    Left = 152
    Top = 120
    Width = 96
    Height = 16
    Caption = 'Fonts codepages'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object btnSave: TButton
    Left = 416
    Top = 584
    Width = 129
    Height = 57
    Anchors = [akLeft, akBottom]
    Caption = 'Save font ...'
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object btnExportTex: TButton
    Left = 280
    Top = 584
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export texture ...'
    TabOrder = 1
    OnClick = btnExportTexClick
  end
  object btnImportTex: TButton
    Left = 280
    Top = 616
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Import texture ...'
    TabOrder = 2
    OnClick = btnImportTexClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 265
    Height = 97
    Caption = ' Texture atlas '
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
      Height = 72
      Caption = ' Size X '
      ItemIndex = 0
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
      Height = 73
      Caption = ' Size Y '
      ItemIndex = 0
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
    Top = 136
    Width = 137
    Height = 241
    TabOrder = 4
    OnClick = ListBox1Click
  end
  object btnCollate: TButton
    Left = 152
    Top = 320
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Collate codepages'
    TabOrder = 5
    OnClick = btnCollateClick
  end
  object btnCollateAuto: TButton
    Left = 152
    Top = 352
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Collate all'
    TabOrder = 6
    OnClick = btnCollateAllClick
  end
  object ListBox2: TListBox
    Left = 152
    Top = 136
    Width = 121
    Height = 177
    MultiSelect = True
    TabOrder = 7
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

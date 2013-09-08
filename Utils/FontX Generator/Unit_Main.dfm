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
  object GroupBox2: TGroupBox
    Left = 8
    Top = 112
    Width = 265
    Height = 353
    Caption = ' Generate from scratch '
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 96
      Width = 92
      Height = 16
      Caption = 'Used characters'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 61
      Height = 16
      Caption = 'Font name'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object Label3: TLabel
      Left = 144
      Top = 24
      Width = 51
      Height = 16
      Caption = 'Font size'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object btnGenerate: TButton
      Left = 136
      Top = 320
      Width = 121
      Height = 25
      Caption = 'Generate font'
      TabOrder = 0
      OnClick = btnGenerateClick
    end
    object Memo1: TMemo
      Left = 8
      Top = 112
      Width = 249
      Height = 201
      Font.Charset = 4
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial Unicode MS'
      Font.Style = []
      Lines.Strings = (
        '!"$%&'#39'()*+,-./0123456789:;=?@ABCD'
        'EFGHIJKLMNOPQRSTUVWXYZ[\]`a'
        'bcdefghijklmnopqrstuvwxyz???????'
        '???????????????????????????'
        '???????????????????????????????'
        '???????????????????????????????'
        '?'
        '???????????????????????'
        '????????????????????????'
        '??????????????????????????'
        '?')
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
      WantReturns = False
    end
    object seFontSize: TSpinEdit
      Left = 144
      Top = 40
      Width = 49
      Height = 26
      MaxValue = 24
      MinValue = 6
      TabOrder = 2
      Value = 11
    end
    object cbBold: TCheckBox
      Left = 200
      Top = 32
      Width = 44
      Height = 20
      Caption = 'Bold'
      TabOrder = 3
    end
    object cbItalic: TCheckBox
      Left = 200
      Top = 48
      Width = 47
      Height = 20
      Caption = 'Italic'
      TabOrder = 4
    end
    object btnCollectChars: TButton
      Left = 8
      Top = 320
      Width = 121
      Height = 25
      Caption = 'Collect "libx" chars'
      TabOrder = 5
      OnClick = btnCollectCharsClick
    end
    object cbAntialias: TCheckBox
      Left = 8
      Top = 72
      Width = 73
      Height = 20
      Caption = 'Antialias'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object cbFontName: TComboBox
      Left = 8
      Top = 40
      Width = 129
      Height = 24
      DropDownCount = 24
      TabOrder = 7
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 472
    Width = 265
    Height = 169
    Caption = ' Collate existing fonts '
    TabOrder = 5
    DesignSize = (
      265
      169)
    object ListBox1: TListBox
      Left = 8
      Top = 24
      Width = 121
      Height = 137
      TabOrder = 0
    end
    object btnCollate: TButton
      Left = 136
      Top = 20
      Width = 121
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Collate codepages'
      TabOrder = 1
      OnClick = btnCollateClick
    end
    object btnCollateAuto: TButton
      Left = 136
      Top = 52
      Width = 121
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Collate all'
      TabOrder = 2
      OnClick = btnCollateAllClick
    end
  end
  object btnOneClick: TButton
    Left = 552
    Top = 584
    Width = 129
    Height = 57
    Anchors = [akLeft, akBottom]
    Caption = 'One click generate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = btnOneClickClick
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

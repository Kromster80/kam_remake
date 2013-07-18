object Form1: TForm1
  Left = 438
  Height = 609
  Top = 169
  Width = 801
  Caption = 'Form1'
  ClientHeight = 609
  ClientWidth = 801
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '1.0.10.0'
  object Label4: TLabel
    Left = 280
    Height = 17
    Top = 8
    Width = 46
    Caption = 'Preview'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Image1: TImage
    Left = 280
    Height = 512
    Top = 24
    Width = 512
  end
  object btnSave: TButton
    Left = 416
    Height = 57
    Top = 544
    Width = 129
    Caption = 'Save font ...'
    OnClick = btnSaveClick
    TabOrder = 0
  end
  object btnExportTex: TButton
    Left = 280
    Height = 25
    Top = 544
    Width = 129
    Caption = 'Export texture ...'
    OnClick = btnExportTexClick
    TabOrder = 1
  end
  object btnImportTex: TButton
    Left = 280
    Height = 25
    Top = 576
    Width = 129
    Caption = 'Import texture ...'
    OnClick = btnImportTexClick
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Height = 97
    Top = 8
    Width = 265
    Caption = ' Texture atlas '
    ClientHeight = 79
    ClientWidth = 261
    TabOrder = 3
    object Label5: TLabel
      Left = 152
      Height = 17
      Top = 24
      Width = 46
      Caption = 'Padding'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object sePadding: TSpinEdit
      Left = 152
      Height = 24
      Top = 40
      Width = 49
      MaxValue = 8
      TabOrder = 0
      Value = 1
    end
    object rgSizeX: TRadioGroup
      Left = 14
      Height = 64
      Top = 24
      Width = 59
      AutoFill = True
      Caption = ' Size X '
      ChildSizing.LeftRightSpacing = 6
      ChildSizing.TopBottomSpacing = 6
      ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
      ChildSizing.EnlargeVertical = crsHomogenousChildResize
      ChildSizing.ShrinkHorizontal = crsScaleChilds
      ChildSizing.ShrinkVertical = crsScaleChilds
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 1
      ClientHeight = 46
      ClientWidth = 55
      ItemIndex = 0
      Items.Strings = (
        '128'
        '256'
        '512'
      )
      TabOrder = 1
    end
    object rgSizeY: TRadioGroup
      Left = 80
      Height = 65
      Top = 24
      Width = 65
      AutoFill = True
      Caption = ' Size Y '
      ChildSizing.LeftRightSpacing = 6
      ChildSizing.TopBottomSpacing = 6
      ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
      ChildSizing.EnlargeVertical = crsHomogenousChildResize
      ChildSizing.ShrinkHorizontal = crsScaleChilds
      ChildSizing.ShrinkVertical = crsScaleChilds
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 1
      ClientHeight = 47
      ClientWidth = 61
      ItemIndex = 0
      Items.Strings = (
        '128'
        '256'
        '512'
      )
      TabOrder = 2
    end
    object cbCells: TCheckBox
      Left = 152
      Height = 20
      Top = 72
      Width = 47
      Caption = 'Cells'
      TabOrder = 3
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Height = 313
    Top = 112
    Width = 265
    Caption = ' Generate from scratch '
    ClientHeight = 295
    ClientWidth = 261
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Height = 17
      Top = 72
      Width = 93
      Caption = 'Used characters'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object Label2: TLabel
      Left = 8
      Height = 17
      Top = 24
      Width = 62
      Caption = 'Font name'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object Label3: TLabel
      Left = 144
      Height = 17
      Top = 24
      Width = 52
      Caption = 'Font size'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object btnGenerate: TButton
      Left = 136
      Height = 25
      Top = 280
      Width = 121
      Caption = 'Generate font'
      OnClick = btnGenerateClick
      TabOrder = 0
    end
    object Memo1: TMemo
      Left = 8
      Height = 185
      Top = 88
      Width = 249
      Font.CharSet = 4
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial Unicode MS'
      Lines.Strings = (
        '!"$%&''()*+,-./0123456789:;=?@ABCD'
        'EFGHIJKLMNOPQRSTUVWXYZ[\]`a'
        'bcdefghijklmnopqrstuvwxyz???????'
        '???????????????????????????'
        '???????????????????????????????'
        '???????????????????????????????'
        '?'
        '???????????????????????'
        '????????????????????????'
        '??????????????????????????'
        '?'
      )
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
      WantReturns = False
    end
    object edtFontName: TEdit
      Left = 8
      Height = 24
      Top = 40
      Width = 129
      TabOrder = 2
      Text = 'Arial MS Uni'
    end
    object seFontSize: TSpinEdit
      Left = 144
      Height = 24
      Top = 40
      Width = 49
      MaxValue = 24
      MinValue = 6
      TabOrder = 3
      Value = 11
    end
    object cbBold: TCheckBox
      Left = 200
      Height = 20
      Top = 32
      Width = 44
      Caption = 'Bold'
      TabOrder = 4
    end
    object cbItalic: TCheckBox
      Left = 200
      Height = 20
      Top = 48
      Width = 47
      Caption = 'Italic'
      TabOrder = 5
    end
    object btnCollectChars: TButton
      Left = 8
      Height = 25
      Top = 280
      Width = 121
      Caption = 'Collect "libx" chars'
      OnClick = btnCollectCharsClick
      TabOrder = 6
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Height = 169
    Top = 432
    Width = 265
    Caption = ' Collate existing fonts '
    ClientHeight = 151
    ClientWidth = 261
    TabOrder = 5
    object ListBox1: TListBox
      Left = 8
      Height = 137
      Top = 24
      Width = 121
      ItemHeight = 0
      TabOrder = 0
    end
    object btnCollate: TButton
      Left = 136
      Height = 25
      Top = 4
      Width = 121
      Anchors = [akLeft, akBottom]
      Caption = 'Collate codepages'
      OnClick = btnCollateClick
      TabOrder = 1
    end
    object btnCollateAuto: TButton
      Left = 136
      Height = 25
      Top = 36
      Width = 121
      Anchors = [akLeft, akBottom]
      Caption = 'Collate all'
      OnClick = btnCollateAllClick
      TabOrder = 2
    end
  end
  object btnOneClick: TButton
    Left = 552
    Height = 57
    Top = 544
    Width = 129
    Caption = 'One click generate'
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    OnClick = btnOneClickClick
    ParentFont = False
    TabOrder = 6
  end
  object dlgSave: TSaveDialog
    left = 288
    top = 32
  end
  object dlgOpen: TOpenDialog
    left = 288
    top = 80
  end
end

object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 609
  ClientWidth = 801
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Label4: TLabel
    Left = 280
    Top = 8
    Width = 45
    Height = 16
    Caption = 'Preview'
  end
  object Image1: TImage
    Left = 280
    Top = 24
    Width = 512
    Height = 512
  end
  object btnSave: TButton
    Left = 416
    Top = 544
    Width = 129
    Height = 57
    Caption = 'Save font ...'
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object btnExportTex: TButton
    Left = 280
    Top = 544
    Width = 129
    Height = 25
    Caption = 'Export texture ...'
    TabOrder = 1
    OnClick = btnExportTexClick
  end
  object btnImportTex: TButton
    Left = 280
    Top = 576
    Width = 129
    Height = 25
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
      Top = 24
      Width = 45
      Height = 16
      Caption = 'Padding'
    end
    object sePadding: TSpinEdit
      Left = 152
      Top = 40
      Width = 49
      Height = 26
      MaxValue = 8
      MinValue = 0
      TabOrder = 0
      Value = 1
    end
    object rgSizeX: TRadioGroup
      Left = 8
      Top = 24
      Width = 65
      Height = 65
      Caption = ' Size X '
      ItemIndex = 0
      Items.Strings = (
        '128'
        '256'
        '512')
      TabOrder = 1
    end
    object rgSizeY: TRadioGroup
      Left = 80
      Top = 24
      Width = 65
      Height = 65
      Caption = ' Size Y '
      ItemIndex = 0
      Items.Strings = (
        '128'
        '256'
        '512')
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 112
    Width = 265
    Height = 313
    Caption = ' Generate from scratch '
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 72
      Width = 92
      Height = 16
      Caption = 'Used characters'
    end
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 61
      Height = 16
      Caption = 'Font name'
    end
    object Label3: TLabel
      Left = 144
      Top = 24
      Width = 51
      Height = 16
      Caption = 'Font size'
    end
    object btnGenerate: TButton
      Left = 136
      Top = 280
      Width = 121
      Height = 25
      Caption = 'Generate font'
      TabOrder = 0
      OnClick = btnGenerateClick
    end
    object Memo1: TMemo
      Left = 8
      Top = 88
      Width = 249
      Height = 185
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial Unicode MS'
      Font.Style = []
      Lines.Strings = (
        '!"$%&'#39'()*+,-./0123456789:;=?@ABCD'
        'EFGHIJKLMNOPQRSTUVWXYZ[\]`a'
        'bcdefghijklmnopqrstuvwxyz'#161#191#192#193#194#195#196
        #197#199#200#201#202#205#206#209#211#213#214#216#218#220#221#223#224#225#226#227#228#229#230#231#232#233#234
        #235#236#237#238#239#241#242#243#244#245#246#248#249#250#251#252#253#258#259#261#262#263#268#269#270#271#278#279#280#281#282
        #283#286#287#302#303#304#305#317#318#321#322#324#328#336#337#341#344#345#346#347#350#351#352#353#356#357#362#363#366#367#368#369
        #370#371#377#378#379#380#381#382#1025#1030#1031#1038#1040#1041#1042#1043#1044#1045#1046#1047#1048#1049#1050#1051#1052#1053
        #1054#1055#1056#1057#1058#1059#1060#1061#1062#1063#1064#1065#1066#1067#1068#1069#1070#1071#1072#1073#1074#1075#1076#1077
        #1078#1079#1080#1081#1082#1083#1084#1085#1086#1087#1088#1089#1090#1091#1092#1093#1094#1095#1096#1097#1098#1099#1100#1101#1102#1103#1105#1108#1110
        #1111#1118#8217)
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
      WantReturns = False
    end
    object edtFontName: TEdit
      Left = 8
      Top = 40
      Width = 129
      Height = 24
      TabOrder = 2
      Text = 'Arial MS Uni'
    end
    object seFontSize: TSpinEdit
      Left = 144
      Top = 40
      Width = 49
      Height = 26
      MaxValue = 24
      MinValue = 6
      TabOrder = 3
      Value = 11
    end
    object cbBold: TCheckBox
      Left = 200
      Top = 40
      Width = 41
      Height = 17
      Caption = 'Bold'
      TabOrder = 4
    end
    object cbItalic: TCheckBox
      Left = 200
      Top = 56
      Width = 49
      Height = 17
      Caption = 'Italic'
      TabOrder = 5
    end
    object btnCollectChars: TButton
      Left = 8
      Top = 280
      Width = 121
      Height = 25
      Caption = 'Collect "libx" chars'
      TabOrder = 6
      OnClick = btnCollectCharsClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 432
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
      Top = 24
      Width = 121
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Collate codepages'
      TabOrder = 1
      OnClick = btnCollateClick
    end
    object btnCollateAuto: TButton
      Left = 136
      Top = 56
      Width = 121
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Collate all'
      TabOrder = 2
      OnClick = btnCollateAllClick
    end
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

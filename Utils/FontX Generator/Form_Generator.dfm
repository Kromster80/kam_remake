object Form1: TForm1
  Left = 438
  Top = 169
  Caption = 'Form1'
  ClientHeight = 641
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
    641)
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
    Height = 608
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitHeight = 512
  end
  object Label6: TLabel
    Left = 16
    Top = 520
    Width = 62
    Height = 16
    Caption = 'Atlas index'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object btnSave: TButton
    Left = 144
    Top = 576
    Width = 129
    Height = 57
    Anchors = [akLeft, akBottom]
    Caption = 'Save font ...'
    TabOrder = 0
    OnClick = btnSaveClick
    ExplicitTop = 624
  end
  object btnExportTex: TButton
    Left = 8
    Top = 576
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export texture ...'
    TabOrder = 1
    OnClick = btnExportTexClick
    ExplicitTop = 624
  end
  object btnImportTex: TButton
    Left = 8
    Top = 608
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Import texture ...'
    TabOrder = 2
    OnClick = btnImportTexClick
    ExplicitTop = 656
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 265
    Height = 497
    Caption = ' Generate from scratch '
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 72
      Width = 92
      Height = 16
      Caption = 'Used characters'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object Label2: TLabel
      Left = 8
      Top = 296
      Width = 61
      Height = 16
      Caption = 'Font name'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object Label3: TLabel
      Left = 144
      Top = 296
      Width = 51
      Height = 16
      Caption = 'Font size'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object Label5: TLabel
      Left = 144
      Top = 360
      Width = 45
      Height = 16
      Caption = 'Padding'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object Label7: TLabel
      Left = 8
      Top = 344
      Width = 90
      Height = 16
      Caption = 'Atlas properties'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
    end
    object btnGenerate: TButton
      Left = 8
      Top = 456
      Width = 249
      Height = 25
      Caption = 'Generate font'
      TabOrder = 0
      OnClick = btnGenerateClick
    end
    object Memo1: TMemo
      Left = 8
      Top = 88
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
        'bcdefghijklmnopqrstuvwxyz'#161#191#192#193#194#195#196
        #197#199#200#201#202#205#206#209#211#213#214#216#218#220#221#223#224#225#226#227#228#229#230#231#232#233#234
        #235#236#237#238#239#241#242#243#244#245#246#248#249#250#251#252#253#258#259#261#262#263#268#269#270#271#278#279#280#281#282
        #283#286#287#302#303#304#305#317#318#321#322#324#328#336#337#341#344#345#346#347#350#351#352#353#356#357#362#363#366#367#368#369
        #370#371#377#378#379#380#381#382#1025#1030#1031#1038#1040#1041#1042#1043#1044#1045#1046#1047#1048#1049#1050#1051#1052#1053#1054
        #1055#1056#1057#1058#1059#1060#1061#1062#1063#1064#1065#1066#1067#1068#1069#1070#1071#1072#1073#1074#1075#1076#1077#1078
        #1079#1080#1081#1082#1083#1084#1085#1086#1087#1088#1089#1090#1091#1092#1093#1094#1095#1096#1097#1098#1099#1100#1101#1102#1103#1105#1108#1110#1111#1118#8217
        #20154#20219#20316#20687#20986#21046#21153#21333#22120#22242#22270#22320#22810#24405
        #25103#28216#30475
        #32534#32593#32622#32852#35266#35774#36753#36864#38431)
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
      WantReturns = False
    end
    object seFontSize: TSpinEdit
      Left = 144
      Top = 312
      Width = 49
      Height = 26
      MaxValue = 24
      MinValue = 6
      TabOrder = 2
      Value = 11
    end
    object cbBold: TCheckBox
      Left = 200
      Top = 304
      Width = 44
      Height = 20
      Caption = 'Bold'
      TabOrder = 3
    end
    object cbItalic: TCheckBox
      Left = 200
      Top = 320
      Width = 47
      Height = 20
      Caption = 'Italic'
      TabOrder = 4
    end
    object btnCollectChars: TButton
      Left = 8
      Top = 24
      Width = 121
      Height = 25
      Caption = 'Collect "libx" chars'
      TabOrder = 5
      OnClick = btnCollectCharsClick
    end
    object cbFontName: TComboBox
      Left = 8
      Top = 312
      Width = 129
      Height = 24
      DropDownCount = 24
      TabOrder = 7
    end
    object btnChnRange: TButton
      Left = 136
      Top = 24
      Width = 121
      Height = 25
      Caption = 'Chinese range'
      TabOrder = 8
      OnClick = btnChnRangeClick
    end
    object rgSizeX: TRadioGroup
      Left = 6
      Top = 360
      Width = 59
      Height = 89
      Caption = ' Size X '
      ItemIndex = 1
      Items.Strings = (
        '128'
        '256'
        '512'
        '1024'
        '2048')
      TabOrder = 9
    end
    object rgSizeY: TRadioGroup
      Left = 72
      Top = 360
      Width = 65
      Height = 89
      Caption = ' Size Y '
      ItemIndex = 1
      Items.Strings = (
        '128'
        '256'
        '512'
        '1024'
        '2048')
      TabOrder = 10
    end
    object sePadding: TSpinEdit
      Left = 144
      Top = 376
      Width = 49
      Height = 26
      MaxValue = 8
      MinValue = 0
      TabOrder = 11
      Value = 1
    end
    object cbCells: TCheckBox
      Left = 144
      Top = 408
      Width = 81
      Height = 20
      Caption = 'Show cells'
      TabOrder = 12
    end
    object cbAntialias: TCheckBox
      Left = 144
      Top = 424
      Width = 105
      Height = 20
      Caption = 'Antialias letters'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object btnJpnRange: TButton
      Left = 136
      Top = 48
      Width = 121
      Height = 25
      Caption = 'Japanese range'
      TabOrder = 13
      OnClick = btnJpnRangeClick
    end
    object btnKorRange: TButton
      Left = 8
      Top = 48
      Width = 121
      Height = 25
      Caption = 'Korean range'
      TabOrder = 14
      OnClick = btnKorRangeClick
    end
  end
  object tbAtlas: TTrackBar
    Left = 8
    Top = 536
    Width = 150
    Height = 33
    TabOrder = 4
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

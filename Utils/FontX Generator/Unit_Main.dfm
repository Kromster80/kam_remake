object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 553
  ClientWidth = 801
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    801
    553)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 64
    Width = 92
    Height = 16
    Caption = 'Used characters'
  end
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 61
    Height = 16
    Caption = 'Font name'
  end
  object Label3: TLabel
    Left = 144
    Top = 16
    Width = 51
    Height = 16
    Caption = 'Font size'
  end
  object Label4: TLabel
    Left = 280
    Top = 16
    Width = 45
    Height = 16
    Caption = 'Preview'
  end
  object Image1: TImage
    Left = 280
    Top = 32
    Width = 512
    Height = 512
  end
  object Label5: TLabel
    Left = 72
    Top = 440
    Width = 45
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Padding'
  end
  object Label6: TLabel
    Left = 8
    Top = 440
    Width = 24
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Size'
  end
  object btnGenerate: TButton
    Left = 8
    Top = 488
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Generate font'
    TabOrder = 0
    OnClick = btnGenerateClick
    ExplicitTop = 264
  end
  object Memo1: TMemo
    Left = 8
    Top = 80
    Width = 265
    Height = 353
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial Unicode MS'
    Font.Style = []
    Lines.Strings = (
      ' '
      '!"$%&'#39'()*,-./0123456789:;?@ABCDEFG'
      'HIJKLMNOPQRSTUVWXYZ[\]`abcdefgh'
      'ijklmnopqrstuvwxyz|'#182#199#231#246#252#261#268#269#279#281#287#302#303#304#305#350#351#352
      #353#363#371#381#382#1025#1026#1027#1030#1031#1033#1034#1035#1038#1040#1041#1042#1043#1044#1045#1046#1047#1048#1049#1050#1051#1052
      #1053#1054#1055#1056#1057#1058#1059#1060#1061#1062#1063#1064#1065#1066#1068#1069#1070#1071#1072#1073#1074#1075#1076#1077#1078#1079
      #1080#1081#1082#1083#1084#1085#1086#1087#1088#1089#1090#1091#1092#1093#1094#1095#1096#1097#1098#1099#1100#1101#1102#1103#1105#1108#1109#1110#1111#1112#1113
      #1114#1115#1116#1118#1119#8217#8249#8470)
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object edtFontName: TEdit
    Left = 8
    Top = 32
    Width = 129
    Height = 24
    TabOrder = 2
    Text = 'Arial MS Uni'
  end
  object seFontSize: TSpinEdit
    Left = 144
    Top = 32
    Width = 49
    Height = 26
    MaxValue = 24
    MinValue = 6
    TabOrder = 3
    Value = 11
  end
  object btnSave: TButton
    Left = 8
    Top = 520
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save font ...'
    TabOrder = 4
    OnClick = btnSaveClick
    ExplicitTop = 296
  end
  object cbBold: TCheckBox
    Left = 200
    Top = 32
    Width = 41
    Height = 17
    Caption = 'Bold'
    TabOrder = 5
  end
  object cbItalic: TCheckBox
    Left = 200
    Top = 48
    Width = 49
    Height = 17
    Caption = 'Italic'
    TabOrder = 6
  end
  object sePadding: TSpinEdit
    Left = 72
    Top = 456
    Width = 49
    Height = 26
    Anchors = [akLeft, akBottom]
    MaxValue = 8
    MinValue = 0
    TabOrder = 7
    Value = 1
  end
  object btnExportTex: TButton
    Left = 144
    Top = 488
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export texture ...'
    TabOrder = 8
    OnClick = btnExportTexClick
    ExplicitTop = 264
  end
  object btnImportTex: TButton
    Left = 144
    Top = 520
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Import texture ...'
    TabOrder = 9
    OnClick = btnImportTexClick
    ExplicitTop = 296
  end
  object btnCollectChars: TButton
    Left = 144
    Top = 440
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Collect chars'
    TabOrder = 10
    OnClick = btnCollectCharsClick
  end
  object SpinEdit1: TSpinEdit
    Left = 8
    Top = 456
    Width = 57
    Height = 26
    Anchors = [akLeft, akBottom]
    MaxValue = 512
    MinValue = 256
    TabOrder = 11
    Value = 256
  end
  object dlgSave: TSaveDialog
    Left = 288
    Top = 40
  end
  object dlgOpen: TOpenDialog
    Left = 352
    Top = 40
  end
end

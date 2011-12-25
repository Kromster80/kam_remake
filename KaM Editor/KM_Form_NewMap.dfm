object FormNewMap: TFormNewMap
  Left = 422
  Top = 417
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Create New Map'
  ClientHeight = 161
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  HelpFile = 'KM_Editor.hlp'
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 216
    Top = 96
    Width = 137
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = CreateMap
  end
  object ButtonCancel: TButton
    Left = 216
    Top = 128
    Width = 137
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = CreateCancel
  end
  object RGMapVersion: TRadioGroup
    Left = 8
    Top = 72
    Width = 201
    Height = 81
    Caption = ' Map format  '
    Enabled = False
    ItemIndex = 1
    Items.Strings = (
      'Original KaM TSK'
      'Original KaM TPR/TSK'
      'KaM Remake'
      'KaM TKE')
    TabOrder = 3
  end
  object GrMapSize: TGroupBox
    Left = 8
    Top = 8
    Width = 201
    Height = 57
    Caption = ' Map size  '
    TabOrder = 4
    object Label1: TLabel
      Left = 58
      Top = 27
      Width = 11
      Height = 13
      Caption = ' x '
    end
    object Label2: TLabel
      Left = 126
      Top = 27
      Width = 22
      Height = 13
      Caption = 'Tiles'
    end
    object RGX: TComboBox
      Left = 8
      Top = 24
      Width = 49
      Height = 21
      Style = csDropDownList
      DropDownCount = 16
      ItemHeight = 13
      ItemIndex = 4
      TabOrder = 0
      Text = '96'
      Items.Strings = (
        '32'
        '48'
        '64'
        '80'
        '96'
        '112'
        '128'
        '144'
        '160'
        '176'
        '192')
    end
    object RGY: TComboBox
      Left = 72
      Top = 24
      Width = 49
      Height = 21
      Style = csDropDownList
      DropDownCount = 16
      ItemHeight = 13
      ItemIndex = 4
      TabOrder = 1
      Text = '96'
      Items.Strings = (
        '32'
        '48'
        '64'
        '80'
        '96'
        '112'
        '128'
        '144'
        '160'
        '176'
        '192')
    end
  end
  object ButtonHelp: TButton
    Left = 216
    Top = 8
    Width = 137
    Height = 25
    HelpType = htKeyword
    Caption = 'Help'
    TabOrder = 2
    OnClick = ButtonHelpClick
  end
end

object Form1: TForm1
  Left = 192
  Top = 148
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Script Validator'
  ClientHeight = 345
  ClientWidth = 593
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    593
    345)
  PixelsPerInch = 96
  TextHeight = 18
  object Label1: TLabel
    Left = 8
    Top = 15
    Width = 62
    Height = 18
    Caption = 'Script file:'
  end
  object Label2: TLabel
    Left = 8
    Top = 78
    Width = 44
    Height = 18
    Caption = 'Result:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 39
    Width = 387
    Height = 26
    TabOrder = 0
    OnChange = Edit1Change
  end
  object btnBrowseFile: TButton
    Left = 496
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Browse File'
    TabOrder = 1
    OnClick = btnBrowseFileClick
  end
  object btnValidate: TButton
    Left = 401
    Top = 39
    Width = 184
    Height = 26
    Caption = 'Validate'
    TabOrder = 2
    OnClick = btnValidateClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 104
    Width = 577
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button1: TButton
    Left = 401
    Top = 71
    Width = 184
    Height = 25
    Caption = 'Validate all'
    TabOrder = 4
    OnClick = Button1Click
  end
  object btnBrowsePath: TButton
    Left = 401
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Browse Path'
    TabOrder = 5
    OnClick = btnBrowsePathClick
  end
  object OpenDialog1: TOpenDialog
    Filter = 'KaM Remake script files (*.script)|*.script'
    Left = 40
    Top = 120
  end
end

object Form1: TForm1
  Left = 192
  Top = 148
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Script Validator'
  ClientHeight = 346
  ClientWidth = 593
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    593
    346)
  PixelsPerInch = 96
  TextHeight = 18
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 62
    Height = 18
    Caption = 'Script file:'
  end
  object Label2: TLabel
    Left = 8
    Top = 72
    Width = 44
    Height = 18
    Caption = 'Result:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 32
    Width = 401
    Height = 26
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 416
    Top = 32
    Width = 89
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnValidate: TButton
    Left = 512
    Top = 32
    Width = 73
    Height = 25
    Caption = 'Validate'
    TabOrder = 2
    OnClick = btnValidateClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 96
    Width = 577
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object OpenDialog1: TOpenDialog
    Filter = 'KaM Remake script files (*.script)|*.script'
    Left = 376
    Top = 32
  end
end

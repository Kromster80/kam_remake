object Form1: TForm1
  Left = 192
  Top = 148
  Width = 677
  Height = 534
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Script Validator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    659
    487)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 16
    Caption = 'Script file:'
  end
  object Label2: TLabel
    Left = 8
    Top = 104
    Width = 40
    Height = 16
    Caption = 'Result:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 529
    Height = 24
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 536
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Browse'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnValidate: TButton
    Left = 8
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Validate'
    TabOrder = 2
    OnClick = btnValidateClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 120
    Width = 646
    Height = 361
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object OpenDialog1: TOpenDialog
    Filter = 'KaM Remake script files (*.script)|*.script'
    Left = 568
    Top = 64
  end
end

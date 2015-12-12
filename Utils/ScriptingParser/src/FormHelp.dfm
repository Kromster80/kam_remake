object helpForm: ThelpForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Help'
  ClientHeight = 128
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 137
    Height = 105
    Caption = 'Shortkeys'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 3
      Top = 24
      Width = 55
      Height = 13
      Caption = 'Open file: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 3
      Top = 43
      Width = 51
      Height = 13
      Caption = 'Save file:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 3
      Top = 62
      Width = 64
      Height = 13
      Caption = 'Exit parser:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 79
      Top = 24
      Width = 43
      Height = 13
      Caption = 'Ctrl + O'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 79
      Top = 43
      Width = 42
      Height = 13
      Caption = 'Ctrl + S'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 79
      Top = 62
      Width = 43
      Height = 13
      Caption = 'Ctrl + Q'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label12: TLabel
      Left = 3
      Top = 81
      Width = 63
      Height = 13
      Caption = 'Help menu:'
    end
    object Label13: TLabel
      Left = 79
      Top = 81
      Width = 43
      Height = 13
      Caption = 'Ctrl + H'
    end
  end
  object GroupBox2: TGroupBox
    Left = 160
    Top = 8
    Width = 169
    Height = 105
    Caption = 'Credits'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object Label7: TLabel
      Left = 3
      Top = 24
      Width = 17
      Height = 13
      Caption = 'By:'
    end
    object Label8: TLabel
      Left = 3
      Top = 81
      Width = 74
      Height = 13
      Caption = 'Intended for:'
    end
    object Label9: TLabel
      Left = 83
      Top = 24
      Width = 64
      Height = 13
      Caption = 'Krom Stern'
    end
    object Label10: TLabel
      Left = 83
      Top = 43
      Width = 76
      Height = 13
      Caption = 'Thimo Braker'
    end
    object Label11: TLabel
      Left = 83
      Top = 81
      Width = 66
      Height = 13
      Hint = 'Knights and Merchants and Knights Province.'
      Caption = 'KMR and KP'
      ParentShowHint = False
      ShowHint = True
    end
  end
end

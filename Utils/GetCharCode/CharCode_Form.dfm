object CharCode: TCharCode
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CharCode'
  ClientHeight = 179
  ClientWidth = 202
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 131
    Width = 52
    Height = 13
    Caption = 'Hex Value:'
  end
  object Label3: TLabel
    Left = 8
    Top = 35
    Width = 53
    Height = 13
    Caption = 'Type here:'
  end
  object Label4: TLabel
    Left = 8
    Top = 83
    Width = 56
    Height = 13
    Caption = 'Char value:'
  end
  object Edit1: TEdit
    Left = 70
    Top = 32
    Width = 123
    Height = 21
    TabOrder = 0
    OnKeyDown = Edit1KeyDown
    OnKeyPress = Edit1KeyPress
    OnKeyUp = Edit1KeyUp
  end
  object Edit2: TEdit
    Left = 70
    Top = 80
    Width = 123
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 70
    Top = 128
    Width = 123
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
end

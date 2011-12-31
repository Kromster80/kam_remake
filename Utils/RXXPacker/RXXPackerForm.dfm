object RXXForm1: TRXXForm1
  Left = 192
  Top = 148
  Width = 256
  Height = 223
  Caption = 'RXX Packer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object btnPackMenu: TButton
    Left = 24
    Top = 32
    Width = 185
    Height = 49
    Caption = 'Pack RemakeMenu.rxx'
    TabOrder = 0
    OnClick = btnPackMenuClick
  end
  object btnPackGame: TButton
    Left = 24
    Top = 96
    Width = 185
    Height = 49
    Caption = 'Pack RemakeGame.rxx'
    TabOrder = 1
    OnClick = btnPackGameClick
  end
end

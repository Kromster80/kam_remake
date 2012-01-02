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
  PixelsPerInch = 96
  TextHeight = 16
  object btnPackRXX: TButton
    Left = 24
    Top = 48
    Width = 185
    Height = 49
    Caption = 'Pack to RXX File'
    TabOrder = 0
    OnClick = btnPackRXXClick
  end
  object ComboBox1: TComboBox
    Left = 24
    Top = 16
    Width = 185
    Height = 24
    Style = csDropDownList
    ImeMode = imHanguel
    ItemHeight = 16
    TabOrder = 1
  end
end

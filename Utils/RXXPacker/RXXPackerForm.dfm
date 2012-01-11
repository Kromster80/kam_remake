object RXXForm1: TRXXForm1
  Left = 635
  Top = 148
  Width = 256
  Height = 344
  Caption = 'RXX Packer'
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object btnPackRXX: TButton
    Left = 24
    Top = 240
    Width = 185
    Height = 49
    Caption = 'Pack to RXX File'
    TabOrder = 0
    OnClick = btnPackRXXClick
  end
  object ListBox1: TListBox
    Left = 24
    Top = 16
    Width = 185
    Height = 201
    ItemHeight = 16
    MultiSelect = True
    TabOrder = 1
  end
end

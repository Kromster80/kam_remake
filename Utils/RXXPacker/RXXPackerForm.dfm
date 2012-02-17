object RXXForm1: TRXXForm1
  Left = 72
  Top = 90
  Caption = 'RXX Packer'
  ClientHeight = 492
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    689
    492)
  PixelsPerInch = 96
  TextHeight = 13
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

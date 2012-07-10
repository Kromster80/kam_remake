object RXXForm1: TRXXForm1
  Left = 72
  Top = 90
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'RXX Packer'
  ClientHeight = 225
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    289
    225)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 176
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object btnPackRXX: TButton
    Left = 16
    Top = 192
    Width = 257
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Pack to RXX File'
    TabOrder = 0
    OnClick = btnPackRXXClick
  end
  object ListBox1: TListBox
    Left = 16
    Top = 16
    Width = 257
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
end

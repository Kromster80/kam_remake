object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Dependencies Grapher'
  ClientHeight = 59
  ClientWidth = 223
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ButChooseDpr: TButton
    Left = 24
    Top = 16
    Width = 153
    Height = 25
    Caption = 'Choose Delphi Project'
    TabOrder = 0
    OnClick = ButChooseDprClick
  end
  object ButBuildGraph: TButton
    Left = 8
    Top = 8
    Width = 169
    Height = 25
    Caption = 'Build Graph'
    TabOrder = 1
    Visible = False
    OnClick = ButBuildGraphClick
  end
  object ChConsSystem: TCheckBox
    Left = 79
    Top = 39
    Width = 144
    Height = 13
    Caption = 'Consider system units'
    TabOrder = 2
    Visible = False
  end
  object OpenDialog: TOpenDialog
    Filter = 'Delphi Project|*.dpr|Any File|*.*'
    Left = 192
    Top = 32
  end
end

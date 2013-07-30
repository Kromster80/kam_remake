object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Dependencies Grapher'
  ClientHeight = 105
  ClientWidth = 233
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ButChooseDpr: TButton
    Left = 40
    Top = 8
    Width = 153
    Height = 25
    Caption = 'Choose Delphi Project'
    TabOrder = 0
    OnClick = ButChooseDprClick
  end
  object ButBuildGraph: TButton
    Left = 40
    Top = 72
    Width = 153
    Height = 25
    Caption = 'Build Graph'
    TabOrder = 1
    Visible = False
    OnClick = ButBuildGraphClick
  end
  object ChConsSystem: TCheckBox
    Left = 39
    Top = 47
    Width = 122
    Height = 13
    Caption = 'Consider system units'
    TabOrder = 2
    Visible = False
  end
  object ProgressBar: TProgressBar
    Left = 43
    Top = 8
    Width = 150
    Height = 17
    TabOrder = 3
    Visible = False
  end
  object OpenDialog: TOpenDialog
    Filter = 'Delphi Project|*.dpr'
    Top = 8
  end
  object Timer: TTimer
    Enabled = False
    Interval = 10
    OnTimer = TimerTimer
    Left = 200
    Top = 48
  end
end

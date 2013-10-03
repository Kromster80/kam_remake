object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Dependencies Grapher'
  ClientHeight = 153
  ClientWidth = 389
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object btnSelectDpr: TButton
    Left = 40
    Top = 8
    Width = 153
    Height = 33
    Caption = 'Select Delphi Project ...'
    TabOrder = 0
    OnClick = btnSelectDprClick
  end
  object btnExportCsv: TButton
    Left = 40
    Top = 112
    Width = 153
    Height = 33
    Caption = 'Export as CSV'
    Enabled = False
    TabOrder = 1
    OnClick = btnExportCsvClick
  end
  object ChConsSystem: TCheckBox
    Left = 39
    Top = 87
    Width = 122
    Height = 13
    Caption = 'Include system units'
    TabOrder = 2
    Visible = False
  end
  object pbProgress: TProgressBar
    Left = 40
    Top = 56
    Width = 153
    Height = 17
    TabOrder = 3
  end
  object btnExportGraphml: TButton
    Left = 200
    Top = 112
    Width = 153
    Height = 33
    Caption = 'Export as GraphML'
    Enabled = False
    TabOrder = 4
    OnClick = btnExportGraphmlClick
  end
  object odSelectProject: TOpenDialog
    Filter = 'Delphi Project|*.dpr'
    Top = 8
  end
  object Timer: TTimer
    Enabled = False
    Interval = 10
    Left = 200
    Top = 48
  end
end

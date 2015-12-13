object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pascal To Wiki Parser'
  ClientHeight = 562
  ClientWidth = 784
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    784
    562)
  PixelsPerInch = 96
  TextHeight = 13
  object txtParserOutput: TMemo
    Left = 0
    Top = 199
    Width = 785
    Height = 364
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyPress = txtParserOutputKeyPress
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 688
    Height = 193
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Settings'
    TabOrder = 1
    DesignSize = (
      688
      193)
    object Label1: TLabel
      Left = 3
      Top = 19
      Width = 56
      Height = 13
      Caption = 'Actions file:'
    end
    object Label2: TLabel
      Left = 3
      Top = 46
      Width = 54
      Height = 13
      Caption = 'Events file:'
    end
    object Label3: TLabel
      Left = 3
      Top = 73
      Width = 52
      Height = 13
      Caption = 'States file:'
    end
    object Label4: TLabel
      Left = 3
      Top = 117
      Width = 93
      Height = 13
      Caption = 'Output file Actions:'
    end
    object Label5: TLabel
      Left = 3
      Top = 144
      Width = 91
      Height = 13
      Caption = 'Output file Events:'
    end
    object Label6: TLabel
      Left = 3
      Top = 171
      Width = 89
      Height = 13
      Caption = 'Output file States:'
    end
    object edtActionsFile: TEdit
      Left = 65
      Top = 16
      Width = 620
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edtOnTextChange
    end
    object edtEventsFile: TEdit
      Left = 65
      Top = 43
      Width = 620
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edtOnTextChange
    end
    object edtStatesFile: TEdit
      Left = 65
      Top = 70
      Width = 620
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edtOnTextChange
    end
    object edtOutputFileActions: TEdit
      Left = 102
      Top = 114
      Width = 582
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edtOnTextChange
    end
    object edtOutputFileEvents: TEdit
      Left = 102
      Top = 141
      Width = 582
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edtOnTextChange
    end
    object edtOutputFileStates: TEdit
      Left = 102
      Top = 168
      Width = 583
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = edtOnTextChange
    end
  end
  object btnParse: TButton
    Left = 694
    Top = 41
    Width = 82
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Parse'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    OnClick = btnParseClick
  end
  object btnSave: TButton
    Left = 694
    Top = 139
    Width = 82
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object OpenTxtDlg: TOpenTextFileDialog
    Filter = 'Pascal files (*.pas)|*.PAS|Any file (*.*)|*.*'
    Left = 552
    Top = 336
  end
end

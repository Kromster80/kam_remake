object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pascal To Wiki Parser'
  ClientHeight = 561
  ClientWidth = 785
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
    785
    561)
  PixelsPerInch = 96
  TextHeight = 13
  object txtParserOutput: TMemo
    Left = 0
    Top = 224
    Width = 785
    Height = 337
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyPress = txtParserOutputKeyPress
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 673
    Height = 201
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Settings '
    TabOrder = 1
    DesignSize = (
      673
      201)
    object Label1: TLabel
      Left = 11
      Top = 19
      Width = 56
      Height = 13
      Caption = 'Actions file:'
    end
    object Label2: TLabel
      Left = 11
      Top = 46
      Width = 54
      Height = 13
      Caption = 'Events file:'
    end
    object Label3: TLabel
      Left = 11
      Top = 73
      Width = 52
      Height = 13
      Caption = 'States file:'
    end
    object Label4: TLabel
      Left = 11
      Top = 117
      Width = 93
      Height = 13
      Caption = 'Output file Actions:'
    end
    object Label5: TLabel
      Left = 11
      Top = 144
      Width = 91
      Height = 13
      Caption = 'Output file Events:'
    end
    object Label6: TLabel
      Left = 11
      Top = 171
      Width = 89
      Height = 13
      Caption = 'Output file States:'
    end
    object edtActionsFile: TEdit
      Left = 72
      Top = 16
      Width = 586
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edtOnTextChange
    end
    object edtEventsFile: TEdit
      Left = 72
      Top = 43
      Width = 586
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edtOnTextChange
    end
    object edtStatesFile: TEdit
      Left = 72
      Top = 70
      Width = 586
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edtOnTextChange
    end
    object edtOutputFileActions: TEdit
      Left = 112
      Top = 114
      Width = 546
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edtOnTextChange
    end
    object edtOutputFileEvents: TEdit
      Left = 112
      Top = 141
      Width = 546
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edtOnTextChange
    end
    object edtOutputFileStates: TEdit
      Left = 112
      Top = 168
      Width = 546
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = edtOnTextChange
    end
  end
  object btnGenerate: TButton
    Left = 687
    Top = 123
    Width = 90
    Height = 73
    Anchors = [akTop, akRight]
    Caption = 'Generate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    OnClick = btnGenerateClick
  end
  object Button1: TButton
    Left = 696
    Top = 16
    Width = 25
    Height = 25
    Caption = '1'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 720
    Top = 16
    Width = 25
    Height = 25
    Caption = '2'
    TabOrder = 4
    OnClick = Button2Click
  end
  object OpenTxtDlg: TOpenTextFileDialog
    Filter = 'Pascal files (*.pas)|*.PAS|Any file (*.*)|*.*'
    Left = 552
    Top = 336
  end
end

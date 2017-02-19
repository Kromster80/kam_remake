object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pascal To Wiki Parser'
  ClientHeight = 648
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
  OnCreate = FormCreate
  DesignSize = (
    785
    648)
  PixelsPerInch = 96
  TextHeight = 13
  object txtParserOutput: TMemo
    Left = 0
    Top = 260
    Width = 785
    Height = 388
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
    Height = 246
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Settings '
    TabOrder = 1
    DesignSize = (
      673
      246)
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
      Left = 12
      Top = 100
      Width = 41
      Height = 13
      Caption = 'Utils file:'
    end
    object Label4: TLabel
      Left = 11
      Top = 137
      Width = 93
      Height = 13
      Caption = 'Output file Actions:'
    end
    object Label5: TLabel
      Left = 11
      Top = 164
      Width = 91
      Height = 13
      Caption = 'Output file Events:'
    end
    object Label6: TLabel
      Left = 11
      Top = 191
      Width = 89
      Height = 13
      Caption = 'Output file States:'
    end
    object Label7: TLabel
      Left = 12
      Top = 218
      Width = 78
      Height = 13
      Caption = 'Output file Utils:'
    end
    object Label8: TLabel
      Left = 12
      Top = 73
      Width = 54
      Height = 13
      Caption = 'Events file:'
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
      Top = 134
      Width = 546
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edtOnTextChange
    end
    object edtOutputFileEvents: TEdit
      Left = 112
      Top = 161
      Width = 546
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = edtOnTextChange
    end
    object edtOutputFileStates: TEdit
      Left = 112
      Top = 188
      Width = 546
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      OnChange = edtOnTextChange
    end
    object edtOutputFileUtils: TEdit
      Left = 112
      Top = 215
      Width = 546
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 7
      OnChange = edtOnTextChange
    end
    object edtUtilsFile: TEdit
      Left = 72
      Top = 97
      Width = 586
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edtOnTextChange
    end
  end
  object Button1: TButton
    Left = 696
    Top = 16
    Width = 25
    Height = 25
    Caption = '1'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 720
    Top = 16
    Width = 25
    Height = 25
    Caption = '2'
    TabOrder = 3
    OnClick = Button2Click
  end
  object btnGenerate: TButton
    Left = 687
    Top = 171
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
    TabOrder = 4
    OnClick = btnGenerateClick
  end
end

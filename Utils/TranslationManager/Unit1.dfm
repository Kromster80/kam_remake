object Form1: TForm1
  Left = 40
  Top = 87
  Caption = 'KaM Remake Translation Manager'
  ClientHeight = 441
  ClientWidth = 769
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  Scaled = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    769
    441)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 464
    Top = 8
    Width = 89
    Height = 16
    Caption = 'Constant name'
  end
  object Label2: TLabel
    Left = 336
    Top = 336
    Width = 85
    Height = 16
    Caption = 'Show Missing:'
  end
  object LabelIncludeSameAsEnglish: TLabel
    Left = 357
    Top = 384
    Width = 97
    Height = 49
    AutoSize = False
    Caption = 'Include strings that are the same in English'
    Enabled = False
    WordWrap = True
    OnClick = LabelIncludeSameAsEnglishClick
  end
  object Label3: TLabel
    Left = 264
    Top = 24
    Width = 34
    Height = 16
    Caption = 'Count'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 48
    Width = 321
    Height = 385
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object EditConstName: TEdit
    Left = 464
    Top = 24
    Width = 297
    Height = 24
    TabOrder = 1
    OnChange = EditConstNameChange
  end
  object btnSortByIndex: TButton
    Left = 336
    Top = 240
    Width = 121
    Height = 25
    Caption = 'Sort by Index'
    TabOrder = 2
    OnClick = btnSortByIndexClick
  end
  object btnLoad: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 33
    Caption = 'Load'
    TabOrder = 3
    OnClick = btnLoadClick
  end
  object btnSave: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 33
    Caption = 'Save'
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object btnInsert: TButton
    Left = 336
    Top = 48
    Width = 121
    Height = 25
    Caption = 'Insert New'
    TabOrder = 5
    OnClick = btnInsertClick
  end
  object ScrollBox1: TScrollBox
    Left = 464
    Top = 48
    Width = 297
    Height = 385
    HorzScrollBar.Visible = False
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 6
  end
  object btnDelete: TButton
    Left = 336
    Top = 112
    Width = 121
    Height = 25
    Caption = 'Delete'
    TabOrder = 7
    OnClick = btnDeleteClick
  end
  object btnInsertSeparator: TButton
    Left = 336
    Top = 80
    Width = 121
    Height = 25
    Caption = 'Insert Separator'
    TabOrder = 8
    OnClick = btnInsertSeparatorClick
  end
  object btnMoveUp: TButton
    Left = 336
    Top = 176
    Width = 121
    Height = 25
    Caption = 'Move Up'
    TabOrder = 9
    OnClick = btnMoveUpClick
  end
  object btnMoveDown: TButton
    Left = 336
    Top = 200
    Width = 121
    Height = 25
    Caption = 'Move Down'
    TabOrder = 10
    OnClick = btnMoveDownClick
  end
  object cbShowMissing: TComboBox
    Left = 336
    Top = 352
    Width = 121
    Height = 24
    Style = csDropDownList
    DropDownCount = 16
    TabOrder = 11
    OnChange = cbShowMissingChange
  end
  object cbIncludeSameAsEnglish: TCheckBox
    Left = 336
    Top = 384
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 12
    OnClick = cbIncludeSameAsEnglishClick
  end
  object btnSortByName: TButton
    Left = 336
    Top = 264
    Width = 121
    Height = 25
    Caption = 'Sort by Name'
    TabOrder = 13
    OnClick = btnSortByNameClick
  end
  object btnCompactIndexes: TButton
    Left = 336
    Top = 288
    Width = 121
    Height = 25
    Caption = 'Compact Indexes'
    TabOrder = 14
    OnClick = btnCompactIndexesClick
  end
end

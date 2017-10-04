object Form1: TForm1
  Left = 230
  Top = 140
  Caption = 'KaM Remake Translation Manager'
  ClientHeight = 643
  ClientWidth = 922
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    922
    643)
  PixelsPerInch = 96
  TextHeight = 16
  object lblConstName: TLabel
    Left = 495
    Top = 8
    Width = 89
    Height = 16
    Caption = 'Constant name'
  end
  object lbFolders: TListBox
    Left = 8
    Top = 71
    Width = 321
    Height = 211
    TabOrder = 7
    OnClick = lbFoldersClick
  end
  object ListBox1: TListBox
    Left = 8
    Top = 288
    Width = 321
    Height = 338
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    OnClick = ListBox1Click
    OnKeyPress = ListBox1KeyPress
  end
  object btnInsert: TButton
    Left = 335
    Top = 447
    Width = 154
    Height = 25
    Caption = 'Insert New'
    TabOrder = 1
    OnClick = btnInsertClick
  end
  object ScrollBox1: TScrollBox
    Left = 495
    Top = 24
    Width = 419
    Height = 560
    HorzScrollBar.Visible = False
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object btnInsertSeparator: TButton
    Left = 335
    Top = 472
    Width = 154
    Height = 25
    Caption = 'Insert Separator'
    TabOrder = 4
    OnClick = btnInsertSeparatorClick
  end
  object btnMoveUp: TButton
    Left = 333
    Top = 569
    Width = 154
    Height = 25
    Caption = 'Move Up'
    TabOrder = 5
    OnClick = btnMoveUpClick
  end
  object btnMoveDown: TButton
    Left = 333
    Top = 594
    Width = 154
    Height = 25
    Caption = 'Move Down'
    TabOrder = 6
    OnClick = btnMoveDownClick
  end
  object btnCopy: TButton
    Left = 493
    Top = 593
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Copy all strings'
    TabOrder = 8
    OnClick = btnCopyClick
  end
  object btnPaste: TButton
    Left = 621
    Top = 593
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Paste all strings'
    Enabled = False
    TabOrder = 9
    OnClick = btnPasteClick
  end
  object btnRename: TButton
    Left = 333
    Top = 508
    Width = 154
    Height = 25
    Caption = 'Rename'
    TabOrder = 10
    OnClick = btnRenameClick
  end
  object clbShowLang: TCheckListBox
    Left = 336
    Top = 8
    Width = 153
    Height = 169
    OnClickCheck = clbShowLangClickCheck
    AutoComplete = False
    Columns = 3
    TabOrder = 11
  end
  object btnDelete: TButton
    Left = 333
    Top = 533
    Width = 154
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 624
    Width = 922
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object clbFolders: TCheckListBox
    Left = 8
    Top = 8
    Width = 321
    Height = 57
    OnClickCheck = clbFoldersClickCheck
    AutoComplete = False
    Columns = 2
    Items.Strings = (
      'Game'
      'Tutorials'
      'Campaigns'
      'Maps'
      'MapsMP')
    TabOrder = 13
  end
  object FilterGroupBox: TGroupBox
    Left = 335
    Top = 183
    Width = 154
    Height = 226
    Caption = 'Filter'
    TabOrder = 14
    object Label4: TLabel
      Left = 5
      Top = 66
      Width = 82
      Height = 16
      Caption = 'Text contains:'
    end
    object Label1: TLabel
      Left = 5
      Top = 118
      Width = 127
      Height = 16
      Caption = 'Label name contains:'
    end
    object Label3: TLabel
      Left = 5
      Top = 170
      Width = 53
      Height = 16
      Caption = 'Label ID:'
    end
    object cbShowMis: TCheckBox
      Left = 5
      Top = 20
      Width = 105
      Height = 17
      Caption = 'Only missing'
      TabOrder = 0
      OnClick = cbShowMisClick
    end
    object cbShowDup: TCheckBox
      Left = 5
      Top = 43
      Width = 113
      Height = 17
      Caption = 'Only duplicate'
      TabOrder = 1
      OnClick = cbShowMisClick
    end
    object edTextFilter: TEdit
      Left = 5
      Top = 88
      Width = 144
      Height = 24
      TabOrder = 2
      OnChange = FilterChanged
    end
    object edLabelName: TEdit
      Left = 5
      Top = 140
      Width = 144
      Height = 24
      TabOrder = 3
      OnChange = FilterChanged
    end
    object edLabelId: TEdit
      Left = 5
      Top = 192
      Width = 144
      Height = 24
      TabOrder = 4
      OnChange = FilterChanged
    end
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 48
    object File1: TMenuItem
      Caption = 'File'
      object mnuSave: TMenuItem
        Caption = 'Save'
        OnClick = btnSaveClick
      end
      object mmSaveAllZIP: TMenuItem
        Caption = 'Export all translations as ZIP'
        OnClick = mmSaveAllZIPClick
      end
      object mnuExit: TMenuItem
        Caption = 'Exit'
        OnClick = mnuExitClick
      end
    end
    object Edit2: TMenuItem
      Caption = 'Edit'
      object mnuSortByIndex: TMenuItem
        Caption = 'Sort consts by index'
        OnClick = btnSortByIndexClick
      end
      object mnuSortByName: TMenuItem
        Caption = 'Sort consts by name'
        OnClick = btnSortByNameClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuCompactIndexes: TMenuItem
        Caption = 'Compact indexes'
        OnClick = btnCompactIndexesClick
      end
      object mnuListUnused: TMenuItem
        Caption = 'List unused'
        OnClick = btnUnusedClick
      end
    end
  end
  object sdExportZIP: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'ZIP file|*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 448
    Top = 192
  end
end

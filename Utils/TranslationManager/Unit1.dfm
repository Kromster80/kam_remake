object Form1: TForm1
  Left = 40
  Top = 87
  Caption = 'KaM Remake Translation Manager'
  ClientHeight = 569
  ClientWidth = 825
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDesktopCenter
  Scaled = False
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    825
    569)
  PixelsPerInch = 96
  TextHeight = 16
  object lblConstName: TLabel
    Left = 488
    Top = 8
    Width = 89
    Height = 16
    Caption = 'Constant name'
  end
  object Label3: TLabel
    Left = 336
    Top = 280
    Width = 34
    Height = 16
    Caption = 'Count'
  end
  object Label4: TLabel
    Left = 336
    Top = 224
    Width = 32
    Height = 16
    Caption = 'Filter:'
  end
  object lbFolders: TListBox
    Left = 8
    Top = 8
    Width = 321
    Height = 249
    TabOrder = 8
    OnClick = lbFoldersClick
  end
  object ListBox1: TListBox
    Left = 8
    Top = 264
    Width = 321
    Height = 297
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object btnInsert: TButton
    Left = 336
    Top = 304
    Width = 145
    Height = 25
    Caption = 'Insert New'
    TabOrder = 1
    OnClick = btnInsertClick
  end
  object ScrollBox1: TScrollBox
    Left = 488
    Top = 24
    Width = 329
    Height = 505
    HorzScrollBar.Visible = False
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object btnInsertSeparator: TButton
    Left = 336
    Top = 328
    Width = 145
    Height = 25
    Caption = 'Insert Separator'
    TabOrder = 4
    OnClick = btnInsertSeparatorClick
  end
  object btnMoveUp: TButton
    Left = 336
    Top = 432
    Width = 145
    Height = 25
    Caption = 'Move Up'
    TabOrder = 5
    OnClick = btnMoveUpClick
  end
  object btnMoveDown: TButton
    Left = 336
    Top = 456
    Width = 145
    Height = 25
    Caption = 'Move Down'
    TabOrder = 6
    OnClick = btnMoveDownClick
  end
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Export TSK'
    TabOrder = 7
    Visible = False
    OnClick = Button1Click
  end
  object btnCopy: TButton
    Left = 488
    Top = 536
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Copy all strings'
    TabOrder = 9
    OnClick = btnCopyClick
  end
  object btnPaste: TButton
    Left = 616
    Top = 536
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Paste all strings'
    Enabled = False
    TabOrder = 10
    OnClick = btnPasteClick
  end
  object Edit1: TEdit
    Left = 336
    Top = 240
    Width = 145
    Height = 24
    TabOrder = 11
    OnChange = Edit1Change
  end
  object btnRename: TButton
    Left = 336
    Top = 368
    Width = 145
    Height = 25
    Caption = 'Rename'
    TabOrder = 12
    OnClick = btnRenameClick
  end
  object clbShowLang: TCheckListBox
    Left = 336
    Top = 8
    Width = 145
    Height = 169
    OnClickCheck = clbShowLangClickCheck
    AutoComplete = False
    Columns = 3
    TabOrder = 13
  end
  object cbShowMis: TCheckBox
    Left = 336
    Top = 184
    Width = 105
    Height = 17
    Caption = 'Show missing'
    TabOrder = 14
    OnClick = cbShowMisClick
  end
  object cbShowDup: TCheckBox
    Left = 336
    Top = 200
    Width = 113
    Height = 17
    Caption = 'Show duplicate'
    TabOrder = 15
    OnClick = cbShowMisClick
  end
  object btnDelete: TButton
    Left = 336
    Top = 392
    Width = 145
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
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
end

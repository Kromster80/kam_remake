object Form1: TForm1
  Left = 226
  Top = 154
  Width = 1275
  Height = 829
  Caption = 'KaM Remake Translation Manager'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  DesignSize = (
    1257
    782)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 472
    Top = 8
    Width = 89
    Height = 16
    Caption = 'Constant name'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 48
    Width = 329
    Height = 727
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 16
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object EditConstName: TEdit
    Left = 472
    Top = 24
    Width = 297
    Height = 24
    TabOrder = 1
    OnChange = EditConstNameChange
  end
  object btnReorderList: TButton
    Left = 344
    Top = 240
    Width = 121
    Height = 25
    Caption = 'Reorder IDs'
    TabOrder = 2
    OnClick = btnReorderListClick
  end
  object btnLoad: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 3
    OnClick = btnLoadClick
  end
  object btnSave: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object btnInsert: TButton
    Left = 344
    Top = 48
    Width = 121
    Height = 25
    Caption = 'Insert New'
    TabOrder = 5
    OnClick = btnInsertClick
  end
  object ScrollBox1: TScrollBox
    Left = 472
    Top = 48
    Width = 777
    Height = 729
    HorzScrollBar.Visible = False
    VertScrollBar.Smooth = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 6
  end
  object btnDelete: TButton
    Left = 344
    Top = 112
    Width = 121
    Height = 25
    Caption = 'Delete'
    TabOrder = 7
    OnClick = btnDeleteClick
  end
  object btnInsertSeparator: TButton
    Left = 344
    Top = 80
    Width = 121
    Height = 25
    Caption = 'Insert Separator'
    TabOrder = 8
    OnClick = btnInsertSeparatorClick
  end
  object btnMoveUp: TButton
    Left = 344
    Top = 176
    Width = 121
    Height = 25
    Caption = 'Move Up'
    TabOrder = 9
    OnClick = btnMoveUpClick
  end
  object btnMoveDown: TButton
    Left = 344
    Top = 208
    Width = 121
    Height = 25
    Caption = 'Move Down'
    TabOrder = 10
    OnClick = btnMoveDownClick
  end
end

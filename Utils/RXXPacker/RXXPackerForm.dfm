object RXXForm1: TRXXForm1
  Left = 72
  Top = 90
  Caption = 'RXX Packer'
  ClientHeight = 489
  ClientWidth = 689
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
  object Image1: TImage
    Left = 272
    Top = 104
    Width = 209
    Height = 169
    Proportional = True
    Stretch = True
    Transparent = True
  end
  object btnPackRXX: TButton
    Left = 496
    Top = 432
    Width = 185
    Height = 49
    Caption = 'Pack to RXX File'
    TabOrder = 0
    OnClick = btnPackRXXClick
  end
  object ListBox1: TListBox
    Left = 496
    Top = 216
    Width = 185
    Height = 201
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
  object btnAdd: TButton
    Left = 272
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Add ...'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnSaveRXX: TButton
    Left = 184
    Top = 456
    Width = 81
    Height = 25
    Caption = 'Save RXX ...'
    TabOrder = 3
    OnClick = btnSaveRXXClick
  end
  object lbSpritesList: TListBox
    Left = 8
    Top = 8
    Width = 257
    Height = 441
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 4
    OnClick = lbSpritesListClick
  end
  object btnLoadRXX: TButton
    Left = 8
    Top = 456
    Width = 81
    Height = 25
    Caption = 'Load RXX ...'
    TabOrder = 5
    OnClick = btnLoadRXXClick
  end
  object btnDelete: TButton
    Left = 272
    Top = 72
    Width = 81
    Height = 25
    Caption = 'Delete'
    TabOrder = 6
    OnClick = btnDeleteClick
  end
  object btnReplace: TButton
    Left = 272
    Top = 40
    Width = 81
    Height = 25
    Caption = 'Replace ...'
    TabOrder = 7
    OnClick = btnReplaceClick
  end
  object OpenDialog1: TOpenDialog
    OnShow = OpenDialog1Show
    Filter = 'Supported images (*.bmp;*.png)|*.bmp;*.png'
    Options = [ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select images'
    Left = 112
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    OnShow = SaveDialog1Show
    DefaultExt = '*.rxx'
    Filter = 'RXX packages (*.rxx)|*.rxx'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 176
    Top = 8
  end
end

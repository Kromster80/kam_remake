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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 37
    Height = 13
    Caption = 'Label1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnPackRXX: TButton
    Left = 552
    Top = 456
    Width = 129
    Height = 25
    Caption = 'Pack to RXX File'
    TabOrder = 0
    OnClick = btnPackRXXClick
  end
  object ListBox1: TListBox
    Left = 552
    Top = 264
    Width = 129
    Height = 185
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
  object btnAdd: TButton
    Left = 224
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Add Image ...'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnSaveRXX: TButton
    Left = 136
    Top = 456
    Width = 81
    Height = 25
    Caption = 'Save RXX ...'
    TabOrder = 3
    OnClick = btnSaveRXXClick
  end
  object lbSpritesList: TListBox
    Left = 8
    Top = 40
    Width = 209
    Height = 409
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
    Left = 488
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Delete Image'
    TabOrder = 6
    OnClick = btnDeleteClick
  end
  object btnImport: TButton
    Left = 312
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Import ...'
    TabOrder = 7
    OnClick = btnImportClick
  end
  object btnExport: TButton
    Left = 400
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Export ...'
    TabOrder = 8
    OnClick = btnExportClick
  end
  object Panel1: TPanel
    Left = 224
    Top = 40
    Width = 242
    Height = 162
    BevelOuter = bvLowered
    TabOrder = 9
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 240
      Height = 160
      Align = alClient
      Proportional = True
      Stretch = True
      Transparent = True
      ExplicitLeft = 24
      ExplicitTop = -56
      ExplicitWidth = 209
      ExplicitHeight = 169
    end
  end
  object OpenDialog1: TOpenDialog
    OnShow = OpenDialog1Show
    Filter = 'Supported images (*.bmp;*.png)|*.bmp;*.png'
    Options = [ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select images'
    Left = 32
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    OnShow = SaveDialog1Show
    DefaultExt = '*.rxx'
    Filter = 'RXX packages (*.rxx)|*.rxx'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 96
    Top = 48
  end
end

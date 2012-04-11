object RXXForm1: TRXXForm1
  Left = 72
  Top = 90
  Caption = 'RXX Packer'
  ClientHeight = 492
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
  object btnPackRXX: TButton
    Left = 392
    Top = 320
    Width = 185
    Height = 49
    Caption = 'Pack to RXX File'
    TabOrder = 0
    OnClick = btnPackRXXClick
  end
  object ListBox1: TListBox
    Left = 392
    Top = 96
    Width = 185
    Height = 201
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
  object btnAddImage: TButton
    Left = 96
    Top = 456
    Width = 81
    Height = 25
    Caption = 'Add images ...'
    TabOrder = 2
    OnClick = btnAddImageClick
  end
  object btnSaveRXX: TButton
    Left = 184
    Top = 456
    Width = 81
    Height = 25
    Caption = 'Save ...'
    TabOrder = 3
    OnClick = btnSaveRXXClick
  end
  object ListBox2: TListBox
    Left = 8
    Top = 8
    Width = 257
    Height = 441
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 4
  end
  object btnLoadRXX: TButton
    Left = 8
    Top = 456
    Width = 81
    Height = 25
    Caption = 'Load ...'
    TabOrder = 5
    OnClick = btnLoadRXXClick
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
  object dlgOpenRXX: TOpenDialog
    OnShow = OpenDialog1Show
    DefaultExt = '*.rxx'
    Filter = 'RXX packages (*.rxx)|*.rxx'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select images'
    Left = 24
    Top = 8
  end
end

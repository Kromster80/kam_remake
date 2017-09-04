object OptForm: TOptForm
  Left = 370
  Top = 179
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 363
  ClientWidth = 278
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 29
    Height = 14
    Caption = 'Rows'
  end
  object Label2: TLabel
    Left = 8
    Top = 43
    Width = 21
    Height = 14
    Caption = 'Cols'
  end
  object Label3: TLabel
    Left = 8
    Top = 74
    Width = 36
    Height = 14
    Caption = 'LHeight'
  end
  object Label4: TLabel
    Left = 128
    Top = 12
    Width = 50
    Height = 14
    Caption = 'Line Zoom'
  end
  object Label5: TLabel
    Left = 128
    Top = 44
    Width = 53
    Height = 14
    Caption = 'Char Zoom'
  end
  object AutoCRCheckBox: TCheckBox
    Left = 8
    Top = 104
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'AutoCR'
    TabOrder = 5
  end
  object AutoLFCheckBox: TCheckBox
    Left = 8
    Top = 136
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'AutoLF'
    TabOrder = 6
  end
  object LocalEchoCheckBox: TCheckBox
    Left = 8
    Top = 168
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Local Echo'
    TabOrder = 7
  end
  object MonoChromeCheckBox: TCheckBox
    Left = 8
    Top = 200
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Monochrome'
    TabOrder = 8
  end
  object RowsEdit: TEdit
    Left = 64
    Top = 8
    Width = 49
    Height = 22
    TabOrder = 0
    Text = 'RowsEdit'
  end
  object ColsEdit: TEdit
    Left = 64
    Top = 40
    Width = 49
    Height = 22
    TabOrder = 1
    Text = 'ColsEdit'
  end
  object OkButton: TButton
    Left = 196
    Top = 308
    Width = 57
    Height = 25
    Caption = '&Ok'
    Default = True
    TabOrder = 20
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 196
    Top = 276
    Width = 57
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 19
    OnClick = CancelButtonClick
  end
  object LaboButton: TButton
    Left = 124
    Top = 136
    Width = 57
    Height = 25
    Caption = '&Labo'
    TabOrder = 13
    OnClick = LaboButtonClick
  end
  object RDVButton: TButton
    Left = 124
    Top = 168
    Width = 57
    Height = 25
    Caption = '&RDV'
    TabOrder = 14
    OnClick = RDVButtonClick
  end
  object USUSButton: TButton
    Left = 124
    Top = 200
    Width = 57
    Height = 25
    Caption = '&USUS'
    TabOrder = 15
    OnClick = USUSButtonClick
  end
  object XlatCheckBox: TCheckBox
    Left = 8
    Top = 232
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'OEM charset'
    TabOrder = 9
  end
  object FontButton: TButton
    Left = 192
    Top = 104
    Width = 57
    Height = 25
    Caption = '&Font'
    TabOrder = 16
    OnClick = FontButtonClick
  end
  object LineHeightEdit: TEdit
    Left = 64
    Top = 72
    Width = 49
    Height = 22
    TabOrder = 2
    Text = 'LineHeightEdit'
  end
  object NamesButton: TButton
    Left = 192
    Top = 136
    Width = 57
    Height = 25
    Caption = '&Names'
    TabOrder = 17
    OnClick = NamesButtonClick
  end
  object UpperLockCheckBox: TCheckBox
    Left = 8
    Top = 264
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Upper Case'
    TabOrder = 10
  end
  object A11Button: TButton
    Left = 124
    Top = 104
    Width = 57
    Height = 25
    Caption = '&A11'
    TabOrder = 12
    OnClick = A11ButtonClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 288
    Width = 177
    Height = 49
    Caption = 'Function &Keys'
    TabOrder = 18
    object FKeys1RadioButton: TRadioButton
      Left = 8
      Top = 24
      Width = 49
      Height = 17
      Caption = 'SCO'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object FKeys2RadioButton: TRadioButton
      Left = 64
      Top = 24
      Width = 65
      Height = 17
      Caption = 'VT100'
      TabOrder = 1
    end
    object FKeys3RadioButton: TRadioButton
      Left = 128
      Top = 24
      Width = 41
      Height = 17
      Caption = 'A11'
      TabOrder = 2
    end
  end
  object LineZoomEdit: TEdit
    Left = 204
    Top = 8
    Width = 45
    Height = 22
    TabOrder = 3
    Text = 'LineZoomEdit'
  end
  object CharZoomEdit: TEdit
    Left = 204
    Top = 40
    Width = 45
    Height = 22
    TabOrder = 4
    Text = 'CharZoomEdit'
  end
  object GraphicDrawCheckBox: TCheckBox
    Left = 136
    Top = 232
    Width = 109
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Graphic Draw'
    TabOrder = 11
  end
  object FontDialog1: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Options = [fdFixedPitchOnly]
    Left = 204
    Top = 175
  end
end

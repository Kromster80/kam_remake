object Form1: TForm1
  Left = 192
  Top = 148
  Width = 1305
  Height = 675
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 40
    Top = 80
    Width = 41
    Height = 16
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 496
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Go!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 40
    Top = 96
    Width = 697
    Height = 521
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 40
    Top = 32
    Width = 457
    Height = 24
    TabOrder = 2
    Text = 'http://lewin.hodgman.id.au/'
  end
end

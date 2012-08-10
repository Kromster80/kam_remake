object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 273
  ClientWidth = 417
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
    Left = 8
    Top = 8
    Width = 257
    Height = 257
    OnMouseUp = Image1MouseUp
  end
  object Memo1: TMemo
    Left = 272
    Top = 8
    Width = 137
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
end

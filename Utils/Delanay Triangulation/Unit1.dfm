object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 561
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    721
    561)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 561
    Height = 545
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseUp = Image1MouseUp
    ExplicitWidth = 257
    ExplicitHeight = 257
  end
  object Memo1: TMemo
    Left = 576
    Top = 8
    Width = 137
    Height = 89
    Anchors = [akTop, akRight]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    ExplicitLeft = 272
  end
end

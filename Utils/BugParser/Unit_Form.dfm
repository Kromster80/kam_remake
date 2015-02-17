object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Bug Parser'
  ClientHeight = 337
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    633
    337)
  PixelsPerInch = 96
  TextHeight = 16
  object Button1: TButton
    Left = 320
    Top = 304
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Scan'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 8
    Width = 617
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 1
    DrawingStyle = gdsGradient
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goRowSelect, goThumbTracking, goFixedRowClick]
    TabOrder = 1
    OnFixedCellClick = StringGrid1FixedCellClick
  end
  object Edit1: TEdit
    Left = 8
    Top = 304
    Width = 305
    Height = 24
    Anchors = [akLeft, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = 'C:\Users\Krom\Desktop\r6720\'
  end
  object Button2: TButton
    Left = 400
    Top = 304
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export links'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button2Click
  end
end

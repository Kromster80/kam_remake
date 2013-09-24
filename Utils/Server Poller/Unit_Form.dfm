object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Server Poller r5503'
  ClientHeight = 585
  ClientWidth = 897
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    897
    585)
  PixelsPerInch = 96
  TextHeight = 18
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 96
    Height = 18
    Caption = 'Master-server:'
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 32
    Width = 881
    Height = 353
    Anchors = [akLeft, akTop, akRight, akBottom]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goThumbTracking]
    TabOrder = 0
  end
  object Button2: TButton
    Left = 128
    Top = 392
    Width = 129
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'Take snapshot'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo2: TMemo
    Left = 264
    Top = 392
    Width = 625
    Height = 185
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    TabOrder = 2
    WordWrap = False
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 400
    Width = 105
    Height = 17
    Caption = 'Auto-refresh'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 24
    Top = 336
  end
end

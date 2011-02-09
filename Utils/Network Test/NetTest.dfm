object frmNetTest: TfrmNetTest
  Left = 268
  Top = 170
  Width = 384
  Height = 267
  Caption = 'KM_Network Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object lblLastPacket: TLabel
    Left = 16
    Top = 168
    Width = 33
    Height = 16
    Caption = 'None'
  end
  object Label2: TLabel
    Left = 16
    Top = 72
    Width = 35
    Height = 16
    Caption = 'Send:'
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 43
    Height = 16
    Caption = 'Server:'
  end
  object Label4: TLabel
    Left = 16
    Top = 144
    Width = 203
    Height = 24
    Caption = 'Last packet recieved:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edtSend: TEdit
    Left = 16
    Top = 88
    Width = 225
    Height = 24
    TabOrder = 0
    Text = 'Enter a message...'
  end
  object Button1: TButton
    Left = 248
    Top = 88
    Width = 81
    Height = 25
    Caption = 'Send'
    TabOrder = 1
    OnClick = Button1Click
  end
  object edtServer: TEdit
    Left = 16
    Top = 32
    Width = 225
    Height = 24
    TabOrder = 2
    Text = '127.0.0.1'
  end
end

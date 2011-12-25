object frmNetTest: TfrmNetTest
  Left = 268
  Top = 170
  BorderStyle = bsSingle
  Caption = 'KM_Network Test'
  ClientHeight = 161
  ClientWidth = 273
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblLastPacket: TLabel
    Left = 13
    Top = 137
    Width = 26
    Height = 13
    Caption = 'None'
  end
  object Label2: TLabel
    Left = 13
    Top = 59
    Width = 28
    Height = 13
    Caption = 'Send:'
  end
  object Label3: TLabel
    Left = 13
    Top = 13
    Width = 34
    Height = 13
    Caption = 'Server:'
  end
  object Label4: TLabel
    Left = 13
    Top = 117
    Width = 163
    Height = 19
    Caption = 'Last packet recieved:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edtSend: TEdit
    Left = 13
    Top = 72
    Width = 183
    Height = 24
    TabOrder = 0
    Text = 'Enter a message...'
  end
  object Button1: TButton
    Left = 202
    Top = 72
    Width = 65
    Height = 20
    Caption = 'Send'
    TabOrder = 1
    OnClick = Button1Click
  end
  object edtServer: TEdit
    Left = 13
    Top = 26
    Width = 183
    Height = 24
    TabOrder = 2
    Text = '127.0.0.1'
  end
end

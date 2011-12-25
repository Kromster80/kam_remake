object frmNetTest: TfrmNetTest
  Left = 268
  Top = 170
  BorderStyle = bsSingle
  Caption = 'KM_Network Test'
  ClientHeight = 385
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
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
  object Label1: TLabel
    Left = 13
    Top = 109
    Width = 21
    Height = 13
    Caption = 'Log:'
  end
  object edtSend: TEdit
    Left = 13
    Top = 72
    Width = 183
    Height = 21
    TabOrder = 0
    Text = 'Enter a message...'
  end
  object btnSend: TButton
    Left = 202
    Top = 72
    Width = 65
    Height = 20
    Caption = 'Send'
    Enabled = False
    TabOrder = 1
    OnClick = btnSendClick
  end
  object edtServer: TEdit
    Left = 13
    Top = 26
    Width = 183
    Height = 21
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object btnHost: TButton
    Left = 204
    Top = 12
    Width = 63
    Height = 19
    Caption = 'Host'
    TabOrder = 3
    OnClick = btnHostClick
  end
  object btnJoin: TButton
    Left = 204
    Top = 36
    Width = 63
    Height = 19
    Caption = 'Join'
    TabOrder = 4
    OnClick = btnJoinClick
  end
  object Memo1: TMemo
    Left = 12
    Top = 126
    Width = 253
    Height = 247
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object btnStop: TButton
    Left = 270
    Top = 12
    Width = 63
    Height = 19
    Caption = 'Stop'
    Enabled = False
    TabOrder = 6
    OnClick = btnStopClick
  end
  object btnQuit: TButton
    Left = 270
    Top = 36
    Width = 63
    Height = 19
    Caption = 'Quit'
    Enabled = False
    TabOrder = 7
    OnClick = btnQuitClick
  end
end

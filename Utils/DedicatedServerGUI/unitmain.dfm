object FormMain: TFormMain
  Left = 431
  Top = 181
  Width = 833
  Height = 554
  Anchors = [akTop]
  Caption = 'KaM Dedicated Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    815
    507)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 606
    Top = 16
    Width = 81
    Height = 16
    Anchors = [akTop, akRight]
    Caption = 'Server status:'
    Color = clBtnFace
    ParentColor = False
  end
  object Splitter1: TSplitter
    Left = 603
    Top = 152
    Width = 9
    Height = 355
    Align = alCustom
  end
  object StartStopButton: TButton
    Left = 624
    Top = 40
    Width = 180
    Height = 48
    Anchors = [akTop, akRight]
    Caption = 'Server is OFFLINE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = StartStopButtonClick
  end
  object Panel1: TPanel
    Left = 611
    Top = 152
    Width = 204
    Height = 355
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object PlayersList: TListBox
      Left = 1
      Top = 1
      Width = 202
      Height = 353
      Align = alClient
      ItemHeight = 16
      Items.Strings = (
        'Player1'
        'Player2'
        'Player3'
        '...')
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 1
    Top = 152
    Width = 602
    Height = 325
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Panel2'
    TabOrder = 1
    object LogsMemo: TMemo
      Left = 1
      Top = 1
      Width = 600
      Height = 323
      TabStop = False
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object Edit1: TEdit
    Left = 1
    Top = 477
    Width = 530
    Height = 24
    TabStop = False
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
  end
  object SendCmdButton: TButton
    Left = 531
    Top = 482
    Width = 72
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'Send'
    TabOrder = 2
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 616
    Height = 152
    ActivePage = Basic
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    object Basic: TTabSheet
      Caption = 'Basic'
      DesignSize = (
        608
        121)
      object Label9: TLabel
        Left = 8
        Top = 64
        Width = 148
        Height = 16
        Caption = 'Server welcome message'
        Color = clBtnFace
        ParentColor = False
      end
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 74
        Height = 16
        Caption = 'Server name'
        Color = clBtnFace
        ParentColor = False
      end
      object Label10: TLabel
        Left = 348
        Top = 8
        Width = 66
        Height = 16
        Caption = 'Max Rooms'
        Color = clBtnFace
        ParentColor = False
      end
			object Label12: TLabel
        Left = 430
        Top = 64
        Width = 128
        Height = 16
        Caption = 'Packets Accumulating Delay:'
        Color = clBtnFace
        ParentColor = False
      end
      object cServerWelcomeMessage: TEdit
        Tag = 1
        Left = 8
        Top = 88
        Width = 530
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = ControlChange
      end
			object cServerPacketsAccDelay: TEdit
        Tag = 1
        Left = 550
        Top = 88
        Width = 40
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = ControlChange
      end
      object cServerName: TEdit
        Tag = 1
        Left = 8
        Top = 32
        Width = 308
        Height = 24
        TabOrder = 1
        OnChange = ControlChange
      end
      object cMaxRooms: TSpinEdit
        Left = 348
        Top = 32
        Width = 112
        Height = 26
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 10
        OnChange = ControlChange
      end
      object cAnnounceServer: TCheckBox
        Left = 484
        Top = 35
        Width = 135
        Height = 24
        Caption = 'Announce Server'
        TabOrder = 3
      end
    end
    object Advanced: TTabSheet
      Caption = 'Advanced'
      object Label4: TLabel
        Left = 10
        Top = 64
        Width = 98
        Height = 16
        Caption = 'Autokick Timeout'
        Color = clBtnFace
        ParentColor = False
      end
      object Label5: TLabel
        Left = 204
        Top = 64
        Width = 70
        Height = 16
        Caption = 'Ping interval'
        Color = clBtnFace
        ParentColor = False
      end
      object Label7: TLabel
        Left = 10
        Top = 8
        Width = 128
        Height = 16
        Caption = 'Master server address'
        Color = clBtnFace
        ParentColor = False
      end
      object Label6: TLabel
        Left = 444
        Top = 8
        Width = 144
        Height = 16
        Caption = 'Master announce interval'
        Color = clBtnFace
        ParentColor = False
      end
      object Label8: TLabel
        Left = 404
        Top = 64
        Width = 91
        Height = 16
        Caption = 'HTML status file'
        Color = clBtnFace
        ParentColor = False
      end
      object Label11: TLabel
        Left = 324
        Top = 8
        Width = 65
        Height = 16
        Caption = 'Server Port'
        Color = clBtnFace
        ParentColor = False
      end
      object cAutoKickTimeout: TSpinEdit
        Tag = 1
        Left = 10
        Top = 88
        Width = 104
        Height = 23
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 10
        OnChange = ControlChange
      end
      object cPingInterval: TSpinEdit
        Left = 204
        Top = 88
        Width = 112
        Height = 23
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 10
        OnChange = ControlChange
      end
      object cMasterServerAddress: TEdit
        Left = 12
        Top = 32
        Width = 250
        Height = 23
        TabOrder = 2
        OnChange = ControlChange
      end
      object cMasterAnnounceInterval: TSpinEdit
        Left = 444
        Top = 32
        Width = 144
        Height = 23
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 10
        OnChange = ControlChange
      end
      object cHTMLStatusFile: TEdit
        Tag = 1
        Left = 404
        Top = 88
        Width = 184
        Height = 23
        TabOrder = 4
        OnChange = ControlChange
      end
      object cServerPort: TEdit
        Left = 324
        Top = 32
        Width = 88
        Height = 23
        TabOrder = 5
        OnChange = ControlChange
      end
    end
  end
  object ButtonApply: TButton
    Left = 624
    Top = 96
    Width = 179
    Height = 33
    Anchors = [akTop, akRight]
    Caption = 'Save and update settings'
    Enabled = False
    TabOrder = 5
    OnClick = ButtonApplyClick
  end
end

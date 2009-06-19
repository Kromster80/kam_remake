object Form1: TForm1
  Left = 149
  Top = 108
  HelpType = htKeyword
  BorderStyle = bsNone
  Caption = 'q'
  ClientHeight = 245
  ClientWidth = 867
  Color = clSkyBlue
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  DesignSize = (
    867
    245)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel5: TPanel
    Left = 0
    Top = 0
    Width = 867
    Height = 225
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'Panel5'
    Color = clMaroon
    UseDockManager = False
    TabOrder = 0
    OnMouseDown = Panel1MouseDown
    OnMouseMove = Panel1MouseMove
    OnMouseUp = Panel1MouseUp
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 225
    Width = 867
    Height = 20
    Panels = <
      item
        Text = 'Map size: 176 x 176'
        Width = 110
      end
      item
        Text = 'Cursor: 46.1 47.2'
        Width = 180
      end
      item
        Text = '50.0 fps (50)'
        Width = 80
      end>
    SimplePanel = False
  end
  object GroupBox1: TGroupBox
    Left = 400
    Top = 8
    Width = 425
    Height = 73
    Caption = 
      '  Additional controls   ..   Press F11 to swap controls visibili' +
      'ty'
    TabOrder = 2
    object Image4: TImage
      Left = 10
      Top = 19
      Width = 13
      Height = 14
      AutoSize = True
      Picture.Data = {
        07544269746D617066020000424D660200000000000036000000280000000D00
        00000E000000010018000000000030020000C30E0000C30E0000000000000000
        0000FFFFFFFFFFFFFFFFFFFFFFFFBDBAB70B202BFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFF99B3C3001A37FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFDFDADBDCC4BF4257
        77001632A18888908487807F7EB9B9B9FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF
        BCB1B51A4540376F7400AF871678632E4043897E7F9E9A9A8484847B7B7BF1F1
        F100FFFFFFF6EFF2006F570059400CB49302797900644E0664595E5B5E7B8788
        837A7A6B6B6B89898900CBD5D50063552FAA8E22635B057F661B665A19363900
        8168003F3217504E81797C8180807676760097CBC31885732BB3951B3E430599
        7820968200725900534529373900453A7483827068678C8D8D004398871B615B
        1771650C746208DFA705AC89009F7E006A5736535700423E909FA0FFFFFFFFFF
        FF00A3D6C9008262008A6C02AD872C9E9C25898418B99211665C006A5700362E
        B7C6C8FFFFFFFFFFFF00F2DDE6006F5A16AB8E0E817A1FA9A7178F900067524C
        5761517674005042D5EDEAFFFFFFFFFFFF00F5EAEF299E8609716137C5C107CF
        AA24CDD710B18A0E49430027251D4C4AFFFFFFFFFFFFFFFFFF00FFFFFFFAFBFC
        004D395CAC9B10846E2BD1C7129C7D06433C154444FFFFFFFFFFFFFFFFFFFFFF
        FF00FFFFFFFFFFFF6B9B9600604D025E4F005F4E0070590055469DBCB9FFFFFF
        FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFD3CDD295B0AF289B82497073C9
        D1D5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00}
      Transparent = True
    end
    object Image3: TImage
      Left = 86
      Top = 14
      Width = 19
      Height = 21
      AutoSize = True
      Picture.Data = {
        07544269746D617022050000424D220500000000000036000000280000001300
        0000150000000100180000000000EC040000C30E0000C30E0000000000000000
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF135576000F26FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00435E000A0FDED8D6FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFD2D1D2002C4B3B494CA29693C0C0C0D0D1D1D6D7
        D7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFF9
        F5F5BEB5B6EADADA85737A002B59484C54827879A091938881815C5C5C8F8F8F
        AEAEAEEDEDEDFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF9B93954557
        5A52988E67565D05C69C0FAC8A5CA99A23534C535B5D7F7A7A7A777757575792
        9292888888FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF26514C01A1806B9C94
        039377209F8D00987A015E4F0035322D605DA59D9F7F7D7CA8A5A6CBCCCC5E5E
        5EA5A5A5FFFFFF000000FFFFFFEAEAED599F9516A4870F56500B846F0CFFCB08
        4F610C9E7932635E07957A1242411A282C215953AF9FA27977777C7C7C676767
        ACACAC000000FCF2F57596962DA68C619C960A9E7E1F2F33006F5900856E0831
        2F447A7602AF8A034C3F213D403E7B7794B7B1817A7B82828272727290909000
        0000A4ECDB009C7A167A6B6BCDB6468C8619413F2C938235C5B9001D1B0D6255
        00232304846D00554A004136163D3D868282807D7D727272A6A6A6000000FFFF
        FF00826362AA9F00C38E0E5C5322A78600B490239E89157A670F7F6502907512
        3436252226051E20016C59827F7F595556B7B8B8FFFFFF0000002B646000302D
        6982810375620D856E16CCA706E4AB0EA0A0119BA4017A5908B0965853612757
        582C5957206B658E8589E6E0E1FFFFFFFFFFFF00000000B68A1EA48D1B5D5614
        B5900A3E3C1A524A16BD9C05B48A10B59011DDB2166F68187664417976002424
        00503DB8E5DDFFFFFFFFFFFFFFFFFF000000A5C6C2006B54277F6F1ECC9F06C1
        9509EAB61D90A2166A7726938D10FFD2348C7C009F77057161009174134649FA
        FBFCFFFFFFFFFFFFFFFFFF000000D5E7E4008363022F2D0D736199FFEA21A19E
        26B4A126F7D1338585008C6A846C753394810A46401E4A49247269478C85FFFF
        FFFFFFFFFFFFFF000000BDB5BD007C6300957207C49C1D807E1D9C8B1F959526
        99BA0C9C9108BEA45B6782113B3B192D33258074007A59FFFFFFFFFFFFFFFFFF
        FFFFFF000000FFFFFF86DEC707B28D72999E0F9E9C2BA8A819E7AF33C5F50097
        7E0AF7BC215F76008B692B847A003D387C8C90FFFFFFFFFFFFFFFFFFFFFFFF00
        0000FFFFFFFFFFFF3BAC9412847257CBBA80E3CF1AE7AA179ED432EAFA24A998
        337A6C194643073536063B39B7CAC9FFFFFFFFFFFFFFFFFFFFFFFF000000FFFF
        FFFFFFFF4C8C8300553D11886B28AC93349E873FCBC52FECD000946B076C582B
        786C2A5656FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF
        FFFFFF7AAEA700321D75EAD135B19434567D0BA38A8FC0B64A8D86107361427A
        75FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFF
        FFFFBCCCCB527171006344005246007A5F007E67008A67347D73FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFADBBBAEBDEE30CBC8F5784856E9E999A99A1FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF000000}
      Transparent = True
    end
    object Label1: TLabel
      Left = 109
      Top = 19
      Width = 26
      Height = 13
      Caption = '100%'
      OnClick = Debug_ResetZoomClick
    end
    object TeamColorPicker: TShape
      Left = 290
      Top = 16
      Width = 22
      Height = 22
      OnDragDrop = TeamColorPickerDragDrop
      OnMouseUp = TeamColorPickerMouseUp
    end
    object Label2: TLabel
      Left = 85
      Top = 55
      Width = 49
      Height = 13
      Caption = 'Passability'
    end
    object Label3: TLabel
      Left = 101
      Top = 39
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Button_W: TButton
      Left = 144
      Top = 52
      Width = 25
      Height = 17
      Caption = 'W'
      TabOrder = 6
      OnClick = Button_WClick
    end
    object TBZoomControl: TTrackBar
      Left = 24
      Top = 18
      Width = 61
      Height = 17
      Max = 7
      Min = 1
      Orientation = trHorizontal
      PageSize = 1
      Frequency = 1
      Position = 4
      SelEnd = 0
      SelStart = 0
      TabOrder = 0
      ThumbLength = 14
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = Debug_ZoomChange
    end
    object CheckBox2: TCheckBox
      Left = 144
      Top = 16
      Width = 73
      Height = 17
      Caption = 'Speed x50'
      TabOrder = 2
      OnClick = CheckBox2Click
    end
    object Debug_Pause: TCheckBox
      Left = 144
      Top = 32
      Width = 49
      Height = 17
      Caption = 'Pause'
      TabOrder = 3
    end
    object Debug_PassabilityTrack: TTrackBar
      Left = 2
      Top = 54
      Width = 83
      Height = 17
      Max = 9
      Orientation = trHorizontal
      PageSize = 1
      Frequency = 1
      Position = 4
      SelEnd = 0
      SelStart = 0
      TabOrder = 1
      ThumbLength = 14
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = Debug_PassabilityTrackChange
    end
    object RGPlayer: TRadioGroup
      Left = 320
      Top = 8
      Width = 97
      Height = 61
      Caption = ' Player '
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8')
      TabOrder = 5
      OnClick = RGPlayerClick
    end
    object Button_V: TButton
      Left = 168
      Top = 52
      Width = 25
      Height = 17
      Caption = 'V'
      TabOrder = 7
      OnClick = Button_VClick
    end
    object Button_6: TButton
      Left = 192
      Top = 52
      Width = 25
      Height = 17
      Caption = '6'
      TabOrder = 8
      OnClick = Button_6Click
    end
    object Step1Frame: TButton
      Left = 196
      Top = 32
      Width = 25
      Height = 17
      Caption = '|| >'
      TabOrder = 4
      OnClick = Timer100msTimer
    end
    object Button_1: TButton
      Left = 216
      Top = 52
      Width = 25
      Height = 17
      Caption = '1'
      TabOrder = 9
      OnClick = Button_1Click
    end
    object Button_Stop: TButton
      Left = 240
      Top = 52
      Width = 25
      Height = 17
      Caption = 'Stop'
      TabOrder = 10
      OnClick = Button_StopClick
    end
    object TB_Angle: TTrackBar
      Left = 2
      Top = 36
      Width = 95
      Height = 17
      Max = 90
      Orientation = trHorizontal
      PageSize = 1
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 11
      ThumbLength = 14
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = TB_Angle_Change
    end
  end
  object MediaPlayer1: TMediaPlayer
    Left = 112
    Top = 201
    Width = 113
    Height = 20
    ColoredButtons = [btPlay, btStop, btNext, btPrev]
    EnabledButtons = [btPlay, btStop, btNext, btPrev, btStep, btBack, btRecord, btEject]
    VisibleButtons = [btPlay, btStop, btNext, btPrev]
    Visible = False
    TabOrder = 3
  end
  object CBHide: TCheckBox
    Left = 8
    Top = 8
    Width = 49
    Height = 17
    Caption = 'Show'
    Checked = True
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    State = cbChecked
    TabOrder = 4
    OnClick = CBHideClick
  end
  object OpenDialog1: TOpenDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 48
    Top = 192
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 192
    object File1: TMenuItem
      Caption = 'File'
      object OpenMapMenu: TMenuItem
        Caption = 'Open map...'
        OnClick = Open_MapClick
      end
      object OpenMissionMenu: TMenuItem
        Caption = 'Open mission...'
        OnClick = Open_MissionMenuClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = ExitClick
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object Debug_ShowWires: TMenuItem
        Caption = 'Show wires'
        OnClick = Debug_ShowWiresClick
      end
      object Debug_ShowObjects: TMenuItem
        Caption = 'Show object ID'
        OnClick = Debug_ShowObjectsClick
      end
      object Debug_PrintScreen: TMenuItem
        Caption = 'PrintScreen'
        OnClick = Debug_PrintScreenClick
      end
      object Debug_ShowOverlay: TMenuItem
        Caption = 'Show Overlay'
        OnClick = Debug_ShowOverlayClick
      end
      object Debug_ShowPanel1: TMenuItem
        Caption = 'Show Debug panel'
        OnClick = Debug_ShowPanel1Click
      end
      object Debug_ShowUnits: TMenuItem
        Caption = 'Show Units'
        OnClick = Debug_ShowUnitClick
      end
    end
    object Export1: TMenuItem
      Caption = 'Export Data'
      object Export_TreesRX: TMenuItem
        Caption = 'Trees.rx'
        OnClick = Export_TreesRXClick
      end
      object Export_HousesRX: TMenuItem
        Caption = 'Houses.rx'
        OnClick = Export_HousesRXClick
      end
      object Export_UnitsRX: TMenuItem
        Caption = 'Units.rx'
        OnClick = Export_UnitsRXClick
      end
      object Export_GUIRX: TMenuItem
        Caption = 'GUI.rx'
        OnClick = Export_GUIRXClick
      end
      object Export_GUIMainRX: TMenuItem
        Caption = 'GUI Main.rx'
        OnClick = Export_GUIMainRXClick
      end
      object Export_GUIMainHRX: TMenuItem
        Caption = 'GUI MainH.rx'
        OnClick = Export_GUIMainHRXClick
      end
      object AnimData1: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object Export_TreeAnim1: TMenuItem
        Caption = 'Tree Anim'
        OnClick = Export_TreeAnim1Click
      end
      object Export_HouseAnim1: TMenuItem
        Caption = 'House Anim'
        OnClick = Export_HouseAnim1Click
      end
      object Export_UnitAnim1: TMenuItem
        Caption = 'Unit Anim'
        OnClick = Export_UnitAnim1Click
      end
      object Other1: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object Export_Fonts1: TMenuItem
        Caption = 'Fonts'
        OnClick = Export_Fonts1Click
      end
      object Export_Text: TMenuItem
        Caption = 'Texts'
        OnClick = Export_TextClick
      end
      object Export_Sounds1: TMenuItem
        Caption = 'Sounds'
        OnClick = Export_Sounds1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Export_Deliverlists1: TMenuItem
        Caption = 'Export Deliver lists'
        OnClick = Export_Deliverlists1Click
      end
    end
    object About1: TMenuItem
      Caption = 'About..'
      OnClick = AboutClick
    end
  end
  object Timer100ms: TTimer
    Interval = 100
    OnTimer = Timer100msTimer
    Left = 80
    Top = 192
  end
end

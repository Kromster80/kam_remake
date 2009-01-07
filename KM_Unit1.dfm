object Form1: TForm1
  Left = 262
  Top = 247
  HelpType = htKeyword
  BorderStyle = bsNone
  ClientHeight = 659
  ClientWidth = 918
  Color = clSkyBlue
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  HelpFile = 'KM_Editor.hlp'
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    918
    659)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel5: TPanel
    Left = 0
    Top = 0
    Width = 918
    Height = 641
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'Panel5'
    Color = clBlack
    TabOrder = 1
    OnMouseDown = Panel1MouseDown
    OnMouseMove = Panel1MouseMove
    OnMouseUp = Panel1MouseUp
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 640
    Width = 918
    Height = 19
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
      end
      item
        Text = 'Brush: None Selected'
        Width = 140
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel_Minimap: TPanel
    Left = 10
    Top = 9
    Width = 176
    Height = 176
    BevelOuter = bvNone
    Color = clMaroon
    TabOrder = 0
    object MiniMap: TImage
      Left = 0
      Top = 0
      Width = 176
      Height = 176
      OnMouseDown = MiniMapMouseDown
      OnMouseMove = MiniMapMouseMove
      OnMouseUp = MiniMapMouseUp
    end
    object ShapeFOV: TShape
      Left = 24
      Top = 24
      Width = 65
      Height = 65
      Brush.Style = bsClear
      Enabled = False
      Pen.Color = clWhite
    end
  end
  object GroupBox1: TGroupBox
    Left = 192
    Top = 8
    Width = 145
    Height = 145
    Caption = '  Additional controls  '
    TabOrder = 3
    object Image4: TImage
      Left = 10
      Top = 70
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
      Top = 65
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
      Top = 70
      Width = 26
      Height = 13
      Caption = '100%'
      OnClick = ResetZoomClick
    end
    object Pl1: TSpeedButton
      Left = 8
      Top = 92
      Width = 22
      Height = 22
      GroupIndex = 100
      Down = True
      Caption = '1'
      Flat = True
      Spacing = 2
      Transparent = False
    end
    object Pl2: TSpeedButton
      Left = 30
      Top = 92
      Width = 22
      Height = 22
      GroupIndex = 100
      Caption = '2'
      Flat = True
    end
    object Pl3: TSpeedButton
      Left = 52
      Top = 92
      Width = 22
      Height = 22
      GroupIndex = 100
      Caption = '3'
      Flat = True
    end
    object Pl6: TSpeedButton
      Left = 52
      Top = 114
      Width = 22
      Height = 22
      GroupIndex = 100
      Caption = '6'
      Flat = True
    end
    object Pl5: TSpeedButton
      Left = 30
      Top = 114
      Width = 22
      Height = 22
      GroupIndex = 100
      Caption = '5'
      Flat = True
    end
    object Pl4: TSpeedButton
      Left = 8
      Top = 114
      Width = 22
      Height = 22
      GroupIndex = 100
      Caption = '4'
      Flat = True
    end
    object Shape267: TShape
      Left = 80
      Top = 94
      Width = 19
      Height = 19
      OnDragDrop = Shape267DragDrop
      OnMouseUp = Shape267MouseUp
    end
    object TBZoomControl: TTrackBar
      Left = 24
      Top = 69
      Width = 61
      Height = 21
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
      OnChange = ZoomChange
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Speedup x10'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Pause'
      TabOrder = 2
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Wires'
      TabOrder = 3
      OnClick = ShowWiresClick
    end
  end
  object OpenDialog1: TOpenDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 800
    Top = 600
  end
  object MainMenu1: TMainMenu
    Left = 768
    Top = 600
    object File1: TMenuItem
      Caption = 'File'
      object OpenMapMenu: TMenuItem
        Caption = 'Open map...'
        OnClick = OpenMapClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = ExitClick
      end
    end
    object Script1: TMenuItem
      Caption = 'Script'
      Enabled = False
      object OpenDAT: TMenuItem
        Caption = 'Open DAT ...'
      end
      object ExportDAT: TMenuItem
        Caption = 'Export DAT ...'
      end
      object DecodeDAT: TMenuItem
        Caption = 'Decode DAT ...'
        OnClick = DecodeDATClick
      end
    end
    object Advanced1: TMenuItem
      Caption = 'Advanced'
      object ShowWires: TMenuItem
        Caption = 'Show wires'
        OnClick = ShowWiresClick
      end
      object ShowObjects: TMenuItem
        Caption = 'Show object ID'
        OnClick = ShowObjectsClick
      end
      object ShowFlatTerrain: TMenuItem
        Caption = 'Show flat terrain'
        OnClick = ShowFlatTerrainClick
      end
      object PrintScreen1: TMenuItem
        Caption = 'PrintScreen'
        OnClick = PrintScreen1Click
      end
    end
    object Export1: TMenuItem
      Caption = 'Export Data'
      object ExportTreesRX: TMenuItem
        Caption = 'Trees'
        OnClick = ExportTreesRXClick
      end
      object ExportHousesRX: TMenuItem
        Caption = 'Houses'
        OnClick = ExportHousesRXClick
      end
      object ExportUnitsRX: TMenuItem
        Caption = 'Units'
        OnClick = ExportUnitsRXClick
      end
      object ExportGUIRX: TMenuItem
        Caption = 'GUI'
        OnClick = ExportGUIRXClick
      end
      object ExportGUIMainRX: TMenuItem
        Caption = 'GUI Main'
        OnClick = ExportGUIMainRXClick
      end
      object Exportfonts1: TMenuItem
        Caption = 'Export Fonts'
        OnClick = Exportfonts1Click
      end
      object ExportText: TMenuItem
        Caption = 'Export Texts'
        OnClick = ExportTextClick
      end
      object ExportSounds1: TMenuItem
        Caption = 'Export Sounds'
        OnClick = ExportSounds1Click
      end
    end
    object ExportStatus1: TMenuItem
      Caption = 'Export Status'
      object ExportDeliverlists1: TMenuItem
        Caption = 'Export Deliver lists'
        OnClick = ExportDeliverlists1Click
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
    Left = 832
    Top = 600
  end
  object Timer1sec: TTimer
    OnTimer = Timer1secTimer
    Left = 864
    Top = 600
  end
end

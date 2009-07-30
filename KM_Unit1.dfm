object Form1: TForm1
  Left = 267
  Top = 518
  HelpType = htKeyword
  BorderStyle = bsNone
  ClientHeight = 245
  ClientWidth = 867
  Color = clBtnFace
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
  OnKeyDown = FormKeyDown
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
        Text = 'Map size: -'
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
    Left = 408
    Top = 8
    Width = 425
    Height = 73
    Caption = 
      '  Additional controls   ..   Press F11 to swap controls visibili' +
      'ty'
    TabOrder = 2
    object TeamColorPicker: TShape
      Left = 144
      Top = 40
      Width = 22
      Height = 22
      OnDragDrop = TeamColorPickerDragDrop
      OnMouseUp = TeamColorPickerMouseUp
    end
    object Label2: TLabel
      Left = 85
      Top = 15
      Width = 49
      Height = 13
      Caption = 'Passability'
    end
    object Label3: TLabel
      Left = 101
      Top = 35
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label1: TLabel
      Left = 169
      Top = 47
      Width = 61
      Height = 13
      Caption = 'Player1 color'
    end
    object Button_W: TButton
      Left = 144
      Top = 16
      Width = 25
      Height = 17
      Caption = 'W'
      TabOrder = 3
      OnClick = Button_WClick
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 52
      Width = 73
      Height = 17
      Caption = 'Speed x50'
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object Debug_PassabilityTrack: TTrackBar
      Left = 2
      Top = 14
      Width = 83
      Height = 17
      Max = 9
      Orientation = trHorizontal
      PageSize = 1
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 0
      ThumbLength = 14
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = Debug_PassabilityTrackChange
    end
    object RGPlayer: TRadioGroup
      Left = 304
      Top = 8
      Width = 105
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
      TabOrder = 2
      OnClick = RGPlayerClick
    end
    object Button_V: TButton
      Left = 168
      Top = 16
      Width = 25
      Height = 17
      Caption = 'V'
      TabOrder = 4
      OnClick = Button_VClick
    end
    object Button_6: TButton
      Left = 192
      Top = 16
      Width = 25
      Height = 17
      Caption = '6'
      TabOrder = 5
      OnClick = Button_6Click
    end
    object Button_1: TButton
      Left = 216
      Top = 16
      Width = 25
      Height = 17
      Caption = '1'
      TabOrder = 6
      OnClick = Button_1Click
    end
    object Button_Stop: TButton
      Left = 240
      Top = 16
      Width = 25
      Height = 17
      Caption = 'Stop'
      TabOrder = 7
      OnClick = Button_StopClick
    end
    object TB_Angle: TTrackBar
      Left = 2
      Top = 32
      Width = 95
      Height = 17
      Max = 90
      Orientation = trHorizontal
      PageSize = 1
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 8
      ThumbLength = 14
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = TB_Angle_Change
    end
  end
  object MediaPlayer1: TMediaPlayer
    Left = 112
    Top = 192
    Width = 141
    Height = 28
    ColoredButtons = [btPlay, btPause, btStop, btNext, btPrev]
    Enabled = False
    EnabledButtons = [btPlay, btPause, btStop, btNext, btPrev]
    VisibleButtons = [btPlay, btPause, btStop, btNext, btPrev]
    Visible = False
    TabOrder = 3
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

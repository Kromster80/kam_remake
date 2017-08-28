unit KM_ResKeys;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, StrUtils, KromUtils, Math,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_FileIO, KM_ResTexts;

type
  TKMFuncArea = (faCommon, faGame, faSpecReplay, faMapEdit);

const
  // There are total of 84 different functions in the game that can have a shortcut
  FUNC_COUNT = 84;

  // Load key IDs from inc file
  {$I KM_KeyIDs.inc}

type
  TKMFuncInfo = record
    Key: Byte;        // Key assigned to this function
    TextId: Word;     // Text description of the function
    Area: TKMFuncArea; // Area of effect for the function (common, game, maped)
    IsChangableByPlayer: Boolean; // Hide debug key and its function from UI
  end;

  TKMKeyLibrary = class
  private
    fFuncs: array [0..FUNC_COUNT-1] of TKMFuncInfo;
    fKeymapPath: string;
    function GetFunc(aIndex: Word): TKMFuncInfo;
    procedure SetFunc(aIndex: Word; aFuncInfo: TKMFuncInfo);
  public
    constructor Create;
    function GetKeyName(aKey: Word): string;
    function GetKeyNameById(aId: Word): string;
    function GetFunctionNameById(aId: Integer): string;
    function AllowKeySet(aArea: TKMFuncArea; aKey: Word): Boolean;
    procedure SetKey(aId: Integer; aKey: Word);
    function Count: Integer;
    property Funcs[aIndex: Word]: TKMFuncInfo read GetFunc write SetFunc; default;
    procedure LoadKeymapFile;
    procedure ResetKeymap;
    procedure SaveKeymap;
  end;

var
  // All Keys accessible from everywhere
  gResKeys: TKMKeyLibrary;

implementation

const
  // Default keys
  DEF_KEYS: array [0..FUNC_COUNT-1] of Byte = (
    //Common Keys
    37, 39, 38, 40,                         // Scroll Left, Right, Up, Down (Arrow keys)
    4,                                      // Map drag scroll (Middle mouse btn)
    34, 33, 8,                              // Zoom In/Out/Reset (Page Down, Page Up, Backspace)
    27,                                     // Close opened menu (Esc)
    82, 67, 87, 68,                         // Plan road/corn/wine/erase plan(building) (R, C, W, D)
    122,                                    // Debug Window hotkey (F11)

    // These keys are not changable by Player in Options menu
    77, 86, 69, 67,                         // Debug hotkeys (Show Map/Victory/Defeat/Add Scout) (M, V, E, C)

    // Game Keys
    112, 113, 114, 115,                     // Game menus (F1-F4)
    72, 83, 76, 70, 88,                     // Army commands (Halt/Split/Link/Food/Storm) (H/S/L/F/X)
    187, 189, 190, 188,                     // Army commands (Increase form./Decrease form./Turn clockwise/Turn counterclockwise) (=/-/./,)
    116, 117, 118, 119,                     // Speed ups (x1/x3/x6/x10) (F5-F8)
    66, 80, 84,                             // Beacon/Pause/Show team in MP (B, P, T)
    32, 46, 13,                             // Center to alert/Delete message/Show chat (Space, Delete, Return)
    9,                                      // Select next building/unit/group with same type (Tab)
    49, 50, 51, 52, 53, 54, 55, 56, 57, 48, // Dynamic selection groups 1-10 (1-9, 0)
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,           // Dynamic selection groups 11-20 (no defaults)

    // Spectate/Replay view Keys
    49, 50, 51, 52, 53, 54, 55, 56,         // Switch between players while spectating/viewing replay (1-8)

    // Map Editor Keys
    13,                                     // Map Editor Extra's menu (Return)
    112, 113, 114, 115, 116,                // Map Editor menus (F1-F5)
    49, 50, 51, 52, 53, 54,                 // Map Editor sub-menus (1-6)
    32,                                     // Map Editor show objects palette (Space)
    46                                      // Map Editor universal erasor (Delete)
  );

  // Function text values
  KEY_FUNC_TX: array [0..FUNC_COUNT-1] of Word = (
    //Common Keys
    TX_KEY_FUNC_SCROLL_LEFT, TX_KEY_FUNC_SCROLL_RIGHT, TX_KEY_FUNC_SCROLL_UP, TX_KEY_FUNC_SCROLL_DOWN,    // Scroll Left, Right, Up, Down
    TX_KEY_FUNC_MAP_DRAG_SCROLL,                                                                          // Map drag scroll
    TX_KEY_FUNC_ZOOM_IN, TX_KEY_FUNC_ZOOM_OUT, TX_KEY_FUNC_ZOOM_RESET,                                    // Zoom In/Out/Reset
    TX_KEY_FUNC_CLOSE_MENU,                                                                               // Close opened menu
    TX_KEY_FUNC_PLAN_ROAD, TX_KEY_FUNC_PLAN_FIELD, TX_KEY_FUNC_PLAN_WINE, TX_KEY_FUNC_ERASE_PLAN,         // Plan road/corn/wine/erase plan(building)
    TX_KEY_FUNC_DBG_WINDOW,                                                                               // Debug window

    // These keys are not changable by Player in Options menu
    TX_KEY_FUNC_DBG_MAP, TX_KEY_FUNC_DBG_VICTORY, TX_KEY_FUNC_DBG_DEFEAT, TX_KEY_FUNC_DBG_SCOUT,          // Debug (Show Map, Victory, Defeat, Add Scout)

    // Game Keys
    TX_KEY_FUNC_MENU_BUILD, TX_KEY_FUNC_MENU_RATIO, TX_KEY_FUNC_MENU_STATS, TX_KEY_FUNC_MENU_MAIN,        // Game menus
    TX_KEY_FUNC_HALT, TX_KEY_FUNC_SPLIT, TX_KEY_FUNC_LINKUP, TX_KEY_FUNC_FOOD, TX_KEY_FUNC_STORM,         // Army commands
    TX_KEY_FUNC_FORM_INCREASE, TX_KEY_FUNC_FORM_DECREASE, TX_KEY_FUNC_TURN_CW, TX_KEY_FUNC_TURN_CCW,      // Army commands
    TX_KEY_FUNC_GAME_SPEED_1,TX_KEY_FUNC_GAME_SPEED_2,TX_KEY_FUNC_GAME_SPEED_3,TX_KEY_FUNC_GAME_SPEED_4,  // Speed ups
    TX_KEY_FUNC_BEACON, TX_KEY_FUNC_PAUSE, TX_KEY_FUNC_SHOW_TEAMS,                                        // Beacon/Pause/Show team in MP
    TX_KEY_FUNC_CENTER_ALERT, TX_KEY_FUNC_DELETE_MSG, TX_KEY_FUNC_SHOW_GAME_CHAT,                         // Center to alert/Delete message/Show chat
    TX_KEY_FUNC_SEL_NXT_BLD_UNIT_SAME_TYPE,                                                               // Select next building/unit/group with same type
    TX_KEY_FUNC_SELECT_1, TX_KEY_FUNC_SELECT_2, TX_KEY_FUNC_SELECT_3, TX_KEY_FUNC_SELECT_4, TX_KEY_FUNC_SELECT_5,   // Dynamic selection groups 1-5
    TX_KEY_FUNC_SELECT_6, TX_KEY_FUNC_SELECT_7, TX_KEY_FUNC_SELECT_8, TX_KEY_FUNC_SELECT_9, TX_KEY_FUNC_SELECT_10,  // Dynamic selection groups 6-10
    TX_KEY_FUNC_SELECT_11,TX_KEY_FUNC_SELECT_12,TX_KEY_FUNC_SELECT_13,TX_KEY_FUNC_SELECT_14,TX_KEY_FUNC_SELECT_15,  // Dynamic selection groups 11-15
    TX_KEY_FUNC_SELECT_16,TX_KEY_FUNC_SELECT_17,TX_KEY_FUNC_SELECT_18,TX_KEY_FUNC_SELECT_19,TX_KEY_FUNC_SELECT_20,  // Dynamic selection groups 16-20

    // Spectate MP game/Replay view Keys
    TX_KEY_FUNC_SPECTATE_PLAYER_1, TX_KEY_FUNC_SPECTATE_PLAYER_2, TX_KEY_FUNC_SPECTATE_PLAYER_3, TX_KEY_FUNC_SPECTATE_PLAYER_4, // Spectator/Replay player switch
    TX_KEY_FUNC_SPECTATE_PLAYER_5, TX_KEY_FUNC_SPECTATE_PLAYER_6, TX_KEY_FUNC_SPECTATE_PLAYER_7, TX_KEY_FUNC_SPECTATE_PLAYER_8, // Spectator/Replay player switch

    // Map Editor Keys
    TX_KEY_FUNC_MAPEDIT_EXTRA,                                                                            // Map Editor Extra's menu
    TX_KEY_FUNC_MAPEDIT_TERAIN_EDIT, TX_KEY_FUNC_MAPEDIT_VILLAGE_PLAN,                                    // Map Editor menus
    TX_KEY_FUNC_MAPEDIT_VISUAL_SCRIPT, TX_KEY_FUNC_MAPEDIT_GLOBAL_SCRIPT, TX_KEY_FUNC_MAPEDIT_MENU_MAIN,  // Map Editor menus
    TX_KEY_FUNC_MAPEDIT_SUBMENU_1, TX_KEY_FUNC_MAPEDIT_SUBMENU_2, TX_KEY_FUNC_MAPEDIT_SUBMENU_3,          // Map Editor sub-menus
    TX_KEY_FUNC_MAPEDIT_SUBMENU_4, TX_KEY_FUNC_MAPEDIT_SUBMENU_5, TX_KEY_FUNC_MAPEDIT_SUBMENU_6,          // Map Editor sub-menus
    TX_KEY_FUNC_MAPEDIT_OBJ_PALETTE,                                                                      // Map Editor show objects palette
    TX_KEY_FUNC_MAPEDIT_UNIV_ERASOR                                                                       // Map Editor universal erasor
  );

{ TKMKeyLibrary }
constructor TKMKeyLibrary.Create;
var
  I: Integer;
begin
  inherited;

  fKeymapPath := (ExeDir + 'keys.keymap');

  LoadKeymapFile;

  for I := 0 to FUNC_COUNT - 1 do
  begin
    fFuncs[I].TextId := KEY_FUNC_TX[I];

    case I of
      0..17:  fFuncs[I].Area := faCommon;
      18..61: fFuncs[I].Area := faGame;
      62..69: fFuncs[I].Area := faSpecReplay;
      else    fFuncs[I].Area := faMapEdit;
    end;

    fFuncs[I].IsChangableByPlayer := (I in [14..17]);
  end;
end;


function TKMKeyLibrary.Count: Integer;
begin
  Result := FUNC_COUNT;
end;


// Each line in .keymap file has an index and a key value. Lines without index are skipped
procedure TKMKeyLibrary.LoadKeymapFile;
var
  I: Integer;
  SL: TStringList;
  delim1, delim2: Integer;
  funcId, keyVal: Integer;
begin
  if not FileExists(fKeymapPath) then
  begin
    ResetKeymap;
    Exit;
  end;

  SL := TStringList.Create;
  {$IFDEF WDC} SL.LoadFromFile(fKeymapPath); {$ENDIF}
  // In FPC TStringList can't cope with BOM (or UnicodeStrings at all really)
  {$IFDEF FPC} SL.Text := ReadTextU(fKeymapPath, 1252); {$ENDIF}

  // Parse text
  for I := 0 to SL.Count - 1 do
  begin
    delim1 := Pos(':', SL[I]);
    delim2 := Pos('//', SL[I]);
    if (delim1 = 0) or (delim2 = 0) or (delim1 > delim2) then Continue;

    funcId := StrToIntDef(Copy(SL[I], 0, delim1 - 1), -1);
    keyVal := StrToIntDef(Copy(SL[I], delim1 + 1, delim2 - delim1 - 1), -1);

    if not InRange(funcId, 0, FUNC_COUNT - 1) or (keyVal = -1) then Continue;
    if not InRange(keyVal, 0, 255) then Continue;

    fFuncs[funcId].Key := keyVal;
  end;

  SL.Free;
end;


function TKMKeyLibrary.GetFunc(aIndex: Word): TKMFuncInfo;
begin
  Result := fFuncs[aIndex];
end;


procedure TKMKeyLibrary.SetFunc(aIndex: Word; aFuncInfo :TKMFuncInfo);
begin
  fFuncs[aIndex] := aFuncInfo;
end;


procedure TKMKeyLibrary.SaveKeymap;
var
  Keystring: string;
  KeyStringList: TStringList;
  I: Integer;
begin
  KeyStringList := TStringList.Create;
  {$IFDEF WDC}KeyStringList.DefaultEncoding := TEncoding.UTF8;{$ENDIF}

  for I := 0 to FUNC_COUNT - 1 do
  begin
    Keystring := IntToStr(I) + ':' + IntToStr(fFuncs[I].Key) + '// ' + GetFunctionNameById(I);
    KeyStringList.Add(Keystring);
  end;

  KeyStringList.SaveToFile(fKeymapPath{$IFDEF WDC}, TEncoding.UTF8{$ENDIF});
  KeyStringList.Free;
end;


procedure TKMKeyLibrary.ResetKeymap;
var
  I: Integer;
begin
  for I := 0 to FUNC_COUNT - 1 do
    fFuncs[I].Key := DEF_KEYS[I];
end;


function TKMKeyLibrary.GetFunctionNameById(aId: Integer): string;
begin
  if InRange(aId, 0, FUNC_COUNT - 1) then
    Result := gResTexts[KEY_FUNC_TX[aId]]
  else
    Result := gResTexts[TX_KEY_FUNC_UNKNOWN] + ' ' + IntToStr(aId) + '! ~~~';
end;


function TKMKeyLibrary.GetKeyName(aKey: Word): string;
begin
  // All the special keys (not characters and not numbers) need a localized name
  case aKey of
    0:  Result := '';
    1:  Result :=  gResTexts[TX_KEY_LMB];
    2:  Result :=  gResTexts[TX_KEY_RMB];
    3:  Result :=  gResTexts[TX_KEY_BREAK];
    4:  Result :=  gResTexts[TX_KEY_MMB];
    5:  Result :=  'Forward mouse button'; // Todo translate
    6:  Result :=  'Backward mouse button'; // Todo translate
    8:  Result :=  gResTexts[TX_KEY_BACKSPACE];
    9:  Result :=  gResTexts[TX_KEY_TAB];
    12: Result :=  gResTexts[TX_KEY_CLEAR];
    13: Result :=  gResTexts[TX_KEY_ENTER];
    16: Result :=  gResTexts[TX_KEY_SHIFT];
    17: Result :=  gResTexts[TX_KEY_CTRL];
    18: Result :=  gResTexts[TX_KEY_ALT];
    19: Result :=  gResTexts[TX_KEY_PAUSE];
    20: Result :=  gResTexts[TX_KEY_CAPS];
    27: Result :=  gResTexts[TX_KEY_ESC];
    32: Result :=  gResTexts[TX_KEY_SPACE];
    33: Result :=  gResTexts[TX_KEY_PG_UP];
    34: Result :=  gResTexts[TX_KEY_PG_DOWN];
    35: Result :=  gResTexts[TX_KEY_END];
    36: Result :=  gResTexts[TX_KEY_HOME];
    37: Result :=  gResTexts[TX_KEY_ARROW_LEFT];
    38: Result :=  gResTexts[TX_KEY_ARROW_UP];
    39: Result :=  gResTexts[TX_KEY_ARROW_RIGHT];
    40: Result :=  gResTexts[TX_KEY_ARROW_DOWN];
    41: Result :=  gResTexts[TX_KEY_SELECT];
    42: Result :=  gResTexts[TX_KEY_PRINT];
    43: Result :=  gResTexts[TX_KEY_EXECUTE];
    44: Result :=  gResTexts[TX_KEY_PRINT_SCREEN];
    45: Result :=  gResTexts[TX_KEY_INSERT];
    46: Result :=  gResTexts[TX_KEY_DELETE];
    47: Result :=  gResTexts[TX_KEY_HELP];
    // NumPad keys
    96: Result :=  gResTexts[TX_KEY_NUM_0];
    97: Result :=  gResTexts[TX_KEY_NUM_1];
    98: Result :=  gResTexts[TX_KEY_NUM_2];
    99: Result :=  gResTexts[TX_KEY_NUM_3];
    100: Result := gResTexts[TX_KEY_NUM_4];
    101: Result := gResTexts[TX_KEY_NUM_5];
    102: Result := gResTexts[TX_KEY_NUM_6];
    103: Result := gResTexts[TX_KEY_NUM_7];
    104: Result := gResTexts[TX_KEY_NUM_8];
    105: Result := gResTexts[TX_KEY_NUM_9];
    106: Result := gResTexts[TX_KEY_NUM_MULTIPLY];
    107: Result := gResTexts[TX_KEY_NUM_PLUS];
    108: Result := gResTexts[TX_KEY_SEPARATOR];
    109: Result := gResTexts[TX_KEY_NUM_MINUS];
    110: Result := gResTexts[TX_KEY_NUM_DOT];
    111: Result := gResTexts[TX_KEY_NUM_DIVIDE];
    // F keys
    112: Result := 'F1';
    113: Result := 'F2';
    114: Result := 'F3';
    115: Result := 'F4';
    116: Result := 'F5';
    117: Result := 'F6';
    118: Result := 'F7';
    119: Result := 'F8';
    120: Result := 'F9';
    121: Result := 'F10';
    122: Result := 'F11';
    123: Result := 'F12';
    // F13..F24 are the special function keys, enabled by default in the EFI(UEFI) BIOS
    //  This is especially the case with Windows 8/8.1 laptops.
    //  Most manufacturers don't give the option to change it in the BIOS, hence we name them here anyways.
    124: Result := 'F13';
    125: Result := 'F14';
    126: Result := 'F15';
    127: Result := 'F16';
    128: Result := 'F17';
    129: Result := 'F18';
    130: Result := 'F19';
    131: Result := 'F20';
    132: Result := 'F21';
    133: Result := 'F22';
    134: Result := 'F23';
    135: Result := 'F24';
    144: Result := gResTexts[TX_KEY_NUM_LOCK];
    145: Result := gResTexts[TX_KEY_SCROLL_LOCK];
    160: Result := gResTexts[TX_KEY_LEFT_SHIFT];
    161: Result := gResTexts[TX_KEY_RIGHT_SHIFT];
    162: Result := gResTexts[TX_KEY_LEFT_CTRL];
    163: Result := gResTexts[TX_KEY_RIGHT_CTRL];
    164: Result := gResTexts[TX_KEY_LEFT_ALT];
    165: Result := gResTexts[TX_KEY_RIGHT_ALT];
    186: Result := ';';
    187: Result := '=';
    188: Result := ',';
    189: Result := '-';
    190: Result := '.';
    191: Result := '/';
    192: Result := '`';
    219: Result := '[';
    220: Result := '\';
    221: Result := ']';
    222: Result := '''';
    250: Result := gResTexts[TX_KEY_PLAY];
    251: Result := gResTexts[TX_KEY_ZOOM];
  else
    Result := Char(aKey);
  end;
end;


function TKMKeyLibrary.GetKeyNameById(aId: Word): string;
begin
  Result := GetKeyName(fFuncs[aId].Key);
end;


function TKMKeyLibrary.AllowKeySet(aArea: TKMFuncArea; aKey: Word): Boolean;
begin
  // False if Key equals to Shift or Ctrl, which are used in game for specific bindings
  Result := not (aKey in [16, 17]);
end;


procedure TKMKeyLibrary.SetKey(aId: Integer; aKey: Word);
var
  I: Integer;
begin
  // Reset previous key binding if Key areas overlap
  if aKey <> 0 then
    for I := 0 to FUNC_COUNT - 1 do
      if fFuncs[I].Key = aKey then
        case fFuncs[I].Area of
          faCommon:     fFuncs[I].Key := 0;
          faGame:       if (fFuncs[aId].Area in [faGame, faCommon]) then
                          fFuncs[I].Key := 0;
          faSpecReplay: if (fFuncs[aId].Area in [faSpecReplay, faCommon]) then
                          fFuncs[I].Key := 0;
          faMapEdit:    if (fFuncs[aId].Area in [faMapEdit, faCommon]) then
                          fFuncs[I].Key := 0;
        end;

  fFuncs[aId].Key := aKey;
end;


end.

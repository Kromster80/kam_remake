unit KM_ResKeys;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, StrUtils, KromUtils, Math,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_FileIO, KM_ResTexts;

type
  TKMKeyArea = (kaCommon, kaGame, kaMapEdit);

const
  // Load key IDs from this include file
  KEYMAP_COUNT = 45;
  {$I KM_KeyIDs.inc}

  DEF_KEYS: array [0..KEYMAP_COUNT-1] of Byte = (
    37,  39,  38,  40, 112, 113, 114, 115,  72,  83,
    76,  70,  88, 187, 189, 190, 188, 116, 117, 118,
    119, 66,  80,  84,  34,  33,   8,  49,  50,  51,
    52,  53,  54,  55,  56,  57,  48,  32,  46,  13,
    27,  77,  86,  68,  67
  );

  KEY_FUNC_TX: array [0..KEYMAP_COUNT-1] of Word = (
    TX_KEY_FUNC_SCROLL_LEFT, TX_KEY_FUNC_SCROLL_RIGHT, TX_KEY_FUNC_SCROLL_UP, TX_KEY_FUNC_SCROLL_DOWN, TX_KEY_FUNC_MENU_BUILD,
    TX_KEY_FUNC_MENU_RATIO, TX_KEY_FUNC_MENU_STATS, TX_KEY_FUNC_MENU_MAIN, TX_KEY_FUNC_HALT, TX_KEY_FUNC_SPLIT,
    TX_KEY_FUNC_LINKUP, TX_KEY_FUNC_FOOD, TX_KEY_FUNC_STORM, TX_KEY_FUNC_FORM_INCREASE, TX_KEY_FUNC_FORM_DECREASE,
    TX_KEY_FUNC_TURN_CW, TX_KEY_FUNC_TURN_CCW, TX_KEY_FUNC_GAME_SPEED_1, TX_KEY_FUNC_GAME_SPEED_2, TX_KEY_FUNC_GAME_SPEED_3,
    TX_KEY_FUNC_GAME_SPEED_4, TX_KEY_FUNC_BEACON, TX_KEY_FUNC_PAUSE, TX_KEY_FUNC_SHOW_TEAMS, TX_KEY_FUNC_ZOOM_IN,
    TX_KEY_FUNC_ZOOM_OUT, TX_KEY_FUNC_ZOOM_RESET, TX_KEY_FUNC_SELECT_1, TX_KEY_FUNC_SELECT_2, TX_KEY_FUNC_SELECT_3,
    TX_KEY_FUNC_SELECT_4, TX_KEY_FUNC_SELECT_5, TX_KEY_FUNC_SELECT_6, TX_KEY_FUNC_SELECT_7, TX_KEY_FUNC_SELECT_8,
    TX_KEY_FUNC_SELECT_9, TX_KEY_FUNC_SELECT_10, TX_KEY_FUNC_CENTER_ALERT, TX_KEY_FUNC_DELETE_MSG, TX_KEY_FUNC_SHOW_GAME_CHAT,
    TX_KEY_FUNC_CLOSE_MENU, TX_KEY_FUNC_DBG_MAP, TX_KEY_FUNC_DBG_VICTORY, TX_KEY_FUNC_DBG_DEFEAT, TX_KEY_FUNC_DBG_SCOUT
  );

  KEY_SEP_TX: array [0..2] of Word = (
    TX_KEY_COMMON, TX_KEY_GAME, TX_KEY_MAPEDIT
  );

type
  TKMKeyInfo = record
    fDefKey: Byte;
    fFuncTextId: Word;
    fFuncArea: TKMKeyArea;
    fFuncId: Integer;
    fFuncIsDebug: Boolean; // Hide key and function
  end;

  TKMKeyLibrary = class
  private
    fCount: Integer;
    fKeys: array [0..KEYMAP_COUNT-1] of TKMKeyInfo;
    fKeymapPath: string;
    function GetKeys(aIndex: Word): TKMKeyInfo;
    {procedure SetKeys(aIndex: Word; aValue: Integer); }
  public
    constructor Create;
    property Count: Integer read fCount;
    function GetKeyName(aKey: Word): string;
    function GetKeyNameById(aId: Word): string;
    function GetFunctionNameById(aId: Integer): string;
    property Keys[aIndex: Word]: TKMKeyInfo read GetKeys{ write SetKeys}; default;
    {procedure LoadKeymapFile; }
    {procedure ResetKeymap; }
    {procedure SaveKeymap; }
    destructor Destroy;
  end;


var
  // All games Keys accessible from everywhere
  gResKeys: TKMKeyLibrary;


implementation


{ TKMKeyLibrary }
constructor TKMKeyLibrary.Create;
var
  I: Integer;
begin
  inherited;

  fCount := KEYMAP_COUNT;
  fKeymapPath := (ExeDir + 'keys.keymap');

  for I := 0 to KEYMAP_COUNT - 1 do
  begin
    fKeys[I].fDefKey := DEF_KEYS[I];
    fKeys[I].fFuncTextId := KEY_FUNC_TX[I];

    if (I in [0..3]) or (I in [24..26]) or (I in [40..44]) then
      fKeys[I].fFuncArea := kaCommon
    else if (I in [4..23]) or (I in [27..39]) then
      fKeys[I].fFuncArea := kaGame
    else
      fKeys[I].fFuncArea := kaMapEdit;

    fKeys[I].fFuncId := I;

    if I in [41..44] then
      fKeys[I].fFuncIsDebug := True
    else
      fKeys[I].fFuncIsDebug := False;
  end;

  //LoadKeymapFile;
end;


destructor TKMKeyLibrary.Destroy;
begin
  inherited;
  FreeAndNil(fKeys);
end;


// Each line in .keymap file has an index and a text. Lines without index are skipped
{procedure TKMKeyLibrary.LoadKeymapFile;
var
  I: Integer;
  SL: TStringList;
  delim1, delim2: Integer;
  keyId, keyVal: Integer;
begin
  if not FileExists(fKeymapPath) then
  begin
    ResetKeymap;
    Exit;
  end;

  SL := TStringList.Create; }
  //{$IFDEF WDC} SL.LoadFromFile(fKeymapPath); {$ENDIF}
  // In FPC TStringList can't cope with BOM (or UnicodeStrings at all really)
  //{$IFDEF FPC} SL.Text := ReadTextU(aFile, 1252); {$ENDIF}

  // Parse text
{  for I := 0 to SL.Count - 1 do
  begin
    delim1 := Pos(':', SL[I]);
    delim2 := Pos('//', SL[I]);
    if (delim1 = 0) or (delim2 = 0) or (delim1 > delim2) then Continue;

    keyId := StrToIntDef(Copy(SL[I], 0, delim1 - 1), -1);
    keyVal := StrToIntDef(Copy(SL[I], delim1 + 1, delim2 - delim1 - 1), -1);

    if not InRange(keyId, 0, fCount - 1) or (keyVal = -1) then Continue;

    //fKeys[keyId] := keyVal;
  end;
end;  }


function TKMKeyLibrary.GetKeys(aIndex: Word): TKMKeyInfo;
begin
  Result := fKeys[aIndex];
end;


{procedure TKMKeyLibrary.SetKeys(aIndex: Word; aValue: Byte);
begin
  fKeys[aIndex].fDefKey := aValue;
end;  }


{procedure TKMKeyLibrary.SaveKeymap;
var
  Keystring: string;
  KeyStringList: TStringList;
  I: Integer;
begin
  KeyStringList := TStringList.Create;}
  //{$IFDEF WDC}KeyStringList.DefaultEncoding := TEncoding.UTF8;{$ENDIF}

{  for I := 0 to fCount - 1 do
  begin
    Keystring := IntToStr(I) + ':'+ IntToStr(fKeys[I]) + '// ' + GetFunctionNameById(I);
    KeyStringList.Add(Keystring);
  end; }

  //KeyStringList.SaveToFile(fKeymapPath{$IFDEF WDC}, TEncoding.UTF8{$ENDIF});
{  KeyStringList.Free;
end;


procedure TKMKeyLibrary.ResetKeymap;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    fKeys[I] := DEF_KEYS[I];
end; }


function TKMKeyLibrary.GetFunctionNameById(aId: Integer): string;
begin
  case aId of
    0..KEYMAP_COUNT - 1: Result := gResTexts[KEY_FUNC_TX[aId]];

    // Higher value to separate bindable keys from special keys
    100: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_EXTRA];
    101: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_TERAIN_EDIT];
    102: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_VILLAGE_PLAN];
    103: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_VISUAL_SCRIPT];
    104: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_GLOBAL_SCRIPT];
    105: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_MENU_MAIN];
    106: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_1];
    107: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_2];
    108: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_3];
    109: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_4];
    110: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_5];
    111: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_6];
    112: Result := gResTexts[TX_KEY_FUNC_UNASSIGNABLE];
    113: Result := gResTexts[TX_KEY_FUNC_UNASSIGNABLE];
  else
    Result := gResTexts[TX_KEY_FUNC_UNKNOWN] + ' ' + IntToStr(aId) + '! ~~~';
  end;
end;


function TKMKeyLibrary.GetKeyName(aKey: Word): string;
begin
  case aKey of
    1:  Result :=  gResTexts[TX_KEY_LMB];
    2:  Result :=  gResTexts[TX_KEY_RMB];
    3:  Result :=  gResTexts[TX_KEY_BREAK];
    4:  Result :=  gResTexts[TX_KEY_MMB];
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
    106: Result := gResTexts[TX_KEY_NUM_TIMES];
    107: Result := gResTexts[TX_KEY_NUM_PLUS];
    108: Result := gResTexts[TX_KEY_SEPARATOR];
    109: Result := gResTexts[TX_KEY_NUM_MINUS];
    110: Result := gResTexts[TX_KEY_NUM_DOT];
    111: Result := gResTexts[TX_KEY_NUM_DEVIDE];
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
  Result := GetKeyName(fKeys[aId].fDefKey);
end;


end.

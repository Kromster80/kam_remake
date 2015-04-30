unit KM_ResKeys;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  {$IFDEF FPC} lconvencoding, LazUTF8, {$ENDIF}
  Classes, SysUtils, StrUtils, KromUtils, Math,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_FileIO, KM_ResTexts;

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

type
  TKMKeyLibrary = class
  private
    fCount: Integer;
    fKeys: array [0..KEYMAP_COUNT-1] of Integer;
    fKeymapPath: string;
    function GetKeys(aIndex: Word): Integer;
    procedure SetKeys(aIndex: Word; aValue: Integer);
  public
    constructor Create;
    property Count: Integer read fCount;
    function GetCharFromVK(aKey: Word): String;
    function GetNameForKey(aValue: Integer): String;
    property Keys[aIndex: Word]: Integer read GetKeys write SetKeys; default;
    procedure LoadKeymapFile;
    procedure ResetKeymap;
    procedure SaveKeymap;
  end;

var
  // All games Keys accessible from everywhere
  gResKeys: TKMKeyLibrary;

implementation

{ TKMKeyLibrary }
constructor TKMKeyLibrary.Create;
begin
  inherited;

  fCount := KEYMAP_COUNT;
  fKeymapPath := (ExeDir + 'keys.keymap');

  LoadKeymapFile;
end;


// Each line in .keymap file has an index and a text. Lines without index are skipped
procedure TKMKeyLibrary.LoadKeymapFile;
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

  SL := TStringList.Create;
  {$IFDEF WDC} SL.LoadFromFile(fKeymapPath); {$ENDIF}
  //In FPC TStringList can't cope with BOM (or UnicodeStrings at all really)
  {$IFDEF FPC} SL.Text := ReadTextU(aFile, 1252); {$ENDIF}

  // Parse text
  for I := 0 to SL.Count - 1 do
  begin
    delim1 := Pos(':', SL[I]);
    delim2 := Pos('//', SL[I]);
    if (delim1 = 0) or (delim2 = 0) or (delim1 > delim2) then Continue;

    keyId := StrToIntDef(Copy(SL[I], 0, delim1 - 1), -1);
    keyVal := StrToIntDef(Copy(SL[I], delim1 + 1, delim2 - delim1 - 1), -1);

    if not InRange(keyId, 0, fCount - 1) or (keyVal = -1) then Continue;

    fKeys[keyId] := keyVal;
  end;
end;


function TKMKeyLibrary.GetKeys(aIndex: Word): Integer;
begin
  Result := fKeys[aIndex];
end;


procedure TKMKeyLibrary.SetKeys(aIndex: Word; aValue: Integer);
begin
  fKeys[aIndex] := aValue;
end;


procedure TKMKeyLibrary.SaveKeymap;
var
  Keystring: string;
  KeyStringList: TStringList;
  I: Integer;
begin
  KeyStringList := TStringList.Create;
  {$IFDEF WDC}KeyStringList.DefaultEncoding := TEncoding.UTF8;{$ENDIF}

  for I := 0 to fCount - 1 do
  begin
    Keystring := IntToStr(I) + ':'+ IntToStr(fKeys[I]) + '//' + GetNameForKey(I);
    KeyStringList.Add(Keystring);
  end;

  KeyStringList.SaveToFile(fKeymapPath{$IFDEF WDC}, TEncoding.UTF8{$ENDIF});
  KeyStringList.Free;
end;


procedure TKMKeyLibrary.ResetKeymap;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    fKeys[I] := DEF_KEYS[I];
end;


// Here we define the action name values
function TKMKeyLibrary.GetNameForKey(aValue: Integer): String;
begin
  case aValue of
    0: Result :=   gResTexts[TX_KEY_FUNC_SCROLL_LEFT];
    1: Result :=   gResTexts[TX_KEY_FUNC_SCROLL_RIGHT];
    2: Result :=   gResTexts[TX_KEY_FUNC_SCROLL_UP];
    3: Result :=   gResTexts[TX_KEY_FUNC_SCROLL_DOWN];
    4: Result :=   gResTexts[TX_KEY_FUNC_MENU_BUILD];
    5: Result :=   gResTexts[TX_KEY_FUNC_MENU_RATIO];
    6: Result :=   gResTexts[TX_KEY_FUNC_MENU_STATS];
    7: Result :=   gResTexts[TX_KEY_FUNC_MENU_MAIN];
    8: Result :=   gResTexts[TX_KEY_FUNC_HALT];
    9: Result :=   gResTexts[TX_KEY_FUNC_SPLIT];
    10: Result :=  gResTexts[TX_KEY_FUNC_LINKUP];
    11: Result :=  gResTexts[TX_KEY_FUNC_FOOD];
    12: Result :=  gResTexts[TX_KEY_FUNC_STORM];
    13: Result :=  gResTexts[TX_KEY_FUNC_FORM_INCREASE];
    14: Result :=  gResTexts[TX_KEY_FUNC_FORM_DECREASE];
    15: Result :=  gResTexts[TX_KEY_FUNC_TURN_CW];
    16: Result :=  gResTexts[TX_KEY_FUNC_TURN_CCW];
    17: Result :=  gResTexts[TX_KEY_FUNC_GAME_SPEED_1];
    18: Result :=  gResTexts[TX_KEY_FUNC_GAME_SPEED_2];
    19: Result :=  gResTexts[TX_KEY_FUNC_GAME_SPEED_3];
    20: Result :=  gResTexts[TX_KEY_FUNC_GAME_SPEED_4];
    21: Result :=  gResTexts[TX_KEY_FUNC_BEACON];
    22: Result :=  gResTexts[TX_KEY_FUNC_PAUSE];
    23: Result :=  gResTexts[TX_KEY_FUNC_SHOW_TEAMS];
    24: Result :=  gResTexts[TX_KEY_FUNC_ZOOM_IN];
    25: Result :=  gResTexts[TX_KEY_FUNC_ZOOM_OUT];
    26: Result :=  gResTexts[TX_KEY_FUNC_ZOOM_RESET];
    27: Result :=  gResTexts[TX_KEY_FUNC_SELECT_1];
    28: Result :=  gResTexts[TX_KEY_FUNC_SELECT_2];
    29: Result :=  gResTexts[TX_KEY_FUNC_SELECT_3];
    30: Result :=  gResTexts[TX_KEY_FUNC_SELECT_4];
    31: Result :=  gResTexts[TX_KEY_FUNC_SELECT_5];
    32: Result :=  gResTexts[TX_KEY_FUNC_SELECT_6];
    33: Result :=  gResTexts[TX_KEY_FUNC_SELECT_7];
    34: Result :=  gResTexts[TX_KEY_FUNC_SELECT_8];
    35: Result :=  gResTexts[TX_KEY_FUNC_SELECT_9];
    36: Result :=  gResTexts[TX_KEY_FUNC_SELECT_10];
    37: Result :=  gResTexts[TX_KEY_FUNC_CENTER_ALERT];
    38: Result :=  gResTexts[TX_KEY_FUNC_DELETE_MSG];
    39: Result :=  gResTexts[TX_KEY_FUNC_SHOW_GAME_CHAT];
    40: Result :=  gResTexts[TX_KEY_FUNC_CLOSE_MENU];
    41: Result :=  gResTexts[TX_KEY_FUNC_DBG_MAP];
    42: Result :=  gResTexts[TX_KEY_FUNC_DBG_VICTORY];
    43: Result :=  gResTexts[TX_KEY_FUNC_DBG_DEFEAT];
    44: Result :=  gResTexts[TX_KEY_FUNC_DBG_SCOUT];
    // Higher value to seporate bindable keys from special keys
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
    Result := gResTexts[TX_KEY_FUNC_UNKNOWN] + ' ' + IntToStr(aValue) + '! ~~~';
  end;
end;


// Here we fix the issue where Char() doesn't always give us the character/button
function TKMKeyLibrary.GetCharFromVK(aKey: Word): String;
begin
  case aKey of
    1: Result :=   gResTexts[TX_KEY_LMB];
    2: Result :=   gResTexts[TX_KEY_RMB];
    3: Result :=   gResTexts[TX_KEY_BREAK];
    4: Result :=   gResTexts[TX_KEY_MMB];
    8: Result :=   gResTexts[TX_KEY_BACKSPACE];
    9: Result :=   gResTexts[TX_KEY_TAB];
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
    { F13..F24 are the special function keys, enabled by default in the EFI(UEFI) BIOS
      This is especially the case with Windows 8/8.1 laptops.
      Most manufacturers don't give the option to change it in the BIOS, hence we name them here anyways. }
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


end.

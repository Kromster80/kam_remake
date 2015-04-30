unit KM_ResKeys;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  {$IFDEF FPC} lconvencoding, LazUTF8, {$ENDIF}
  Classes, SysUtils, StrUtils, KromUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_FileIO, KM_ResTexts;

const
  // Load key IDs from this include file
  {$I KM_KeyIDs.inc}

  // For missing Keys
  NO_KEY = nil;

type
  TKMKeyLibrary = class
  private
    fKeys: TIntegerArray;
    FilePath: String;
    function GetKeys(aIndex: Word): Integer;
    procedure LoadKeymapFile(var aArray: TIntegerArray);
  public
    KeyCount: Integer;
    constructor Create;
    function HasKey(aIndex: Word): Boolean;
    function GetCharFromVK(aKey: Word): String;
    function GetNameForKey(aValue: Integer): String;
    property Keys[aIndex: Word]: Integer read GetKeys; default;
    procedure LoadKeys; // Load all the keys
    procedure ResetKeyBind;
    procedure SaveKey(aKeyID: Integer; aKeyValue: Word);
  end;

var
  // All games Keys accessible from everywhere
  gResKeys: TKMKeyLibrary;

implementation

{ TKMKeyLibrary }
constructor TKMKeyLibrary.Create;
begin
  FilePath := (ExeDir + 'data' + PathDelim + 'keys.keymap');
  if (not FileExists(FilePath)) then // If file is missing, create one here
    ResetKeyBind;
  inherited Create;
end;


// .keymap files consist of lines. Each line has an index and a text. Lines without index are skipped
procedure TKMKeyLibrary.LoadKeymapFile(var aArray: TIntegerArray);
  function KeyToArray(const Value: UnicodeString): TUnicodeStringArray;
  var
    Point, Start: PWideChar;
    KeyString: UnicodeString;
  begin
    SetLength(Result, 0);
    KeyCount := 0;
    Point := Pointer(Value);
    if Point = nil then Exit;

    // This is a lot faster than using StrPos/AnsiStrPos when
    // LineBreak is the default (#13#10)
    while Point^ <> #0 do
    begin
      Start := Point;
      while not KromUtils.CharInSet(Point^, [#0, #10, #13]) do Inc(Point);
      SetString(KeyString, Start, Point - Start);

      SetLength(Result, KeyCount + 1);
      Result[KeyCount] := KeyString;
      Inc(KeyCount);

      if Point^ = #13 then Inc(Point);
      if Point^ = #10 then Inc(Point);
    end;
  end;
var
  KeyStringArray: TUnicodeStringArray;
  KeyString, LibKey: UnicodeString;
  I, KeyID, KeyValue, TopId, FirstDelimiter, SecondDelimiter: Integer;
begin
  if not FileExists(FilePath) then Exit;

  // Load UniCode file with codepage
  LibKey := ReadTextU(FilePath, 1252);
  KeyStringArray := KeyToArray(LibKey);

  for I := High(KeyStringArray) downto 0 do
  begin
    FirstDelimiter := Pos(':', KeyStringArray[I]);
    if FirstDelimiter = 0 then Continue;

    if TryStrToInt(LeftStr(KeyStringArray[I], FirstDelimiter - 1), TopId) then
      Break;
  end;

  Assert(TopId <= 1024, 'Dont allow too many strings for no reason');

  // Don't shrink the array, we might be overloading
  if Length(aArray) < TopId + 1 then
    SetLength(aArray, TopId + 1);

  for I := 0 to High(KeyStringArray) do
  begin
    KeyString := KeyStringArray[I];
    // Get Key index and Value
    FirstDelimiter := Pos(':', KeyString);
    SecondDelimiter := Pos('//', KeyString);
    KeyID := StrToInt(Copy(KeyString, 0, FirstDelimiter - 1));
    KeyValue := StrToInt(Copy(KeyString, FirstDelimiter + 1, SecondDelimiter - FirstDelimiter -1));
    aArray[KeyID] := KeyValue;
  end;
end;


// Check if requested string is empty
function TKMKeyLibrary.HasKey(aIndex: Word): Boolean;
begin
  Result := ((aIndex < Length(fKeys)) and (fKeys[aIndex] <> 0));
end;


function TKMKeyLibrary.GetKeys(aIndex: Word): Integer;
begin
  if (aIndex < Length(fKeys)) and (fKeys[aIndex] <> 0) then
    Result := fKeys[aIndex]
  else
    Result := -1;
end;


procedure TKMKeyLibrary.LoadKeys;
begin
  LoadKeymapFile(fKeys);
end;


procedure TKMKeyLibrary.SaveKey(aKeyID: Integer; aKeyValue: Word);
var
  Keystring: string;
  KeyStringList: TStringList;
  I: Integer;
begin
  KeyStringList := TStringList.Create;
  {$IFDEF WDC}KeyStringList.DefaultEncoding := TEncoding.UTF8;{$ENDIF}
  fKeys[aKeyID] := aKeyValue;

  for I := 0 to KeyCount - 1 do
  begin
    Keystring := IntToStr(I) + ':'+ IntToStr(fKeys[I]) + '//' + GetNameForKey(I);
    KeyStringList.Add(Keystring);
  end;

  KeyStringList.SaveToFile(FilePath{$IFDEF WDC}, TEncoding.UTF8{$ENDIF});
  KeyStringList.Free;
end;


procedure TKMKeyLibrary.ResetKeyBind;
var
  Keystring: string;
  KeyStringList: TStringList;
  I, KeyValue: Integer;
begin
  KeyStringList := TStringList.Create;
  {$IFDEF WDC}KeyStringList.DefaultEncoding := TEncoding.UTF8;{$ENDIF}

  for I := 0 to KeyCount - 1 do
  begin
    case I of
      0:  KeyValue := 37;
      1:  KeyValue := 39;
      2:  KeyValue := 38;
      3:  KeyValue := 40;
      4:  KeyValue := 112;
      5:  KeyValue := 113;
      6:  KeyValue := 114;
      7:  KeyValue := 115;
      8:  KeyValue := 72;
      9:  KeyValue := 83;
      10: KeyValue := 76;
      11: KeyValue := 70;
      12: KeyValue := 88;
      13: KeyValue := 187;
      14: KeyValue := 189;
      15: KeyValue := 190;
      16: KeyValue := 188;
      17: KeyValue := 116;
      18: KeyValue := 117;
      19: KeyValue := 118;
      20: KeyValue := 119;
      21: KeyValue := 66;
      22: KeyValue := 80;
      23: KeyValue := 84;
      24: KeyValue := 34;
      25: KeyValue := 33;
      26: KeyValue := 8;
      27: KeyValue := 49;
      28: KeyValue := 50;
      29: KeyValue := 51;
      30: KeyValue := 52;
      31: KeyValue := 53;
      32: KeyValue := 54;
      33: KeyValue := 55;
      34: KeyValue := 56;
      35: KeyValue := 57;
      36: KeyValue := 48;
      37: KeyValue := 32;
      38: KeyValue := 46;
      39: KeyValue := 13;
      40: KeyValue := 27;
      41: KeyValue := 77;
      42: KeyValue := 86;
      43: KeyValue := 68;
      44: KeyValue := 67;
    else
      KeyValue := -1;
    end;
    Keystring := IntToStr(I) + ':' + IntToStr(KeyValue) + '//' + GetNameForKey(I);
    KeyStringList.Add(Keystring);
  end;

  KeyStringList.SaveToFile(FilePath{$IFDEF WDC}, TEncoding.UTF8{$ENDIF});
  KeyStringList.Free;
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

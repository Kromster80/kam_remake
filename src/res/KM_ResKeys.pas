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
    0: Result := gResTexts[TX_KEY_FUNC_SCROLL_LEFT];
    1: Result := gResTexts[TX_KEY_FUNC_SCROLL_RIGHT];
    2: Result := gResTexts[TX_KEY_FUNC_SCROLL_UP];
    3: Result := gResTexts[TX_KEY_FUNC_SCROLL_DOWN];
    4: Result := gResTexts[TX_KEY_FUNC_MENU_BUILD];//'Build Menu';
    5: Result := gResTexts[TX_KEY_FUNC_MENU_RATIO];//'Ratio Menu';
    6: Result := gResTexts[TX_KEY_FUNC_MENU_STATS];//'Stats Menu';
    7: Result := gResTexts[TX_KEY_FUNC_MENU_MAIN];//'Main Menu';
    8: Result := gResTexts[TX_KEY_FUNC_HALT];//'Halt Command';
    9: Result := gResTexts[TX_KEY_FUNC_SPLIT];//'Split Command';
    10: Result := gResTexts[TX_KEY_FUNC_LINKUP];//'Linkup Command';
    11: Result := gResTexts[TX_KEY_FUNC_FOOD];//'Food Command';
    12: Result := gResTexts[TX_KEY_FUNC_STORM];//'Storm Command';
    13: Result := gResTexts[TX_KEY_FUNC_FORM_INCREASE];//'Increase Formation';
    14: Result := gResTexts[TX_KEY_FUNC_FORM_DECREASE];//'Decrease Formation';
    15: Result := gResTexts[TX_KEY_FUNC_TURN_CW];//'Turn Clockwise';
    16: Result := gResTexts[TX_KEY_FUNC_TURN_CCW];//'Turn Counter-clockwise';
    17: Result := gResTexts[TX_KEY_FUNC_GAME_SPEED_1];//'Normal Game speed';
    18: Result := gResTexts[TX_KEY_FUNC_GAME_SPEED_2];//'Game speed x3';
    19: Result := gResTexts[TX_KEY_FUNC_GAME_SPEED_3];//'Game speed x6';
    20: Result := gResTexts[TX_KEY_FUNC_GAME_SPEED_4];//'Game speed x10';
    21: Result := gResTexts[TX_KEY_FUNC_BEACON];//'Beacon';
    22: Result := gResTexts[TX_KEY_FUNC_PAUSE];//'Pause';
    23: Result := gResTexts[TX_KEY_FUNC_SHOW_TEAMS];//'Show teams in MP';
    24: Result := gResTexts[TX_KEY_FUNC_ZOOM_IN];//'Zoom In';
    25: Result := gResTexts[TX_KEY_FUNC_ZOOM_OUT];//'Zoom Out';
    26: Result := gResTexts[TX_KEY_FUNC_ZOOM_RESET];//'Reset Zoom';
    27: Result := gResTexts[TX_KEY_FUNC_SELECT_1];//'Selection 1';
    28: Result := gResTexts[TX_KEY_FUNC_SELECT_2];//'Selection 2';
    29: Result := gResTexts[TX_KEY_FUNC_SELECT_3];//'Selection 3';
    30: Result := gResTexts[TX_KEY_FUNC_SELECT_4];//'Selection 4';
    31: Result := gResTexts[TX_KEY_FUNC_SELECT_5];//'Selection 5';
    32: Result := gResTexts[TX_KEY_FUNC_SELECT_6];//'Selection 6';
    33: Result := gResTexts[TX_KEY_FUNC_SELECT_7];//'Selection 7';
    34: Result := gResTexts[TX_KEY_FUNC_SELECT_8];//'Selection 8';
    35: Result := gResTexts[TX_KEY_FUNC_SELECT_9];//'Selection 9';
    36: Result := gResTexts[TX_KEY_FUNC_SELECT_10];//'Selection 10';
    37: Result := gResTexts[TX_KEY_FUNC_CENTER_ALERT];//'Center to latest alert';
    38: Result := gResTexts[TX_KEY_FUNC_DELETE_MSG];//'Delete message';
    39: Result := gResTexts[TX_KEY_FUNC_SHOW_GAME_CHAT];//'Show game chat in MP';
    40: Result := gResTexts[TX_KEY_FUNC_CLOSE_MENU];//'Close menu''s';
    41: Result := gResTexts[TX_KEY_FUNC_DBG_MAP];//'Debug Menu Map';
    42: Result := gResTexts[TX_KEY_FUNC_DBG_VICTORY];//'Debug Menu Victory';
    43: Result := gResTexts[TX_KEY_FUNC_DBG_DEFEAT];//'Debug Menu Defeat';
    44: Result := gResTexts[TX_KEY_FUNC_DBG_SCOUT];//'Debug Menu Add Scout';
    // Higher value to seporate bindable keys from special keys
    100: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_EXTRA];//'MapEdit Extra''s Menu';
    101: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_TERAIN_EDIT];//'MapEdit Terain Editing';
    102: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_VILLAGE_PLAN];//'MapEdit Village Planning';
    103: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_VISUAL_SCRIPT];//'MapEdit Visual Scripts';
    104: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_GLOBAL_SCRIPT];//'MapEdit Global Scripting';
    105: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_MENU_MAIN];//'MapEdit Main Menu';
    106: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_1];//'MapEdit Sub-menu 1';
    107: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_2];//'MapEdit Sub-menu 2';
    108: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_3];//'MapEdit Sub-menu 3';
    109: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_4];//'MapEdit Sub-menu 4';
    110: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_5];//'MapEdit Sub-menu 5';
    111: Result := gResTexts[TX_KEY_FUNC_MAPEDIT_SUBMENU_6];//'MapEdit Sub-menu 6';
    112: Result := gResTexts[TX_KEY_FUNC_UNASSIGNABLE];//'Unassignable Delphi';
    113: Result := gResTexts[TX_KEY_FUNC_UNASSIGNABLE];//'Unassignable Delphi';
  else
    Result := gResTexts[TX_KEY_FUNC_UNKNOWN] + ' ' + IntToStr(aValue) + '! ~~~';
  end;
end;


// Here we fix the issue where Char() doesn't always give us the character/button
function TKMKeyLibrary.GetCharFromVK(aKey: Word): String;
begin
  case aKey of
    1: Result := 'Left mouse button';
    2: Result := 'Right mouse button';
    3: Result := 'Control-break';
    4: Result := 'Middle mouse button';
    8: Result := 'Backspace';
    9: Result := 'Tab';
    12: Result := 'Clear';
    13: Result := 'Enter';
    16: Result := 'Shift';
    17: Result := 'CTRL';
    18: Result := 'Alt';
    19: Result := 'Pause';
    20: Result := 'Caps Lock';
    27: Result := 'Escape';
    32: Result := 'Space bar';
    33: Result := 'Page Up';
    34: Result := 'Page Down';
    35: Result := 'End';
    36: Result := 'Home';
    37: Result := 'Left Arrow';
    38: Result := 'Up Arrow';
    39: Result := 'Right Arrow';
    40: Result := 'Down Arrow';
    41: Result := 'Select';
    42: Result := 'Print';
    43: Result := 'Execute';
    44: Result := 'Print Screen';
    45: Result := 'Insert';
    46: Result := 'Delete';
    47: Result := 'Help';
    96: Result := 'NumPad 0';
    97: Result := 'NumPad 1';
    98: Result := 'NumPad 2';
    99: Result := 'NumPad 3';
    100: Result := 'NumPad 4';
    101: Result := 'NumPad 5';
    102: Result := 'NumPad 6';
    103: Result := 'NumPad 7';
    104: Result := 'NumPad 8';
    105: Result := 'NumPad 9';
    106: Result := 'NumPad *';
    107: Result := 'NumPad +';
    108: Result := 'Separator';
    109: Result := 'NumPad -';
    110: Result := 'NumPad .';
    111: Result := 'NumPad /';
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
    144: Result := 'Num Lock';
    145: Result := 'Scroll Lock';
    160: Result := 'Left Shift';
    161: Result := 'Right Shift';
    162: Result := 'Left CTRL';
    163: Result := 'Right CTRL';
    164: Result := 'Left Alt';
    165: Result := 'Right Alt';
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
    250: Result := 'Play';
    251: Result := 'Zoom';
  else
    Result := Char(aKey);
  end;
end;


end.

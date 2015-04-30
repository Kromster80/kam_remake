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
    0: Result := gResTexts[TX_KEYS_SCROLL_LEFT];
    1: Result := gResTexts[TX_KEYS_SCROLL_RIGHT];
    2: Result := gResTexts[TX_KEYS_SCROLL_UP];
    3: Result := gResTexts[TX_KEYS_SCROLL_DOWN];
    4: Result := 'Build Menu'; //TX_KEYS_MENU_BUILD
    5: Result := 'Ratio Menu'; //TX_KEYS_MENU_RATIO
    6: Result := 'Stats Menu'; //TX_KEYS_MENU_STATS
    7: Result := 'Main Menu'; //TX_KEYS_MENU_MAIN
    8: Result := 'Halt Command'; //TX_KEYS_HALT
    9: Result := 'Split Command'; //TX_KEYS_SPLIT
    10: Result := 'Linkup Command'; //TX_KEYS_LINKUP
    11: Result := 'Food Command'; ////TX_KEYS_FOOD
    12: Result := 'Storm Command'; //TX_KEYS_STORM
    13: Result := 'Increase Formation'; //TX_KEYS_FORM_INCREASE
    14: Result := 'Decrease Formation'; //TX_KEYS_FORM_DECREASE
    15: Result := 'Turn Clockwise'; //TX_KEYS_TURN_CW
    16: Result := 'Turn Counter-clockwise'; //TX_KEYS_TURN_CCW
    17: Result := 'Normal Game speed'; //TX_KEYS_GAME_SPEED_1
    18: Result := 'Game speed x3'; //TX_KEYS_GAME_SPEED_2
    19: Result := 'Game speed x6'; //TX_KEYS_GAME_SPEED_3
    20: Result := 'Game speed x10'; //TX_KEYS_GAME_SPEED_4
    21: Result := 'Beacon'; //TX_KEYS_BEACON
    22: Result := 'Pause'; //TX_KEYS_PAUSE
    23: Result := 'Show teams in MP'; //TX_KEYS_SHOW_TEAMS
    24: Result := 'Zoom In'; //TX_KEYS_ZOOM_IN
    25: Result := 'Zoom Out'; //TX_KEYS_ZOOM_OUT
    26: Result := 'Reset Zoom'; //TX_KEYS_ZOOM_RESET
    27: Result := 'Selection 1'; //TX_KEYS_SELECT_1
    28: Result := 'Selection 2'; //TX_KEYS_SELECT_2
    29: Result := 'Selection 3'; //TX_KEYS_SELECT_3
    30: Result := 'Selection 4'; //TX_KEYS_SELECT_4
    31: Result := 'Selection 5'; //TX_KEYS_SELECT_5
    32: Result := 'Selection 6'; //TX_KEYS_SELECT_6
    33: Result := 'Selection 7'; //TX_KEYS_SELECT_7
    34: Result := 'Selection 8'; //TX_KEYS_SELECT_8
    35: Result := 'Selection 9'; //TX_KEYS_SELECT_9
    36: Result := 'Selection 10'; //TX_KEYS_SELECT_10
    37: Result := 'Center to latest alert'; //TX_KEYS_CENTER_ALERT
    38: Result := 'Delete message'; //TX_KEYS_DELETE_MSG
    39: Result := 'Show game chat in MP'; //TX_KEYS_SHOW_GAME_CHAT
    40: Result := 'Close menu''s'; //TX_KEYS_CLOSE_MENU
    41: Result := 'Debug Menu Map'; //TX_KEYS_DBG_MAP
    42: Result := 'Debug Menu Victory'; //TX_KEYS_DBG_VICTORY
    43: Result := 'Debug Menu Defeat'; //TX_KEYS_DBG_DEFEAT
    44: Result := 'Debug Menu Add Scout'; //TX_KEYS_DBG_SCOUT
    // Higher value to seporate bindable keys from special keys
    100: Result := 'MapEdit Extra''s Menu'; //TX_KEYS_MAPEDIT_EXTRA
    101: Result := 'MapEdit Terain Editing'; //TX_KEYS_MAPEDIT_TERAIN_EDIT
    102: Result := 'MapEdit Village Planning'; //TX_KEYS_MAPEDIT_VILLAGE_PLAN
    103: Result := 'MapEdit Visual Scripts'; //TX_KEYS_MAPEDIT_VISUAL_SCRIPT
    104: Result := 'MapEdit Global Scripting'; //TX_KEYS_MAPEDIT_GLOBAL_SCRIPT
    105: Result := 'MapEdit Main Menu'; //TX_KEYS_MAPEDIT_MENU_MAIN
    106: Result := 'MapEdit Sub-menu 1'; //TX_KEYS_MAPEDIT_SUBMENU_1
    107: Result := 'MapEdit Sub-menu 2'; //TX_KEYS_MAPEDIT_SUBMENU_2
    108: Result := 'MapEdit Sub-menu 3'; //TX_KEYS_MAPEDIT_SUBMENU_3
    109: Result := 'MapEdit Sub-menu 4'; //TX_KEYS_MAPEDIT_SUBMENU_4
    110: Result := 'MapEdit Sub-menu 5'; //TX_KEYS_MAPEDIT_SUBMENU_5
    111: Result := 'MapEdit Sub-menu 6'; //TX_KEYS_MAPEDIT_SUBMENU_6
    112: Result := 'Unassignable Delphi'; //TX_KEYS_UNASSIGNABLE
    113: Result := 'Unassignable Delphi';
  else
    Result := '~~~ Unknown value ' + IntToStr(aValue) + '! ~~~';
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

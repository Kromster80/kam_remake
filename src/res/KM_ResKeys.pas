unit KM_ResKeys;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  {$IFDEF FPC} lconvencoding, LazUTF8, {$ENDIF}
  Classes, SysUtils, StrUtils, KromUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_FileIO;


const
  //Load key IDs from this include file
  {$I KM_KeyIDs.inc}

  //for missing Keys
  NO_KEY = nil;

type
  TKMKeyLibraryCommon = class
  private
    FILE_PATH: String;
    fKeyCount: Integer;
    procedure LoadKeymapFile(var aArray: TIntegerArray);
  end;


  TKMKeyLibraryMulti = class(TKMKeyLibraryCommon)
  private
    fPref: Integer;
    fKeys: TIntegerArray;
    function GetKeys(aIndex: Word): Integer;
  public
    KeyCount: Integer;
    constructor Create;
    procedure LoadKeys; //All locales for Mission strings
    function HasKey(aIndex: Word): Boolean;
    function GetCharFromVK(aKey: Word): String;
    function GetNameForKey(aValue: Integer): String;
    property Keys[aIndex: Word]: Integer read GetKeys; default;
    procedure Save(aStream: TKMemoryStream);
    procedure SaveKey(aKeyID, aKeyValue: Integer);
    procedure Load(aStream: TKMemoryStream);
  end;


var
  //All games Keys accessible from everywhere
  gResKeys: TKMKeyLibraryMulti;


implementation


{ TKMKeyLibraryCommon }
//keymap files consist of lines. Each line has an index and a text. Lines without index are skipped
procedure TKMKeyLibraryCommon.LoadKeymapFile(var aArray: TIntegerArray);
  function KeyToArray(const Value: String): TStringArray;
  var
    P, Start: PWideChar;
    S: String;
  begin
    SetLength(Result, 0);
    P := Pointer(Value);
    if P = nil then Exit;

    // This is a lot faster than using StrPos/AnsiStrPos when
    // LineBreak is the default (#13#10)
    while P^ <> #0 do
    begin
      Start := P;
      while not KromUtils.CharInSet(P^, [#0, #10, #13]) do Inc(P);
      SetString(S, Start, P - Start);

      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := S;

      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
  end;
var
  Tmp: TStringArray;
  s, libKey: String;
  I, id, topId, firstDelimiter: Integer;
  {$IFDEF FPC} tmpA: AnsiString; {$ENDIF}
begin
  FILE_PATH := (ExeDir + 'data' + PathDelim + 'keys.keymap');
  if not FileExists(FILE_PATH) then Exit;

  //Load ANSI file with codepage
  libKey := ReadTextA(FILE_PATH);
  Tmp := KeyToArray(libKey);
  fKeyCount := High(Tmp);

  for I := High(Tmp) downto 0 do
  begin
    firstDelimiter := Pos(':', Tmp[I]);
    if firstDelimiter = 0 then Continue;

    if TryStrToInt(LeftStr(Tmp[I], firstDelimiter - 1), topId) then
      Break;
  end;

  Assert(topId <= 1024, 'Dont allow too many strings for no reason');

  //Don't shrink the array, we might be overloading base locale with a partial translation
  if Length(aArray) < topId + 1 then
    SetLength(aArray, topId + 1);

  for I := 0 to High(Tmp) do
  begin
    s := Tmp[I];

    //Get string index and skip erroneous lines
    firstDelimiter := Pos(':', s);
    if firstDelimiter = 0 then Continue;
    if not TryStrToInt(TrimLeft(LeftStr(s, firstDelimiter - 1)), id) then Continue;

    s := RightStr(s, Length(s) - firstDelimiter);
    //Required characters that can't be stored in plain text
    //todo: Remove them in favor of | for eol (and update libx files)
    {$IFDEF WDC}
    s := StringReplace(s, '\n', EolW, [rfReplaceAll, rfIgnoreCase]); //EOL
    s := StringReplace(s, '\\', '\', [rfReplaceAll, rfIgnoreCase]); //Slash
    {$ENDIF}
    {$IFDEF FPC}
    //In FPC StringReplace only works for UTF8/Ansi strings
    tmpA := UTF16toUTF8(s);
    tmpA := StringReplace(tmpA, '\n', EolW, [rfReplaceAll, rfIgnoreCase]); //EOL
    tmpA := StringReplace(tmpA, '\\', '\', [rfReplaceAll, rfIgnoreCase]); //Slash
    s := UTF8toUTF16(tmpA);
    {$ENDIF}
    aArray[id] := StrToInt(s);
  end;
end;

{ TKMKeyLibraryMulti }
constructor TKMKeyLibraryMulti.Create;
begin
  inherited Create;

  fPref := 0;
end;


//Check if requested string is empty
function TKMKeyLibraryMulti.HasKey(aIndex: Word): Boolean;
begin
  Result := ((fPref <> -1) and (aIndex < Length(fKeys)) and (fKeys[aIndex] <> 0));
end;


function TKMKeyLibraryMulti.GetKeys(aIndex: Word): Integer;
begin
  if (fPref <> -1) and (aIndex < Length(fKeys)) and (fKeys[aIndex] <> 0) then
    Result := fKeys[aIndex]
  //else
    //Result := '~~~Key ' + IntToStr(aIndex) + ' out of range!~~~';
end;


procedure TKMKeyLibraryMulti.LoadKeys;
begin
  LoadKeymapFile(fKeys);
  KeyCount := fKeyCount;
end;


procedure TKMKeyLibraryMulti.Save(aStream: TKMemoryStream);
var
  K: Integer;
  aKeyCount: Integer;
begin
  //Only save values containing keys
  aStream.Write(1);
  if Length(fKeys) > 0 then
  begin
    aStream.WriteA('keys');

    aKeyCount := Length(fKeys);

    aStream.Write(aKeyCount);
    for K := 0 to aKeyCount - 1 do
      aStream.Write(fKeys[K]);
  end;
end;


procedure TKMKeyLibraryMulti.SaveKey(aKeyID, aKeyValue: Integer);
var
  SL: TStringList;
  I: Integer;
  s: string;
begin
  SL := TStringList.Create;
  SL.DefaultEncoding := TEncoding.UTF8;

  for I := 0 to KeyCount do
    if (fKeys[I] = aKeyID) then
    begin
      s := IntToStr(I) + ':'+ IntToStr(aKeyValue);
      s := StringReplace(s, '\', '\\', [rfReplaceAll, rfIgnoreCase]); //Slash
      s := StringReplace(s, #13#10, '\n', [rfReplaceAll, rfIgnoreCase]); //EOL
      SL.Add(s);
    end else
    begin
      s := IntToStr(I) + ':'+ IntToStr(fKeys[I]);
      s := StringReplace(s, '\', '\\', [rfReplaceAll, rfIgnoreCase]); //Slash
      s := StringReplace(s, #13#10, '\n', [rfReplaceAll, rfIgnoreCase]); //EOL
      SL.Add(s);
    end;

  //Don't create blank files for unused translations
  if (SL.Count > 0) or FileExists(FILE_PATH) then
    SL.SaveToFile(FILE_PATH);
  SL.Free;
end;


procedure TKMKeyLibraryMulti.Load(aStream: TKMemoryStream);
var
  I,K: Integer;
  KCount, aKeyCount: Integer;
  curK: AnsiString;
  Tmp: String;
begin
  //Try to match savegame locales with players locales,
  //cos some players might have non-native locales missing
  //We might add locale selection to setup.exe

  SetLength(fKeys, 1);

  aStream.Read(KCount);
  for I := 0 to KCount - 1 do
  begin
    aStream.ReadA(curK);
    aStream.Read(aKeyCount);

    SetLength(fKeys, aKeyCount);
    for K := 0 to aKeyCount - 1 do
      aStream.Read(fKeys[K]);
  end;
end;


// Here we fix the issue where Char() doesn't always give us the character/button
function TKMKeyLibraryMulti.GetCharFromVK(aKey: Word): String;
begin
  case aKey of
    VK_LBUTTON: Result := 'Left mouse button';
    VK_RBUTTON: Result := 'Right mouse button';
    VK_CANCEL: Result := 'Control-break';
    VK_MBUTTON: Result := 'Middle mouse button';
    VK_BACK: Result := 'Backspace';
    VK_TAB: Result := 'Tab';
    VK_CLEAR: Result := 'Clear';
    VK_RETURN: Result := 'Enter';
    VK_SHIFT: Result := 'Shift';
    VK_CONTROL: Result := 'CTRL';
    VK_MENU: Result := 'Alt';
    VK_PAUSE: Result := 'Pause';
    VK_CAPITAL: Result := 'Caps Lock';
    VK_ESCAPE: Result := 'Escape';
    VK_SPACE: Result := 'Space bar';
    VK_PRIOR: Result := 'Page Up';
    VK_NEXT: Result := 'Page Down';
    VK_END: Result := 'End';
    VK_HOME: Result := 'Home';
    VK_LEFT: Result := 'Left Arrow';
    VK_UP: Result := 'Up Arrow';
    VK_RIGHT: Result := 'Right Arrow';
    VK_DOWN: Result := 'Down Arrow';
    VK_SELECT: Result := 'Select';
    VK_PRINT: Result := 'Print';
    VK_EXECUTE: Result := 'Execute';
    VK_SNAPSHOT: Result := 'Print Screen';
    VK_INSERT: Result := 'Insert';
    VK_DELETE: Result := 'Delete';
    VK_HELP: Result := 'Help';
    VK_NUMPAD0: Result := 'Num 0';
    VK_NUMPAD1: Result := 'Num 1';
    VK_NUMPAD2: Result := 'Num 2';
    VK_NUMPAD3: Result := 'Num 3';
    VK_NUMPAD4: Result := 'Num 4';
    VK_NUMPAD5: Result := 'Num 5';
    VK_NUMPAD6: Result := 'Num 6';
    VK_NUMPAD7: Result := 'Num 7';
    VK_NUMPAD8: Result := 'Num 8';
    VK_NUMPAD9: Result := 'Num 9';
    VK_SEPARATOR: Result := 'Separator';
    VK_SUBTRACT: Result := 'Num -';
    VK_DECIMAL: Result := 'Num .';
    VK_DIVIDE: Result := 'Num /';
    VK_F1: Result := 'F1';
    VK_F2: Result := 'F2';
    VK_F3: Result := 'F3';
    VK_F4: Result := 'F4';
    VK_F5: Result := 'F5';
    VK_F6: Result := 'F6';
    VK_F7: Result := 'F7';
    VK_F8: Result := 'F8';
    VK_F9: Result := 'F9';
    VK_F10: Result := 'F10';
    VK_F11: Result := 'F11';
    VK_F12: Result := 'F12';
    VK_F13: Result := 'F13';
    VK_F14: Result := 'F14';
    VK_F15: Result := 'F15';
    VK_F16: Result := 'F16';
    VK_F17: Result := 'F17';
    VK_F18: Result := 'F18';
    VK_F19: Result := 'F19';
    VK_F20: Result := 'F20';
    VK_F21: Result := 'F21';
    VK_F22: Result := 'F22';
    VK_F23: Result := 'F23';
    VK_F24: Result := 'F24';
    VK_NUMLOCK: Result := 'Num Lock';
    VK_SCROLL: Result := 'Scroll Lock';
    VK_LSHIFT: Result := 'Left Shift';
    VK_RSHIFT: Result := 'Right Shift';
    VK_LCONTROL: Result := 'Left CTRL';
    VK_RCONTROL: Result := 'Right CTRL';
    VK_LMENU: Result := 'Left Alt';
    VK_RMENU: Result := 'Right Alt';
    VK_PLAY: Result := 'Play';
    VK_ZOOM: Result := 'Zoom';
    106: Result := 'Num *';
    107: Result := 'Num +';
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
  else
    Result := Char(aKey);
  end;
end;


// Here we define the action name values
function TKMKeyLibraryMulti.GetNameForKey(aValue: Integer): String;
begin
  case aValue of
    0: Result := 'Scroll Left';
    1: Result := 'Scroll Right';
    2: Result := 'Scroll Up';
    3: Result := 'Scroll Down';
    4: Result := 'Build Menu';
    5: Result := 'Ratio Menu';
    6: Result := 'Stats Menu';
    7: Result := 'Main Menu';
    8: Result := 'Halt Command';
    9: Result := 'Split Command';
    10: Result := 'Linkup Command';
    11: Result := 'Food Command';
    12: Result := 'Storm Command';
    13: Result := 'Increase Formation';
    14: Result := 'Decrease Formation';
    15: Result := 'Turn Clockwise';
    16: Result := 'Turn Counter-clockwise';
    17: Result := 'Debug Menu Map';
    18: Result := 'Debug Menu Victory';
    19: Result := 'Debug Menu Defeat';
    20: Result := 'Debug Menu Add Scout';
    21: Result := 'Beacon';
    22: Result := 'Pause';
    23: Result := 'Show teams in MP';
    24: Result := 'Zoom In';
    25: Result := 'Zoom Out';
    26: Result := 'Reset Zoom';
    27: Result := 'Selection 1';
    28: Result := 'Selection 2';
    29: Result := 'Selection 3';
    30: Result := 'Selection 4';
    31: Result := 'Selection 5';
    32: Result := 'Selection 6';
    33: Result := 'Selection 7';
    34: Result := 'Selection 8';
    35: Result := 'Selection 9';
    36: Result := 'Selection 10';
  else
    Result := '~~~ Unknown value ' + IntToStr(aValue) + '! ~~~';
  end;
end;


end.

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
  // Load key IDs from this include file
  {$I KM_KeyIDs.inc}

  // For missing Keys
  NO_KEY = nil;

type
  TKMKeyLibraryMulti = class
  private
    fKeys: TIntegerArray;
    FILE_PATH: String;
    fKeyCount: Integer;
    procedure LoadKeymapFile(var aArray: TIntegerArray);
    function GetKeys(aIndex: Word): Integer;
  public
    KeyCount: Integer;
    constructor Create;
    procedure LoadKeys; // Load all the keys
    function HasKey(aIndex: Word): Boolean;
    function GetCharFromVK(aKey: Word): String;
    property Keys[aIndex: Word]: Integer read GetKeys; default;
    procedure SaveKey(aKeyID: Integer; aKeyValue: Word);
  end;

var
  // All games Keys accessible from everywhere
  gResKeys: TKMKeyLibraryMulti;

implementation

{ TKMKeyLibraryMulti }
constructor TKMKeyLibraryMulti.Create;
begin
  inherited Create;
end;


// .keymap files consist of lines. Each line has an index and a text. Lines without index are skipped
procedure TKMKeyLibraryMulti.LoadKeymapFile(var aArray: TIntegerArray);
  function KeyToArray(const Value: UnicodeString): TUnicodeStringArray;
  var
    P, Start: PWideChar;
    S: UnicodeString;
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
  Tmp: TUnicodeStringArray;
  s, libKey: UnicodeString;
  I, id, topId, firstDelimiter: Integer;
  {$IFDEF FPC} tmpA: AnsiString; {$ENDIF}
begin
  FILE_PATH := (ExeDir + 'data' + PathDelim + 'keys.keymap');
  if not FileExists(FILE_PATH) then Exit;

  // Load UniCode file with codepage
  libKey := ReadTextU(FILE_PATH, 1252);
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

  // Don't shrink the array, we might be overloading
  if Length(aArray) < topId + 1 then
    SetLength(aArray, topId + 1);

  for I := 0 to High(Tmp) do
  begin
    s := Tmp[I];

    // Get Key index and skip erroneous lines
    firstDelimiter := Pos(':', s);
    if firstDelimiter = 0 then Continue;
    if not TryStrToInt(TrimLeft(LeftStr(s, firstDelimiter - 1)), id) then Continue;

    s := RightStr(s, Length(s) - firstDelimiter);
    // Required characters that can't be stored in plain text
    //todo: Remove them in favor of | for eol (and update libx files)
    {$IFDEF WDC}
    s := StringReplace(s, '\n', EolW, [rfReplaceAll, rfIgnoreCase]); // EOL
    s := StringReplace(s, '\\', '\', [rfReplaceAll, rfIgnoreCase]); // Slash
    {$ENDIF}
    {$IFDEF FPC}
    // In FPC StringReplace only works for UTF8/Ansi strings
    tmpA := UTF16toUTF8(s);
    tmpA := StringReplace(tmpA, '\n', EolW, [rfReplaceAll, rfIgnoreCase]); // EOL
    tmpA := StringReplace(tmpA, '\\', '\', [rfReplaceAll, rfIgnoreCase]); // Slash
    s := UTF8toUTF16(tmpA);
    {$ENDIF}
    aArray[id] := StrToInt(s);
  end;
end;


// Check if requested string is empty
function TKMKeyLibraryMulti.HasKey(aIndex: Word): Boolean;
begin
  Result := ((0 <> -1) and (aIndex < Length(fKeys)) and (fKeys[aIndex] <> 0));
end;


function TKMKeyLibraryMulti.GetKeys(aIndex: Word): Integer;
begin
  if (0 <> -1) and (aIndex < Length(fKeys)) and (fKeys[aIndex] <> 0) then
    Result := fKeys[aIndex]
  else
    Result := -1;
end;


procedure TKMKeyLibraryMulti.LoadKeys;
begin
  fKeys := nil;
  LoadKeymapFile(fKeys);
  KeyCount := fKeyCount;
end;


procedure TKMKeyLibraryMulti.SaveKey(aKeyID: Integer; aKeyValue: Word);
var
  SL: TStringList;
  I: Integer;
  s: string;
begin
  SL := TStringList.Create;
  {$IFDEF WDC}SL.DefaultEncoding := TEncoding.UTF8;{$ENDIF}

  for I := 0 to KeyCount do
    if (I = aKeyID) then
    begin
      s := IntToStr(I) + ':'+ IntToStr(aKeyValue);
      SL.Add(s);
    end else
    begin
      s := IntToStr(I) + ':'+ IntToStr(fKeys[I]);
      SL.Add(s);
    end;

  // Don't create blank files for unused Keys
  if (SL.Count > 0) or FileExists(FILE_PATH) then
    SL.SaveToFile(FILE_PATH{$IFDEF WDC}, TEncoding.UTF8{$ENDIF});
  SL.Free;
end;


// Here we fix the issue where Char() doesn't always give us the character/button
function TKMKeyLibraryMulti.GetCharFromVK(aKey: Word): String;
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

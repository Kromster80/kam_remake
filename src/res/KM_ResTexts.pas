unit KM_ResTexts;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF FPC} lconvencoding, LazUTF8, {$ENDIF}
  Classes, SysUtils, StrUtils, KromUtils,
  KM_CommonClasses, KM_FileIO, KM_ResLocales;


const
  // NAME__## means that this const defines first element of some range that is ## long
  // TX_UNITS_NAMES__29 = 70; //todo: add animal unit names

  // Load text IDs from this include file that is managed by the Translation Manager
  {$I KM_TextIDs.inc}

  // Supposed for places where some text must be placed
  // That string was used in all Synetic games for missing texts
  NO_TEXT = '<<<LEER>>>';

type
  TKMTextLibraryCommon = class
  private
    procedure LoadLIBXFile(FilePath: string; var aArray: TUnicodeStringArray);
  end;


  TKMTextLibrarySingle = class(TKMTextLibraryCommon)
  private
    fTexts: TUnicodeStringArray;
    function GetTexts(aIndex: Word): UnicodeString;
  public
    procedure LoadLocale(aPathTemplate: string); // Initial locale for UI strings
    property Texts[aIndex: Word]: UnicodeString read GetTexts; default;
  end;


  TKMTextLibraryMulti = class(TKMTextLibraryCommon)
  private
    fPref: array [0..2] of Integer;

    fTexts: array of TUnicodeStringArray;
    function GetTexts(aIndex: Word): UnicodeString;
    procedure InitLocaleIds;
  public
    constructor Create;
    procedure LoadLocale(aPathTemplate: string); // All locales for Mission strings
    function ParseTextMarkup(const aText: UnicodeString; aTagSym: Char): UnicodeString;
    function HasText(aIndex: Word): Boolean;
    function GetCharFromVK(aKey: Word): String;
    function GetNameForKey(aValue: Integer): String;
    property Texts[aIndex: Word]: UnicodeString read GetTexts; default;
    procedure Save(aStream: TKMemoryStream);
    procedure Load(aStream: TKMemoryStream);
  end;


var
  // All games texts accessible from everywhere
  gResTexts: TKMTextLibraryMulti;

implementation

{ TKMTextLibraryCommon }
// LIBX files consist of lines. Each line has an index and a text. Lines without index are skipped
procedure TKMTextLibraryCommon.LoadLIBXFile(FilePath: string; var aArray: TUnicodeStringArray);
  function TextToArray(const Value: UnicodeString): TUnicodeStringArray;
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
  langCode: AnsiString;
  libTxt: UnicodeString;
  I: Integer;
  s: UnicodeString;
  firstDelimiter: Integer;
  id, topId: Integer;
  {$IFDEF FPC} tmpA: AnsiString; {$ENDIF}
begin
  if not FileExists(FilePath) then Exit;

  // Load ANSI file with codepage we say into unicode string
  langCode := AnsiString(Copy(FilePath, Length(FilePath) - 7, 3));
  libTxt := ReadTextU(FilePath, gResLocales.LocaleByCode(langCode).FontCodepage);
  Tmp := TextToArray(libTxt);

  for I := High(Tmp) downto 0 do
  begin
    firstDelimiter := Pos(':', Tmp[I]);
    if firstDelimiter = 0 then Continue;

    if TryStrToInt(LeftStr(Tmp[I], firstDelimiter - 1), topId) then
      Break;
  end;

  Assert(topId <= 1024, 'Dont allow too many strings for no reason');

  // Don't shrink the array, we might be overloading base locale with a partial translation
  if Length(aArray) < topId + 1 then
    SetLength(aArray, topId + 1);

  for I := 0 to High(Tmp) do
  begin
    s := Tmp[I];

    // Get string index and skip erroneous lines
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
    aArray[id] := s;
  end;
end;


function TKMTextLibrarySingle.GetTexts(aIndex: Word): UnicodeString;
begin
  if aIndex < Length(fTexts) then
    Result := fTexts[aIndex]
  else
    Result := '~~~String ' + IntToStr(aIndex) + ' out of range!~~~';
end;


// Text file template, e.g.: ExeDir\text.%s.libx
// We need locale separate to assemble Fallback and Default locales paths
procedure TKMTextLibrarySingle.LoadLocale(aPathTemplate: string);
begin
  // We load the English LIBX by default, then overwrite it with the selected language
  // (this way missing strings are in English)
  LoadLIBXFile(Format(aPathTemplate, [gResLocales.DefaultLocale]), fTexts);

  if gResLocales.FallbackLocale <> '' then
    LoadLIBXFile(Format(aPathTemplate, [gResLocales.FallbackLocale]), fTexts);

  LoadLIBXFile(Format(aPathTemplate, [gResLocales.UserLocale]), fTexts);
end;


{ TKMTextLibraryMulti }
constructor TKMTextLibraryMulti.Create;
begin
  inherited Create;

  InitLocaleIds;
end;


procedure TKMTextLibraryMulti.InitLocaleIds;
begin
  // Using indexes is fatsre than always looking them up for every string requested
  fPref[0] := gResLocales.IndexByCode(gResLocales.UserLocale);
  fPref[1] := gResLocales.IndexByCode(gResLocales.FallbackLocale);
  fPref[2] := gResLocales.IndexByCode(gResLocales.DefaultLocale);
end;


// Check if requested string is empty
function TKMTextLibraryMulti.HasText(aIndex: Word): Boolean;
begin
  Result := ((fPref[0] <> -1) and (aIndex < Length(fTexts[fPref[0]])) and (fTexts[fPref[0], aIndex] <> ''))
         or ((fPref[1] <> -1) and (aIndex < Length(fTexts[fPref[1]])) and (fTexts[fPref[1], aIndex] <> ''))
         or ((fPref[2] <> -1) and (aIndex < Length(fTexts[fPref[2]])) and (fTexts[fPref[2], aIndex] <> ''));
end;


// Order of preference: Locale > Fallback > Default(Eng)
// Some locales may have no strings at all, just skip them
function TKMTextLibraryMulti.GetTexts(aIndex: Word): UnicodeString;
begin
  if (fPref[0] <> -1) and (aIndex < Length(fTexts[fPref[0]])) and (fTexts[fPref[0], aIndex] <> '') then
    Result := fTexts[fPref[0], aIndex]
  else
  if (fPref[1] <> -1) and (aIndex < Length(fTexts[fPref[1]])) and (fTexts[fPref[1], aIndex] <> '') then
    Result := fTexts[fPref[1], aIndex]
  else
  if (fPref[2] <> -1) and (aIndex < Length(fTexts[fPref[2]])) and (fTexts[fPref[2], aIndex] <> '') then
    Result := fTexts[fPref[2], aIndex]
  else
    Result := '~~~String ' + IntToStr(aIndex) + ' out of range!~~~';
end;


// Path template with %s
procedure TKMTextLibraryMulti.LoadLocale(aPathTemplate: string);
var
  I: Integer;
begin
  SetLength(fTexts, gResLocales.Count);

  for I := 0 to gResLocales.Count - 1 do
    LoadLIBXFile(Format(aPathTemplate, [gResLocales[I].Code]), fTexts[I]);
end;


// Dynamic Scripts should not have access to the actual strings (script variables should be identical for all MP players)
// Take the string and replace every occurence of <$tag> with corresponding text from LibX
function TKMTextLibraryMulti.ParseTextMarkup(const aText: UnicodeString; aTagSym: Char): UnicodeString;
var
  I, ID, Last: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(aText) do
  begin
    if (I + 3 <= Length(aText)) and (aText[I] = '<') and (aText[I+1] = aTagSym) then
    begin
      Last := PosEx('>', aText, I);
      ID := StrToIntDef(Copy(aText, I+2, Last-(I+2)), -1);
      if ID >= 0 then
      begin
        Result := Result + Texts[ID];
        I := Last + 1;
        Continue;
      end;
    end;
    Result := Result + aText[I];
    Inc(I);
  end;
end;


procedure TKMTextLibraryMulti.Save(aStream: TKMemoryStream);

  function LocalesWithText: Integer;
  var I: Integer;
  begin
    Result := 0;
    for I := 0 to gResLocales.Count - 1 do
      if Length(fTexts[I]) > 0 then
        Inc(Result);
  end;

var
  I,K: Integer;
  TextCount: Integer;
begin
  // Only save locales containing text (otherwise locale list must be synced in MP)
  aStream.Write(LocalesWithText);
  for I := 0 to gResLocales.Count - 1 do
    if Length(fTexts[I]) > 0 then
    begin
      aStream.WriteA(gResLocales[I].Code);

      TextCount := Length(fTexts[I]);

      aStream.Write(TextCount);
      for K := 0 to TextCount - 1 do
        aStream.WriteW(fTexts[I,K]);
    end;
end;


procedure TKMTextLibraryMulti.Load(aStream: TKMemoryStream);
var
  I,K: Integer;
  LocCount, TextCount: Integer;
  curLoc: AnsiString;
  Id: Integer;
  Tmp: UnicodeString;
begin
  // Try to match savegame locales with players locales,
  // because some players might have non-native locales missing
  // We might add locale selection to setup.exe

  SetLength(fTexts, gResLocales.Count);

  aStream.Read(LocCount);
  for I := 0 to LocCount - 1 do
  begin
    aStream.ReadA(curLoc);
    Id := gResLocales.IndexByCode(curLoc);

    aStream.Read(TextCount);

    if Id <> -1 then
    begin
      SetLength(fTexts[Id], TextCount);
      for K := 0 to TextCount - 1 do
        aStream.ReadW(fTexts[Id,K]);
    end
    else
    begin
      for K := 0 to TextCount - 1 do
        aStream.ReadW(Tmp);
    end;
  end;

  InitLocaleIds;
end;


// Here we define the action name values
function TKMTextLibraryMulti.GetNameForKey(aValue: Integer): String;
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
    17: Result := 'Normal Game speed';
    18: Result := 'Game speed x3';
    19: Result := 'Game speed x6';
    20: Result := 'Game speed x10';
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
    37: Result := 'Center to latest alert';
    38: Result := 'Delete message';
    39: Result := 'Show game chat in MP';
    40: Result := 'Close menu''s';
    41: Result := 'Debug Menu Map';
    42: Result := 'Debug Menu Victory';
    43: Result := 'Debug Menu Defeat';
    44: Result := 'Debug Menu Add Scout';
    // Higher value to seporate bindable keys from special keys
    100: Result := 'MapEdit Extra''s Menu';
    101: Result := 'MapEdit Terain Editing';
    102: Result := 'MapEdit Village Planning';
    103: Result := 'MapEdit Visual Scripts';
    104: Result := 'MapEdit Global Scripting';
    105: Result := 'MapEdit Main Menu';
    106: Result := 'MapEdit Sub-menu 1';
    107: Result := 'MapEdit Sub-menu 2';
    108: Result := 'MapEdit Sub-menu 3';
    109: Result := 'MapEdit Sub-menu 4';
    110: Result := 'MapEdit Sub-menu 5';
    111: Result := 'MapEdit Sub-menu 6';
    112: Result := 'Unassignable Delphi';
    113: Result := 'Unassignable Delphi';
  else
    Result := '~~~ Unknown value ' + IntToStr(aValue) + '! ~~~';
  end;
end;


// Here we fix the issue where Char() doesn't always give us the character/button
function TKMTextLibraryMulti.GetCharFromVK(aKey: Word): String;
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

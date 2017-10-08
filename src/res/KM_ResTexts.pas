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
    procedure LoadLIBXFile(const FilePath: string; var aArray: TUnicodeStringArray);
  end;


  TKMTextLibrarySingle = class(TKMTextLibraryCommon)
  private
    fTexts: TUnicodeStringArray;
    function GetTexts(aIndex: Word): UnicodeString;
  public
    procedure LoadLocale(const aPathTemplate: string); // Initial locale for UI strings
    property Texts[aIndex: Word]: UnicodeString read GetTexts; default;
  end;


  TKMTextLibraryMulti = class(TKMTextLibraryCommon)
  private
    fPref: array [0..2] of Integer;

    fTexts: array of TUnicodeStringArray;
    function GetTexts(aIndex: Word): UnicodeString;
    function GetDefaultTexts(aIndex: Word): UnicodeString;
    procedure InitLocaleIds;
    function DoParseTextMarkup(const aText: UnicodeString; aTagSym: Char): UnicodeString;
  public
    constructor Create;
    procedure LoadLocale(const aPathTemplate: string); // All locales for Mission strings
    function ParseTextMarkup(const aText: UnicodeString): UnicodeString; overload;
    function ParseTextMarkup(const aText: UnicodeString; aParams: array of const): UnicodeString; overload;
    function HasText(aIndex: Word): Boolean;
    property Texts[aIndex: Word]: UnicodeString read GetTexts; default;
    property DefaultTexts[aIndex: Word]: UnicodeString read GetDefaultTexts;
    procedure Save(aStream: TKMemoryStream);
    procedure Load(aStream: TKMemoryStream);
  end;


var
  // All games texts accessible from everywhere
  gResTexts: TKMTextLibraryMulti;

implementation

{ TKMTextLibraryCommon }
// LIBX files consist of lines. Each line has an index and a text. Lines without index are skipped
procedure TKMTextLibraryCommon.LoadLIBXFile(const FilePath: string; var aArray: TUnicodeStringArray);
  function TextToArray(const aText: UnicodeString): TUnicodeStringArray;
  var
    P, Start: PWideChar;
    S: UnicodeString;
  begin
    SetLength(Result, 0);
    P := Pointer(aText);
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
procedure TKMTextLibrarySingle.LoadLocale(const aPathTemplate: string);
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


// Returns in text default locale
function TKMTextLibraryMulti.GetDefaultTexts(aIndex: Word): UnicodeString;
begin
  if (fPref[2] <> -1) and (aIndex < Length(fTexts[fPref[2]])) and (fTexts[fPref[2], aIndex] <> '') then
    Result := fTexts[fPref[2], aIndex]
  else
    Result := '~~~String ' + IntToStr(aIndex) + ' out of range!~~~';
end;


// Path template with %s
procedure TKMTextLibraryMulti.LoadLocale(const aPathTemplate: string);
var
  I: Integer;
begin
  SetLength(fTexts, gResLocales.Count);

  for I := 0 to gResLocales.Count - 1 do
    LoadLIBXFile(Format(aPathTemplate, [gResLocales[I].Code]), fTexts[I]);
end;


// Dynamic Scripts should not have access to the actual strings (script variables should be identical for all MP players)
// Take the string and replace every occurence of <$tag> with corresponding text from LibX
// - aTagSym says which tags should be replaced ($ for missions, % for game texts)
function TKMTextLibraryMulti.DoParseTextMarkup(const aText: UnicodeString; aTagSym: Char): UnicodeString;
var
  I, ID, Last: Integer;
begin
  Assert((aTagSym = '$') or (aTagSym = '%'));

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


function TKMTextLibraryMulti.ParseTextMarkup(const aText: UnicodeString): UnicodeString;
begin
  Assert(Self <> gResTexts, 'Only missions so far can do text parsing');

  Result := DoParseTextMarkup(aText, '$');
  Result := gResTexts.DoParseTextMarkup(Result, '%');
end;


function TKMTextLibraryMulti.ParseTextMarkup(const aText: UnicodeString; aParams: array of const): UnicodeString;
begin
  Result := ParseTextMarkup(Format(ParseTextMarkup(aText), aParams));
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


end.

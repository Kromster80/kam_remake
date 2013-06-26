unit KM_TextLibrary;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, StrUtils, KromUtils, KM_Defaults;


const
  //NAME__## means that this const defines first element of some range that is ## long
  //TX_UNITS_NAMES__29 = 70; //todo: add animal unit names

  //Load text IDs from this include file that is managed by the Translation Manager
  {$I KM_TextIDs.inc}

  //Supposed for places where some text must be placed
  //That string was used in all Synetic games for missing texts
  NO_TEXT = '<<<LEER>>>';

type
  {$IFDEF UNICODE}
    TKMStringArray = TStringArray;
  {$ENDIF}
  {$IFNDEF UNICODE}
    TKMStringArray = TAnsiStringArray;
  {$ENDIF}

  TKMTextLibrary = class
  private
    fLocale: AnsiString;
    fFallbackLocale: AnsiString;
    GameStrings: TKMStringArray;
    MissionStrings: TKMStringArray; //Strings used in a mission
    procedure LoadLIBXFile(FilePath: string; aFirstIndex: Word; var aArray: TKMStringArray; aOverwrite: Boolean);
    procedure ExportTextLibrary(aLibrary: TKMStringArray; aFileName: string);
    function GetTexts(aIndex: Word): string;
  public
    constructor Create(aLibPath: string; aLocale: AnsiString);

    function AppendCampaign(aFileName: string): Word;
    procedure LoadMissionStrings(aFileName: string);

    function GetMissionString(aIndex: Word): string;
    function ParseTextMarkup(const aText: string): string;

    property Texts[aIndex: Word]: string read GetTexts; default;

    procedure ExportTextLibraries;
  end;


var
  fTextLibrary: TKMTextLibrary;


implementation
uses KM_Log, KM_Locales;


{ TTextLibrary }
constructor TKMTextLibrary.Create(aLibPath: string; aLocale: AnsiString);
begin
  inherited Create;

  //Remember preferred locale, it will remain constant until reinit
  fLocale := aLocale;
  fFallbackLocale := fLocales.GetLocale(aLocale).FallbackLocale;

  //We load the English LIBX by default, then overwrite it with the selected language (this way missing strings are in English)
  LoadLIBXFile(aLibPath+'text.'+DEFAULT_LOCALE+'.libx', 0, GameStrings, False); //Initialize with English strings

  if (fFallbackLocale <> '') and FileExists(aLibPath+'text.'+fFallbackLocale+'.libx') then
    LoadLIBXFile(aLibPath+'text.'+fFallbackLocale+'.libx', 0, GameStrings, True);

  if (fLocale <> DEFAULT_LOCALE) and FileExists(aLibPath+'text.'+fLocale+'.libx') then
    LoadLIBXFile(aLibPath+'text.'+fLocale+'.libx', 0, GameStrings, True); //Overwrite with selected locale

  gLog.AddTime('TextLib init done');
end;


{LIBX files consist of lines. Each line has an index and a text. Lines without index are skipped}
procedure TKMTextLibrary.LoadLIBXFile(FilePath: string; aFirstIndex: Word; var aArray: TKMStringArray; aOverwrite: Boolean);
  {$IFDEF UNICODE}
  function GetCodepage(aLang: string): Word;
  begin
    //Using slower but more compact comparisons
    if Pos(aLang, 'bel,rus,bul,ukr') <> 0 then
      Result := 1251
    else if Pos(aLang, 'pol,hun,cze,svk,rom') <> 0 then
      Result := 1250
    else if Pos(aLang, 'tur') <> 0 then
      Result := 1254
    else if Pos(aLang, 'lit,lat') <> 0 then
      Result := 1257
    else if Pos(aLang, 'eng,spa,ita,nor,chn,dut,est,ptb,fre,ger,jpn,swe') <> 0 then
      Result := 1252
    else
      Result := 1252;
  end;
  {$ENDIF}
var
  aStringList: TStringList;
  {$IFDEF UNICODE}
  lang: string;
  {$ENDIF}
  I: Integer;
  s: string;
  firstDelimiter: Integer;
  ID, MaxID: Integer;
begin
  if not FileExists(FilePath) then Exit;

  aStringList := TStringList.Create;
  {$IFDEF UNICODE}
    //Load in right encoding
    lang := Copy(FilePath, Length(FilePath) - 7, 3);
    aStringList.DefaultEncoding := TEncoding.GetEncoding(GetCodepage(fLocale));
  {$ENDIF}
  aStringList.LoadFromFile(FilePath);

  //First line is empty or comment and could have first 3 bytes Unicode Byte-Order Mark (BOM)
  s := aStringList[1];
  if Copy(s, 1, 6) <> 'MaxID:' then Exit;

  firstDelimiter := Pos(':', s);
  if not TryStrToInt(RightStr(s, Length(s) - firstDelimiter), MaxID) then Exit;

  if not aOverwrite then
    SetLength(aArray, aFirstIndex + MaxID + 1);

  for I := 0 to aStringList.Count - 1 do
  begin
    s := aStringList[I];

    //Get string index and skip erroneous lines
    firstDelimiter := Pos(':', s);
    if firstDelimiter = 0 then Continue;
    if not TryStrToInt(TrimLeft(LeftStr(s, firstDelimiter - 1)), ID) then Continue;
    if ID > MaxID then Continue;

    s := RightStr(s, Length(s) - firstDelimiter);
    //Required characters that can't be stored in plain text
    s := StringReplace(s, '\n', eol, [rfReplaceAll, rfIgnoreCase]); //EOL
    s := StringReplace(s, '\\', '\', [rfReplaceAll, rfIgnoreCase]); //Slash
    if not aOverwrite or (s <> '') then aArray[aFirstIndex + ID] := s;
  end;

  aStringList.Free;
end;


//Campaign description and briefings get appended to main list
//as they are used in Main Menu right away
function TKMTextLibrary.AppendCampaign(aFileName: string): Word;
begin
  Assert(Pos('%s', aFileName) <> 0, 'Input string must be formatted properly with an %s');

  Result := Length(GameStrings);
  LoadLIBXFile(Format(aFileName, [DEFAULT_LOCALE]), Result, GameStrings, False);
  
  if (fFallbackLocale <> '') and FileExists(Format(aFileName, [fFallbackLocale])) then
    LoadLIBXFile(Format(aFileName, [fFallbackLocale]), Result, GameStrings, True);

  if (fLocale <> DEFAULT_LOCALE) and FileExists(Format(aFileName, [fLocale])) then
    LoadLIBXFile(Format(aFileName, [fLocale]), Result, GameStrings, True);
end;


//Load mission strings into separate array, as they get reloaded for each mission
//Only one set of mission strings is required at a time
procedure TKMTextLibrary.LoadMissionStrings(aFileName: string);
begin
  LoadLIBXFile(Format(aFileName, [DEFAULT_LOCALE]), 0, MissionStrings, False);

  if (fFallbackLocale <> '') and FileExists(Format(aFileName, [fFallbackLocale])) then
    LoadLIBXFile(Format(aFileName, [fFallbackLocale]), 0, MissionStrings, True);

  if (fLocale <> DEFAULT_LOCALE) and FileExists(Format(aFileName, [fLocale])) then
    LoadLIBXFile(Format(aFileName, [fLocale]), 0, MissionStrings, True);
end;


function TKMTextLibrary.GetTexts(aIndex: word): string;
begin
  if aIndex < Length(GameStrings) then
    Result := GameStrings[aIndex]
  else
    Result := '~~~String '+IntToStr(aIndex)+' out of range!~~~';
end;


function TKMTextLibrary.GetMissionString(aIndex: Word): string;
begin
  if aIndex < Length(MissionStrings) then
    Result := MissionStrings[aIndex]
  else
    Result := '~~~MissionString '+IntToStr(aIndex)+' out of range!~~~';
end;


//Take the string and replace every occurence of <$tag> with corresponding text from LibX
function TKMTextLibrary.ParseTextMarkup(const aText: string): string;
var
  I, ID, Last: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(aText) do
  begin
    if (I + 3 <= Length(aText)) and (aText[I] = '<') and (aText[I+1] = '$') then
    begin
      Last := PosEx('>', aText, I);
      ID := StrToIntDef(Copy(aText, I+2, Last-(I+2)), -1);
      if ID >= 0 then
      begin
        Result := Result + GetMissionString(ID);
        I := Last + 1;
        Continue;
      end;
    end;
    Result := Result + aText[I];
    Inc(I);
  end;
end;


procedure TKMTextLibrary.ExportTextLibrary(aLibrary: TKMStringArray; aFileName: string);
var
  i: integer;
  FileData: TStringList;
begin
  //Here we will export all of the text to a file
  FileData := TStringList.Create;
  try
    if FileExists(aFileName) then
      DeleteFile(aFileName);

    for i := Low(GameStrings) to High(GameStrings) do
      FileData.Add(IntToStr(i) + ':' + aLibrary[i]);

    FileData.SaveToFile(aFileName);
  finally
    FileData.Free;
  end;
end;


procedure TKMTextLibrary.ExportTextLibraries;
begin
  CreateDir(ExeDir + 'Export' + PathDelim);
  ExportTextLibrary(GameStrings, ExeDir + 'Export' + PathDelim + 'text.'+fLocale+'.libx');
end;


end.

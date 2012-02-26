unit KM_TextLibrary;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, StrUtils, KromUtils, KM_Defaults;


const
  //NAME__## means that this const defines first element of some range that is ## long
  //TX_HOUSES_NAMES__29 = 1; //Todo: Add 30th as Marketplace
  //TX_UNITS_NAMES__29 = 70; //todo: add animal unit names

  //Load text IDs from this include file that is managed by the Translation Manager
  {$I KM_TextIDs.inc}


type
  TTextLibrary = class
  private
    fLocale: AnsiString;
    fCount: Integer;
    GameStrings: TAnsiStringArray;
    MissionStrings: TAnsiStringArray; //Strings used in a mission
    procedure LoadLIBFile(FilePath: string; aFirstIndex: Word);
    procedure LoadLIBXFile(FilePath: string; aFirstIndex: Word; var aArray: TAnsiStringArray; aOverwrite: Boolean);
    procedure ExportTextLibrary(aLibrary: array of AnsiString; aFileName: string);
    function GetTexts(aIndex:word): AnsiString;
  public
    constructor Create(aLibPath: string; aLocale: AnsiString);

    function AppendCampaign(aFilename: string): Word;
    procedure LoadMissionStrings(aFilename: string);

    function GetMissionString(aIndex: word): AnsiString;

    property Texts[aIndex: word]: AnsiString read GetTexts; default;

    procedure ExportTextLibraries;
  end;


var
  fTextLibrary: TTextLibrary;


implementation
uses KM_Log;


{ TTextLibrary }
constructor TTextLibrary.Create(aLibPath: string; aLocale: AnsiString);
begin
  inherited Create;

  //SetLength(GameStrings, 2000);

  //Remember preferred locale, it will remain constant until reinit
  fLocale := aLocale;

  //We load the English LIBX by default, then overwrite it with the selected language (this way missing strings are in English)
  LoadLIBXFile(aLibPath+'remake.'+DEFAULT_LOCALE+'.libx', 0, GameStrings, False); //Initialize with English strings
  if (fLocale <> DEFAULT_LOCALE) and FileExists(aLibPath+'remake.'+fLocale+'.libx') then
    LoadLIBXFile(aLibPath+'remake.'+fLocale+'.libx', 0, GameStrings, True); //Overwrite with selected locale

  if FileExists(aLibPath + 'text.' + fLocale + '.lib') then
    LoadLIBFile(aLibPath + 'text.' + fLocale + '.lib', 0)
  else
    LoadLIBFile(aLibPath + 'text.lib', 0);

  if FileExists(aLibPath + 'setup.' + fLocale + '.lib') then
    LoadLIBFile(aLibPath + 'setup.' + fLocale + '.lib', 1000)
  else
    LoadLIBFile(aLibPath + 'setup.lib', 1000);

  fLog.AppendLog('TextLib init done');
end;


procedure TTextLibrary.LoadLIBFile(FilePath: string; aFirstIndex: Word);
var
  f:file; NumRead:integer;
  i2, i3, StrCount, Byte1, Byte2, LastStrLen, LastFirstFFIndex, StrLen, TheIndex, ExtraCount: integer;
  FileData: array[0..100000] of byte;
  TheString: AnsiString;
  LastWasFF: boolean;
begin
  {
  By reading this code you will probably think that I'm crazy. But all the weird stuff
  with the so called "FF byte pairs" is actually needed. If I just load everything in
  order then stuff won't be correct and it WILL cause us problems later on. Just trust
  me on this one, I spent a long time making a tool to edit these files so I DO know
  what I'm talking about. ;)
  }
  if not CheckFileExists(FilePath) then exit;

  AssignFile(f,FilePath);
  FileMode := 0;
  Reset(f,1);
  FileMode := 2;
  blockread(f,FileData,100000,NumRead); //100kb should be enough
  closefile(f);

  //Load AnsiString count from first two bytes
  StrCount := FileData[0] + (FileData[1] * 256);
  //Load the length of the last AnsiString which is stored here
  LastStrLen := FileData[2] + (FileData[3] * 256);

  //Now starts the indexes, set some defaults then run a loop
  ExtraCount := 1;
  LastWasFF := false;
  LastFirstFFIndex := 1;
  for i3 := 1 to StrCount do
  begin
    //Load index bytes for this AnsiString
    Byte1 := FileData[8+((i3-1)*2)];
    Byte2 := FileData[9+((i3-1)*2)];
    //Check for FF byte pars
    if (Byte1 = $FF) and (Byte2 = $FF) then
    begin
      //This AnsiString is unused, meaning we must store it as blank, but also for some extreamly
      //annoying reason they also change the order of bytes around them (don't ask...)
      GameStrings[aFirstIndex + i3] := ''; //Make it blank
      if not LastWasFF then
        LastFirstFFIndex := i3;
      LastWasFF := true;
    end
    else
    begin
      StrLen := Byte1 + (Byte2 * 256);
      if i3 = StrCount then //For the last AnsiString we must get the length from the header
        StrLen := LastStrLen;

      TheString := '';
      for i2 := ExtraCount to StrLen - 1 do //Extract the AnsiString from the main section of the file
      begin
        TheString := TheString + AnsiChar(FileData[(StrCount * 2) + 5 + i2]);
      end;
      ExtraCount := StrLen + 1;
      if LastWasFF then  TheIndex := LastFirstFFIndex
      else TheIndex := i3;
      GameStrings[aFirstIndex + TheIndex-1] := TheString;
      LastWasFF := false;
    end;
  end;
end;


{LIBX files consist of lines. Each line has an index and a text. Lines without index are skipped}
procedure TTextLibrary.LoadLIBXFile(FilePath: string; aFirstIndex: Word; var aArray: TAnsiStringArray; aOverwrite: Boolean);
var
  aStringList: TStringList;
  I: Integer;
  s: AnsiString;
  firstDelimiter: Integer;
  ID, MaxID: Integer;
begin
  if not FileExists(FilePath) then Exit;

  aStringList := TStringList.Create;
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

  aStringList.Free; //Must free at last to avoid memory-leaks
end;


//Campaign description and briefings get appended to main list
//as they are used in Main Menu right away
function TTextLibrary.AppendCampaign(aFilename: string): Word;
begin
  Assert(Pos('%s', aFilename) <> 0, 'Input string must be formatted properly with an %s');

  Result := High(GameStrings);
  LoadLIBXFile(Format(aFilename, [DEFAULT_LOCALE]), Result, GameStrings, False);
  if (fLocale <> DEFAULT_LOCALE) and FileExists(Format(aFilename, [fLocale])) then
    LoadLIBXFile(Format(aFilename, [fLocale]), Result, GameStrings, True);
end;


//Load mission strings into separate array, as they get reloaded for each mission
//Only one set of mission strings is required at a time
procedure TTextLibrary.LoadMissionStrings(aFilename: string);
begin
  LoadLIBXFile(Format(aFilename, [DEFAULT_LOCALE]), 0, MissionStrings, False);
  if (fLocale <> DEFAULT_LOCALE) and FileExists(Format(aFilename, [fLocale])) then
    LoadLIBXFile(Format(aFilename, [fLocale]), 0, MissionStrings, True);
end;


function TTextLibrary.GetTexts(aIndex: word): AnsiString;
begin
  if aIndex < Length(GameStrings) then
    Result := GameStrings[aIndex]
  else
    Result := '~~~String out of range!~~~';
end;


function TTextLibrary.GetMissionString(aIndex: word): AnsiString;
begin
  if aIndex < Length(MissionStrings) then
    Result := MissionStrings[aIndex]
  else
    Result := '~~~MissionString out of range!~~~';
end;


procedure TTextLibrary.ExportTextLibrary(aLibrary: array of AnsiString; aFileName: string);
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


procedure TTextLibrary.ExportTextLibraries;
begin
  CreateDir(ExeDir + 'Export\');
  ExportTextLibrary(GameStrings, ExeDir + 'Export\Strings.'+fLocale+'.txt');
end;


end.

unit KM_TextLibrary;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, StrUtils, KromUtils, KM_Defaults;


const
  MaxStrings = 610; //Text.lib has the most entries - 590, but Russian font file has StrCount=609

  //NAME__## means that this const defines first element of some range that is ## long
  TX_HOUSES_NAMES__29 = 1; //Todo: Add 30th as Marketplace
  TX_RESOURCES_NAMES__27 = 40;
  TX_UNITS_NAMES__29 = 70; //todo: add animal unit names
  TX_UNITS_DESCRIPTIONS__13 = 100;

  TX_MENU_LOAD_GAME = 174;
  TX_MENU_SAVE_GAME = 175;

  TX_MENU_TAB_HINT_BUILD = 160;
  TX_MENU_TAB_HINT_DISTRIBUTE = 161;
  TX_MENU_TAB_HINT_STATISTICS = 162;
  TX_MENU_TAB_HINT_OPTIONS = 164;
  TX_MENU_TAB_HINT_GO_BACK = 165;

  TX_MENU_TAB_BUILD = 166;
  TX_MENU_TAB_DISTRIBUTE = 167;
  TX_MENU_TAB_STATISTICS = 168;
  TX_MENU_TAB_OPTIONS = 170;

  TX_MENU_QUIT_QUESTION = 176;
  TX_MENU_QUIT_MISSION = 177;
  TX_MENU_DONT_QUIT_MISSION = 178;
  TX_MENU_SETTINGS = 179;
  TX_MENU_QUIT_MAPED = 180;
  TX_MENU_SFX_VOLUME = 194;
  TX_MENU_SFX_VOLUME_HINT = 195;
  TX_MENU_MUSIC_VOLUME = 196;
  TX_MENU_MUSIC_VOLUME_HINT = 195;
  TX_MENU_OPTIONS_MUSIC_DISABLE_HINT = 198;

  TX_MUSIC_PLAYER = 207;
  TX_MUSIC_PREV_HINT = 208;
  TX_MUSIC_NEXT_HINT = 209;

  TX_BUILD_CANCEL_HINT = 211;
  TX_BUILD_ROAD_HINT = 213;
  TX_BUILD_FIELD_HINT = 215;
  TX_BUILD_WINE_HINT = 219;

  TX_HOUSE_CONDITION = 228;
  TX_HOUSE_GOODS_HINT = 249;
  TX_HOUSE_REPAIR_HINT = 250;
  TX_HOUSE_NOT_AVAIABLE = 251;
  TX_UNIT_CONDITION = 254;

  TX_ARMY_ROTATE_CCW_HINT = 255;
  TX_ARMY_ROTATE_CW_HINT = 256;
  TX_ARMY_ATTACK_HINT = 257;
  TX_ARMY_GOTO_HINT = 259;
  TX_ARMY_FEED_HINT = 262;
  TX_ARMY_STORM_HINT = 263;
  TX_ARMY_LINE_ADD_HINT = 264;
  TX_ARMY_LINE_REM_HINT = 265;

  TX_ARMY_JOIN_SELECT = 272;
  TX_ARMY_JOIN_CANCEL = 274;

  TX_MSG_DELETE = 276;
  TX_MSG_DELETE_HINT = 277;
  TX_MSG_GOTO = 280;
  TX_MSG_GOTO_HINT = 281;
  TX_MSG_CLOSE = 282;
  TX_MSG_CLOSE_HINT = 283;

  TX_MSG_STONE_DEPLETED = 290;
  TX_MSG_COAL_DEPLETED = 291;
  TX_MSG_IRON_DEPLETED = 292;
  TX_MSG_GOLD_DEPLETED = 293;
  //TX_MSG_FISHERMAN_CANNOT_CATCH = 294;
  TX_MSG_HOUSE_UNOCCUPIED = 295;
  TX_MSG_TROOP_HUNGRY = 296;

  TX_POPUP_PAUSE = 308;

  //These constants are made so to be able to use Texts[] instead of GetSetupString(), which is consistent style for MainMenu
  TX_MENU_MULTIPLAYER = 1011;
  TX_MENU_OPTIONS = 1012;
  TX_MENU_OPTIONS_AUTOSAVE = 203;
  TX_MENU_CREDITS = 1013;
  TX_MENU_QUIT = 1014;

  TX_MENU_CAMP_TSK = 1001;
  TX_MENU_CAMP_TPR = 1002;
  TX_MENU_SINGLE_MAP = 1004;
  TX_MENU_SINGLE_START_MAP = 1008;
  TX_MENU_BACK = 1009;
  TX_MENU_LOAD_SAVEGAME = 1010;
  TX_MENU_MISSION_NEXT = 1017;
  TX_MENU_MISSION_REPEAT = 1018;
  TX_MENU_OPTIONS_RESOLUTION = 1020;
  TX_MENU_MISSION_VICTORY = 1111;
  TX_MENU_MISSION_DEFEAT = 1112;

  TX_RESULTS_UNITS_LOST = 1113;
  TX_RESULTS_UNITS_DEFEATED = 1114;
  TX_RESULTS_HOUSES_LOST = 1115;
  TX_RESULTS_HOUSES_DESTROYED = 1116;
  TX_RESULTS_HOUSES_BUILT = 1117;
  TX_RESULTS_UNITS_TRAINED = 1118;
  TX_RESULTS_WEAPONS_MADE = 1119;
  TX_RESULTS_SOLDIERS_TRAINED = 1120;
  TX_RESULTS_MISSION_TIME = 1121;

  TX_CREDITS_TEXT = 1300;

  //Load text IDs from this include file that is managed by the Translation Manager
  {$I KM_TextIDs.inc}


type
  TTextLibrary = class
  private
    fLocale: AnsiString;
    TextStrings: array [0..MaxStrings] of AnsiString;
    SetupStrings: array [0..MaxStrings] of AnsiString;
    RemakeStrings: TAnsiStringArray;
    MissionStrings: TAnsiStringArray; //Strings used in a mission
    procedure LoadLIBFile(FilePath: string; var aArray: array of AnsiString);
    procedure LoadLIBXFile(FilePath: string; aFirstIndex: Word; var aArray: TAnsiStringArray; aInitializeValues: Boolean);
    procedure ExportTextLibrary(aLibrary: array of AnsiString; aFileName: string);
    function GetSetupString(aIndex: word): AnsiString;
    function GetRemakeString(aIndex:word): AnsiString;
    function GetTexts(aIndex:word): AnsiString;
  public
    constructor Create(aLibPath: string; aLocale: AnsiString);

    function AppendCampaign(aFilename: string): Word;
    procedure LoadMissionStrings(aFilename: string);

    function GetTextString(aIndex: word): AnsiString;
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

  //Remember preferred locale, it will remain constant until reinit
  fLocale := aLocale;

  if FileExists(aLibPath+'text.'+fLocale+'.lib')
  then LoadLIBFile(aLibPath+'text.'+fLocale+'.lib', TextStrings)
  else LoadLIBFile(aLibPath+'text.lib', TextStrings);

  if FileExists(aLibPath+'setup.'+fLocale+'.lib')
  then LoadLIBFile(aLibPath+'setup.'+fLocale+'.lib', SetupStrings)
  else LoadLIBFile(aLibPath+'setup.lib', SetupStrings);

  //We load the English LIBX by default, then overwrite it with the selected language (this way missing strings are in English)
  LoadLIBXFile(aLibPath+'remake.'+DEFAULT_LOCALE+'.libx', 0, RemakeStrings, True); //Initialize with English strings
  if (fLocale <> DEFAULT_LOCALE) and FileExists(aLibPath+'remake.'+fLocale+'.libx') then
    LoadLIBXFile(aLibPath+'remake.'+fLocale+'.libx', 0, RemakeStrings, False); //Overwrite with selected locale

  fLog.AppendLog('TextLib init done');
end;


procedure TTextLibrary.LoadLIBFile(FilePath: string; var aArray:array of AnsiString);
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
      aArray[i3] := ''; //Make it blank
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
      aArray[TheIndex-1] := TheString;
      LastWasFF := false;
    end;
  end;
end;


{LIBX files consist of lines. Each line has an index and a text. Lines without index are skipped}
procedure TTextLibrary.LoadLIBXFile(FilePath: string; aFirstIndex: Word; var aArray: TAnsiStringArray; aInitializeValues: Boolean);
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
  Assert(MaxID <= 255, 'Check strings count');

  SetLength(aArray, Max(Length(aArray), aFirstIndex + MaxID + 1));

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
    if aInitializeValues or (s <> '') then aArray[aFirstIndex + ID] := s;
  end;

  aStringList.Free; //Must free at last to avoid memory-leaks
end;


//Campaign description and briefings get appended to main list
//as they are used in Main Menu right away
function TTextLibrary.AppendCampaign(aFilename: string): Word;
var S: string;
begin
  Assert(Pos('%s', aFilename) <> 0, 'Input string must be formatted properly with an %s');

  Result := High(RemakeStrings);
  LoadLIBXFile(Format(aFilename, [DEFAULT_LOCALE]), Result, RemakeStrings, True);
  if (fLocale <> DEFAULT_LOCALE) and FileExists(Format(aFilename, [fLocale])) then
    LoadLIBXFile(Format(aFilename, [fLocale]), Result, RemakeStrings, False);
end;


//Load mission strings into separate array, as they get reloaded for each mission
//Only one set of mission strings is required at a time
procedure TTextLibrary.LoadMissionStrings(aFilename: string);
begin
  LoadLIBXFile(Format(aFilename, [DEFAULT_LOCALE]), 0, MissionStrings, True);
  if (fLocale <> DEFAULT_LOCALE) and FileExists(Format(aFilename, [fLocale])) then
    LoadLIBXFile(Format(aFilename, [fLocale]), 0, MissionStrings, False);
end;


function TTextLibrary.GetTexts(aIndex: word): AnsiString;
begin
  if aIndex < 1000 then
    Result := GetTextString(aIndex)
  else
  if aIndex < 2000 then
    Result := GetSetupString(aIndex - 1000)
  else
    Result := GetRemakeString(aIndex - 2000);
end;


function TTextLibrary.GetTextString(aIndex: word): AnsiString;
begin
  if aIndex <= MaxStrings then
    Result := TextStrings[aIndex]
  else
    Result := '~~~TextString out of range!~~~';
end;


function TTextLibrary.GetSetupString(aIndex: word): AnsiString;
begin
  if aIndex <= MaxStrings then
    Result := SetupStrings[aIndex]
  else
    Result := '~~~SetupString out of range!~~~';
end;


function TTextLibrary.GetMissionString(aIndex: word): AnsiString;
begin
  if aIndex < Length(MissionStrings) then
    Result := MissionStrings[aIndex]
  else
    Result := '~~~MissionString out of range!~~~';
end;


function TTextLibrary.GetRemakeString(aIndex: word): AnsiString;
begin
  if aIndex < Length(RemakeStrings) then
    Result := RemakeStrings[aIndex]
  else
    Result := '~~~RemakeString out of range!~~~';
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

    for i := 0 to MaxStrings do
      FileData.Add(IntToStr(i) + ': ' + aLibrary[i]);

    FileData.SaveToFile(aFileName);
  finally
    FileData.Free;
  end;
end;


procedure TTextLibrary.ExportTextLibraries;
begin
  CreateDir(ExeDir + 'Export\');
  ExportTextLibrary(SetupStrings, ExeDir + 'Export\LIB_Setup.txt');
  ExportTextLibrary(TextStrings, ExeDir + 'Export\LIB_Text.txt');
end;


end.

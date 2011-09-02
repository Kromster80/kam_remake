unit KM_TextLibrary;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, StrUtils, KromUtils, KM_Defaults;


const
  MaxStrings = 610; //Text.lib has the most entries - 590, but Russian font file has StrCount=609
  //Here are some indexes into the files for different items. (e.g. unit names) si is for "String Index"
  siHouseNames = 0;
  siResourceNames = 39;
  siUnitNames = 69;
  siUnitDescriptions = 99;
  siTrackNames = 570;
  siCampTSKTexts = 250;
  siCampTPRTexts = 350;

  TX_MENU_MULTIPLAYER = 1011;
  TX_MENU_OPTIONS = 1012;
  TX_MENU_CREDITS = 1013;
  TX_MENU_QUIT = 1014;

  TX_MENU_MISSION_CANCELED = 2001;
  TX_MENU_TUTORIAL_TOWN = 2002;
  TX_MENU_TUTORIAL_BATTLE = 2003;
  TX_MENU_SINGLEPLAYER = 2004;
  TX_MENU_MAP_EDITOR = 2005;
  TX_MENU_VIEW_LAST_REPLAY = 2006;
  TX_MENU_LAN = 2007;
  TX_MENU_INTERNET = 2008;
  TX_MENU_LOGIN = 2009;
  TX_MENU_PASSWORD = 2010;
  TX_MENU_START_MISSION = 2011;
  TX_MENU_TITLE = 2012;
  TX_MENU_SIZE = 2013;
  TX_MENU_MISSION_TYPE = 2014;
  TX_MENU_WIN_CONDITION = 2015;
  TX_MENU_DEFEAT_CONDITION = 2016;
  TX_MENU_ALLIES = 2017;
  TX_MENU_ENEMIES = 2018;
  TX_MENU_SLOT = 2019;
  TX_MENU_MAP_SIZE = 2020;
  TX_MENU_MAP_WIDTH = 2021;
  TX_MENU_MAP_HEIGHT = 2022;
  TX_MENU_MAP_CREATE_NEW_MAP = 2023;
  TX_MENU_MAP_AVAILABLE = 2024;
  TX_MENU_MAP_LOAD_EXISTING = 2025;
  TX_MENU_OPTIONS_CONTROLS = 2026;
  TX_MENU_OPTIONS_GAMEPLAY = 2027;
  TX_MENU_OPTIONS_SOUND = 2028;
  TX_MENU_OPTIONS_SOUND_DISABLE = 2036;
  TX_MENU_OPTIONS_GRAPHICS = 2029;
  TX_MENU_OPTIONS_BRIGHTNESS = 2030;
  TX_MENU_OPTIONS_FULLSCREEN = 2031;
  TX_MENU_OPTIONS_APPLY = 2032;
  TX_MENU_OPTIONS_LANGUAGE = 2033;
  TX_MENU_LOADING = 2034;
  TX_MENU_ERROR = 2035;
  TX_MENU_NEWLOCAL = 2037;
  TX_UNKNOWN = 2038;

  TX_GAMEPLAY_WON = 2039;
  TX_GAMEPLAY_CONTINUE_PLAYING = 2040;
  TX_GAMEPLAY_VICTORY = 2041;
  TX_GAMEPLAY_LOST = 2042;
  TX_GAMEPLAY_DEFEAT_CONTINUEWATCHING = 2043;
  TX_GAMEPLAY_DEFEAT = 2044;
  TX_GAMEPLAY_REPLAY_ENDED = 2045;
  TX_GAMEPLAY_REPLAY_CONTINUEWATCHING = 2046;
  TX_GAMEPLAY_QUIT_TO_MENU = 2047;
  TX_GAMEPLAY_NOT_AVAILABLE = 2104;
  TX_GAMEPLAY_PAUSE_INFO = 2105;
  TX_GAMEPLAY_CHAT_TOTEAM = 2106;
  TX_GAMEPLAY_PLAYERS_HINT = 2108;
  TX_GAMEPLAY_CHAT_HINT = 2109;

  TX_GAME_ERROR_CAPTION = 2048;
  TX_GAME_ERROR_SEND_REPORT = 2049;
  TX_GAME_ERROR_OLD_OPENGL = 2053;
  TX_GAME_ERROR_NETWORK = 2098;
  TX_GAME_ERROR_BLANK_PLAYERNAME = 2099;
  TX_GAME_ERROR_CONNECTION_FAILED = 2100;
  TX_GAME_ERROR_DISCONNECT = 2101;

  TX_GAME_MISSION = 2092;

  TX_UNITS_MINING_WRONG_SPOT = 2050;
  TX_UNITS_FISHERMAN_TOO_FAR = 2051;
  TX_UNITS_FISHERMAN_CANNOT_CATCH = 2052;
  TX_UNITS_DIE = 2107;

  TX_MAPEDITOR_TERRAIN = 2054;
  TX_MAPEDITOR_VILLAGE = 2055;
  TX_MAPEDITOR_SCRIPTS_VISUAL = 2056;
  TX_MAPEDITOR_SCRIPTS_GLOBAL = 2057;
  TX_MAPEDITOR_MENU = 2058;

  TX_LANLOGIN_PLAYERNAME = 2059;
  TX_LANLOGIN_IP_SELF = 2060;
  TX_LANLOGIN_IP_HOST = 2061;
  TX_LANLOGIN_SERVER_CREATE = 2062;
  TX_LANLOGIN_SERVER_JOIN = 2063;
  TX_LANLOGIN_CONNECTING = 2102;
  TX_LANLOGIN_WAITING = 2103;

  TX_LOBBY_HEADER_PLAYERS = 2064;
  TX_LOBBY_HEADER_STARTLOCATION = 2065;
  TX_LOBBY_HEADER_TEAM = 2066;
  TX_LOBBY_HEADER_FLAGCOLOR = 2067;
  TX_LOBBY_HEADER_READY = 2068;
  TX_LOBBY_HEADER_PING = 2069;
  TX_LOBBY_SLOT_OPEN = 2070;
  TX_LOBBY_SLOT_AI_PLAYER = 2071;
  TX_LOBBY_SELECT = 2097;
  TX_LOBBY_RANDOM = 2072;
  TX_LOBBY_NONE = 2073;
  TX_LOBBY_TEAM_X = 2074;
  TX_LOBBY_POST_LIST = 2075;
  TX_LOBBY_POST_WRITE = 2076;
  TX_LOBBY_MAP_CHOOSE = 2077;
  TX_LOBBY_MAP_SINGLE = 2078;
  TX_LOBBY_MAP_SAVED = 2079;
  TX_LOBBY_MAP_SELECT = 2080;
  TX_LOBBY_MAP_SELECT_SAVED = 2096;
  TX_LOBBY_MAP_INFO = 2081;
  TX_LOBBY_MAP_PLAYERS = 2093;
  TX_LOBBY_MAP_MODE = 2094;
  TX_LOBBY_MAP_CONDITIONS = 2095;
  TX_LOBBY_MAP_SIZE = 2110;
  TX_LOBBY_QUIT = 2082;
  TX_LOBBY_READY = 2083;
  TX_LOBBY_START = 2084;

  TX_CREDITS = 2085;
  TX_CREDITS_ORIGINAL = 2086;
  TX_CREDITS_PROGRAMMING = 2087;
  TX_CREDITS_ADDITIONAL_PROGRAMMING = 2088;
  TX_CREDITS_ADDITIONAL_GRAPHICS = 2089;
  TX_CREDITS_ADDITIONAL_TRANSLATIONS = 2090;
  TX_CREDITS_SPECIAL = 2091;


type
  TTextLibrary = class
  private
    TextStrings: array[0..MaxStrings] of string;
    SetupStrings: array[0..MaxStrings] of string;
    RemakeStrings: TStringArray;
    procedure LoadLIBFile(FilePath:string; var aArray:array of string);
    procedure LoadLIBXFile(FilePath:string; var aArray:TStringArray);
    procedure ExportTextLibrary(var aLibrary: array of string; aFileName:string);
    function GetRemakeString(aIndex:word):string;
    function GetTexts(aIndex:word):string;
  public
    constructor Create(aLibPath, aLocale: string);
    function GetTextString(aIndex:word):string;
    function GetSetupString(aIndex:word):string;
    property Texts[aIndex:word]:string read GetTexts; default;
    procedure ExportTextLibraries;
end;

var
  fTextLibrary:TTextLibrary;


implementation
uses KM_Log;


{ TTextLibrary }
constructor TTextLibrary.Create(aLibPath,aLocale: string);
begin
  Inherited Create;

  if FileExists(aLibPath+'text.'+aLocale+'.lib')
  then LoadLIBFile(aLibPath+'text.'+aLocale+'.lib', TextStrings)
  else LoadLIBFile(aLibPath+'text.lib', TextStrings);

  if FileExists(aLibPath+'setup.'+aLocale+'.lib')
  then LoadLIBFile(aLibPath+'setup.'+aLocale+'.lib', SetupStrings)
  else LoadLIBFile(aLibPath+'setup.lib', SetupStrings);

  if FileExists(aLibPath+'remake.'+aLocale+'.libx')
  then LoadLIBXFile(aLibPath+'remake.'+aLocale+'.libx', RemakeStrings)
  else LoadLIBXFile(aLibPath+'remake.eng.libx', RemakeStrings);

  fLog.AppendLog('TextLib init done');
end;


procedure TTextLibrary.LoadLIBFile(FilePath:string; var aArray:array of string);
var
  f:file; NumRead:integer;
  i2, i3, StrCount, Byte1, Byte2, LastStrLen, LastFirstFFIndex, StrLen, TheIndex, ExtraCount: integer;
  FileData: array[0..100000] of byte;
  TheString: string;
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

  //Load string count from first two bytes
  StrCount := FileData[0] + (FileData[1] * 256);
  //Load the length of the last string which is stored here
  LastStrLen := FileData[2];
  
  //Now starts the indexes, set some defaults then run a loop
  ExtraCount := 1;
  LastWasFF := false;
  LastFirstFFIndex := 1;
  for i3 := 1 to StrCount do
  begin
    //Load index bytes for this string
    Byte1 := FileData[8+((i3-1)*2)];
    Byte2 := FileData[9+((i3-1)*2)];
    //Check for FF byte pars
    if (Byte1 = $FF) and (Byte2 = $FF) then
    begin
      //This string is unused, meaning we must store it as blank, but also for some extreamly
      //annoying reason they also change the order of bytes around them (don't ask...)
      aArray[i3] := ''; //Make it blank
      if not LastWasFF then
        LastFirstFFIndex := i3;
      LastWasFF := true;
    end
    else
    begin
      StrLen := Byte1 + (Byte2 * 256);
      if i3 = StrCount then //For the last string we must get the length from the header
        StrLen := LastStrLen;

      TheString := '';
      for i2 := ExtraCount to StrLen - 1 do //Extract the string from the main section of the file
      begin
        TheString := TheString + char(FileData[(StrCount * 2) + 5 + i2]);
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
procedure TTextLibrary.LoadLIBXFile(FilePath:string; var aArray:TStringArray);
var
  aStringList:TStringList;
  i:integer;
  s:string;
  firstDelimiter:integer;
  ID, MaxID:integer;
begin
  if not CheckFileExists(FilePath) then exit;

  aStringList := TStringList.Create;
  aStringList.LoadFromFile(FilePath);

  //First line is empty or comment and could have first 3 bytes Unicode Byte-Order Mark (BOM)
  s := aStringList[1];
  if Copy(s,1,6) <> 'MaxID:' then exit;

  firstDelimiter := Pos(':', s);
  if not TryStrToInt(RightStr(s, Length(s)-firstDelimiter), MaxID) then exit;

  SetLength(aArray, MaxID+1);

  for i:=0 to aStringList.Count-1 do
  begin
    s := aStringList[i];

    firstDelimiter := Pos(':', s);
    if firstDelimiter=0 then continue;
    
    if not TryStrToInt(TrimLeft(LeftStr(s, firstDelimiter-1)), ID) then continue;

    if ID <= MaxID then
    begin
      s := RightStr(s, Length(s)-firstDelimiter);
      //Required characters that can't be stored in plain text
      s := StringReplace(s, '\n', eol, [rfReplaceAll, rfIgnoreCase]); //EOL
      s := StringReplace(s, '\\', '\', [rfReplaceAll, rfIgnoreCase]); //Slash
      aArray[ID] := s;
    end;
  end;

  aStringList.Free; //Must free at last to avoid memory-leaks
end;


function TTextLibrary.GetTexts(aIndex:word):string;
begin
  if aIndex < 1000 then
    Result := GetTextString(aIndex)
  else
  if aIndex < 2000 then
    Result := GetSetupString(aIndex-1000)
  else
    Result := GetRemakeString(aIndex-2000);
end;


function TTextLibrary.GetTextString(aIndex:word):string;
begin
  if aIndex <= MaxStrings then
    Result := TextStrings[aIndex]
  else
    Result := '~~~TextString out of range!~~~';
end;


function TTextLibrary.GetSetupString(aIndex:word):string;
begin
  if aIndex <= MaxStrings then
    Result := SetupStrings[aIndex]
  else
    Result := '~~~SetupString out of range!~~~';
end;


function TTextLibrary.GetRemakeString(aIndex:word):string;
begin
  if aIndex < length(RemakeStrings) then
    Result := RemakeStrings[aIndex]
  else
    Result := '~~~RemakeString out of range!~~~';
end;


procedure TTextLibrary.ExportTextLibrary(var aLibrary: array of string; aFileName:string);
var
  i: integer;
  FileData: TStringList;
begin
  //Here we will export all of the text to a file
  FileData := TStringList.Create;
  if FileExists(aFileName) then DeleteFile(aFileName);
  for i:= 0 to MaxStrings do
    FileData.Add(IntToStr(i)+': '+aLibrary[i]);
  FileData.SaveToFile(aFileName);
  FileData.Free;
end;


procedure TTextLibrary.ExportTextLibraries;
begin
  CreateDir(ExeDir+'Export\');
  ExportTextLibrary(SetupStrings,ExeDir+'Export\LIB_Setup.txt');
  ExportTextLibrary(TextStrings,ExeDir+'Export\LIB_Text.txt');
end;


end.

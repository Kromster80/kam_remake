unit KM_Maps;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SyncObjs, SysUtils, StrUtils
  {$IFDEF FPC} , FileUtil {$ENDIF}
  {$IFDEF WDC} , IOUtils {$ENDIF}
  , KM_Defaults;


type
  TMapsSortMethod = (
    smByFavouriteAsc, smByFavouriteDesc,
    smByNameAsc, smByNameDesc,
    smBySizeAsc, smBySizeDesc,
    smByPlayersAsc, smByPlayersDesc,
    smByHumanPlayersAsc, smByHumanPlayersDesc,
    smByHumanPlayersMPAsc, smByHumanPlayersMPDesc,
    smByModeAsc, smByModeDesc);

  TKMapInfo = class;
  TMapEvent = procedure (aMap: TKMapInfo) of object;
  TKMMapInfoAmount = (iaBase, iaExtra);

  TKMMapGoalInfo = packed record
    Cond: TGoalCondition;
    Play: TKMHandIndex;
    Stat: TGoalStatus;
  end;

  TKMapInfo = class
  private
    fPath: string;
    fFileName: UnicodeString; //without extension
    fCRC: Cardinal;
    fDatCRC: Cardinal; //Used to speed up scanning
    fVersion: AnsiString; //Savegame version, yet unused in maps, they always have actual version
    fInfoAmount: TKMMapInfoAmount;
    fMapFolder: TMapFolder;
    fSizeText: string;
    procedure ResetInfo;
    procedure LoadTXTInfo;
    procedure LoadFromFile(const aPath: string);
    procedure SaveToFile(const aPath: string);
    function GetSizeText: string;
    function DetermineReadmeFilePath: String;
  public
    MapSizeX, MapSizeY: Integer;
    MissionMode: TKMissionMode;
    LocCount: Byte;
    CanBeHuman: array [0..MAX_HANDS-1] of Boolean;
    CanBeAI: array [0..MAX_HANDS-1] of Boolean;
    DefaultHuman: TKMHandIndex;
    GoalsVictoryCount, GoalsSurviveCount: array [0..MAX_HANDS-1] of Byte;
    GoalsVictory: array [0..MAX_HANDS-1] of array of TKMMapGoalInfo;
    GoalsSurvive: array [0..MAX_HANDS-1] of array of TKMMapGoalInfo;
    Alliances: array [0..MAX_HANDS-1, 0..MAX_HANDS-1] of TAllianceType;
    FlagColors: array [0..MAX_HANDS-1] of Cardinal;
    Author, SmallDesc, BigDesc: UnicodeString;
    IsCoop: Boolean; //Some multiplayer missions are defined as coop
    IsSpecial: Boolean; //Some missions are defined as special (e.g. tower defence, quest, etc.)
    IsFavourite: Boolean;
    BlockTeamSelection: Boolean;
    BlockPeacetime: Boolean;
    BlockFullMapPreview: Boolean;

    constructor Create(const aFolder: string; aStrictParsing: Boolean; aMapFolder: TMapFolder); overload;
    destructor Destroy; override;

    procedure AddGoal(aType: TGoalType; aPlayer: TKMHandIndex; aCondition: TGoalCondition; aStatus: TGoalStatus; aPlayerIndex: TKMHandIndex);
    procedure LoadExtra;

    property InfoAmount: TKMMapInfoAmount read fInfoAmount;
    property Path: string read fPath;
    property MapFolder: TMapFolder read fMapFolder;
    property FileName: UnicodeString read fFileName;
    function FullPath(const aExt: string): string;
    function HumanUsableLocations: TKMHandIndexArray;
    function AIUsableLocations: TKMHandIndexArray;
    property CRC: Cardinal read fCRC;
    function LocationName(aIndex: TKMHandIndex): string;
    property SizeText: string read GetSizeText;
    function IsValid: Boolean;
    function HumanPlayerCount: Byte;
    function HumanPlayerCountMP: Byte;
    function AIOnlyLocCount: Byte;
    function FileNameWithoutHash: UnicodeString;
    function HasReadme: Boolean;
    function ViewReadme: Boolean;
    function GetLobbyColor: Cardinal;
    function IsFilenameEndMatchHash: Boolean;
  end;


  TTCustomMapsScanner = class(TThread)
  private
    fMapFolders: TMapFolderSet;
    procedure ProcessMap(aPath: UnicodeString; aFolder: TMapFolder); virtual; abstract;
  public
    constructor Create(aMapFolders: TMapFolderSet);
    procedure Execute; override;
  end;

  TTMapsScanner = class(TTCustomMapsScanner)
  private
    fOnMapAdd: TMapEvent;
    fOnMapAddDone: TNotifyEvent;
    procedure ProcessMap(aPath: UnicodeString; aFolder: TMapFolder); override;
  public
    constructor Create(aMapFolders: TMapFolderSet; aOnMapAdd: TMapEvent; aOnMapAddDone, aOnComplete: TNotifyEvent);
  end;

  TTMapsCacheUpdater = class(TTCustomMapsScanner)
  private
    procedure ProcessMap(aPath: UnicodeString; aFolder: TMapFolder); override;
  public
    constructor Create(aMapFolders: TMapFolderSet);
  end;


  TKMapsCollection = class
  private
    fCount: Integer;
    fMaps: array of TKMapInfo;
    fMapFolders: TMapFolderSet;
    fSortMethod: TMapsSortMethod;
    fDoSortWithFavourites: Boolean;
    CS: TCriticalSection;
    fScanner: TTMapsScanner;
    fScanning: Boolean; //Flag if scan is in progress
    fUpdateNeeded: Boolean;
    fOnRefresh: TNotifyEvent;
    fOnComplete: TNotifyEvent;
    procedure Clear;
    procedure MapAdd(aMap: TKMapInfo);
    procedure MapAddDone(Sender: TObject);
    procedure ScanComplete(Sender: TObject);
    procedure DoSort;
    function GetMap(aIndex: Integer): TKMapInfo;
  public
    constructor Create(aMapFolders: TMapFolderSet; aSortMethod: TMapsSortMethod = smByNameDesc; aDoSortWithFavourites: Boolean = False); overload;
    constructor Create(aMapFolder: TMapFolder; aSortMethod: TMapsSortMethod = smByNameDesc; aDoSortWithFavourites: Boolean = False); overload;
    destructor Destroy; override;

    property Count: Integer read fCount;
    property Maps[aIndex: Integer]: TKMapInfo read GetMap; default;
    procedure Lock;
    procedure Unlock;

    class function FullPath(const aName, aExt: string; aMultiplayer: Boolean): string; overload;
    class function FullPath(const aName, aExt: string; aMapFolder: TMapFolder): string; overload;
    class function FullPath(const aName, aExt: string; aMapFolder: TMapFolder; aCRC: Cardinal): string; overload;
    class function GuessMPPath(const aName, aExt: string; aCRC: Cardinal): string;
    class procedure GetAllMapPaths(aExeDir: string; aList: TStringList);

    procedure Refresh(aOnRefresh: TNotifyEvent; aOnComplete: TNotifyEvent = nil);
    procedure TerminateScan;
    procedure Sort(aSortMethod: TMapsSortMethod; aOnSortComplete: TNotifyEvent);
    property SortMethod: TMapsSortMethod read fSortMethod; //Read-only because we should not change it while Refreshing

    procedure DeleteMap(aIndex: Integer);
    procedure MoveMap(aIndex: Integer; aName: UnicodeString; aMapFolder: TMapFolder);

    procedure UpdateState;
  end;


  function DetermineMapFolder(aFolderName: UnicodeString; out oMapFolder: TMapFolder): Boolean;


implementation
uses
  KM_CommonClasses, KM_MissionScript_Info, KM_ResTexts, KM_Utils, KM_GameApp;


const
  //Folder name containing single maps for SP/MP/DL mode
  MAP_FOLDER: array [TMapFolder] of string = (MAPS_FOLDER_NAME, MAPS_MP_FOLDER_NAME, MAPS_DL_FOLDER_NAME);
  MAP_FOLDER_TYPE_MP: array [Boolean] of TMapFolder = (mfSP, mfMP);


{ TKMapInfo }
constructor TKMapInfo.Create(const aFolder: string; aStrictParsing: Boolean; aMapFolder: TMapFolder);

  function GetLIBXCRC(aSearchFile: UnicodeString): Cardinal;
  var SearchRec: TSearchRec;
  begin
    Result := 0;
    FindFirst(aSearchFile, faAnyFile - faDirectory, SearchRec);
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        Result := Result xor Adler32CRC(ExtractFilePath(aSearchFile) + SearchRec.Name);
    until (FindNext(SearchRec) <> 0);
    FindClose(SearchRec);
  end;

var
  DatFile, MapFile, ScriptFile, TxtFile, LIBXFiles: string;
  DatCRC, OthersCRC: Cardinal;
  fMissionParser: TMissionParserInfo;
begin
  inherited Create;

  fPath := ExeDir + MAP_FOLDER[aMapFolder] + PathDelim + aFolder + PathDelim;
  fFileName := aFolder;
  fMapFolder := aMapFolder;

  DatFile := fPath + fFileName + '.dat';
  MapFile := fPath + fFileName + '.map';
  ScriptFile := fPath + fFileName + '.script'; //Needed for CRC
  TxtFile := fPath + fFileName + '.txt'; //Needed for CRC
  LIBXFiles := fPath + fFileName + '.*.libx'; //Needed for CRC

  fSizeText := ''; //Lazy initialization

  if not FileExists(DatFile) then Exit;

  //Try loading info from cache, since map scanning is rather slow
  LoadFromFile(fPath + fFileName + '.mi'); //Data will be empty if failed

  //We will scan map once again if anything has changed
  //In SP mode (non-strict) we check DAT CRC and version, that is enough
  //In MP mode (strict) we also need exact CRCs to match maps between players

  DatCRC := Adler32CRC(DatFile);
  //.map file CRC is the slowest, so only calculate it if necessary
  OthersCRC := 0; //Supresses incorrect warning by Delphi
  if aStrictParsing then
    OthersCRC := Adler32CRC(MapFile) xor Adler32CRC(ScriptFile) xor Adler32CRC(TxtFile) xor GetLIBXCRC(LIBXFiles);

  //Does the map need to be fully rescanned? (.mi cache is outdated?)
  if (fVersion <> GAME_REVISION) or
     (fDatCRC <> DatCRC) or //In non-strict mode only DAT CRC matters (SP)
     (aStrictParsing and (fCRC <> DatCRC xor OthersCRC)) //In strict mode we check all CRCs (MP)
  then
  begin
    //Calculate OthersCRC if it wasn't calculated before
    if not aStrictParsing then
      OthersCRC := Adler32CRC(MapFile) xor Adler32CRC(ScriptFile) xor Adler32CRC(TxtFile);

    fCRC := DatCRC xor OthersCRC;
    fDatCRC := DatCRC;
    fVersion := GAME_REVISION;

    //First reset everything because e.g. CanBeHuman is assumed false by default and set true when we encounter SET_USER_PLAYER
    ResetInfo;

    fMissionParser := TMissionParserInfo.Create;
    try
      //Fill Self properties with MissionParser
      fMissionParser.LoadMission(DatFile, Self, pmBase);
    finally
      fMissionParser.Free;
    end;

    //Load additional text info
    LoadTXTInfo;

    IsFavourite := gGameApp.GameSettings.FavouriteMaps.Contains(fCRC);

    SaveToFile(fPath + fFileName + '.mi'); //Save new cache file
  end;

  fInfoAmount := iaBase;
end;


destructor TKMapInfo.Destroy;
begin

  inherited;
end;


procedure TKMapInfo.AddGoal(aType: TGoalType; aPlayer: TKMHandIndex; aCondition: TGoalCondition; aStatus: TGoalStatus; aPlayerIndex: TKMHandIndex);
var G: TKMMapGoalInfo;
begin
  G.Cond := aCondition;
  G.Play := aPlayerIndex;
  G.Stat := aStatus;

  case aType of
    glt_Victory:  begin
                    SetLength(GoalsVictory[aPlayer], GoalsVictoryCount[aPlayer] + 1);
                    GoalsVictory[aPlayer, GoalsVictoryCount[aPlayer]] := G;
                    Inc(GoalsVictoryCount[aPlayer]);
                  end;
    glt_Survive:  begin
                    SetLength(GoalsSurvive[aPlayer], GoalsSurviveCount[aPlayer] + 1);
                    GoalsSurvive[aPlayer, GoalsSurviveCount[aPlayer]] := G;
                    Inc(GoalsSurviveCount[aPlayer]);
                  end;
    else          ;
  end;
end;


function TKMapInfo.FullPath(const aExt: string): string;
begin
  Result := fPath + fFileName + aExt;
end;


function TKMapInfo.HumanUsableLocations: TKMHandIndexArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to MAX_HANDS - 1 do
    if CanBeHuman[I] then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := I;
    end;
end;


function TKMapInfo.AIUsableLocations: TKMHandIndexArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to MAX_HANDS - 1 do
    if CanBeAI[I] then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := I;
    end;
end;


function TKMapInfo.LocationName(aIndex: TKMHandIndex): string;
begin
  Result := Format(gResTexts[TX_LOBBY_LOCATION_X], [aIndex + 1]);
end;


function TKMapInfo.GetSizeText: string;
begin
  if fSizeText = '' then
    fSizeText := MapSizeText(MapSizeX, MapSizeY);
  Result := fSizeText;
end;


procedure TKMapInfo.LoadTXTInfo;

  function LoadDescriptionFromLIBX(aIndex: Integer): UnicodeString;
  var MissionTexts: TKMTextLibrarySingle;
  begin
    if aIndex = -1 then Exit;
    MissionTexts := TKMTextLibrarySingle.Create;
    MissionTexts.LoadLocale(fPath + fFileName + '.%s.libx');
    Result := MissionTexts.Texts[aIndex];
    MissionTexts.Free;
  end;

var
  st, S: string;
  ft: TextFile;
begin
  //Load additional text info
  if FileExists(fPath + fFileName + '.txt') then
  begin
    AssignFile(ft, fPath + fFileName + '.txt');
    FileMode := fmOpenRead;
    Reset(ft);
    repeat
      ReadLn(ft, st);
      if SameText(st, 'Author')    then Readln(ft, Author);
      if SameText(st, 'BigDesc')   then Readln(ft, BigDesc);
      if SameText(st, 'BigDescLIBX') then
      begin
        Readln(ft, S);
        BigDesc := LoadDescriptionFromLIBX(StrToIntDef(S, -1));
      end;
      if SameText(st, 'SmallDesc') then ReadLn(ft, SmallDesc);
      if SameText(st, 'SmallDescLIBX') then
      begin
        Readln(ft, S);
        SmallDesc := LoadDescriptionFromLIBX(StrToIntDef(S, -1));
      end;
      if SameText(st, 'SetCoop')   then
      begin
        IsCoop := True;
        BlockPeacetime := True;
        BlockTeamSelection := True;
        BlockFullMapPreview := True;
      end;
      if SameText(st, 'SetSpecial')then IsSpecial := True;
      if SameText(st, 'BlockPeacetime') then BlockPeacetime := True;
      if SameText(st, 'BlockTeamSelection') then BlockTeamSelection := True;
      if SameText(st, 'BlockFullMapPreview') then BlockFullMapPreview := True;
    until(eof(ft));
    CloseFile(ft);
  end;
end;


//Load additional information for map that is not in main SP list
procedure TKMapInfo.LoadExtra;
var
  DatFile: string;
  fMissionParser: TMissionParserInfo;
begin
  //Do not append Extra info twice
  if fInfoAmount = iaExtra then Exit;

  //First reset everything because e.g. CanBeHuman is assumed false by default and set true when we encounter SET_USER_PLAYER
  ResetInfo;

  DatFile := fPath + fFileName + '.dat';

  fMissionParser := TMissionParserInfo.Create;
  try
    //Fill Self properties with MissionParser
    fMissionParser.LoadMission(DatFile, Self, pmExtra);
  finally
    fMissionParser.Free;
  end;

  if MissionMode = mm_Tactic then
    BlockPeacetime := True;

  LoadTXTInfo;

  fInfoAmount := iaExtra;
end;


procedure TKMapInfo.ResetInfo;
var I, K: Integer;
begin
  IsCoop := False;
  IsSpecial := False;
  MissionMode := mm_Normal;
  DefaultHuman := 0;
  Author := '';
  SmallDesc := '';
  BigDesc := '';
  for I:=0 to MAX_HANDS-1 do
  begin
    FlagColors[I] := DefaultTeamColors[I];
    CanBeHuman[I] := False;
    CanBeAI[I] := False;
    GoalsVictoryCount[I] := 0;
    SetLength(GoalsVictory[I], 0);
    GoalsSurviveCount[I] := 0;
    SetLength(GoalsSurvive[I], 0);
    for K:=0 to MAX_HANDS-1 do
      if I = K then
        Alliances[I,K] := at_Ally
      else
        Alliances[I,K] := at_Enemy;
  end;
end;


procedure TKMapInfo.LoadFromFile(const aPath: string);
var
  S: TKMemoryStream;
begin
  if not FileExists(aPath) then Exit;

  S := TKMemoryStream.Create;
  S.LoadFromFile(aPath);

  //Internal properties
  S.Read(fCRC);
  S.Read(fDatCRC);
  S.ReadA(fVersion);

  //Exposed properties
  S.Read(MapSizeX);
  S.Read(MapSizeY);
  S.Read(MissionMode, SizeOf(TKMissionMode));
  S.Read(LocCount);
  S.ReadW(SmallDesc);
  S.Read(IsCoop);
  S.Read(IsSpecial);
  S.Read(CanBeHuman, SizeOf(CanBeHuman));
  S.Read(BlockTeamSelection);
  S.Read(BlockPeacetime);
  S.Read(BlockFullMapPreview);

  IsFavourite := gGameApp.GameSettings.FavouriteMaps.Contains(fCRC);

  //Other properties are not saved, they are fast to reload
  S.Free;
end;


procedure TKMapInfo.SaveToFile(const aPath: string);
var
  S: TKMemoryStream;
begin
  S := TKMemoryStream.Create;
  try
    //Internal properties
    S.Write(fCRC);
    S.Write(fDatCRC);
    S.WriteA(fVersion);

    //Exposed properties
    S.Write(MapSizeX);
    S.Write(MapSizeY);
    S.Write(MissionMode, SizeOf(TKMissionMode));
    S.Write(LocCount);
    S.WriteW(SmallDesc);
    S.Write(IsCoop);
    S.Write(IsSpecial);
    S.Write(CanBeHuman, SizeOf(CanBeHuman));
    S.Write(BlockTeamSelection);
    S.Write(BlockPeacetime);
    S.Write(BlockFullMapPreview);

    //Other properties from text file are not saved, they are fast to reload
    S.SaveToFile(aPath);
  finally
    S.Free;
  end;
end;


function TKMapInfo.IsValid: Boolean;
begin
  Result := (LocCount > 0) and
            FileExists(fPath + fFileName + '.dat') and
            FileExists(fPath + fFileName + '.map');
end;


function TKMapInfo.HumanPlayerCount: Byte;
var I: Integer;
begin
  Result := 0;
  for I := 0 to MAX_HANDS - 1 do
    if CanBeHuman[I] then
      Inc(Result);
end;


function TKMapInfo.HumanPlayerCountMP: Byte;
begin
  Result := HumanPlayerCount;
  //Enforce MP limit
  if Result > MAX_LOBBY_PLAYERS then
    Result := MAX_LOBBY_PLAYERS;
end;


function TKMapInfo.AIOnlyLocCount: Byte;
var I: Integer;
begin
  Result := 0;
  for I := 0 to MAX_HANDS - 1 do
    if CanBeAI[I] and not CanBeHuman[I] then
      Inc(Result);
end;


//Returns True if map filename ends with this map actual CRC hash.
//Used to check if downloaded map was changed
function TKMapInfo.IsFilenameEndMatchHash: Boolean;
begin
  Result := (Length(fFileName) > 9)
    and (fFileName[Length(FileName)-8] = '_')
    and (IntToHex(fCRC, 8) = RightStr(fFileName, 8));
end;


function TKMapInfo.FileNameWithoutHash: UnicodeString;
begin
  if (MapFolder = mfDL) and IsFilenameEndMatchHash then
    Result := LeftStr(FileName, Length(FileName)-9)
  else
    Result := FileName;
end;


function TKMapInfo.DetermineReadmeFilePath: String;
var Path: String;
    Locale: AnsiString;
begin
  Result := '';
  Locale := gGameApp.GameSettings.Locale;
  Path := fPath + fFileName + '.' + String(Locale) + '.pdf'; // Try to file with our locale first
  if FileExists(Path) then
    Result := Path
  else
  begin
    Path := fPath + fFileName + '.' + String(DEFAULT_LOCALE) + '.pdf'; // then with default locale
    if FileExists(Path) then
      Result := Path
    else
    begin
      Path := fPath + fFileName + '.pdf'; // and finally without any locale
      if FileExists(Path) then
        Result := Path;
    end;
  end;
end;


function TKMapInfo.HasReadme: Boolean;
begin
  Result := DetermineReadmeFilePath <> '';
end;


function TKMapInfo.ViewReadme: Boolean;
begin
  Result := OpenPDF(DetermineReadmeFilePath);
end;


function TKMapInfo.GetLobbyColor: Cardinal;
begin
  if fMapFolder = mfDL then
    Result := $FFC9BBBB
  else
    Result := $FF9CF6FF;
end;


{ TKMapsCollection }
constructor TKMapsCollection.Create(aMapFolders: TMapFolderSet; aSortMethod: TMapsSortMethod = smByNameDesc; aDoSortWithFavourites: Boolean = False);
begin
  inherited Create;
  fMapFolders := aMapFolders;
  fSortMethod := aSortMethod;
  fDoSortWithFavourites := aDoSortWithFavourites;

  //CS is used to guard sections of code to allow only one thread at once to access them
  //We mostly don't need it, as UI should access Maps only when map events are signaled
  //it mostly acts as a safenet
  CS := TCriticalSection.Create;
end;


constructor TKMapsCollection.Create(aMapFolder: TMapFolder; aSortMethod: TMapsSortMethod = smByNameDesc; aDoSortWithFavourites: Boolean = False);
begin
  Create([aMapFolder], aSortMethod, aDoSortWithFavourites);
end;


destructor TKMapsCollection.Destroy;
begin
  //Terminate and release the Scanner if we have one working or finished
  TerminateScan;

  //Release TKMapInfo objects
  Clear;

  CS.Free;
  inherited;
end;


function TKMapsCollection.GetMap(aIndex: Integer): TKMapInfo;
begin
  //No point locking/unlocking here since we return a TObject that could be modified/freed
  //by another thread before the caller uses it.
  Assert(InRange(aIndex, 0, fCount - 1));
  Result := fMaps[aIndex];
end;


class function TKMapsCollection.GuessMPPath(const aName, aExt: string; aCRC: Cardinal): string;
var S: UnicodeString;
begin
  S := aName + '_' + IntToHex(aCRC, 8);
  Result := MAP_FOLDER[mfDL] + PathDelim + S + PathDelim + S + aExt;
  if not FileExists(ExeDir + Result) then
    Result := MAP_FOLDER[mfMP] + PathDelim + aName + PathDelim + aName + aExt;
end;


procedure TKMapsCollection.Lock;
begin
  CS.Enter;
end;


procedure TKMapsCollection.Unlock;
begin
  CS.Leave;
end;


procedure TKMapsCollection.Clear;
var
  I: Integer;
begin
  Assert(not fScanning, 'Guarding from access to inconsistent data');
  for I := 0 to fCount - 1 do
    FreeAndNil(fMaps[I]);
  fCount := 0;
end;


procedure TKMapsCollection.UpdateState;
begin
  if fUpdateNeeded then
  begin
    if Assigned(fOnRefresh) then
      fOnRefresh(Self);

    fUpdateNeeded := False;
  end;
end;


procedure TKMapsCollection.DeleteMap(aIndex: Integer);
var
  I: Integer;
begin
   Lock;
   try
     Assert(InRange(aIndex, 0, fCount - 1));
     {$IFDEF FPC} DeleteDirectory(fMaps[aIndex].Path, False); {$ENDIF}
     {$IFDEF WDC} TDirectory.Delete(fMaps[aIndex].Path, True); {$ENDIF}
     fMaps[aIndex].Free;
     for I  := aIndex to fCount - 2 do
       fMaps[I] := fMaps[I + 1];
     Dec(fCount);
     SetLength(fMaps, fCount);
   finally
     Unlock;
   end;
end;


procedure TKMapsCollection.MoveMap(aIndex: Integer; aName: UnicodeString; aMapFolder: TMapFolder);
var
  I: Integer;
  Dest, RenamedFile: UnicodeString;
  SearchRec: TSearchRec;
begin
  if Trim(aName) = '' then Exit;
  
  Lock;
  try
    Dest := ExeDir + MAP_FOLDER[aMapFolder] + PathDelim + aName + PathDelim;
    Assert(fMaps[aIndex].Path <> Dest);
    Assert(InRange(aIndex, 0, fCount - 1));

    //Remove existing dest directory
    if DirectoryExists(Dest) then
    begin
     {$IFDEF FPC} DeleteDirectory(Dest, False); {$ENDIF}
     {$IFDEF WDC} TDirectory.Delete(Dest, True); {$ENDIF}
    end;

    //Move directory to dest
    {$IFDEF FPC} RenameFile(fMaps[aIndex].Path, Dest); {$ENDIF}
    {$IFDEF WDC} TDirectory.Move(fMaps[aIndex].Path, Dest); {$ENDIF}

    //Rename all the files in dest
    FindFirst(Dest + fMaps[aIndex].FileName + '*', faAnyFile - faDirectory, SearchRec);
    repeat
     if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
     and (Length(SearchRec.Name) > Length(fMaps[aIndex].FileName)) then
     begin
       RenamedFile := Dest + aName + RightStr(SearchRec.Name, Length(SearchRec.Name) - Length(fMaps[aIndex].FileName));
       if not FileExists(RenamedFile) and (Dest + SearchRec.Name <> RenamedFile) then
         {$IFDEF FPC} RenameFile(Dest + SearchRec.Name, RenamedFile); {$ENDIF}
         {$IFDEF WDC} TFile.Move(Dest + SearchRec.Name, RenamedFile); {$ENDIF}
     end;
    until (FindNext(SearchRec) <> 0);
    FindClose(SearchRec);

    //Remove the map from our list
    fMaps[aIndex].Free;
    for I  := aIndex to fCount - 2 do
     fMaps[I] := fMaps[I + 1];
    Dec(fCount);
    SetLength(fMaps, fCount);
  finally
    Unlock;
  end;
end;


//For private access, where CS is managed by the caller
procedure TKMapsCollection.DoSort;
var TempMaps: array of TKMapInfo;

  //Return True if items should be exchanged
  function Compare(A, B: TKMapInfo): Boolean;
  begin
    Result := False; //By default everything remains in place
    case fSortMethod of
      smByFavouriteAsc:       Result := A.IsFavourite and not B.IsFavourite;
      smByFavouriteDesc:      Result := not A.IsFavourite and B.IsFavourite;
      smByNameAsc:            Result := CompareText(A.FileName, B.FileName) < 0;
      smByNameDesc:           Result := CompareText(A.FileName, B.FileName) > 0;
      smBySizeAsc:            Result := MapSizeIndex(A.MapSizeX, A.MapSizeY) < MapSizeIndex(B.MapSizeX, B.MapSizeY);
      smBySizeDesc:           Result := MapSizeIndex(A.MapSizeX, A.MapSizeY) > MapSizeIndex(B.MapSizeX, B.MapSizeY);
      smByPlayersAsc:         Result := A.LocCount < B.LocCount;
      smByPlayersDesc:        Result := A.LocCount > B.LocCount;
      smByHumanPlayersAsc:    Result := A.HumanPlayerCount < B.HumanPlayerCount;
      smByHumanPlayersDesc:   Result := A.HumanPlayerCount > B.HumanPlayerCount;
      smByHumanPlayersMPAsc:  Result := A.HumanPlayerCountMP < B.HumanPlayerCountMP;
      smByHumanPlayersMPDesc: Result := A.HumanPlayerCountMP > B.HumanPlayerCountMP;
      smByModeAsc:            Result := A.MissionMode < B.MissionMode;
      smByModeDesc:           Result := A.MissionMode > B.MissionMode;
    end;
    if fDoSortWithFavourites and not (fSortMethod in [smByFavouriteAsc, smByFavouriteDesc]) then
    begin
      if A.IsFavourite and not B.IsFavourite then
        Result := False
      else if not A.IsFavourite and B.IsFavourite then
        Result := True
    end;

  end;

  procedure MergeSort(aLeft, aRight: Integer);
  var Middle, I, J, Ind1, Ind2: integer;
  begin
    if aRight <= aLeft then
      exit;

    Middle := (aLeft+aRight) div 2;
    MergeSort(aLeft, Middle);
    Inc(Middle);
    MergeSort(Middle, aRight);
    Ind1 := aLeft;
    Ind2 := Middle;
    for I := aLeft to aRight do
    begin
      if (Ind1 < Middle) and ((Ind2 > aRight) or not Compare(fMaps[Ind1], fMaps[Ind2])) then
      begin
        TempMaps[I] := fMaps[Ind1];
        Inc(Ind1);
      end
      else
      begin
        TempMaps[I] := fMaps[Ind2];
        Inc(Ind2);
      end;
    end;
    for J := aLeft to aRight do
      fMaps[J] := TempMaps[J];
  end;
begin
  SetLength(TempMaps, Length(fMaps));
  MergeSort(Low(fMaps), High(fMaps));
end;


//For public access
//Apply new Sort within Critical Section, as we could be in the Refresh phase
//note that we need to preserve fScanning flag
procedure TKMapsCollection.Sort(aSortMethod: TMapsSortMethod; aOnSortComplete: TNotifyEvent);
begin
  Lock;
  try
    if fScanning then
    begin
      fScanning := False;
      fSortMethod := aSortMethod;
      DoSort;
      if Assigned(aOnSortComplete) then
        aOnSortComplete(Self);
      fScanning := True;
    end
    else
    begin
      fSortMethod := aSortMethod;
      DoSort;
      if Assigned(aOnSortComplete) then
        aOnSortComplete(Self);
    end;
  finally
    Unlock;
  end;
end;


procedure TKMapsCollection.TerminateScan;
begin
  if (fScanner <> nil) then
  begin
    fScanner.Terminate;
    fScanner.WaitFor;
    fScanner.Free;
    fScanner := nil;
    fScanning := False;
  end;
  fUpdateNeeded := False; //If the scan was terminated we should not run fOnRefresh next UpdateState
end;


//Start the refresh of maplist
procedure TKMapsCollection.Refresh(aOnRefresh: TNotifyEvent; aOnComplete: TNotifyEvent = nil);
begin
  //Terminate previous Scanner if two scans were launched consequentialy
  TerminateScan;
  Clear;

  fOnRefresh := aOnRefresh;
  fOnComplete := aOnComplete;

  //Scan will launch upon create automatcally
  fScanning := True;
  fScanner := TTMapsScanner.Create(fMapFolders, MapAdd, MapAddDone, ScanComplete);
end;


procedure TKMapsCollection.MapAdd(aMap: TKMapInfo);
begin
  Lock;
  try
    SetLength(fMaps, fCount + 1);
    fMaps[fCount] := aMap;
    Inc(fCount);

    //Set the scanning to false so we could Sort
    fScanning := False;

    //Keep the maps sorted
    //We signal from Locked section, so everything caused by event can safely access our Maps
    DoSort;

    fScanning := True;
  finally
    Unlock;
  end;
end;


procedure TKMapsCollection.MapAddDone(Sender: TObject);
begin
  fUpdateNeeded := True; //Next time the GUI thread calls UpdateState we will run fOnRefresh
end;


//All maps have been scanned
//No need to resort since that was done in last MapAdd event
procedure TKMapsCollection.ScanComplete(Sender: TObject);
begin
  Lock;
  try
    fScanning := False;
    if Assigned(fOnComplete) then
      fOnComplete(Self);
  finally
    Unlock;
  end;
end;


class function TKMapsCollection.FullPath(const aName, aExt: string; aMultiplayer: Boolean): string;
begin
  Result := FullPath(aName, aExt, MAP_FOLDER_TYPE_MP[aMultiplayer]);
end;


class function TKMapsCollection.FullPath(const aName, aExt: string; aMapFolder: TMapFolder): string;
begin
  Result := ExeDir + MAP_FOLDER[aMapFolder] + PathDelim + aName + PathDelim + aName + aExt;
end;


class function TKMapsCollection.FullPath(const aName, aExt: string; aMapFolder: TMapFolder; aCRC: Cardinal): string;
var S: UnicodeString;
begin
  S := aName;
  if aMapFolder = mfDL then
    S := S + '_' + IntToHex(Integer(aCRC), 8);
  Result := FullPath(S, aExt, aMapFolder);
end;


class procedure TKMapsCollection.GetAllMapPaths(aExeDir: string; aList: TStringList);
var
  I: Integer;
  SearchRec: TSearchRec;
  PathToMaps: TStringList;
begin
  aList.Clear;

  PathToMaps := TStringList.Create;
  try
    PathToMaps.Add(aExeDir + MAPS_FOLDER_NAME + PathDelim);
    PathToMaps.Add(aExeDir + MAPS_MP_FOLDER_NAME + PathDelim);
    PathToMaps.Add(aExeDir + TUTORIALS_FOLDER_NAME + PathDelim);

    //Include all campaigns maps
    FindFirst(aExeDir + CAMPAIGNS_FOLDER_NAME + PathDelim + '*', faDirectory, SearchRec);
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        PathToMaps.Add(aExeDir + CAMPAIGNS_FOLDER_NAME + PathDelim + SearchRec.Name + PathDelim);
    until (FindNext(SearchRec) <> 0);
    FindClose(SearchRec);

    for I := 0 to PathToMaps.Count - 1 do
    if DirectoryExists(PathToMaps[I]) then
    begin
      FindFirst(PathToMaps[I] + '*', faDirectory, SearchRec);
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
        and FileExists(PathToMaps[I] + SearchRec.Name + PathDelim + SearchRec.Name + '.dat')
        and FileExists(PathToMaps[I] + SearchRec.Name + PathDelim + SearchRec.Name + '.map') then
          aList.Add(PathToMaps[I] + SearchRec.Name + PathDelim + SearchRec.Name + '.dat');
      until (FindNext(SearchRec) <> 0);
      FindClose(SearchRec);
    end;
  finally
    PathToMaps.Free;
  end;
end;


{ TTCustomMapsScanner }
constructor TTCustomMapsScanner.Create(aMapFolders: TMapFolderSet);
begin
  //Thread isn't started until all constructors have run to completion
  //so Create(False) may be put in front as well
  inherited Create(False);

  fMapFolders := aMapFolders;
  FreeOnTerminate := False;
end;


procedure TTCustomMapsScanner.Execute;
var
  SearchRec: TSearchRec;
  PathToMaps: string;
  MF: TMapFolder;
begin
  for MF in fMapFolders do
  begin
    PathToMaps := ExeDir + MAP_FOLDER[MF] + PathDelim;

    if not DirectoryExists(PathToMaps) then Exit;

    FindFirst(PathToMaps + '*', faDirectory, SearchRec);
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
      and FileExists(TKMapsCollection.FullPath(SearchRec.Name, '.dat', MF))
      and FileExists(TKMapsCollection.FullPath(SearchRec.Name, '.map', MF)) then
      begin
        ProcessMap(SearchRec.Name, MF);
      end;
    until (FindNext(SearchRec) <> 0) or Terminated;
    FindClose(SearchRec);
  end;
end;


{ TTMapsScanner }
//aOnMapAdd - signal that there's new map that should be added
//aOnMapAddDone - signal that map has been added
//aOnComplete - scan is complete
constructor TTMapsScanner.Create(aMapFolders: TMapFolderSet; aOnMapAdd: TMapEvent; aOnMapAddDone, aOnComplete: TNotifyEvent);
begin
  inherited Create(aMapFolders);

  Assert(Assigned(aOnMapAdd));

  fOnMapAdd := aOnMapAdd;
  fOnMapAddDone := aOnMapAddDone;
  OnTerminate := aOnComplete;
  FreeOnTerminate := False;
end;


procedure TTMapsScanner.ProcessMap(aPath: UnicodeString; aFolder: TMapFolder);
var
  Map: TKMapInfo;
begin
  Map := TKMapInfo.Create(aPath, False, aFolder);

  if SLOW_MAP_SCAN then
    Sleep(50);

  fOnMapAdd(Map);
  fOnMapAddDone(Self);
end;


{ TTMapsCacheUpdater }
constructor TTMapsCacheUpdater.Create(aMapFolders: TMapFolderSet);
begin
  inherited Create(aMapFolders);
  FreeOnTerminate := True;
end;


procedure TTMapsCacheUpdater.ProcessMap(aPath: UnicodeString; aFolder: TMapFolder);
var
  Map: TKMapInfo;
begin
  //Simply creating the TKMapInfo updates the .mi cache file
  Map := TKMapInfo.Create(aPath, False, aFolder);
  Map.Free;
end;


{Utility methods}
//Try to determine TMapFolder for specified aFolderName
//Returns true when succeeded
function DetermineMapFolder(aFolderName: UnicodeString; out oMapFolder: TMapFolder): Boolean;
var F: TMapFolder;
begin
  for F := Low(TMapFolder) to High(TMapFolder) do
    if aFolderName = MAP_FOLDER[F] then
    begin
      oMapFolder := F;
      Result := True;
      Exit;
    end;
  Result := False;
end;


end.


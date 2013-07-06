unit KM_Maps;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SyncObjs, SysUtils, KM_Defaults;


type
  TMapsSortMethod = (
    smByNameAsc, smByNameDesc,
    smBySizeAsc, smBySizeDesc,
    smByPlayersAsc, smByPlayersDesc,
    smByHumanPlayersAsc, smByHumanPlayersDesc,
    smByModeAsc, smByModeDesc);

  TKMapInfo = class;
  TMapEvent = procedure (aMap: TKMapInfo) of object;
  TKMMapInfoAmount = (iaBase, iaExtra);

  TKMMapGoalInfo = packed record
    Cond: TGoalCondition;
    Play: TPlayerIndex;
    Stat: TGoalStatus;
  end;

  TKMapInfo = class
  private
    fPath: string;
    fFileName: string; //without extension
    fStrictParsing: Boolean; //Use strict map checking, important for MP
    fCRC: Cardinal;
    fDatCRC: Cardinal; //Used to speed up scanning
    fVersion: AnsiString; //Savegame version, yet unused in maps, they always have actual version
    fInfoAmount: TKMMapInfoAmount;
    procedure ResetInfo;
    procedure LoadFromFile(const aPath: string);
    procedure SaveToFile(const aPath: string);
  public
    MapSizeX, MapSizeY: Integer;
    MissionMode: TKMissionMode;
    LocCount: Byte;
    CanBeHuman: array [0..MAX_PLAYERS-1] of Boolean;
    CanBeAI: array [0..MAX_PLAYERS-1] of Boolean;
    DefaultHuman: TPlayerIndex;
    GoalsVictoryCount, GoalsSurviveCount: array [0..MAX_PLAYERS-1] of Byte;
    GoalsVictory: array [0..MAX_PLAYERS-1] of array of TKMMapGoalInfo;
    GoalsSurvive: array [0..MAX_PLAYERS-1] of array of TKMMapGoalInfo;
    Alliances: array [0..MAX_PLAYERS-1, 0..MAX_PLAYERS-1] of TAllianceType;
    FlagColors: array [0..MAX_PLAYERS-1] of Cardinal;
    Author, SmallDesc, BigDesc: AnsiString;
    IsCoop: Boolean; //Some multiplayer missions are defined as coop
    IsSpecial: Boolean; //Some missions are defined as special (e.g. tower defence, quest, etc.)

    constructor Create(const aFolder: string; aStrictParsing, aIsMultiplayer: Boolean);
    destructor Destroy; override;

    procedure AddGoal(aType: TGoalType; aPlayer: TPlayerIndex; aCondition: TGoalCondition; aStatus: TGoalStatus; aPlayerIndex: TPlayerIndex);
    procedure LoadExtra;

    property InfoAmount: TKMMapInfoAmount read fInfoAmount;
    property Path: string read fPath;
    property FileName: string read fFileName;
    function FullPath(const aExt: string): string;
    function HumanUsableLocations: TPlayerIndexArray;
    function AIUsableLocations: TPlayerIndexArray;
    property CRC: Cardinal read fCRC;
    function LocationName(aIndex: TPlayerIndex): string;
    function SizeText: string;
    function IsValid: Boolean;
    function HumanPlayerCount: Byte;
  end;

  TTMapsScanner = class(TThread)
  private
    fMultiplayerPath: Boolean;
    fOnMapAdd: TMapEvent;
    fOnMapAddDone: TNotifyEvent;
  public
    constructor Create(aMultiplayerPath: Boolean; aOnMapAdd: TMapEvent; aOnMapAddDone, aOnComplete: TNotifyEvent);
    procedure Execute; override;
  end;

  TKMapsCollection = class
  private
    fCount: Integer;
    fMaps: array of TKMapInfo;
    fMultiplayerPath: Boolean;
    fSortMethod: TMapsSortMethod;
    CS: TCriticalSection;
    fScanner: TTMapsScanner;
    fScanning: Boolean; //Flag if scan is in progress
    fUpdateNeeded: Boolean;
    fOnRefresh: TNotifyEvent;
    procedure Clear;
    procedure MapAdd(aMap: TKMapInfo);
    procedure MapAddDone(Sender: TObject);
    procedure ScanComplete(Sender: TObject);
    procedure DoSort;
    function GetMap(aIndex: Integer): TKMapInfo;
  public
    constructor Create(aMultiplayerPath: Boolean; aSortMethod: TMapsSortMethod = smByNameDesc);
    destructor Destroy; override;

    property Count: Integer read fCount;
    property Maps[aIndex: Integer]: TKMapInfo read GetMap; default;
    procedure Lock;
    procedure Unlock;

    class function FullPath(const aName, aExt: string; aMultiplayer: Boolean): string;
    class procedure GetAllMapPaths(aExeDir: string; aList: TStringList);

    procedure Refresh(aOnRefresh: TNotifyEvent);
    procedure TerminateScan;
    procedure Sort(aSortMethod: TMapsSortMethod; aOnSortComplete: TNotifyEvent);
    property SortMethod: TMapsSortMethod read fSortMethod; //Read-only because we should not change it while Refreshing

    //Should be accessed only as a part of aOnRefresh/aOnSort events handlers
    function MapList: string;
    procedure UpdateState;
  end;


implementation
uses KM_CommonClasses, KM_MissionScript_Info, KM_TextLibrary, KM_Utils;


const
  //Folder name containing single maps for SP/MP mode
  MAP_FOLDER_MP: array [Boolean] of string = ('Maps', 'MapsMP');


{ TKMapInfo }
constructor TKMapInfo.Create(const aFolder: string; aStrictParsing, aIsMultiplayer: Boolean);
var
  st, DatFile, MapFile, ScriptFile: string;
  DatCRC, OthersCRC: Cardinal;
  ft: TextFile;
  fMissionParser: TMissionParserInfo;
begin
  inherited Create;

  fPath := ExeDir + MAP_FOLDER_MP[aIsMultiplayer] + PathDelim + aFolder + PathDelim;
  fFileName := aFolder;

  fStrictParsing := aStrictParsing;

  DatFile := fPath + fFileName + '.dat';
  MapFile := fPath + fFileName + '.map';
  ScriptFile := fPath + fFileName + '.script'; //Needed for CRC

  if not FileExists(DatFile) then Exit;

  //Try loading info from cache, since map scanning is rather slow
  LoadFromFile(fPath + fFileName + '.mi'); //Data will be empty if failed

  //We will scan map once again if anything has changed
  //In SP mode (non-strict) we check DAT CRC and version, that is enough
  //In MP mode (strict) we also need exact CRCs to match maps between players

  DatCRC := Adler32CRC(DatFile);
  //.map file CRC is the slowest, so only calculate it if necessary
  OthersCRC := 0; //Supresses incorrect warning by Delphi
  if fStrictParsing then
    OthersCRC := Adler32CRC(MapFile) xor Adler32CRC(ScriptFile);

  //Does the map need to be fully rescanned? (.mi cache is outdated?)
  if (fVersion <> GAME_REVISION) or
     (fDatCRC <> DatCRC) or //In non-strict mode only DAT CRC matters (SP)
     (fStrictParsing and (fCRC <> DatCRC xor OthersCRC)) //In strict mode we check all CRCs (MP)
  then
  begin
    //Calculate OthersCRC if it wasn't calculated before
    if not fStrictParsing then
      OthersCRC := Adler32CRC(MapFile) xor Adler32CRC(ScriptFile);

    fCRC := DatCRC xor OthersCRC;
    fDatCRC := DatCRC;
    fVersion := GAME_REVISION;

    //First reset everything because e.g. CanBeHuman is assumed false by default and set true when we encounter SET_USER_PLAYER
    ResetInfo;

    fMissionParser := TMissionParserInfo.Create(False);
    try
      //Fill Self properties with MissionParser
      fMissionParser.LoadMission(DatFile, Self, pmBase);
    finally
      fMissionParser.Free;
    end;

    //Load additional text info
    if FileExists(fPath + fFileName + '.txt') then
    begin
      AssignFile(ft, fPath + fFileName + '.txt');
      FileMode := fmOpenRead;
      Reset(ft);
      repeat
        ReadLn(ft, st);
        if SameText(st, 'SmallDesc') then ReadLn(ft, SmallDesc);
        if SameText(st, 'SetCoop')   then IsCoop := True;
        if SameText(st, 'SetSpecial')then IsSpecial := True;
      until(eof(ft));
      CloseFile(ft);
    end;

    SaveToFile(fPath + fFileName + '.mi'); //Save new cache file
  end;

  fInfoAmount := iaBase;
end;


destructor TKMapInfo.Destroy;
begin

  inherited;
end;


procedure TKMapInfo.AddGoal(aType: TGoalType; aPlayer: TPlayerIndex; aCondition: TGoalCondition; aStatus: TGoalStatus; aPlayerIndex: TPlayerIndex);
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


function TKMapInfo.HumanUsableLocations: TPlayerIndexArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to MAX_PLAYERS - 1 do
    if CanBeHuman[I] then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := I;
    end;
end;


function TKMapInfo.AIUsableLocations: TPlayerIndexArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to MAX_PLAYERS - 1 do
    if CanBeAI[I] then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := I;
    end;
end;


function TKMapInfo.LocationName(aIndex: TPlayerIndex): string;
begin
  Result := Format(fTextMain[TX_LOBBY_LOCATION_X], [aIndex + 1]);
end;


function TKMapInfo.SizeText: string;
begin
  Result := MapSizeText(MapSizeX, MapSizeY);
end;


//Load additional information for map that is not in main SP list
procedure TKMapInfo.LoadExtra;
var
  st, DatFile, MapFile: string;
  ft: TextFile;
  fMissionParser: TMissionParserInfo;
begin
  //Do not append Extra info twice
  if fInfoAmount = iaExtra then Exit;

  //First reset everything because e.g. CanBeHuman is assumed false by default and set true when we encounter SET_USER_PLAYER
  ResetInfo;

  DatFile := fPath + fFileName + '.dat';
  MapFile := fPath + fFileName + '.map';

  fMissionParser := TMissionParserInfo.Create(False);
  try
    //Fill Self properties with MissionParser
    fMissionParser.LoadMission(DatFile, Self, pmExtra);
  finally
    fMissionParser.Free;
  end;

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
      if SameText(st, 'SetCoop')   then IsCoop := True;
      if SameText(st, 'SetSpecial')then IsSpecial := True;
    until(eof(ft));
    CloseFile(ft);
  end;

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
  for I:=0 to MAX_PLAYERS-1 do
  begin
    FlagColors[I] := DefaultTeamColors[I];
    CanBeHuman[I] := False;
    CanBeAI[I] := False;
    GoalsVictoryCount[I] := 0;
    SetLength(GoalsVictory[I], 0);
    GoalsSurviveCount[I] := 0;
    SetLength(GoalsSurvive[I], 0);
    for K:=0 to MAX_PLAYERS-1 do
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
  S.Read(fVersion);

  //Exposed properties
  S.Read(MapSizeX);
  S.Read(MapSizeY);
  S.Read(MissionMode, SizeOf(TKMissionMode));
  S.Read(LocCount);
  S.Read(SmallDesc);
  S.Read(IsCoop);
  S.Read(IsSpecial);
  S.Read(CanBeHuman, SizeOf(CanBeHuman));

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
    S.Write(fVersion);

    //Exposed properties
    S.Write(MapSizeX);
    S.Write(MapSizeY);
    S.Write(MissionMode, SizeOf(TKMissionMode));
    S.Write(LocCount);
    S.Write(SmallDesc);
    S.Write(IsCoop);
    S.Write(IsSpecial);
    S.Write(CanBeHuman, SizeOf(CanBeHuman));

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
  for I := 0 to MAX_PLAYERS - 1 do
    if CanBeHuman[I] then
      Inc(Result);
end;


{ TKMapsCollection }
constructor TKMapsCollection.Create(aMultiplayerPath: Boolean; aSortMethod: TMapsSortMethod = smByNameDesc);
begin
  inherited Create;
  fMultiplayerPath := aMultiplayerPath;
  fSortMethod := aSortMethod;

  //CS is used to guard sections of code to allow only one thread at once to access them
  //We mostly don't need it, as UI should access Maps only when map events are signaled
  //it mostly acts as a safenet
  CS := TCriticalSection.Create;
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


procedure TKMapsCollection.Lock;
begin
  CS.Enter;
end;


procedure TKMapsCollection.Unlock;
begin
  CS.Leave;
end;


function TKMapsCollection.MapList: string;
var I: Integer;
begin
  Lock;
    Result := '';
    for I := 0 to fCount - 1 do
      Result := Result + fMaps[I].FileName + eol;
  Unlock;
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


//For private acces, where CS is managed by the caller
procedure TKMapsCollection.DoSort;
  //Return True if items should be exchanged
  function Compare(A, B: TKMapInfo; aMethod: TMapsSortMethod): Boolean;
  begin
    Result := False; //By default everything remains in place
    case aMethod of
      smByNameAsc:      Result := CompareText(A.FileName, B.FileName) < 0;
      smByNameDesc:     Result := CompareText(A.FileName, B.FileName) > 0;
      smBySizeAsc:      Result := (A.MapSizeX * A.MapSizeY) < (B.MapSizeX * B.MapSizeY);
      smBySizeDesc:     Result := (A.MapSizeX * A.MapSizeY) > (B.MapSizeX * B.MapSizeY);
      smByPlayersAsc:   Result := A.LocCount < B.LocCount;
      smByPlayersDesc:  Result := A.LocCount > B.LocCount;
      smByHumanPlayersAsc:   Result := A.HumanPlayerCount < B.HumanPlayerCount;
      smByHumanPlayersDesc:  Result := A.HumanPlayerCount > B.HumanPlayerCount;
      smByModeAsc:      Result := A.MissionMode < B.MissionMode;
      smByModeDesc:     Result := A.MissionMode > B.MissionMode;
    end;
  end;
var
  I, K: Integer;
begin
  for I := 0 to fCount - 1 do
  for K := I to fCount - 1 do
  if Compare(fMaps[I], fMaps[K], fSortMethod) then
    SwapInt(Cardinal(fMaps[I]), Cardinal(fMaps[K])); //Exchange only pointers to MapInfo objects
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
end;


//Start the refresh of maplist
procedure TKMapsCollection.Refresh(aOnRefresh: TNotifyEvent);
begin
  //Terminate previous Scanner if two scans were launched consequentialy
  TerminateScan;
  Clear;

  fOnRefresh := aOnRefresh;

  //Scan will launch upon create automatcally
  fScanning := True;
  fScanner := TTMapsScanner.Create(fMultiplayerPath, MapAdd, MapAddDone, ScanComplete);
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
  finally
    Unlock;
  end;
end;


class function TKMapsCollection.FullPath(const aName, aExt: string; aMultiplayer: Boolean): string;
begin
  Result := ExeDir + MAP_FOLDER_MP[aMultiplayer] + PathDelim + aName + PathDelim + aName + aExt;
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
    PathToMaps.Add(aExeDir + 'Maps' + PathDelim);
    PathToMaps.Add(aExeDir + 'MapsMP' + PathDelim);
    PathToMaps.Add(aExeDir + 'Tutorials' + PathDelim);

    //Include all campaigns maps
    FindFirst(aExeDir + 'Campaigns'+PathDelim+'*', faDirectory, SearchRec);
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        PathToMaps.Add(aExeDir + 'Campaigns'+PathDelim + SearchRec.Name + PathDelim);
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


{ TTMapsScanner }
//aOnMapAdd - signal that there's new map that should be added
//aOnMapAddDone - signal that map has been added
//aOnComplete - scan is complete
constructor TTMapsScanner.Create(aMultiplayerPath: Boolean; aOnMapAdd: TMapEvent; aOnMapAddDone, aOnComplete: TNotifyEvent);
begin
  //Thread isn't started until all constructors have run to completion
  //so Create(False) may be put in front as well
  inherited Create(False);

  Assert(Assigned(aOnMapAdd));

  fMultiplayerPath := aMultiplayerPath;
  fOnMapAdd := aOnMapAdd;
  fOnMapAddDone := aOnMapAddDone;
  OnTerminate := aOnComplete;
  FreeOnTerminate := False;
end;


procedure TTMapsScanner.Execute;
var
  SearchRec: TSearchRec;
  PathToMaps: string;
  Map: TKMapInfo;
begin
  PathToMaps := ExeDir + MAP_FOLDER_MP[fMultiplayerPath] + PathDelim;

  if not DirectoryExists(PathToMaps) then Exit;

  FindFirst(PathToMaps + '*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
    and FileExists(TKMapsCollection.FullPath(SearchRec.Name, '.dat', fMultiplayerPath))
    and FileExists(TKMapsCollection.FullPath(SearchRec.Name, '.map', fMultiplayerPath)) then
    begin
      Map := TKMapInfo.Create(SearchRec.Name, false, fMultiplayerPath);
      if SLOW_MAP_SCAN then
        Sleep(50);
      fOnMapAdd(Map);
      fOnMapAddDone(Self);
    end;
  until (FindNext(SearchRec) <> 0) or Terminated;
  FindClose(SearchRec);
end;


end.


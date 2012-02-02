unit KM_MapInfo;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SyncObjs, SysUtils, KM_GameInfo;


type
  TKMapInfo = class
  private
    fInfo: TKMGameInfo;

    fPath: string;
    fFilename: string; //without extension

    fStrictParsing:boolean; //Use strict map checking, important for MP
    fDatSize:integer;
    fCRC: Cardinal;

    procedure ScanMap;
    procedure LoadFromFile(const aPath:string);
    procedure SaveToFile(const aPath:string);
  public
    SmallDesc, BigDesc: string;
    IsCoop: boolean; //Some multiplayer missions are defined as coop

    constructor Create;
    destructor Destroy; override;

    procedure Load(const aFolder:string; aStrictParsing, aIsMultiplayer:boolean);

    property Info: TKMGameInfo read fInfo;
    property Path: string read fPath;
    property Filename: string read fFilename;
    property CRC: Cardinal read fCRC;

    function IsValid:boolean;
  end;


  TMapsSortMethod = (
    smByNameAsc, smByNameDesc,
    smBySizeAsc, smBySizeDesc,
    smByPlayersAsc, smByPlayersDesc,
    smByModeAsc, smByModeDesc);

  TMapEvent = procedure (aMap: TKMapInfo) of object;


  TTScanner = class(TThread)
  private
    fMultiplayerPath: Boolean;
  public
    OnMapAdd: TMapEvent;
    constructor Create(aMultiplayerPath: Boolean);
    procedure Execute; override;
  end;

  TKMapsCollection = class
  private
    fCount: Integer;
    fMaps: array of TKMapInfo;
    fMultiplayerPath: Boolean;
    fSortMethod: TMapsSortMethod;
    CS: TCriticalSection;
    fScanner: TTScanner;
    fScanning: Boolean;
    function GetMap(aIndex: Integer): TKMapInfo;
    procedure SetSortMethod(aMethod: TMapsSortMethod);
    procedure MapAdd(aMap: TKMapInfo);
    procedure ScanComplete(Sender: TObject);
    procedure Sort;
  public
    OnRefreshComplete: TNotifyEvent;
    constructor Create(aMultiplayerPath: Boolean);
    destructor Destroy; override;

    property Count: Integer read fCount;
    property Map[aIndex: Integer]: TKMapInfo read GetMap; default;
    property Scanning: Boolean read fScanning;

    procedure Clear;
    procedure Lock;
    procedure Unlock;
    procedure Refresh;
    property SortMethod: TMapsSortMethod read fSortMethod write SetSortMethod;

    function MapList: string;
    function MapListBuild: string;
    function MapListFight: string;
    function MapListCoop: string;
  end;


implementation
uses KM_CommonClasses, KM_Defaults, KM_MissionScript, KM_Player, KM_TextLibrary, KM_Utils;


{ TKMapInfo }
constructor TKMapInfo.Create;
begin
  inherited;
  fInfo := TKMGameInfo.Create;
end;


destructor TKMapInfo.Destroy;
begin
  fInfo.Free;
  inherited;
end;


procedure TKMapInfo.Load(const aFolder:string; aStrictParsing, aIsMultiplayer:boolean);
begin
  if aIsMultiplayer then fPath := ExeDir+'MapsMP\'
                    else fPath := ExeDir+'Maps\';
  fPath := fPath+aFolder+'\';
  fFilename := aFolder;

  fStrictParsing := aStrictParsing;
  ScanMap;
end;


procedure TKMapInfo.ScanMap;
var
  st,DatFile,MapFile:string;
  ft:textfile;
  i:integer;
  fMissionParser:TMissionParser;
begin
  //We scan only single-player maps which are in Maps\ folder, so DAT\MAP paths are straight
  DatFile := fPath + fFilename + '.dat';
  MapFile := fPath + fFilename + '.map';

  LoadFromFile(fPath + fFilename + '.mi'); //Data will be empty if failed

  //We will scan map once again if anything has changed
  //In SP mode we check DAT size and version, that is enough
  //In MP mode we also need exact CRCs to match maps between players
  if FileExists(DatFile) then
  if (fDatSize <> GetFileSize(DatFile)) or
     (fInfo.Version <> GAME_REVISION) or
     (fStrictParsing and (fCRC <> Adler32CRC(DatFile) xor Adler32CRC(MapFile)))
  then
  begin
    fDatSize := GetFileSize(DatFile);

    fMissionParser := TMissionParser.Create(mpm_Info,false);
    try
      fMissionParser.LoadMission(DatFile);

      //Single maps Titles are the same as filename for now.
      //Campaign maps are in different folder
      fInfo.Title             := Filename;
      fInfo.Version           := GAME_REVISION;
      fInfo.TickCount         := 0;
      fInfo.MissionMode       := fMissionParser.MissionInfo.MissionMode;
      fInfo.MapSizeX          := fMissionParser.MissionInfo.MapSizeX;
      fInfo.MapSizeY          := fMissionParser.MissionInfo.MapSizeY;
      fInfo.VictoryCondition  := fMissionParser.MissionInfo.VictoryCond;
      fInfo.DefeatCondition   := fMissionParser.MissionInfo.DefeatCond;

      //This feature is only used for saves yet
      fInfo.PlayerCount       := fMissionParser.MissionInfo.PlayerCount;
      for i:=Low(fInfo.LocationName) to High(fInfo.LocationName) do
      begin
        fInfo.LocationName[i] := Format(fTextLibrary[TX_LOBBY_LOCATION_X],[i+1]);;
        fInfo.PlayerTypes[i] := pt_Human;
        fInfo.ColorID[i] := 0;
        fInfo.Team[i] := 0;
      end;

      fCRC := Adler32CRC(DatFile) xor Adler32CRC(MapFile);
      SaveToFile(fPath + fFilename + '.mi'); //Save new TMP file
    finally
      fMissionParser.Free;
    end;
  end;

  //Load additional text info
  if FileExists(fPath + fFilename + '.txt') then
  begin
    AssignFile(ft, fPath + fFilename + '.txt');
    FileMode := 0;
    Reset(ft);
    FileMode := 2;
    repeat
      readln(ft,st);
      if SameText(st, 'SmallDesc') then readln(ft, SmallDesc);
      if SameText(st, 'BigDesc')   then readln(ft, BigDesc);
      if SameText(st, 'SetCoop')   then IsCoop := true;
    until(eof(ft));
    closefile(ft);
  end;
end;


procedure TKMapInfo.LoadFromFile(const aPath:string);
var S:TKMemoryStream;
begin
  if not FileExists(aPath) then Exit;

  S := TKMemoryStream.Create;
  S.LoadFromFile(aPath);
  fInfo.Load(S);
  S.Read(fCRC);
  S.Read(fDatSize);
  S.Free;
end;


procedure TKMapInfo.SaveToFile(const aPath:string);
var S:TKMemoryStream;
begin
  S := TKMemoryStream.Create;
  try
    fInfo.Save(S);
    S.Write(fCRC);
    S.Write(fDatSize);
    S.SaveToFile(aPath);
  finally
    S.Free;
  end;
end;


function TKMapInfo.IsValid:boolean;
begin
  Result := fInfo.IsValid and
            FileExists(fPath + fFilename + '.dat') and
            FileExists(fPath + fFilename + '.map');
end;


{ TKMapsCollection }
constructor TKMapsCollection.Create(aMultiplayerPath: Boolean);
begin
  inherited Create;
  fMultiplayerPath := aMultiplayerPath;
  fSortMethod := smByNameDesc;

  CS := TCriticalSection.Create;

  //fScanner := TTScanner.Create(fMultiplayerPath);
  //fScanner.OnMapAdd := MapAdd;
  //fScanner.OnTerminate := ScanComplete;
end;


destructor TKMapsCollection.Destroy;
begin
  if (fScanner <> nil) then
  begin
    fScanner.Terminate;
    fScanner.WaitFor;
    fScanner.Free;
    fScanner := nil;
  end;

  Clear;

  CS.Free;
  inherited;
end;


function TKMapsCollection.GetMap(aIndex: Integer): TKMapInfo;
begin
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


procedure TKMapsCollection.ScanComplete(Sender: TObject);
begin
  Lock;
  try
    fScanning := False;
    Sort;
    if Assigned(OnRefreshComplete) then
      OnRefreshComplete(Self);
  finally
    Unlock;
  end;
end;


procedure TKMapsCollection.SetSortMethod(aMethod: TMapsSortMethod);
begin
  fSortMethod := aMethod;
  Sort; //New sorting method has been set, we need to apply it
end;


procedure TKMapsCollection.MapAdd(aMap: TKMapInfo);
begin
  Lock;
  try
    SetLength(fMaps, fCount + 1);
    fMaps[fCount] := aMap;
    Inc(fCount);
  finally
    Unlock;
  end;
end;


function TKMapsCollection.MapList: string;
var i:integer;
begin
  Assert(not Scanning);
  Result := '';
  for i:=0 to fCount-1 do
    Result := Result + fMaps[i].Filename + eol;
end;


function TKMapsCollection.MapListBuild: string;
var i:integer;
begin
  Assert(not Scanning);
  Result := '';
  for i:=0 to fCount-1 do
    if (fMaps[i].Info.MissionMode = mm_Normal) and not fMaps[i].IsCoop then
      Result := Result + fMaps[i].Filename + eol;
end;


function TKMapsCollection.MapListFight: string;
var i:integer;
begin
  Assert(not Scanning);
  Result := '';
  for i:=0 to fCount-1 do
    if (fMaps[i].Info.MissionMode = mm_Tactic) and not fMaps[i].IsCoop then
      Result := Result + fMaps[i].Filename + eol;
end;


function TKMapsCollection.MapListCoop: string;
var i:integer;
begin
  Assert(not Scanning);
  Result := '';
  for i:=0 to fCount-1 do
    if fMaps[i].IsCoop then
      Result := Result + fMaps[i].Filename + eol;
end;


procedure TKMapsCollection.Clear;
var
  I: Integer;
begin
  Assert(not Scanning);
  for I := 0 to fCount - 1 do
    FreeAndNil(fMaps[I]);
  fCount := 0;
end;


procedure TKMapsCollection.Sort;

  //Return True if items should be exchanged
  function Compare(A, B: TKMapInfo; aMethod: TMapsSortMethod): Boolean;
  begin
    Result := False; //By default everything remains in place
    case aMethod of
      smByNameAsc:      Result := CompareText(A.Info.Title, B.Info.Title) < 0;
      smByNameDesc:     Result := CompareText(A.Info.Title, B.Info.Title) > 0;
      smBySizeAsc:      Result := (A.Info.MapSizeX * A.Info.MapSizeY) < (B.Info.MapSizeX * B.Info.MapSizeY);
      smBySizeDesc:     Result := (A.Info.MapSizeX * A.Info.MapSizeY) > (B.Info.MapSizeX * B.Info.MapSizeY);
      smByPlayersAsc:   Result := A.Info.PlayerCount < B.Info.PlayerCount;
      smByPlayersDesc:  Result := A.Info.PlayerCount > B.Info.PlayerCount;
      smByModeAsc:      Result := A.Info.MissionMode < B.Info.MissionMode;
      smByModeDesc:     Result := A.Info.MissionMode > B.Info.MissionMode;
    end;
  end;

var
  i, k: Integer;
begin
  Assert(not Scanning);
  for i:=0 to fCount-1 do
  for k:=i to fCount-1 do
  if Compare(fMaps[i], fMaps[k], fSortMethod) then
    SwapInt(Cardinal(fMaps[i]), Cardinal(fMaps[k])); //Exchange only pointers to MapInfo objects
end;


procedure TKMapsCollection.Refresh;
begin
  if (fScanner <> nil) then
  begin
    fScanner.Terminate;
    fScanner.WaitFor;
    fScanner.Free;
    fScanner := nil;
  end;

  Clear;

  fScanning := True;
  fScanner := TTScanner.Create(fMultiplayerPath);
  fScanner.OnMapAdd := MapAdd;
  fScanner.OnTerminate := ScanComplete;
  fScanner.Resume;
end;


{ TTScanner }
constructor TTScanner.Create(aMultiplayerPath: Boolean);
begin
  inherited Create(True);
  fMultiplayerPath := aMultiplayerPath;
  FreeOnTerminate := False;
end;


procedure TTScanner.Execute;
var
  SearchRec: TSearchRec;
  PathToMaps: string;
  Map: TKMapInfo;
begin
  if fMultiplayerPath then
    PathToMaps := ExeDir + 'MapsMP\'
  else
    PathToMaps := ExeDir + 'Maps\';

  if DirectoryExists(PathToMaps) then
  begin
    FindFirst(PathToMaps + '*', faDirectory, SearchRec);
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
      and FileExists(MapNameToPath(SearchRec.Name, 'dat', fMultiplayerPath))
      and FileExists(MapNameToPath(SearchRec.Name, 'map', fMultiplayerPath)) then
      begin
        Map := TKMapInfo.Create;
        Map.Load(SearchRec.Name, false, fMultiplayerPath);
        Sleep(50);
        if Assigned(OnMapAdd) then
          OnMapAdd(Map);
      end;
    until (FindNext(SearchRec) <> 0) or Terminated;
    FindClose(SearchRec);
  end;
end;


end.


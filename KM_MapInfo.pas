unit KM_MapInfo;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils, KM_Defaults, KM_Player, KM_GameInfo;


type
  //@Lewin: It is not very good idea to mix savegames into MapInfo class. There should be separate SaveInfo and SavesCollection classes. I can make it if you like. There's a lot of duplicate code across Saves functions
  //@Krom: I agree 100%, it would be great if you could do it. It would be nice if SaveInfo and MapInfo inherit from a parent class with
  //       all the common values, (e.g. both a map and a save have a MapSize, PlayerCount, CRC, etc.) so fNetworking can have
  //       a TSaveOrMapInfo (need a better name) property that is created as either a TSaveInfo or a TMapInfo when the player selects one,
  //       but we don't end up with "if IsSave then Text := fSave.GetMapSize else Text := fMap.GetMapSize" at every case we want to extract a value,
  //       we can simply use "Text := fMapOrSave.GetMapSize" and it will load either. (similar to how the GIC works) We can discuss it if you like.

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

    constructor Create;
    destructor Destroy; override;

    procedure Load(const aFolder:string; aStrictParsing:boolean);

    property Info: TKMGameInfo read fInfo;
    property Path: string read fPath;
    property Filename: string read fFilename;
    property CRC: Cardinal read fCRC;

    function IsValid:boolean;
  end;


  TKMapsCollection = class
  private
    fCount:byte;
    fMaps:array of TKMapInfo;
    function GetMap(Index:integer):TKMapInfo;
  public
    destructor Destroy; override;
    procedure ScanMapsFolder;
    property Count:byte read fCount;
    property Map[Index:integer]:TKMapInfo read GetMap; default;

    function MapList: string;
  end;


implementation
uses KM_Utils, KM_MissionScript, KM_CommonTypes;


{ TKMapInfo }
procedure TKMapInfo.Load(const aFolder:string; aStrictParsing:boolean);
begin
  fPath := ExeDir+'Maps\'+aFolder+'\';
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
        fInfo.LocationName[i] := 'Location '+IntToStr(i+1);
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


{ TKMapsCollection }
destructor TKMapsCollection.Destroy;
var i:integer;
begin
  for i:=0 to fCount-1 do
    fMaps[i].Free;
end;


function TKMapsCollection.GetMap(Index:integer):TKMapInfo;
begin
  Result := fMaps[Index];
end;


function TKMapsCollection.MapList: string;
var i:integer;
begin
  Result := '';
  for i:=0 to fCount-1 do
    Result := Result + fMaps[i].Filename + eol;
end;


procedure TKMapsCollection.ScanMapsFolder;
var SearchRec:TSearchRec; i:integer;
begin
  for i:=0 to fCount-1 do
    FreeAndNil(fMaps[i]);

  fCount := 0;
  if not DirectoryExists(ExeDir+'Maps\') then exit;

  FindFirst(ExeDir+'Maps\*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Name<>'.') and (SearchRec.Name<>'..')
    and FileExists(MapNameToPath(SearchRec.Name,'dat'))
    and FileExists(MapNameToPath(SearchRec.Name,'map')) then
    begin
      inc(fCount);
      SetLength(fMaps, fCount);
      fMaps[fCount-1] := TKMapInfo.Create;
      fMaps[fCount-1].Load(SearchRec.Name, false);
    end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
end;


end.


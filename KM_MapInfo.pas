unit KM_MapInfo;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils, KM_Defaults;


type
  TKMapInfo = class
  private
    fFolder:string; //Map folder
    fDatSize:integer;
    fVersion:string;
    fMissionMode:TKMissionMode; //Fighting or Build-a-City map
    fPlayerCount:byte;
    fSmallDesc:string;
    fMapSize:string; //S,M,L,XL
    VictoryCond:string;
    DefeatCond:string;
    procedure ScanMap;
    procedure LoadFromFile(const aPath:string);
    procedure SaveToFile(const aPath:string);
  public
    BigDesc:string;
    procedure Load(const aFolder:string);
    property Folder:string read fFolder;
    function IsValid:boolean;
    property MissionMode:TKMissionMode read fMissionMode;
    property PlayerCount:byte read fPlayerCount;
    property MapSize:string read fMapSize;
    function SmallDesc:string;
    function MissionModeText:string;
    function VictoryCondition:string;
    function DefeatCondition:string;
  end;


  TKMMapsInfo = class
  private
    fCount:byte;
    fMaps:array of TKMapInfo;
    function GetMap(Index:integer):TKMapInfo;
  public
    procedure ScanMapsFolder;
    property Count:byte read fCount;
    property Map[Index:integer]:TKMapInfo read GetMap; default;
  end;


implementation
uses KM_Utils, KM_MissionScript, KM_CommonTypes;


{ TKMapInfo }
procedure TKMapInfo.Load(const aFolder:string);
begin
  fFolder := aFolder;
  ScanMap;
  if not IsValid then fFolder := '';
end;


procedure TKMapInfo.ScanMap;
var
  st:string;
  ft:textfile;
  MissionDetails: TKMMissionDetails;
  MapDetails: TKMMapDetails;
  fMissionParser:TMissionParser;
begin
  LoadFromFile(KMMapNameToPath(fFolder,'tmp')); //Data will be empty if failed

  //We will scan map once again if anything has changed
  if (fDatSize <> GetFileSize(KMMapNameToPath(fFolder,'dat'))) or (fVersion <> SAVE_VERSION) {or HashChanged} then
  begin
    fMissionParser := TMissionParser.Create(mpm_Game);
    try
      MissionDetails := fMissionParser.GetMissionDetails(KMMapNameToPath(fFolder,'dat'));
      MapDetails     := fMissionParser.GetMapDetails(KMMapNameToPath(fFolder,'map'));
      fMissionMode   := MissionDetails.MissionMode;
      fPlayerCount   := MissionDetails.TeamCount;
      VictoryCond    := MissionDetails.VictoryCond;
      DefeatCond     := MissionDetails.DefeatCond;
      fMapSize       := MapSizeToString(MapDetails.MapSize.X, MapDetails.MapSize.Y);

      SaveToFile(KMMapNameToPath(fFolder,'tmp')); //Save new TMP file
    finally
      fMissionParser.Free;
    end;
  end;

  fSmallDesc     := '-';
  BigDesc        := '-';

  //Load additional text info
  if FileExists(KMMapNameToPath(fFolder,'txt')) then
  begin
    AssignFile(ft, KMMapNameToPath(fFolder,'txt'));
    FileMode := 0;
    Reset(ft);
    FileMode := 2;
    repeat
      readln(ft,st);
      if SameText(st, 'SmallDesc') then readln(ft,fSmallDesc);
      if SameText(st, 'BigDesc') then readln(ft,BigDesc);
    until(eof(ft));
    closefile(ft);
  end;
end;


procedure TKMapInfo.LoadFromFile(const aPath:string);
var S:TKMemoryStream;
begin
  if not FileExists(aPath) then exit;
  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(aPath);
    S.Read(fDatSize);
    S.Read(fVersion);
    S.Read(fMissionMode, SizeOf(fMissionMode));
    S.Read(fPlayerCount);
    S.Read(VictoryCond);
    S.Read(DefeatCond);
    S.Read(fMapSize);
  finally
    S.Free;
  end;
end;


procedure TKMapInfo.SaveToFile(const aPath:string);
var S:TKMemoryStream;
begin
  S := TKMemoryStream.Create;
  try
    S.Write(fDatSize);
    S.Write(SAVE_VERSION); //Use actual version
    S.Write(fMissionMode, SizeOf(fMissionMode));
    S.Write(fPlayerCount);
    S.Write(VictoryCond);
    S.Write(DefeatCond);
    S.Write(fMapSize);
    S.SaveToFile(aPath);
  finally
    S.Free;
  end;
end;


function TKMapInfo.IsValid:boolean;
begin
  Result := (Folder <> '') and
            FileExists(KMMapNameToPath(fFolder,'dat')) and
            FileExists(KMMapNameToPath(fFolder,'map')) and
            (fPlayerCount > 0);
end;


//Remove any EOLs and trim
function TKMapInfo.SmallDesc:string;
begin
  Result := StringReplace(fSmallDesc, #124, ' ', [rfReplaceAll]);
  if length(Result)>36 then Result := Copy(Result,0,36)+' ...';
end;


function TKMapInfo.MissionModeText:string;
begin
  case fMissionMode of
    mm_Normal: Result := 'Building and Fighting';
    mm_Tactic: Result := 'Fighting'
    else       Result := 'Unknown';
  end;
end;


function TKMapInfo.VictoryCondition:string;
begin
  Result := 'Win'; //todo: scan Goals and make a text essence out of it
end;


function TKMapInfo.DefeatCondition:string;
begin
  Result := 'Loose'; //todo: scan Goals and make a text essence out of it
end;


{ TKMMapsInfo }
function TKMMapsInfo.GetMap(Index:integer):TKMapInfo;
begin
  Result := fMaps[Index];
end;


procedure TKMMapsInfo.ScanMapsFolder;
var SearchRec:TSearchRec; i:integer;
begin
  for i:=0 to fCount-1 do 
    FreeAndNil(fMaps[fCount-1]);

  fCount := 0;
  if not DirectoryExists(ExeDir+'Maps\') then exit;

  ChDir(ExeDir+'Maps\');
  FindFirst('*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Attr and faDirectory = faDirectory)
    and(SearchRec.Name<>'.')and(SearchRec.Name<>'..')
    and FileExists(KMMapNameToPath(SearchRec.Name,'dat'))
    and FileExists(KMMapNameToPath(SearchRec.Name,'map')) then
    begin
      inc(fCount);
      SetLength(fMaps, fCount);
      fMaps[fCount-1] := TKMapInfo.Create;
      fMaps[fCount-1].Load(SearchRec.Name);
    end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
end;


end.


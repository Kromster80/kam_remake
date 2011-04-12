unit KM_MapInfo;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils, Dialogs;


type
  TKMapInfo = class
  private
    fSmallDesc:string;
    VictoryCond:string;
    DefeatCond:string;
    procedure ScanMap;
  public
    Folder:string; //Map folder
    DatSize:integer;
    IsFight:boolean; //Fight or Build map
    PlayerCount:byte;
    BigDesc:string;
    MapSize:string; //S,M,L,XL
    constructor Create(const aName:string; aSize:integer);
    function SmallDesc:string;
    function MissionMode:string;
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
uses KM_Defaults, KM_Utils, KM_MissionScript, KM_CommonTypes;


{ TKMMapInfo }
constructor TKMapInfo.Create(const aName:string; aSize:integer);
begin
  Folder := aName;
  DatSize := aSize;
  ScanMap;
end;


procedure TKMapInfo.ScanMap;
  function CanTakeDataFromINI(aFile:string; aSize:integer):boolean;
  var S:TKMemoryStream; Size:integer; Vers:string;
  begin
    Result := false;
    if FileExists(aFile) then begin
      S := TKMemoryStream.Create;
      try
        S.LoadFromFile(aFile);
        S.Read(Size);
        S.Read(Vers);
        Result := (Size = aSize) and (Vers = SAVE_VERSION);
      finally
        S.Free;
      end;
    end;
  end;
var
  i:integer;
  S:TKMemoryStream;
  st:string;
  ft:textfile;
  MissionDetails: TKMMissionDetails;
  MapDetails: TKMMapDetails;
  fMissionParser:TMissionParser;
begin
  fMissionParser := TMissionParser.Create(mpm_Game);
  try
    //Take data from existing Info file if it exists and DAT size matches
    if CanTakeDataFromINI(KMMapNameToPath(Folder,'tmp'), DatSize) then
    begin
      S := TKMemoryStream.Create;
      try
        S.LoadFromFile(KMMapNameToPath(Folder,'tmp'));
        S.Read(i);
        S.Read(st);
        S.Read(IsFight);
        S.Read(PlayerCount);
        S.Read(VictoryCond);
        S.Read(DefeatCond);
        S.Read(MapSize);
      finally
        S.Free;
      end;
    end else begin
      MissionDetails := fMissionParser.GetMissionDetails(KMMapNameToPath(Folder,'dat'));
      MapDetails     := fMissionParser.GetMapDetails(KMMapNameToPath(Folder,'map'));
      IsFight        := MissionDetails.IsFight;
      PlayerCount    := MissionDetails.TeamCount;
      VictoryCond    := MissionDetails.VictoryCond;
      DefeatCond     := MissionDetails.DefeatCond;
      MapSize        := MapSizeToString(MapDetails.MapSize.X, MapDetails.MapSize.Y);
      S := TKMemoryStream.Create;
      try
        S.Write(DatSize);
        S.Write(SAVE_VERSION);
        S.Write(IsFight);
        S.Write(PlayerCount);
        S.Write(VictoryCond);
        S.Write(DefeatCond);
        S.Write(MapSize);
        S.SaveToFile(KMMapNameToPath(Folder,'tmp'));
      finally
        S.Free;
      end;
    end;

    fSmallDesc     := '-';
    BigDesc        := '-';

    if FileExists(KMMapNameToPath(Folder,'txt')) then
    begin
      AssignFile(ft,KMMapNameToPath(Folder,'txt'));
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
  finally
    fMissionParser.Free;
  end;
end;


//Remove any EOLs and trim
function TKMapInfo.SmallDesc:string;
begin
  Result := StringReplace(fSmallDesc, #124, ' ', [rfReplaceAll]);
  if length(Result)>36 then Result := Copy(Result,0,36)+' ...';
end;


function TKMapInfo.MissionMode:string;
begin
  if IsFight then
    Result := 'Fighting'
  else
    Result := 'Building and Fighting';
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
var
  SearchRec:TSearchRec;
begin
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
      fMaps[fCount-1] := TKMapInfo.Create(SearchRec.Name, SearchRec.Size);
    end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
end;


end.


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
    public
      Folder:string; //Map folder
      DatSize:integer;
      IsFight:boolean; //Fight or Build map
      PlayerCount:byte;
      BigDesc:string;
      MapSize:string; //S,M,L,XL
      function SmallDesc:string;
      function MissionMode:string;
      function VictoryCondition:string;
      function DefeatCondition:string;
    end;


  TKMMapsInfo = class
    private
      fCount:byte;
      fMaps:array of TKMapInfo;
      function GetMapInfo(Index:integer):TKMapInfo;
      procedure AddMap(aFolder:string; aSize:integer);
    public
      procedure ScanSingleMapsFolder;
      property Count:byte read fCount;
      property Map[Index:integer]:TKMapInfo read GetMapInfo; default;
    end;


implementation
uses KM_Defaults, KM_Utils, KM_MissionScript, KM_CommonTypes;


{ TKMMapInfo }
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
function TKMMapsInfo.GetMapInfo(Index:integer):TKMapInfo;
begin
  Result := fMaps[Index];
end;


procedure TKMMapsInfo.AddMap(aFolder:string; aSize:integer);
begin
  SetLength(fMaps, fCount+1);
  fMaps[fCount] := TKMapInfo.Create;
  fMaps[fCount].Folder := aFolder;
  fMaps[fCount].DatSize := aSize;
  inc(fCount);
end;


procedure TKMMapsInfo.ScanSingleMapsFolder;
  function CanTakeDataFromINI(aFile:string; aSize:integer):boolean;
  var S:TKMemoryStream; I:integer; Vers:string;
  begin
    Result := false;
    if FileExists(aFile) then begin
      S := TKMemoryStream.Create;
      try
        S.LoadFromFile(aFile);
        S.Read(I);
        S.Read(Vers);
        Result := (I = aSize) and (Vers = SAVE_VERSION);
      finally
        S.Free;
      end;
    end;
  end;
var
  i,ii:integer;
  SearchRec:TSearchRec;
  S:TKMemoryStream;
  st:string;
  ft:textfile;
  MissionDetails: TKMMissionDetails;
  MapDetails: TKMMapDetails;
  fMissionParser:TMissionParser;
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
      AddMap(SearchRec.Name, SearchRec.Size);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  fMissionParser := TMissionParser.Create(mpm_Game);
  try    
    for i:=0 to fCount-1 do with fMaps[i] do begin
      //Take data from existing Info file if it exists and DAT size matches
      if CanTakeDataFromINI(KMMapNameToPath(fMaps[i].Folder,'tmp'), fMaps[i].DatSize) then begin
        S := TKMemoryStream.Create;
        try
          S.LoadFromFile(KMMapNameToPath(fMaps[i].Folder,'tmp'));
          S.Read(ii);
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
        MissionDetails := fMissionParser.GetMissionDetails(KMMapNameToPath(fMaps[i].Folder,'dat'));
        MapDetails     := fMissionParser.GetMapDetails(KMMapNameToPath(fMaps[i].Folder,'map'));
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
          S.SaveToFile(KMMapNameToPath(fMaps[i].Folder,'tmp'));
        finally
          S.Free;
        end;
      end;

      fSmallDesc     := '-';
      BigDesc        := '-';

      if FileExists(KMMapNameToPath(fMaps[i].Folder,'txt')) then
      begin
        AssignFile(ft,KMMapNameToPath(fMaps[i].Folder,'txt'));
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
  finally
    fMissionParser.Free;
  end;
end;


end.


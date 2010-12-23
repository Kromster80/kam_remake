unit KM_MapInfo;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils, Dialogs;

type
  TKMMapsInfo = class
  private
    fCount:byte;
    Maps:array[1..255]of record
      Folder:string; //Map folder
      DatSize:integer;
      IsFight:boolean; //Fight or Build map
      PlayerCount:byte;
      SmallDesc,BigDesc:string;
      MapSize:string; //S,M,L,XL
      VictoryCond:string;
      DefeatCond:string;
    end;
  public
    procedure ScanSingleMapsFolder();
    property Count:byte read fCount;
    function IsFight(ID:integer):boolean;
    function GetPlayerCount(ID:integer):byte;
    function GetSmallDesc(ID:integer):string;
    function GetBigDesc(ID:integer):string;
    function GetMapSize(ID:integer):string;
    function GetFolder(ID:integer):string;
    function GetMissionFile(ID:integer):string;
    function GetTyp(ID:integer):string;
    function GetWin(ID:integer):string;
    function GetDefeat(ID:integer):string;
  end;

implementation
uses KM_Defaults, KM_Utils, KM_MissionScript, KM_CommonTypes;


{ TKMMapInfo }
procedure TKMMapsInfo.ScanSingleMapsFolder();
  function TakeDataFromINI(aFile:string; aSize:integer):boolean;
  var S:TKMemoryStream; I:integer; Vers:string;
  begin
    Result := false;
    if FileExists(aFile) then begin
      S := TKMemoryStream.Create;
      S.LoadFromFile(aFile);
      S.Seek(0, soFromBeginning);
      S.Read(I);
      S.Read(Vers);
      Result := (I = aSize) and (Vers = SAVE_VERSION);
      S.Free;
    end;
  end;
var
  i,ii,k:integer;
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
    and fileexists(KMMapNameToPath(SearchRec.Name,'dat'))
    and fileexists(KMMapNameToPath(SearchRec.Name,'map')) then
    begin
      inc(fCount);
      Maps[fCount].Folder := SearchRec.Name;
      Maps[fCount].DatSize := SearchRec.Size;
    end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  fMissionParser := TMissionParser.Create(mpm_Game);

  for k:=1 to 1 do
  for i:=1 to fCount do with Maps[i] do begin

    //Take data from existing Info file if it exists and DAT size matches
    if TakeDataFromINI(KMMapNameToPath(Maps[i].Folder,'tmp'), Maps[i].DatSize) then begin
      S := TKMemoryStream.Create;
      S.LoadFromFile(KMMapNameToPath(Maps[i].Folder,'tmp'));
      S.Seek(0, soFromBeginning);
      S.Read(ii);
      S.Read(st);
      S.Read(IsFight);
      S.Read(PlayerCount);
      S.Read(VictoryCond);
      S.Read(DefeatCond);
      S.Read(MapSize);
      S.Free;
    end else begin
      MissionDetails := fMissionParser.GetMissionDetails(KMMapNameToPath(Maps[i].Folder,'dat'));
      MapDetails     := fMissionParser.GetMapDetails(KMMapNameToPath(Maps[i].Folder,'map'));
      IsFight        := MissionDetails.IsFight;
      PlayerCount    := MissionDetails.TeamCount;
      VictoryCond    := MissionDetails.VictoryCond;
      DefeatCond     := MissionDetails.DefeatCond;
      MapSize        := MapSizeToString(MapDetails.MapSize.X, MapDetails.MapSize.Y);
      S := TKMemoryStream.Create;
      S.Write(DatSize);
      S.Write(SAVE_VERSION);
      S.Write(IsFight);
      S.Write(PlayerCount);
      S.Write(VictoryCond);
      S.Write(DefeatCond);
      S.Write(MapSize);
      S.SaveToFile(KMMapNameToPath(Maps[i].Folder,'tmp'));
      S.Free;
    end;

    SmallDesc      := '-';
    BigDesc        := '-';

    if FileExists(KMMapNameToPath(Maps[i].Folder,'txt')) then
    begin
      assignfile(ft,KMMapNameToPath(Maps[i].Folder,'txt'));
      reset(ft);
      repeat
        readln(ft,st);
        if SameText(st, 'SmallDesc') then readln(ft,SmallDesc);
        if SameText(st, 'BigDesc') then readln(ft,BigDesc);
      until(eof(ft));
      closefile(ft);
    end;
  end;

  FreeAndNil(fMissionParser);
end;


{ Get map properties}
function TKMMapsInfo.IsFight(ID:integer):boolean;        begin Result:=Maps[ID].IsFight;         end;
function TKMMapsInfo.GetPlayerCount(ID:integer):byte;    begin Result:=Maps[ID].PlayerCount;     end;

//Remove any EOLs and limit length
function TKMMapsInfo.GetSmallDesc(ID:integer):string;
begin
  Result := StringReplace(Maps[ID].SmallDesc, #124, ' ', [rfReplaceAll]);
  if length(Result)>36 then Result := Copy(Result,0,36)+' ...';
end;


function TKMMapsInfo.GetBigDesc(ID:integer):string;      begin Result:=Maps[ID].BigDesc;         end;
function TKMMapsInfo.GetMapSize(ID:integer):string;      begin Result:=Maps[ID].MapSize;         end;
function TKMMapsInfo.GetFolder(ID:integer):string;       begin Result:=Maps[ID].Folder;          end;
function TKMMapsInfo.GetMissionFile(ID:integer):string;  begin Result:=Maps[ID].Folder+'.dat';   end;

function TKMMapsInfo.GetTyp(ID:integer):string;
begin
  if Maps[ID].IsFight then
    Result := 'Fighting'
  else
    Result := 'Town building & fighting';
end;

function TKMMapsInfo.GetWin(ID:integer):string;          begin Result:=Maps[ID].VictoryCond;     end;
function TKMMapsInfo.GetDefeat(ID:integer):string;       begin Result:=Maps[ID].DefeatCond;      end;

end.


unit KM_MapInfo;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils, Dialogs;

type
  TKMMapsInfo = class
  private
    MapCount:byte;
    Maps:array[1..255]of record
      Folder:string; //Map folder
      IsFight:boolean; //Fight or Build map
      PlayerCount:byte;
      SmallDesc,BigDesc:string;
      MapSize:string; //S,M,L,XL
      VictoryCond:string;
      DefeatCond:string;
    end;
  public
    procedure ScanSingleMapsFolder();
    property GetMapCount:byte read MapCount;
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
uses KM_Defaults, KM_Utils, KM_MissionScript;


{ TKMMapInfo }
procedure TKMMapsInfo.ScanSingleMapsFolder();
var
  i,k:integer;
  SearchRec:TSearchRec;
  ft:textfile;
  s:string;
  MissionDetails: TKMMissionDetails;
  MapDetails: TKMMapDetails;
  fMissionParser:TMissionParser;
begin
  MapCount:=0;
  if not DirectoryExists(ExeDir+'Maps\') then exit;

  ChDir(ExeDir+'Maps\');
  FindFirst('*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Attr and faDirectory = faDirectory)
    and(SearchRec.Name<>'.')and(SearchRec.Name<>'..')
    and fileexists(KMMapNameToPath(SearchRec.Name,'dat'))
    and fileexists(KMMapNameToPath(SearchRec.Name,'map')) then
    begin
      inc(MapCount);
      Maps[MapCount].Folder := SearchRec.Name;
    end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  fMissionParser := TMissionParser.Create(mpm_Game);

  for k:=1 to 1 do
  for i:=1 to MapCount do with Maps[i] do begin

    MissionDetails := fMissionParser.GetMissionDetails(KMMapNameToPath(Maps[i].Folder,'dat'));
    MapDetails     := fMissionParser.GetMapDetails(KMMapNameToPath(Maps[i].Folder,'map'));

    IsFight        := MissionDetails.IsFight;
    PlayerCount    := MissionDetails.TeamCount;
    MapSize        := MapSizeToString(MapDetails.MapSize.X, MapDetails.MapSize.Y);
    SmallDesc      := '-';
    BigDesc        := '-';
    VictoryCond    := MissionDetails.VictoryCond;
    DefeatCond     := MissionDetails.DefeatCond;

    if fileexists(KMMapNameToPath(Maps[i].Folder,'txt')) then
    begin
      assignfile(ft,KMMapNameToPath(Maps[i].Folder,'txt'));
      reset(ft);
      repeat
        readln(ft,s);
        if UpperCase(s)=UpperCase('SmallDesc') then readln(ft,SmallDesc);
        if UpperCase(s)=UpperCase('BigDesc') then readln(ft,BigDesc);
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


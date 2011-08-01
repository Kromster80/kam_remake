unit KM_Campaigns;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Points;


type
  PicID = record
    RX,ID:word;
  end;

  TKMCampaign = class
    Maps:array of record
      MapName:string;
      MissionText:string;
      Node:TKMPoint;
      PrevMap:byte; //Should be used to draw connecting dots in Campaign screen
      ScriptPath:string;
    end;
  private
    fBackGroundPic:PicID;
    fMapCount:byte;
    fShortTitle:string;
    fUnlockedMaps:byte;
    procedure SetUnlockedMaps(Value:byte);
  public
    constructor Create(const aShortTitle:string; aMapCount:byte; aBackRX,aBackID:word);
    procedure LoadProgress(M:TKMemoryStream);
    procedure SaveProgress(M:TKMemoryStream);

    property BackGroundPicRX:word read fBackGroundPic.RX;
    property BackGroundPicID:word read fBackGroundPic.ID;
    property MapCount:byte read fMapCount;
    function MissionText(aIndex:byte):string;
    property ShortTitle:string read fShortTitle;
    property UnlockedMaps:byte read fUnlockedMaps write SetUnlockedMaps;
  end;

  TKMCampaignsCollection = class
  private
    fList:TList;
    procedure CreateTSK;
    procedure CreateTPR;
    function GetCampaign(aIndex:byte):TKMCampaign;
    procedure ScanFolder(const aPath:string);
    procedure LoadProgress(const FileName:string);
    procedure SaveProgress(const FileName:string);
  public
    constructor Create;
    destructor Destroy; override;

    function Count:integer;
    property Campaigns[aIndex:byte]:TKMCampaign read GetCampaign; default;
    function CampaignByTitle(const aShortTitle:string):TKMCampaign;

    procedure UnlockMap(const aShortTitle:string; aMapIndex:byte);
  end;


const
  MAX_CMP_MAPS = 32;


implementation
uses KM_Log, KM_TextLibrary;


const
  TSK_MAPS = 20;
  TPR_MAPS = 14;

  CampTSKNodes: array [0..TSK_MAPS-1] of TKMPoint = (
  (X:170; Y:670), (X:160; Y:455), (X:320; Y:595), (X:442; Y:625), (X:395; Y:525),
  (X:350; Y:420), (X: 95; Y:345), (X:140; Y:190), (X:550; Y:520), (X:735; Y:510),
  (X:885; Y:550), (X:305; Y:290), (X:380; Y:270), (X:475; Y:290), (X:580; Y:290),
  (X:820; Y:175), (X:700; Y:145), (X:595; Y: 40), (X:720; Y: 80), (X:820; Y: 50));

  CampTPRNodes: array [0..TPR_MAPS-1] of TKMPoint = (
  (X:175; Y:525), (X:180; Y:465), (X:110; Y:435), (X:180; Y:420), (X:230; Y:440),
  (X:370; Y:475), (X:130; Y:205), (X:350; Y:185), (X:515; Y:355), (X:590; Y:360),
  (X:665; Y:190), (X:760; Y:285), (X:775; Y:220), (X:730; Y:160));


{ TCampaignCollection }
constructor TKMCampaignsCollection.Create;
begin
  Inherited;
  fList := TList.Create;
  CreateTSK;
  CreateTPR;
  ScanFolder(ExeDir + 'Campaigns\');

  LoadProgress(ExeDir+'Saves\KaM_Remake_Campaigns.ini');
end;


destructor TKMCampaignsCollection.Destroy;
begin
  CreateDir(ExeDir+'Saves\'); //Makes the folder incase it was deleted
  SaveProgress(ExeDir+'Saves\KaM_Remake_Campaigns.ini');
  fLog.AppendLog('Campaign.ini saved');

  fList.Free;
  inherited;
end;


procedure TKMCampaignsCollection.CreateTPR;
var i:integer; C:TKMCampaign;
begin
  C := TKMCampaign.Create('TPR', TPR_MAPS, 5, 20);

  for i:=0 to C.MapCount-1 do
  begin
    C.Maps[i].MapName := 'TPR mission '+inttostr(i+1);
    C.Maps[i].MissionText := '';
    C.Maps[i].Node := CampTPRNodes[i];
    C.Maps[i].ScriptPath := ExeDir+'data\mission\dmission'+inttostr(i+1)+'.dat'
  end;

  fList.Add(C);
end;


procedure TKMCampaignsCollection.CreateTSK;
var i:integer; C:TKMCampaign;
begin
  C := TKMCampaign.Create('TSK', TSK_MAPS, 5, 12);

  for i:=0 to C.MapCount-1 do
  begin
    C.Maps[i].MapName := 'TSK mission '+inttostr(i+1);
    C.Maps[i].MissionText := '';
    C.Maps[i].Node := CampTSKNodes[i];
    C.Maps[i].ScriptPath := ExeDir+'data\mission\mission'+inttostr(i+1)+'.dat';
  end;

  fList.Add(C);
end;


//Scan custom campaigns folders
procedure TKMCampaignsCollection.ScanFolder(const aPath:string);
begin

end;


function TKMCampaignsCollection.GetCampaign(aIndex: byte): TKMCampaign;
begin
  Result := TKMCampaign(fList[aIndex]);
end;


//Read progress from file trying to find matching campaigns
procedure TKMCampaignsCollection.LoadProgress(const FileName:string);
var
  M:TKMemoryStream;
  C: TKMCampaign;
  i,CampCount:word;
  CampName:string;
begin
  M := TKMemoryStream.Create;

  M.Read(CampCount);
  for i:=0 to CampCount-1 do
  begin
    M.Read(CampName);
    C := CampaignByTitle(CampName);
    if C<>nil then
      C.LoadProgress(M);
  end;

  M.Free;
end;


procedure TKMCampaignsCollection.SaveProgress(const FileName:string);
var
  M:TKMemoryStream;
  i:word;
begin
  M := TKMemoryStream.Create;

  M.Write(Count);
  for i:=0 to Count-1 do
  begin
    M.Write(Campaigns[i].ShortTitle);
    Campaigns[i].SaveProgress(M);
  end;

  M.SaveToFile(FileName);
  M.Free;
end;


function TKMCampaignsCollection.Count:integer;
begin
  Result := fList.Count;
end;


function TKMCampaignsCollection.CampaignByTitle(const aShortTitle: string): TKMCampaign;
var i:integer;
begin
  Result := nil;
  for i:=0 to Count-1 do
    if Campaigns[i].ShortTitle = aShortTitle then
      Result := Campaigns[i];
end;


procedure TKMCampaignsCollection.UnlockMap(const aShortTitle: string; aMapIndex: byte);
var C:TKMCampaign;
begin
  C := CampaignByTitle(aShortTitle);
  Assert(C<>nil);

  C.UnlockedMaps := aMapIndex;
end;


{ TKMCampaign }
constructor TKMCampaign.Create(const aShortTitle: string; aMapCount:byte; aBackRX,aBackID:word);
begin
  fShortTitle := aShortTitle;
  fMapCount := aMapCount;
  fBackGroundPic.RX := aBackRX;
  fBackGroundPic.ID := aBackID;
  SetLength(Maps, fMapCount);
  fUnlockedMaps := 1; //1st map should be always unlocked to allow to start campaign
end;


procedure TKMCampaign.LoadProgress(M: TKMemoryStream);
begin
  M.Read(fUnlockedMaps);
end;


//Mission texts of original campaigns are available in all languages,
//custom campaigns are unlikely to have more texts in more than 1-2 languages
function TKMCampaign.MissionText(aIndex:byte): string;
begin
  if fShortTitle = 'TPR' then
    Result := fTextLibrary.GetSetupString(siCampTPRTexts + aIndex)
  else
  if fShortTitle = 'TSK' then
    Result := fTextLibrary.GetSetupString(siCampTSKTexts + aIndex)
  else
    Result := Maps[aIndex].MissionText;
end;


procedure TKMCampaign.SaveProgress(M: TKMemoryStream);
begin
  M.Write(fUnlockedMaps);
end;


{When player completes one map we allow to reveal the next one, note that
player may be replaying previous maps, in that case his progress remains the same}
procedure TKMCampaign.SetUnlockedMaps(Value: byte);
begin
  fUnlockedMaps := EnsureRange(Value, fUnlockedMaps, fMapCount);
end;


end.

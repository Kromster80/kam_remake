unit KM_Campaigns;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points;


const
  MAX_CAMP_MAPS = 20;
  MAX_CAMP_NODES = 20;

type
  TPicID = record
    RX: TRXType;
    ID: Word;
  end;

  TKMCampaign = class
  private
    fFirstTextIndex: Word;
    fFullTitle: AnsiString;
    fShortTitle: AnsiString;
    fBackGroundPic: TPicID;
    fMapCount: Byte;
    fUnlockedMaps: Byte;
    procedure SetUnlockedMaps(Value: Byte);
    procedure SetMapCount(aValue: byte);
  public
    Maps: array of record
      MapName: AnsiString;
      MissionText: AnsiString;
      TextPos: Byte; //Text position (TL, TR, BL, BR corner)
      Flag: TKMPoint;
      NodeCount: Byte;
      Nodes: array [0 .. MAX_CAMP_NODES - 1] of TKMPoint;
      ScriptPath: AnsiString;
    end;
    constructor Create;

    procedure LoadFromFile(aFilename: string);
    procedure SaveToFile(aFilename: string);
    procedure LoadFromPath(aPath: string);

    property BackGroundPic: TPicID read fBackGroundPic write fBackGroundPic;
    property MapCount: byte read fMapCount write SetMapCount;
    property FullTitle: AnsiString read fFullTitle write fFullTitle;
    property ShortTitle: AnsiString read fShortTitle write fShortTitle;
    property UnlockedMaps: byte read fUnlockedMaps write SetUnlockedMaps;

    function MissionText(aIndex: byte): AnsiString;
  end;


  TKMCampaignsCollection = class
  private
    fActiveCampaign: TKMCampaign; //Campaign we are playing
    fActiveCampaignMap:byte; //Map of campaign we are playing, could be different than MaxRevealedMap
    fList: TList;
    function GetCampaign(aIndex:byte): TKMCampaign;
    procedure AddCampaign(const aPath: string);
    procedure ScanFolder(const aPath: string);
    procedure LoadProgress(const FileName: string);
    procedure SaveProgress(const FileName: string);
  public
    constructor Create;
    destructor Destroy; override;

    property ActiveCampaign: TKMCampaign read fActiveCampaign write fActiveCampaign;
    property ActiveCampaignMap:byte read fActiveCampaignMap write fActiveCampaignMap;

    function Count:integer;
    property Campaigns[aIndex:byte]:TKMCampaign read GetCampaign; default;
    function CampaignByTitle(const aShortTitle: AnsiString):TKMCampaign;
    procedure UnlockNextMap;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


implementation
uses KM_Log, KM_TextLibrary;


const
  CAMP_HEADER = $FEED; //Just some header to separate right progress files from wrong


{ TCampaignCollection }
constructor TKMCampaignsCollection.Create;
begin
  inherited;
  fList := TList.Create;
  ScanFolder(ExeDir + 'Campaigns\');
  LoadProgress(ExeDir + 'Campaigns\Campaigns.ini');
end;


destructor TKMCampaignsCollection.Destroy;
var I: Integer;
begin
  CreateDir(ExeDir + 'Campaigns\'); //Makes the folder incase it was deleted
  SaveProgress(ExeDir + 'Campaigns\Campaigns.ini');
  fLog.AppendLog('Campaign.ini saved');

  //Free list objects
  for I := 0 to Count - 1 do
    Self[I].Free;

  fList.Free;
  inherited;
end;


procedure TKMCampaignsCollection.AddCampaign(const aPath: string);
var
  C: TKMCampaign;
begin
  C := TKMCampaign.Create;
  C.LoadFromPath(aPath);
  fList.Add(C);
end;


//Scan custom campaigns folders
procedure TKMCampaignsCollection.ScanFolder(const aPath: string);
var C: TKMCampaign;
begin
  AddCampaign(aPath + 'The Shattered Kingdom\');
  AddCampaign(aPath + 'The Peasants Rebellion\');

  //todo: So far TSK and TPR are hardcoded, but we need to switch to real Scan later

  //Hardcoded for now
  C := CampaignByTitle('TSK');
  C.fBackGroundPic.RX := rxGuiMainH;
  C.fBackGroundPic.ID := 12;
  C.fFirstTextIndex := 340; //+10 added later on

  //Hardcoded for now
  C := CampaignByTitle('TPR');
  C.fBackGroundPic.RX := rxGuiMain;
  C.fBackGroundPic.ID := 20;
  C.fFirstTextIndex := 240; //+10 added later on
end;


function TKMCampaignsCollection.GetCampaign(aIndex: byte): TKMCampaign;
begin
  Result := TKMCampaign(fList[aIndex]);
end;


//Read progress from file trying to find matching campaigns
procedure TKMCampaignsCollection.LoadProgress(const FileName: string);
var
  M: TKMemoryStream;
  C: TKMCampaign;
  I, CampCount: Integer;
  CampName: string;
  Unlocked: Byte;
begin
  if not FileExists(FileName) then Exit;

  M := TKMemoryStream.Create;
  try
    M.LoadFromFile(FileName);

    M.Read(I); //Check for wrong file format
    if I <> CAMP_HEADER then Exit; //All campaigns will be kept in initial state

    M.Read(CampCount);
    for I := 0 to CampCount - 1 do
    begin
      M.Read(CampName);
      M.Read(Unlocked);
      C := CampaignByTitle(AnsiString(CampName));
      if C <> nil then
        C.UnlockedMaps := 12;//Unlocked;
    end;
  finally
    M.Free;
  end;
end;


procedure TKMCampaignsCollection.SaveProgress(const FileName: string);
var
  M: TKMemoryStream;
  I: word;
begin
  M := TKMemoryStream.Create;
  try
    M.Write(Integer(CAMP_HEADER)); //Identify our format
    M.Write(Count);
    for I := 0 to Count - 1 do
    begin
      M.Write(Campaigns[I].ShortTitle);
      M.Write(Campaigns[I].UnlockedMaps);
    end;

    M.SaveToFile(FileName);
  finally
    M.Free;
  end;
end;


function TKMCampaignsCollection.Count:integer;
begin
  Result := fList.Count;
end;


function TKMCampaignsCollection.CampaignByTitle(const aShortTitle: AnsiString): TKMCampaign;
var i:integer;
begin
  Result := nil;
  for i:=0 to Count-1 do
    if SameText(Campaigns[i].ShortTitle, aShortTitle) then
      Result := Campaigns[i];
end;


procedure TKMCampaignsCollection.UnlockNextMap;
begin
  if ActiveCampaign <> nil then
    ActiveCampaign.UnlockedMaps := ActiveCampaignMap + 1 + 1;
end;


procedure TKMCampaignsCollection.Load(LoadStream: TKMemoryStream);
var s: AnsiString;
begin
  LoadStream.ReadAssert('CampaignInfo');
  LoadStream.Read(s);
  fActiveCampaign := CampaignByTitle(s);
  LoadStream.Read(fActiveCampaignMap);
  //If loaded savegame references to missing campaign it will be treated as single-map (fActiveCampaign = nil)
end;


procedure TKMCampaignsCollection.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write('CampaignInfo');
  if fActiveCampaign <> nil then
    SaveStream.Write(fActiveCampaign.ShortTitle)
  else
    SaveStream.Write('');
  SaveStream.Write(fActiveCampaignMap);
end;


{ TKMCampaign }
constructor TKMCampaign.Create;//(const aShortTitle: AnsiString; aMapCount:byte; aBackRX: TRXType; aBackID:word);
begin
  inherited;
  fUnlockedMaps := 1; //1st map should be always unlocked to allow to start campaign
end;


procedure TKMCampaign.LoadFromFile(aFilename: string);
var
  M: TKMemoryStream;
  I, K: Integer;
begin
  M := TKMemoryStream.Create;
  M.LoadFromFile(aFilename);

  M.ReadAssert('KaM Campaign');
  M.Read(fFullTitle);
  M.Read(fShortTitle);
  M.Read(Byte(fBackGroundPic.RX));
  M.Read(fBackGroundPic.ID);
  M.Read(fMapCount);
  SetLength(Maps, fMapCount);

  for I := 0 to fMapCount - 1 do
  begin
    M.Read(Maps[I].MapName);
    M.Read(Maps[I].MissionText);
    M.Read(Maps[I].Flag);
    M.Read(Maps[I].NodeCount);
    //SetLength(Maps[I].Nodes, Maps[I].NodeCount);
    for K := 0 to Maps[I].NodeCount - 1 do
      M.Read(Maps[I].Nodes[K]);
  end;

  M.Free;
end;


procedure TKMCampaign.SaveToFile(aFilename: string);
var
  M: TKMemoryStream;
  I, K: Integer;
begin
  M := TKMemoryStream.Create;
  M.Write('KaM Campaign');
  M.Write(fFullTitle);
  M.Write(fShortTitle);
  M.Write(Byte(fBackGroundPic.RX));
  M.Write(fBackGroundPic.ID);
  M.Write(fMapCount);

  for I := 0 to fMapCount - 1 do
  begin
    M.Write(Maps[I].MapName);
    M.Write(Maps[I].MissionText);
    M.Write(Maps[I].Flag);
    M.Write(Maps[I].NodeCount);
    for K := 0 to Maps[I].NodeCount - 1 do
      M.Write(Maps[I].Nodes[K]);
  end;

  M.SaveToFile(aFilename);
  M.Free;
end;


procedure TKMCampaign.LoadFromPath(aPath: string);
begin
  LoadFromFile(aPath + 'info.cmp');
  fFirstTextIndex := fTextLibrary.AppendCampaign(aPath + 'text.eng.libx');
  //LoadRX(aPath + '\info.cmp');

  //fUnlockedMaps := 5; //Unlock more maps for debug
end;


procedure TKMCampaign.SetMapCount(aValue: byte);
begin
  fMapCount := aValue;
  SetLength(Maps, fMapCount);
end;


//Mission texts of original campaigns are available in all languages,
//custom campaigns are unlikely to have more texts in more than 1-2 languages
function TKMCampaign.MissionText(aIndex: byte): AnsiString;
begin
  Result := fTextLibrary[2000 + fFirstTextIndex + 10 + aIndex];
end;


{When player completes one map we allow to reveal the next one, note that
player may be replaying previous maps, in that case his progress remains the same}
procedure TKMCampaign.SetUnlockedMaps(Value: byte);
begin
  fUnlockedMaps := EnsureRange(Value, fUnlockedMaps, fMapCount - 1);
end;


end.

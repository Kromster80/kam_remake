unit KM_Campaigns;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC}AnsiStrings, {$ENDIF}
  Classes, KromUtils, Math, SysUtils,
  KM_CommonClasses, KM_Pics, KM_Points, KM_ResTexts, KM_ResLocales;


const
  MAX_CAMP_MAPS = 64;
  MAX_CAMP_NODES = 64;

type
  TBriefingCorner = (bcBottomRight, bcBottomLeft);

  TKMCampaign = class
  private
    //Runtime variables
    fPath: UnicodeString;
    fTextLib: TKMTextLibrarySingle;
    fUnlockedMap: Byte;

    //Saved in CMP
    fShortTitle: AnsiString; //Used to identify the campaign
    fBackGroundPic: TKMPic;
    fMapCount: Byte;
    procedure SetUnlockedMap(aValue: Byte);
    procedure SetMapCount(aValue: Byte);
  public
    Maps: array of record
      Flag: TKMPoint;
      NodeCount: Byte;
      Nodes: array [0 .. MAX_CAMP_NODES - 1] of TKMPoint;
      TextPos: TBriefingCorner;
    end;
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(aFileName: UnicodeString);
    procedure SaveToFile(aFileName: UnicodeString);
    procedure LoadFromPath(aPath: UnicodeString);

    property BackGroundPic: TKMPic read fBackGroundPic write fBackGroundPic;
    property MapCount: Byte read fMapCount write SetMapCount;
    property ShortTitle: AnsiString read fShortTitle write fShortTitle;
    property UnlockedMap: Byte read fUnlockedMap write SetUnlockedMap;

    function CampaignTitle: UnicodeString;
    function CampaignDescription: UnicodeString;
    function MissionFile(aIndex: Byte): UnicodeString;
    function MissionTitle(aIndex: Byte): UnicodeString;
    function MissionBriefing(aIndex: Byte): UnicodeString;
    function BreifingAudioFile(aIndex: Byte): UnicodeString;
  end;


  TKMCampaignsCollection = class
  private
    fActiveCampaign: TKMCampaign; //Campaign we are playing
    fActiveCampaignMap: Byte; //Map of campaign we are playing, could be different than UnlockedMaps
    fList: TList;
    function GetCampaign(aIndex: Integer): TKMCampaign;
    procedure AddCampaign(const aPath: UnicodeString);
  public
    constructor Create;
    destructor Destroy; override;

    //Initialization
    procedure ScanFolder(const aPath: UnicodeString);
    procedure LoadProgress(const aFileName: UnicodeString);
    procedure SaveProgress(const aFileName: UnicodeString);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    //Usage
    property ActiveCampaign: TKMCampaign read fActiveCampaign;// write fActiveCampaign;
    function Count: Integer;
    property Campaigns[aIndex: Integer]: TKMCampaign read GetCampaign; default;
    function CampaignByTitle(const aShortTitle: AnsiString): TKMCampaign;
    procedure SetActive(aCampaign: TKMCampaign; aMap: Byte);
    procedure UnlockNextMap;
  end;


implementation
uses KM_Defaults, KM_Resource, KM_ResSprites, KM_Log;


const
  CAMP_HEADER = $FEED; //Just some header to separate right progress files from wrong


{ TCampaignsCollection }
constructor TKMCampaignsCollection.Create;
begin
  inherited Create;

  fList := TList.Create;
end;


destructor TKMCampaignsCollection.Destroy;
var
  I: Integer;
begin
  //Free list objects
  for I := 0 to Count - 1 do
    Campaigns[I].Free;

  fList.Free;
  inherited;
end;


procedure TKMCampaignsCollection.AddCampaign(const aPath: UnicodeString);
var
  C: TKMCampaign;
begin
  C := TKMCampaign.Create;
  C.LoadFromPath(aPath);
  fList.Add(C);
end;


//Scan campaigns folder
procedure TKMCampaignsCollection.ScanFolder(const aPath: UnicodeString);
var
  SearchRec: TSearchRec;
begin
  if not DirectoryExists(aPath) then Exit;

  FindFirst(aPath + '*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
    and (SearchRec.Attr and faDirectory = faDirectory)
    and FileExists(aPath + SearchRec.Name + PathDelim+'info.cmp') then
      AddCampaign(aPath + SearchRec.Name + PathDelim);
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


procedure TKMCampaignsCollection.SetActive(aCampaign: TKMCampaign; aMap: Byte);
begin
  fActiveCampaign := aCampaign;
  fActiveCampaignMap := aMap;
end;


function TKMCampaignsCollection.GetCampaign(aIndex: Integer): TKMCampaign;
begin
  Result := fList[aIndex];
end;


//Read progress from file trying to find matching campaigns
procedure TKMCampaignsCollection.LoadProgress(const aFileName: UnicodeString);
var
  M: TKMemoryStream;
  C: TKMCampaign;
  I, CampCount: Integer;
  CampName: AnsiString;
  Unlocked: Byte;
begin
  if not FileExists(aFileName) then Exit;

  M := TKMemoryStream.Create;
  try
    M.LoadFromFile(aFileName);

    M.Read(I); //Check for wrong file format
    if I <> CAMP_HEADER then Exit; //All campaigns will be kept in initial state

    M.Read(CampCount);
    for I := 0 to CampCount - 1 do
    begin
      M.ReadA(CampName);
      M.Read(Unlocked);
      C := CampaignByTitle(AnsiString(CampName));
      if C <> nil then
        C.UnlockedMap := Unlocked;
    end;
  finally
    M.Free;
  end;
end;


procedure TKMCampaignsCollection.SaveProgress(const aFileName: UnicodeString);
var
  M: TKMemoryStream;
  I: Integer;
begin
  //Makes the folder incase it is missing
  ForceDirectories(ExtractFilePath(aFileName));

  M := TKMemoryStream.Create;
  try
    M.Write(Integer(CAMP_HEADER)); //Identify our format
    M.Write(Count);
    for I := 0 to Count - 1 do
    begin
      M.WriteA(Campaigns[I].ShortTitle);
      M.Write(Campaigns[I].UnlockedMap);
    end;

    M.SaveToFile(aFileName);
  finally
    M.Free;
  end;

  gLog.AddTime('Campaigns.dat saved');
end;


function TKMCampaignsCollection.Count: Integer;
begin
  Result := fList.Count;
end;


function TKMCampaignsCollection.CampaignByTitle(const aShortTitle: AnsiString): TKMCampaign;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if {$IFDEF WDC}AnsiStrings.{$ENDIF}SameText(Campaigns[I].ShortTitle, aShortTitle) then
      Result := Campaigns[I];
end;


procedure TKMCampaignsCollection.UnlockNextMap;
begin
  if ActiveCampaign <> nil then
    ActiveCampaign.UnlockedMap := fActiveCampaignMap + 1;
end;


procedure TKMCampaignsCollection.Load(LoadStream: TKMemoryStream);
var s: AnsiString;
begin
  LoadStream.ReadAssert('CampaignInfo');
  LoadStream.ReadA(s);
  fActiveCampaign := CampaignByTitle(s);
  LoadStream.Read(fActiveCampaignMap);
  //If loaded savegame references to missing campaign it will be treated as single-map (fActiveCampaign = nil)
end;


procedure TKMCampaignsCollection.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('CampaignInfo');
  if fActiveCampaign <> nil then
    SaveStream.WriteA(fActiveCampaign.ShortTitle)
  else
    SaveStream.WriteA('');
  SaveStream.Write(fActiveCampaignMap);
end;


{ TKMCampaign }
constructor TKMCampaign.Create;//(const aShortTitle: AnsiString; aMapCount:byte; aBackRX: TRXType; aBackID:word);
begin
  inherited;

  //1st map is always unlocked to allow to start campaign
  fUnlockedMap := 0;
end;


destructor TKMCampaign.Destroy;
begin
  FreeAndNil(fTextLib);

  inherited;
end;


//Load campaign info from *.cmp file
//It should be private, but it is used by CampaignBuilder
procedure TKMCampaign.LoadFromFile(aFileName: UnicodeString);
var
  M: TKMemoryStream;
  I, K: Integer;
begin
  if not FileExists(aFileName) then Exit;

  M := TKMemoryStream.Create;
  M.LoadFromFile(aFileName);

  M.ReadA(fShortTitle);
  M.Read(fMapCount);
  SetLength(Maps, fMapCount);

  for I := 0 to fMapCount - 1 do
  begin
    M.Read(Maps[I].Flag);
    M.Read(Maps[I].NodeCount);
    for K := 0 to Maps[I].NodeCount - 1 do
      M.Read(Maps[I].Nodes[K]);
    M.Read(Maps[I].TextPos, SizeOf(TBriefingCorner));
  end;

  M.Free;
end;


procedure TKMCampaign.SaveToFile(aFileName: UnicodeString);
var
  M: TKMemoryStream;
  I, K: Integer;
begin
  Assert(aFileName <> '');

  M := TKMemoryStream.Create;
  M.WriteA(fShortTitle);
  M.Write(fMapCount);

  for I := 0 to fMapCount - 1 do
  begin
    M.Write(Maps[I].Flag);
    M.Write(Maps[I].NodeCount);
    for K := 0 to Maps[I].NodeCount - 1 do
    begin
      //One-time fix for campaigns made before r4880
      //Inc(Maps[I].Nodes[K].X, 5);
      //Inc(Maps[I].Nodes[K].Y, 5);
      M.Write(Maps[I].Nodes[K]);
    end;
    M.Write(Maps[I].TextPos, SizeOf(TBriefingCorner));
  end;

  M.SaveToFile(aFileName);
  M.Free;
end;


procedure TKMCampaign.LoadFromPath(aPath: UnicodeString);
var
  SP: TKMSpritePack;
  FirstSpriteIndex: Word;
begin
  fPath := aPath;

  LoadFromFile(fPath + 'info.cmp');

  FreeAndNil(fTextLib);
  fTextLib := TKMTextLibrarySingle.Create;
  fTextLib.LoadLocale(fPath + 'text.%s.libx');

  if fResource.Sprites <> nil then
  begin
    SP := fResource.Sprites[rxGuiMainH];
    FirstSpriteIndex := SP.RXData.Count;
    SP.LoadFromRXXFile(fPath + 'images.rxx', SP.RXData.Count);

    if FirstSpriteIndex < SP.RXData.Count then
    begin
      //Images were successfuly loaded
      SP.MakeGFX(False, FirstSpriteIndex);
      fBackGroundPic.RX := rxGuiMainH;
      fBackGroundPic.ID := FirstSpriteIndex;
    end
    else
    begin
      //Images were not found - use blank
      fBackGroundPic.RX := rxGuiMainH;
      fBackGroundPic.ID := 0;
    end;
  end;

  if UNLOCK_CAMPAIGN_MAPS then //Unlock more maps for debug
    fUnlockedMap := fMapCount;
end;


procedure TKMCampaign.SetMapCount(aValue: Byte);
begin
  fMapCount := aValue;
  SetLength(Maps, fMapCount);
end;


function TKMCampaign.CampaignTitle: UnicodeString;
begin
  Result := fTextLib[0];
end;


function TKMCampaign.CampaignDescription: UnicodeString;
begin
  Result := fTextLib[2];
end;


function TKMCampaign.MissionFile(aIndex: Byte): UnicodeString;
begin
  Result := fPath + UnicodeString(fShortTitle) + Format('%.2d', [aIndex + 1]) + PathDelim +
            UnicodeString(fShortTitle) + Format('%.2d', [aIndex + 1]) + '.dat';
end;


function TKMCampaign.MissionTitle(aIndex: Byte): UnicodeString;
begin
  Result := Format(fTextLib[1], [aIndex+1]);
end;


//Mission texts of original campaigns are available in all languages,
//custom campaigns are unlikely to have more texts in more than 1-2 languages
function TKMCampaign.MissionBriefing(aIndex: Byte): UnicodeString;
begin
  Result := fTextLib[10 + aIndex];
end;


function TKMCampaign.BreifingAudioFile(aIndex: Byte): UnicodeString;
begin
  Result := fPath + UnicodeString(fShortTitle) + Format('%.2d', [aIndex+1]) + PathDelim +
            UnicodeString(fShortTitle) + Format('%.2d', [aIndex + 1]) + '.' + UnicodeString(gResLocales.UserLocale) + '.mp3';

  if not FileExists(Result) then
    Result := fPath + UnicodeString(fShortTitle) + Format('%.2d', [aIndex+1]) + PathDelim +
              UnicodeString(fShortTitle) + Format('%.2d', [aIndex + 1]) + '.' + UnicodeString(gResLocales.FallbackLocale) + '.mp3';

  if not FileExists(Result) then
    Result := fPath + UnicodeString(fShortTitle) + Format('%.2d', [aIndex+1]) + PathDelim +
              UnicodeString(fShortTitle) + Format('%.2d', [aIndex + 1]) + '.' + UnicodeString(gResLocales.DefaultLocale) + '.mp3';
end;


//When player completes one map we allow to reveal the next one, note that
//player may be replaying previous maps, in that case his progress remains the same
procedure TKMCampaign.SetUnlockedMap(aValue: Byte);
begin
  fUnlockedMap := EnsureRange(aValue, fUnlockedMap, fMapCount - 1);
end;


end.

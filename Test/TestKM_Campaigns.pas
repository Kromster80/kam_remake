unit TestKM_Campaigns;
interface
uses
  TestFramework, SysUtils, KM_Points, KM_Defaults, KM_CommonClasses, Classes, KromUtils,
  KM_Campaigns, KM_ResLocales, KM_Log, KM_Pics, KM_ResTexts, KM_Resource, Math;

type
  // Test methods for class TKMCampaign
  TestTKMCampaign = class(TTestCase)
  strict private
    FKMCampaign: TKMCampaign;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadFromFile;
    procedure TestSaveToFile;
    procedure TestLoadFromPath;
    procedure TestMissionFile;
    procedure TestMissionTitle;
    procedure TestMissionText;
  end;

  // Test methods for class TKMCampaignsCollection
  TestTKMCampaignsCollection = class(TTestCase)
  strict private
    fCampaigns: TKMCampaignsCollection;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestScanFolder;
    procedure TestLoadProgress;
    procedure TestSaveProgress;
    procedure TestSetActive;
    procedure TestCount;
    procedure TestCampaignByTitle;
    procedure TestUnlockNextMap;
    procedure TestSave;
    procedure TestLoad;
  end;

implementation

procedure TestTKMCampaign.SetUp;
begin
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';
  FKMCampaign := TKMCampaign.Create;
  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\temp.log');
  gRes := TKMResource.Create(nil, nil);
end;

procedure TestTKMCampaign.TearDown;
begin
  gRes.Free;
  gLog.Free;
  FKMCampaign.Free;
  FKMCampaign := nil;
end;

procedure TestTKMCampaign.TestLoadFromFile;
begin
  FKMCampaign.LoadFromFile('..\Campaigns\The Shattered Kingdom\info.cmp');

  Check(FKMCampaign.MapCount = 20);
  Check(FKMCampaign.Maps[0].NodeCount > 0);
  Check(FKMCampaign.CampName <> '');
  Check(FKMCampaign.UnlockedMap = 0);
  Check(FKMCampaign.MissionFile(0) <> '');

  //Pic is assigned in LoadFromPath
  Check(FKMCampaign.BackGroundPic.RX = rxTrees);
  Check(FKMCampaign.BackGroundPic.ID = 0);
end;

procedure TestTKMCampaign.TestSaveToFile;
var
  FileLoad, FileSave: string;
begin
  //Test with sample file
  FileLoad := ExtractFilePath(ParamStr(0)) + '..\Campaigns\The Shattered Kingdom\info.cmp';
  FileSave := ExtractFilePath(ParamStr(0)) + 'Temp\campaign.tmp';
  ForceDirectories(ExtractFilePath(FileSave));
  FKMCampaign.LoadFromFile(FileLoad);
  FKMCampaign.SaveToFile(FileSave);
  Check(CheckSameContents(FileLoad, FileSave));
end;

procedure TestTKMCampaign.TestLoadFromPath;
begin
  //
end;

procedure TestTKMCampaign.TestMissionFile;
begin
  FKMCampaign.LoadFromFile('..\Campaigns\The Shattered Kingdom\info.cmp');
  Check(FKMCampaign.MissionFile(0) = 'TSK01\TSK01.dat', 'Unexpected result: ' + FKMCampaign.MissionFile(0));
  FKMCampaign.LoadFromFile('..\Campaigns\The Peasants Rebellion\info.cmp');
  Check(FKMCampaign.MissionFile(0) = 'TPR01\TPR01.dat', 'Unexpected result: ' + FKMCampaign.MissionFile(0));
end;

procedure TestTKMCampaign.TestMissionTitle;
begin
  //FKMCampaign.MissionTitle(aIndex);
end;

procedure TestTKMCampaign.TestMissionText;
begin
  //ReturnValue := FKMCampaign.MissionText(aIndex);
end;

procedure TestTKMCampaignsCollection.SetUp;
begin
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';
  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\log.tmp');

  gRes := TKMResource.Create(nil, nil);
  gRes.LoadLocaleResources(DEFAULT_LOCALE);

  fCampaigns := TKMCampaignsCollection.Create;
end;

procedure TestTKMCampaignsCollection.TearDown;
begin
  FreeAndNil(gRes);
  FreeAndNil(gLog);
  FreeAndNil(fCampaigns);
end;

procedure TestTKMCampaignsCollection.TestCount;
begin
  Check(fCampaigns.Count = 0);

  fCampaigns.ScanFolder(ExeDir + 'Campaigns\');
  Check(fCampaigns.Count >= 2, 'TSK and TPR campaigns should be there');
end;

procedure TestTKMCampaignsCollection.TestCampaignByTitle;
var
  ReturnValue: TKMCampaign;
  aShortTitle: AnsiString;
begin
  // TODO: Setup method call parameters
  //ReturnValue := FKMCampaignsCollection.CampaignByTitle(aShortTitle);
  // TODO: Validate method results
end;

procedure TestTKMCampaignsCollection.TestUnlockNextMap;
var I: Integer;
begin
  //Check that first map is available
  fCampaigns.ScanFolder(ExeDir + 'Campaigns\');
  fCampaigns.SetActive(fCampaigns.Campaigns[0], 0);
  Check(fCampaigns.ActiveCampaign.UnlockedMap = 0, 'First map should be unlocked');

  //Unlock all the maps consequently
  for I := 0 to fCampaigns.Campaigns[0].MapCount do
  begin
    fCampaigns.SetActive(fCampaigns.Campaigns[0], I);
    fCampaigns.UnlockNextMap;
    Check(fCampaigns.ActiveCampaign.UnlockedMap = Min(I+1, fCampaigns.Campaigns[0].MapCount - 1), 'Wrong next map ' + IntToStr(I));
  end;
end;

procedure TestTKMCampaignsCollection.TestSave;
var
  SaveStream: TKMemoryStream;
begin
  //Empty collection
  SaveStream := TKMemoryStream.Create;
  fCampaigns.Save(SaveStream);
  SaveStream.Position := 0;
  fCampaigns.Load(SaveStream);
  Check(fCampaigns.Count = 0);
  SaveStream.Free;
end;

procedure TestTKMCampaignsCollection.TestLoad;
begin
  //
end;

procedure TestTKMCampaignsCollection.TestScanFolder;
begin
  fCampaigns.ScanFolder(ExeDir + 'Campaigns\');
  Check(fCampaigns.Count >= 2);
end;

procedure TestTKMCampaignsCollection.TestLoadProgress;
begin
  //
end;

procedure TestTKMCampaignsCollection.TestSaveProgress;
var
  FileName: string;
begin
  //Empty
  FileName := ExtractFilePath(ParamStr(0)) + 'Temp\camp.tmp';
  fCampaigns.SaveProgress(FileName);
  fCampaigns.LoadProgress(FileName);
  Check(fCampaigns.Count = 0);
  Check(fCampaigns.ActiveCampaign = nil, 'Empty campaign should be nil');

  //Filled
end;

procedure TestTKMCampaignsCollection.TestSetActive;
begin
  //Check that first map is available
  fCampaigns.ScanFolder(ExeDir + 'Campaigns\');
  Check(fCampaigns.ActiveCampaign = nil, 'Initial campaign should be nil');

  //Select first campaign
  fCampaigns.SetActive(fCampaigns.Campaigns[0], 0);
  Check(fCampaigns.ActiveCampaign = fCampaigns.Campaigns[0]);
  Check(fCampaigns.ActiveCampaign.UnlockedMap = 0, 'First map should be unlocked');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Campaigns', TestTKMCampaign.Suite);
  RegisterTest('Campaigns', TestTKMCampaignsCollection.Suite);
end.


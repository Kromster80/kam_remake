unit KM_Settings;
interface
uses Classes, SysUtils, KromUtils, Math, KM_Defaults, inifiles, KM_CommonTypes;

{Global game settings}
type
  TGameSettings = class
  private
    fBrightness:byte;
    fAutosave:boolean;
    fFastScroll:boolean;
    fMouseSpeed:byte;
    fSoundFXVolume:byte;
    fMusicVolume:byte;
    fMusicOnOff:boolean;
    fFullScreen:boolean;
    fLocale:shortstring;
    fPace:word;
    fSpeedup:word;
    fResolutionID:word; //Relates to index in SupportedResolution
    SlidersMin,SlidersMax:byte;
    fNeedsSave: boolean;
    function LoadSettingsFromFile(filename:string):boolean;
    procedure SaveSettingsToFile(filename:string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveSettings;
    property GetBrightness:byte read fBrightness default 1;
    property GetLocale:shortstring read fLocale;
    property SetLocale:shortstring write fLocale;
    property GetResolutionID:word read fResolutionID;
    property SetResolutionID:word write fResolutionID;
    property GetPace:word read fPace;
    property GetSpeedup:word read fSpeedup;
    procedure IncBrightness;
    procedure DecBrightness;
    procedure SetIsAutosave(val:boolean);
    procedure SetIsFastScroll(val:boolean);
    procedure SetIsFullScreen(val:boolean);
    property IsAutosave:boolean read fAutosave write SetIsAutosave default true;
    property IsFastScroll:boolean read fFastScroll write SetIsFastScroll default false;
    property GetSlidersMin:byte read SlidersMin;
    property GetSlidersMax:byte read SlidersMax;
    property GetNeedsSave:boolean read fNeedsSave;
    procedure SetMouseSpeed(Value:integer);
    procedure SetSoundFXVolume(Value:integer);
    procedure SetMusicVolume(Value:integer);
    procedure SetMusicOnOff(Value:boolean);
    procedure UpdateSFXVolume();
    property GetMouseSpeed:byte read fMouseSpeed;
    property GetSoundFXVolume:byte read fSoundFXVolume;
    property GetMusicVolume:byte read fMusicVolume;
    property IsMusic:boolean read fMusicOnOff write SetMusicOnOff default true;
    property IsFullScreen:boolean read fFullScreen write SetIsFullScreen default true;
  end;

{These are mission specific settings and stats for each player}
type
  TMissionSettings = class
  private
    HouseTotalCount,HouseBuiltCount,HouseLostCount:array[1..HOUSE_COUNT]of integer;
    UnitTotalCount,UnitTrainedCount,UnitLostCount:array[1..40]of integer;
    ResourceRatios:array[1..4,1..4]of byte;
    MissionTimeInSec:cardinal;
  public
    AllowToBuild:array[1..HOUSE_COUNT]of boolean; //Allowance derived from mission script
    BuildReqDone:array[1..HOUSE_COUNT]of boolean; //If building requirements performed or assigned from script
    constructor Create;
    procedure CreatedHouse(aType:THouseType; aWasBuilt:boolean);
    procedure CreatedUnit(aType:TUnitType; aWasTrained:boolean);
    procedure DestroyedHouse(aType:THouseType);
    procedure DestroyedUnit(aType:TUnitType);
  public
    procedure UpdateReqDone(aType:THouseType);
    procedure IncreaseMissionTime(aSeconds:cardinal);
  public
    function GetHouseQty(aType:THouseType):integer;
    function GetTotalHouseQty():integer;
    function GetUnitQty(aType:TUnitType):integer;
    function GetArmyCount():integer;
    function GetCanBuild(aType:THouseType):boolean;

    function GetRatio(aRes:TResourceType; aHouse:THouseType):byte;
    procedure SetRatio(aRes:TResourceType; aHouse:THouseType; aValue:byte);

    function GetUnitsLost:cardinal;
    function GetUnitsKilled:cardinal;
    function GetHousesLost:cardinal;
    function GetHousesDestroyed:cardinal;
    function GetHousesConstructed:cardinal;
    function GetUnitsTrained:cardinal;
    function GetWeaponsProduced:cardinal;
    function GetSoldiersTrained:cardinal;
    property GetMissionTime:cardinal read MissionTimeInSec;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


implementation
uses KM_SoundFX;


constructor TGameSettings.Create;
begin
  Inherited;
  SlidersMin:=0;
  SlidersMax:=20;
  LoadSettingsFromFile(ExeDir+SETTINGS_FILE);

  fNeedsSave:=false;
end;

destructor TGameSettings.Destroy;
begin
  SaveSettingsToFile(ExeDir+SETTINGS_FILE);
  Inherited;
end;

procedure TGameSettings.SaveSettings;
begin
  SaveSettingsToFile(ExeDir+SETTINGS_FILE);
end;

function TGameSettings.LoadSettingsFromFile(filename:string):boolean;
var f:TIniFile;
begin
  Result := FileExists(filename);

  f := TIniFile.Create(filename);

  fBrightness    := f.ReadInteger('GFX','Brightness',1);
  fFullScreen    := f.ReadBool   ('GFX','FullScreen',false);
  fResolutionID  := f.ReadInteger('GFX','ResolutionID',1);

  fAutosave      := f.ReadBool   ('Game','Autosave',false);
  fFastScroll    := f.ReadBool   ('Game','FastScroll',false);
  fMouseSpeed    := f.ReadInteger('Game','MouseSpeed',10);
  fLocale        := f.ReadString ('Game','Locale','eng');
  fPace          := f.ReadInteger('Game','GamePace',100);
  fSpeedup       := f.ReadInteger('Game','Speedup',10);

  fSoundFXVolume := f.ReadInteger('SFX','SFXVolume',10);
  fMusicVolume   := f.ReadInteger('SFX','MusicVolume',10);
  fMusicOnOff    := f.ReadBool   ('SFX','MusicEnabled',true);

  FreeAndNil(f);
  fNeedsSave:=false;
end;


procedure TGameSettings.SaveSettingsToFile(filename:string);
var f:TIniFile;
begin      
  f := TIniFile.Create(filename);

  f.WriteInteger('GFX','Brightness',  fBrightness);
  f.WriteBool   ('GFX','FullScreen',  fFullScreen);
  f.WriteInteger('GFX','ResolutionID',fResolutionID);

  f.WriteBool   ('Game','Autosave',   fAutosave);
  f.WriteBool   ('Game','FastScroll', fFastScroll);
  f.WriteInteger('Game','MouseSpeed', fMouseSpeed);
  f.WriteString ('Game','Locale',     fLocale);
  f.WriteInteger('Game','GamePace',   fPace);
  f.WriteInteger('Game','Speedup',    fSpeedup);

  f.WriteInteger('SFX','SFXVolume',   fSoundFXVolume);
  f.WriteInteger('SFX','MusicVolume', fMusicVolume);
  f.WriteBool   ('SFX','MusicEnabled',fMusicOnOff);

  FreeAndNil(f);
  fNeedsSave:=false;
end;

procedure TGameSettings.IncBrightness;
begin
  fBrightness:= EnsureRange(fBrightness+1,1,6);
  fNeedsSave:=true;
end;

procedure TGameSettings.DecBrightness;
begin
  fBrightness:= EnsureRange(fBrightness-1,1,6);
  fNeedsSave:=true;
end;

procedure TGameSettings.SetIsAutosave(val:boolean);
begin
  fAutosave:=val;
  fNeedsSave:=true;
end;

procedure TGameSettings.SetIsFastScroll(val:boolean);
begin
  fFastScroll:=val;
  fNeedsSave:=true;
end;

procedure TGameSettings.SetIsFullScreen(val:boolean);
begin
  fFullScreen:=val;
  fNeedsSave:=true;
end;

procedure TGameSettings.SetMouseSpeed(Value:integer);
begin
  fMouseSpeed:=EnsureRange(Value,SlidersMin,SlidersMax);
  fNeedsSave:=true;
end;

procedure TGameSettings.SetSoundFXVolume(Value:integer);
begin
  fSoundFXVolume:=EnsureRange(Value,SlidersMin,SlidersMax);
  UpdateSFXVolume();
  fNeedsSave:=true;
end;

procedure TGameSettings.SetMusicVolume(Value:integer);
begin
  fMusicVolume:=EnsureRange(Value,SlidersMin,SlidersMax);
  UpdateSFXVolume();
  fNeedsSave:=true;
end;

procedure TGameSettings.SetMusicOnOff(Value:boolean);
begin
  if fMusicOnOff <> Value then
  begin
    fMusicOnOff:=Value;
    if Value then fMusicLib.PlayMenuTrack(not IsMusic) //Start with the default track
    else fMusicLib.StopMusic;
  end;
  fNeedsSave:=true;
end;


procedure TGameSettings.UpdateSFXVolume();
begin
  fSoundLib.UpdateSFXVolume(fSoundFXVolume/SlidersMax);
  fMusicLib.UpdateMusicVolume(fMusicVolume/SlidersMax);
  fNeedsSave:=true;
end;


{ TMissionSettings }
constructor TMissionSettings.Create;
var i,k:integer;
begin
  Inherited;
  for i:=1 to length(AllowToBuild) do AllowToBuild[i]:=true;
  BuildReqDone[byte(ht_Store)]:=true;
  for i:=1 to 4 do
    for k:=1 to 4 do
      ResourceRatios[i,k] := DistributionDefaults[i,k];
  MissionTimeInSec:=0; //Init mission timer
end;


procedure TMissionSettings.CreatedHouse(aType:THouseType; aWasBuilt:boolean);
begin
  inc(HouseTotalCount[byte(aType)]);
  if aWasBuilt then inc(HouseBuiltCount[byte(aType)]);
  UpdateReqDone(aType);
end;


procedure TMissionSettings.CreatedUnit(aType:TUnitType; aWasTrained:boolean);
begin
  if aWasTrained then
    inc(UnitTrainedCount[byte(aType)]);

  inc(UnitTotalCount[byte(aType)]);
end;


procedure TMissionSettings.UpdateReqDone(aType:THouseType);
var i:integer;
begin
  for i:=1 to length(BuildingAllowed[1]) do
    if BuildingAllowed[byte(aType),i]<>ht_None then
      BuildReqDone[byte(BuildingAllowed[byte(aType),i])]:=true;
end;


procedure TMissionSettings.IncreaseMissionTime(aSeconds:cardinal);
begin
  inc(MissionTimeInSec,aSeconds);
end;


procedure TMissionSettings.DestroyedHouse(aType:THouseType);
begin
  inc(HouseLostCount[byte(aType)]);
end;


procedure TMissionSettings.DestroyedUnit(aType:TUnitType);
begin
  inc(UnitLostCount[byte(aType)]);
end;


function TMissionSettings.GetHouseQty(aType:THouseType):integer;
begin
  Result := HouseTotalCount[byte(aType)] - HouseLostCount[byte(aType)];
end;


function TMissionSettings.GetTotalHouseQty():integer;
var i:integer;
begin
  Result := 0;
  for i:=1 to HOUSE_COUNT do
    inc(Result, HouseTotalCount[i] - HouseLostCount[i]);
end;


function TMissionSettings.GetUnitQty(aType:TUnitType):integer;
begin
  Result := UnitTotalCount[byte(aType)] - UnitLostCount[byte(aType)];
end;


function TMissionSettings.GetArmyCount():integer;
var i:byte;
begin
  Result:=0;
  for i:=byte(ut_Militia) to byte(ut_Barbarian) do
    Result := Result + GetUnitQty(TUnitType(i));
end;


function TMissionSettings.GetCanBuild(aType:THouseType):boolean;
begin
  Result := BuildReqDone[byte(aType)] AND AllowToBuild[byte(aType)];
end;


function TMissionSettings.GetRatio(aRes:TResourceType; aHouse:THouseType):byte;
begin
  Result:=5; //Default should be 5, for house/resource combinations that don't have a setting (on a side note this should be the only place the resourse limit is defined)
  case aRes of
    rt_Steel: if aHouse=ht_WeaponSmithy   then Result:=ResourceRatios[1,1] else
              if aHouse=ht_ArmorSmithy    then Result:=ResourceRatios[1,2];
    rt_Coal:  if aHouse=ht_IronSmithy     then Result:=ResourceRatios[2,1] else
              if aHouse=ht_Metallurgists  then Result:=ResourceRatios[2,2] else
              if aHouse=ht_WeaponSmithy   then Result:=ResourceRatios[2,3] else
              if aHouse=ht_ArmorSmithy    then Result:=ResourceRatios[2,4];
    rt_Wood:  if aHouse=ht_ArmorWorkshop  then Result:=ResourceRatios[3,1] else
              if aHouse=ht_WeaponWorkshop then Result:=ResourceRatios[3,2];
    rt_Corn:  if aHouse=ht_Mill           then Result:=ResourceRatios[4,1] else
              if aHouse=ht_Swine          then Result:=ResourceRatios[4,2] else
              if aHouse=ht_Stables        then Result:=ResourceRatios[4,3];
  end;
end;


procedure TMissionSettings.SetRatio(aRes:TResourceType; aHouse:THouseType; aValue:byte);
begin
  case aRes of
    rt_Steel: if aHouse=ht_WeaponSmithy   then ResourceRatios[1,1]:=aValue else
              if aHouse=ht_ArmorSmithy    then ResourceRatios[1,2]:=aValue;
    rt_Coal:  if aHouse=ht_IronSmithy     then ResourceRatios[2,1]:=aValue else
              if aHouse=ht_Metallurgists  then ResourceRatios[2,2]:=aValue else
              if aHouse=ht_WeaponSmithy   then ResourceRatios[2,3]:=aValue else
              if aHouse=ht_ArmorSmithy    then ResourceRatios[2,4]:=aValue;
    rt_Wood:  if aHouse=ht_ArmorWorkshop  then ResourceRatios[3,1]:=aValue else
              if aHouse=ht_WeaponWorkshop then ResourceRatios[3,2]:=aValue;
    rt_Corn:  if aHouse=ht_Mill           then ResourceRatios[4,1]:=aValue else
              if aHouse=ht_Swine          then ResourceRatios[4,2]:=aValue else
              if aHouse=ht_Stables        then ResourceRatios[4,3]:=aValue;
    else fLog.AssertToLog(false,'Unexpected resource at SetRatio');
  end;
end;


function TMissionSettings.GetUnitsLost:cardinal;
var i:integer;
begin
  Result:=0;
  for i:=low(UnitLostCount) to high(UnitLostCount) do
    inc(Result,UnitLostCount[i]);
end;


function TMissionSettings.GetUnitsKilled:cardinal;
begin
  Result:=0;
end;


function TMissionSettings.GetHousesLost:cardinal;
var i:integer;
begin
  Result:=0;
  for i:=low(HouseLostCount) to high(HouseLostCount) do
    inc(Result,HouseLostCount[i]);
end;


function TMissionSettings.GetHousesDestroyed:cardinal;
begin
  Result:=0;
end;


function TMissionSettings.GetHousesConstructed:cardinal;
var i:integer;
begin
  Result:=0;
  for i:=low(HouseBuiltCount) to high(HouseBuiltCount) do
    inc(Result,HouseBuiltCount[i]);
end;


function TMissionSettings.GetUnitsTrained:cardinal;
var i:integer;
begin
  Result:=0;
  for i:=byte(ut_Serf) to byte(ut_Recruit) do
    inc(Result,UnitTrainedCount[i]);
end;


function TMissionSettings.GetWeaponsProduced:cardinal;
begin
  Result:=0;
end;


function TMissionSettings.GetSoldiersTrained:cardinal;
var i:integer;
begin
  Result:=0;
  for i:=byte(ut_Militia) to byte(ut_Barbarian) do
    inc(Result,UnitTrainedCount[i]);
end;


procedure TMissionSettings.Save(SaveStream:TKMemoryStream);
var i,k:integer;
begin
  for i:=1 to HOUSE_COUNT do SaveStream.Write(HouseTotalCount[i]);
  for i:=1 to HOUSE_COUNT do SaveStream.Write(HouseBuiltCount[i]);
  for i:=1 to HOUSE_COUNT do SaveStream.Write(HouseLostCount[i]);
  for i:=1 to 40 do SaveStream.Write(UnitTotalCount[i]);
  for i:=1 to 40 do SaveStream.Write(UnitTrainedCount[i]);
  for i:=1 to 40 do SaveStream.Write(UnitLostCount[i]);
  for i:=1 to 4 do for k:=1 to 4 do SaveStream.Write(ResourceRatios[i,k]);
  SaveStream.Write(MissionTimeInSec, 4);
  for i:=1 to HOUSE_COUNT do SaveStream.Write(AllowToBuild[i]);
  for i:=1 to HOUSE_COUNT do SaveStream.Write(BuildReqDone[i]);
end;


procedure TMissionSettings.Load(LoadStream:TKMemoryStream);
var i,k:integer;
begin
  for i:=1 to HOUSE_COUNT do LoadStream.Read(HouseTotalCount[i]);
  for i:=1 to HOUSE_COUNT do LoadStream.Read(HouseBuiltCount[i]);
  for i:=1 to HOUSE_COUNT do LoadStream.Read(HouseLostCount[i]);
  for i:=1 to 40 do LoadStream.Read(UnitTotalCount[i]);
  for i:=1 to 40 do LoadStream.Read(UnitTrainedCount[i]);
  for i:=1 to 40 do LoadStream.Read(UnitLostCount[i]);
  for i:=1 to 4 do for k:=1 to 4 do LoadStream.Read(ResourceRatios[i,k]);
  LoadStream.Read(MissionTimeInSec, 4);
  for i:=1 to HOUSE_COUNT do LoadStream.Read(AllowToBuild[i]);
  for i:=1 to HOUSE_COUNT do LoadStream.Read(BuildReqDone[i]);
end;


end.

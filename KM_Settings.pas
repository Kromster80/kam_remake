unit KM_Settings;
interface
uses Windows, Classes, SysUtils, KromUtils, Math, KM_Defaults, inifiles;

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
    HouseBuiltCount,HouseLostCount:array[1..HOUSE_COUNT]of word;
    UnitTrainedCount,UnitLostCount:array[1..40]of word;
  public
    AllowToBuild:array[1..HOUSE_COUNT]of boolean; //Allowance derived from mission script
    BuildReqDone:array[1..HOUSE_COUNT]of boolean; //If building requirements performed or assigned from script
    constructor Create;
    procedure CreatedHouse(aType:THouseType);
    procedure CreatedUnit(aType:TUnitType);
    procedure DestroyedHouse(aType:THouseType);
    procedure DestroyedUnit(aType:TUnitType);
  public
    procedure UpdateReqDone(aType:THouseType);
  public
    function GetHouseQty(aType:THouseType):integer;
    function GetUnitQty(aType:TUnitType):integer;
    function GetCanBuild(aType:THouseType):boolean;
  end;


var
  fGameSettings: TGameSettings;


implementation
uses KM_LoadSFX;

constructor TGameSettings.Create;
begin
  Inherited Create;
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
  fFullScreen    := f.ReadBool   ('GFX','FullScreen',true);

  fAutosave      := f.ReadBool   ('Game','Autosave',false);
  fFastScroll    := f.ReadBool   ('Game','FastScroll',false);
  fMouseSpeed    := f.ReadInteger('Game','MouseSpeed',10);
  fLocale        := f.ReadString ('Game','Locale','eng');

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

  f.WriteInteger('GFX','Brightness', fBrightness);
  f.WriteBool   ('GFX','FullScreen', fFullScreen);

  f.WriteBool   ('Game','Autosave',  fAutosave);
  f.WriteBool   ('Game','FastScroll',fFastScroll);
  f.WriteInteger('Game','MouseSpeed',fMouseSpeed);
  f.WriteString ('Game','Locale',    fLocale);

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
var OldValue: boolean;
begin
  OldValue:=fMusicOnOff;
  fMusicOnOff:=Value;
  if fMusicOnOff <> OldValue then
    if Value then fSoundLib.PlayMenuTrack //Start with the default track
    else fSoundLib.StopMusic;
  fNeedsSave:=true;
end;


procedure TGameSettings.UpdateSFXVolume();
begin
  fSoundLib.UpdateSFXVolume(fSoundFXVolume/SlidersMax);
  fSoundLib.UpdateMusicVolume(fMusicVolume/SlidersMax);
  fNeedsSave:=true;
end;


{ TMissionSettings }
constructor TMissionSettings.Create;
var i:integer;
begin
  Inherited;
  for i:=1 to length(AllowToBuild) do AllowToBuild[i]:=true;
  BuildReqDone[byte(ht_Store)]:=true;
end;


procedure TMissionSettings.CreatedHouse(aType:THouseType);
begin
  inc(HouseBuiltCount[byte(aType)]);
  UpdateReqDone(aType);
end;


procedure TMissionSettings.CreatedUnit(aType:TUnitType);
begin
  inc(UnitTrainedCount[byte(aType)]);
end;


procedure TMissionSettings.UpdateReqDone(aType:THouseType);
var i:integer;
begin
  for i:=1 to length(BuildingAllowed[1]) do if BuildingAllowed[byte(aType),i]<>ht_None then
    BuildReqDone[byte(BuildingAllowed[byte(aType),i])]:=true;
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
  Result:=HouseBuiltCount[byte(aType)]-HouseLostCount[byte(aType)];
end;


function TMissionSettings.GetUnitQty(aType:TUnitType):integer;
begin
  Result:=UnitTrainedCount[byte(aType)]-UnitLostCount[byte(aType)];
end;


function TMissionSettings.GetCanBuild(aType:THouseType):boolean;
begin
  Result:=BuildReqDone[byte(aType)] AND AllowToBuild[byte(aType)];
end;


end.

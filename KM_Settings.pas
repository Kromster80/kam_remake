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
    SlidersMin,SlidersMax:byte;
    function LoadSettingsFromFile(filename:string):boolean;
    procedure SaveSettingsToFile(filename:string);
  public
    constructor Create;
    destructor Destroy; override;
    property GetBrightness:byte read fBrightness default 1;
    procedure IncBrightness;
    procedure DecBrightness;
    property IsAutosave:boolean read fAutosave write fAutosave default true;
    property IsFastScroll:boolean read fFastScroll write fFastScroll default false;
    property GetSlidersMin:byte read SlidersMin;
    property GetSlidersMax:byte read SlidersMax;
    procedure SetMouseSpeed(Value:integer);
    procedure SetSoundFXVolume(Value:integer);
    procedure SetMusicVolume(Value:integer);
    procedure SetMusicOnOff(Value:boolean);
    procedure UpdateSFXVolume();
    property GetMouseSpeed:byte read fMouseSpeed;
    property GetSoundFXVolume:byte read fSoundFXVolume;
    property GetMusicVolume:byte read fMusicVolume;
    property IsMusic:boolean read fMusicOnOff write SetMusicOnOff default true;
    property IsFullScreen:boolean read fFullScreen write fFullScreen default true;
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
  LoadSettingsFromFile(ExeDir+'KaM_Remake_Settings.ini');
  UpdateSFXVolume(); //Other settings may added here as well
end;

destructor TGameSettings.Destroy;
begin
  SaveSettingsToFile(ExeDir+'KaM_Remake_Settings.ini');
  Inherited;
end;

function TGameSettings.LoadSettingsFromFile(filename:string):boolean;
var f:TIniFile;
begin
  f := TIniFile.Create(filename);

  fBrightness    := f.ReadInteger('GFX','Brightness',1);
  fFullScreen    := f.ReadBool   ('GFX','FullScreen',true);

  fAutosave      := f.ReadBool   ('Game','Autosave',false);
  fFastScroll    := f.ReadBool   ('Game','FastScroll',false);
  fMouseSpeed    := f.ReadInteger('Game','MouseSpeed',10);

  fSoundFXVolume := f.ReadInteger('SFX','SFXVolume',10);
  fMusicVolume   := f.ReadInteger('SFX','MusicVolume',10);
  fMusicOnOff    := f.ReadBool   ('SFX','MusicEnabled',true);

  FreeAndNil(f);
end;


procedure TGameSettings.SaveSettingsToFile(filename:string);
var f:TIniFile;
begin      
  f := TIniFile.Create(filename);

  f.WriteInteger('GFX','Brightness',fBrightness);
  f.WriteBool   ('GFX','FullScreen',fFullScreen);

  f.WriteBool   ('Game','Autosave',fAutosave);
  f.WriteBool   ('Game','FastScroll',fFastScroll);
  f.WriteInteger('Game','MouseSpeed',fMouseSpeed);

  f.WriteInteger('SFX','SFXVolume',fSoundFXVolume); 
  f.WriteInteger('SFX','MusicVolume',fMusicVolume);
  f.WriteBool   ('SFX','MusicEnabled',fMusicOnOff);

  FreeAndNil(f);
end;

procedure TGameSettings.IncBrightness;
begin
  fBrightness:= EnsureRange(fBrightness+1,1,6);
end;

procedure TGameSettings.DecBrightness;
begin
  fBrightness:= EnsureRange(fBrightness-1,1,6);
end;

procedure TGameSettings.SetMouseSpeed(Value:integer);
begin
  fMouseSpeed:=EnsureRange(Value,SlidersMin,SlidersMax);
end;

procedure TGameSettings.SetSoundFXVolume(Value:integer);
begin
  fSoundFXVolume:=EnsureRange(Value,SlidersMin,SlidersMax);
  UpdateSFXVolume();
end;

procedure TGameSettings.SetMusicVolume(Value:integer);
begin
  fMusicVolume:=EnsureRange(Value,SlidersMin,SlidersMax);
  UpdateSFXVolume();
end;

procedure TGameSettings.SetMusicOnOff(Value:boolean);
var OldValue: boolean;
begin
  OldValue:=fMusicOnOff;
  fMusicOnOff:=Value;
  if fMusicOnOff <> OldValue then
    if Value then fSoundLib.PlayMenuTrack //Start with the default track
    else fSoundLib.StopMusic;
end;


procedure TGameSettings.UpdateSFXVolume();
begin
  fSoundLib.UpdateSFXVolume(fSoundFXVolume/SlidersMax);
  fSoundLib.UpdateMusicVolume(fMusicVolume/SlidersMax);
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

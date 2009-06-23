unit KM_Settings;
interface
uses Windows, Classes, SysUtils, KromUtils, Math, KM_Defaults;

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
    procedure SetDefaultValues;
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
var f:file;
begin
  //@Krom: This needs some kind of check that the file is complete
  // When I ran it then it crashed with "Read beyond end of file" because my config
  // is old and doesn't have the FullScreen value. Would you like me to make this
  // a true INI file format? I always use TIniFile from the unit "inifiles", which works well and manages
  // everything automatically. The only disadvantage of this is that anyone can read and edit the
  // config, but do we really care? Sometimes it is useful to be able to change the config
  // outside of a game. Also the file will be maybe 100 bytes larger, but that's not a problem either IMO.
  // Let me know what you think.
  //@Lewin: Please do it! :)
  //        In fact my config was just a quick patch, of course it should be proper INI all-in-all    
  Result:=false;
  if not CheckFileExists(filename,true) then
  begin
    SetDefaultValues; //There is no config, so set defaults
    exit;
  end;
  try
  assignfile(f,filename); reset(f,1);
  blockread(f, fBrightness, 1);
  blockread(f, fAutosave, 1);
  blockread(f, fFastScroll, 1);
  blockread(f, fMouseSpeed, 1);
  blockread(f, fSoundFXVolume, 1);
  blockread(f, fMusicVolume, 1);
  blockread(f, fMusicOnOff, 1);
  blockread(f, fFullScreen, 1);
  closefile(f);
  except end;
  Result:=true;
end;


procedure TGameSettings.SaveSettingsToFile(filename:string);
var f:file;
begin
  assignfile(f,filename); rewrite(f,1);
  blockwrite(f, fBrightness, 1);
  blockwrite(f, fAutosave, 1);
  blockwrite(f, fFastScroll, 1);
  blockwrite(f, fMouseSpeed, 1);
  blockwrite(f, fSoundFXVolume, 1);
  blockwrite(f, fMusicVolume, 1);
  blockwrite(f, fMusicOnOff, 1);
  blockwrite(f, fFullScreen, 1);
  closefile(f);
end;

procedure TGameSettings.SetDefaultValues;
begin
  //This procedure will set the default values for all of the settings
  fBrightness := 1;
  fAutosave := false;
  fFastScroll := false;
  fMouseSpeed := 10;
  fSoundFXVolume := 10;
  fMusicVolume := 10;
  fMusicOnOff := true;
  fFullScreen := true;
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
  Inherited Create;
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

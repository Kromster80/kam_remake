unit KM_Settings;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, Math, KM_Defaults, INIfiles, KM_CommonTypes;

{Global game settings}
type
  TGlobalSettings = class
  private
    fNeedsSave: boolean;

    fAutosave:boolean;
    fBrightness:byte;
    fFastScroll:boolean;
    fFullScreen:boolean;
    fLocale:shortstring;
    fMouseSpeed:byte;
    fMusicOn:boolean;
    fMusicVolume:byte;
    fResolutionID:word; //Relates to index in SupportedResolution
    fSlidersMin:byte;
    fSlidersMax:byte;
    fSoundFXVolume:byte;
    fSpeedPace:word;
    fSpeedup:word;
    fVSync:boolean;
    function LoadSettingsFromFile(filename:string):boolean;
    procedure SaveSettingsToFile(filename:string);

    procedure SetAutosave(aValue:boolean);
    procedure SetBrightness(aValue:byte);
    procedure SetFastScroll(aValue:boolean);
    procedure SetFullScreen(aValue:boolean);
    procedure SetLocale(aLocale:shortstring);
    procedure SetMouseSpeed(aValue:byte);
    procedure SetMusicOn(aValue:boolean);
    procedure SetMusicVolume(aValue:byte);
    procedure SetSoundFXVolume(aValue:byte);
  public
    //Temp for fight simulator
    fHitPointRestorePace:word;
    fHitPointRestoreInFights:boolean;
    constructor Create;
    destructor Destroy; override;
    procedure SaveSettings;

    property Autosave:boolean read fAutosave write SetAutosave default true;
    property Brightness:byte read fBrightness write SetBrightness default 1;
    property FastScroll:boolean read fFastScroll write SetFastScroll default false;
    property FullScreen:boolean read fFullScreen write SetFullScreen default true;
    property Locale:shortstring read fLocale write SetLocale;
    property MouseSpeed:byte read fMouseSpeed write SetMouseSpeed;
    property MusicOn:boolean read fMusicOn write SetMusicOn default true;
    property MusicVolume:byte read fMusicVolume write SetMusicVolume;
    property ResolutionID:word read fResolutionID write fResolutionID;
    property SlidersMin:byte read fSlidersMin;
    property SlidersMax:byte read fSlidersMax;
    property SoundFXVolume:byte read fSoundFXVolume write SetSoundFXVolume;
    property SpeedPace:word read fSpeedPace;
    property Speedup:word read fSpeedup;
    property VSync:boolean read fVSync;
  end;


{These are campaign settings }
type
  TCampaignSettings = class
  private
    fUnlockedMapsTSK:byte; //When player wins campaign mission this should be increased
    fUnlockedMapsTPR:byte;
    procedure LoadINI(filename:string);
    procedure SaveINI(filename:string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RevealMap(aCamp:TCampaign; aMap:byte);
    function GetMapsCount(aCamp:TCampaign):byte;
    function GetUnlockedMaps(aCamp:TCampaign):byte;
    function GetMapText(aCamp:TCampaign; MapID:byte):string;
  end;


implementation
uses KM_TextLibrary, KM_Sound, KM_Game;


constructor TGlobalSettings.Create;
begin
  Inherited;
  fSlidersMin := 0;
  fSlidersMax := 20;
  LoadSettingsFromFile(ExeDir+SETTINGS_FILE);
  fNeedsSave := false;
  fLog.AppendLog('Global settings init from '+SETTINGS_FILE);
end;


destructor TGlobalSettings.Destroy;
begin
  SaveSettingsToFile(ExeDir+SETTINGS_FILE);
  Inherited;
end;


//Save only when needed
procedure TGlobalSettings.SaveSettings;
begin
  if fNeedsSave then
    SaveSettingsToFile(ExeDir+SETTINGS_FILE);
end;


function TGlobalSettings.LoadSettingsFromFile(filename:string):boolean;
var f:TIniFile;
begin
  Result := FileExists(filename);

  f := TIniFile.Create(filename);
  
  fBrightness    := f.ReadInteger('GFX','Brightness',1);
  fFullScreen    := f.ReadBool   ('GFX','FullScreen',false);
  fVSync         := f.ReadBool   ('GFX','VSync',true);
  fResolutionID  := f.ReadInteger('GFX','ResolutionID',1);

  fAutosave      := f.ReadBool   ('Game','Autosave',true); //Should be ON by default
  fFastScroll    := f.ReadBool   ('Game','FastScroll',false);
  fMouseSpeed    := f.ReadInteger('Game','MouseSpeed',10);
  Locale         := f.ReadString ('Game','Locale','eng'); //Wrong name will become ENG too
  fSpeedPace     := f.ReadInteger('Game','SpeedPace',100);
  fSpeedup       := f.ReadInteger('Game','Speedup',10);

  fSoundFXVolume := f.ReadInteger('SFX','SFXVolume',10);
  fMusicVolume   := f.ReadInteger('SFX','MusicVolume',10);
  fMusicOn       := f.ReadBool   ('SFX','MusicEnabled',true);

  fHitPointRestorePace := f.ReadInteger('Fights','HitPointRestorePace',0);
  fHitPointRestoreInFights := f.ReadBool('Fights','HitPointRestoreInFights',true);

  FreeAndNil(f);
  fNeedsSave := false;
end;


procedure TGlobalSettings.SaveSettingsToFile(filename:string);
var f:TIniFile;
begin
  f := TIniFile.Create(filename);

  f.WriteInteger('GFX','Brightness',  fBrightness);
  f.WriteBool   ('GFX','FullScreen',  fFullScreen);
  f.WriteBool   ('GFX','VSync',       fVSync);
  f.WriteInteger('GFX','ResolutionID',fResolutionID);

  f.WriteBool   ('Game','Autosave',   fAutosave);
  f.WriteBool   ('Game','FastScroll', fFastScroll);
  f.WriteInteger('Game','MouseSpeed', fMouseSpeed);
  f.WriteString ('Game','Locale',     fLocale);
  f.WriteInteger('Game','SpeedPace',  fSpeedPace);
  f.WriteInteger('Game','Speedup',    fSpeedup);

  f.WriteInteger('SFX','SFXVolume',   fSoundFXVolume);
  f.WriteInteger('SFX','MusicVolume', fMusicVolume);
  f.WriteBool   ('SFX','MusicEnabled',fMusicOn);

  f.WriteInteger('Fights','HitPointRestorePace',fHitPointRestorePace);
  f.WriteBool   ('Fights','HitPointRestoreInFights',fHitPointRestoreInFights);

  FreeAndNil(f);
  fNeedsSave := false;
end;


//Scan list of available locales and pick existing one, or ignore
procedure TGlobalSettings.SetLocale(aLocale:shortstring);
var i:integer;
begin
  fLocale := Locales[1,1]; //Default - ENG
  for i:=low(Locales) to high(Locales) do
    if Locales[i,1] = aLocale then
      fLocale := aLocale;
end;


procedure TGlobalSettings.SetBrightness(aValue:byte);
begin
  fBrightness := EnsureRange(aValue,0,20);
  fNeedsSave  := true;
end;


procedure TGlobalSettings.SetAutosave(aValue:boolean);
begin
  fAutosave  := aValue;
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetFastScroll(aValue:boolean);
begin
  fFastScroll := aValue;
  fNeedsSave  := true;
end;


procedure TGlobalSettings.SetFullScreen(aValue:boolean);
begin
  fFullScreen := aValue;
  fNeedsSave  := true;
end;


procedure TGlobalSettings.SetMouseSpeed(aValue:byte);
begin
  fMouseSpeed := EnsureRange(aValue,fSlidersMin,fSlidersMax);
  fNeedsSave  := true;
end;


procedure TGlobalSettings.SetSoundFXVolume(aValue:byte);
begin
  fSoundFXVolume := EnsureRange(aValue,fSlidersMin,fSlidersMax);
  fSoundLib.UpdateSoundVolume(fSoundFXVolume/fSlidersMax);
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetMusicVolume(aValue:byte);
begin
  fMusicVolume := EnsureRange(aValue,fSlidersMin,fSlidersMax);
  fGame.fMusicLib.UpdateMusicVolume(fMusicVolume/fSlidersMax);
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetMusicOn(aValue:boolean);
begin
  if fMusicOn <> aValue then
  begin
    fMusicOn:=aValue;
    if aValue then
      fGame.fMusicLib.PlayMenuTrack(not MusicOn) //Start with the default track
    else
      fGame.fMusicLib.StopMusic;
  end;
  fNeedsSave := true;
end;


{ TCampaignSettings }
constructor TCampaignSettings.Create;
begin
  Inherited;
  LoadINI(ExeDir+'Saves\KaM_Remake_Campaigns.ini');
  fLog.AppendLog('Campaign.ini loaded');
end;


destructor TCampaignSettings.Destroy;
begin
  SaveINI(ExeDir+'Saves\KaM_Remake_Campaigns.ini');
  fLog.AppendLog('Campaign.ini saved');
  Inherited;
end;


{When player completes one map we allow to reveal the next one, note that
player may be replaying previous maps, in that case his progress remains the same}
procedure TCampaignSettings.RevealMap(aCamp:TCampaign; aMap:byte);
begin
  case aCamp of
    cmp_TSK: fUnlockedMapsTSK := EnsureRange(aMap, fUnlockedMapsTSK, TSK_MAPS);
    cmp_TPR: fUnlockedMapsTPR := EnsureRange(aMap, fUnlockedMapsTPR, TPR_MAPS);
  end;
end;


function TCampaignSettings.GetMapsCount(aCamp:TCampaign):byte;
begin
  Result := 1;
  case aCamp of
    cmp_Nil: Result := 0;
    cmp_TSK: Result := TSK_MAPS;
    cmp_TPR: Result := TPR_MAPS;
    cmp_Custom: Result := 1; //Yet unknown
    else Assert(false,'Unknown campaign');
  end;
end;


function TCampaignSettings.GetUnlockedMaps(aCamp:TCampaign):byte;
begin
  Result := 1;
  case aCamp of
    cmp_Nil: Result := 0;
    cmp_TSK: Result := fUnlockedMapsTSK;
    cmp_TPR: Result := fUnlockedMapsTPR;
    cmp_Custom: Result := 1; //Yet unknown
    else Assert(false,'Unknown campaign');
  end;
end;


{Get mission text description}
function TCampaignSettings.GetMapText(aCamp:TCampaign; MapID:byte):string;
begin
  case aCamp of
    cmp_Nil: Result := '';
    cmp_TSK: Result := fTextLibrary.GetSetupString(siCampTSKTexts + MapID - 1);
    cmp_TPR: Result := fTextLibrary.GetSetupString(siCampTPRTexts + MapID - 1);
    cmp_Custom: Result := '';
  end;
end;


procedure TCampaignSettings.SaveINI(filename:string);
var f:TIniFile;
begin
  f := TIniFile.Create(filename);
  f.WriteInteger('Campaign', 'TSK', fUnlockedMapsTSK);
  f.WriteInteger('Campaign', 'TPR', fUnlockedMapsTPR);
  FreeAndNil(f);
end;


procedure TCampaignSettings.LoadINI(filename:string);
var f:TIniFile;
begin
  f := TIniFile.Create(filename);
  fUnlockedMapsTSK := f.ReadInteger('Campaign', 'TSK', 1);
  fUnlockedMapsTPR := f.ReadInteger('Campaign', 'TPR', 1);
  FreeAndNil(f);
end;


end.

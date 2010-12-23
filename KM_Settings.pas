unit KM_Settings;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, Math, KM_Defaults, INIfiles, KM_CommonTypes;

{Global game settings}
type
  TGlobalSettings = class
  private
    fBrightness:byte;
    fAutosave:boolean;
    fFastScroll:boolean;
    fMouseSpeed:byte;
    fSoundFXVolume:byte;
    fMusicVolume:byte;
    fMusicOnOff:boolean;
    fFullScreen:boolean;
    fVSync:boolean;
    fLocale:shortstring;
    fPace:word;
    fSpeedup:word;
    fResolutionID:word; //Relates to index in SupportedResolution
    fSlidersMin,fSlidersMax:byte;
    fNeedsSave: boolean;
    function LoadSettingsFromFile(filename:string):boolean;
    procedure SaveSettingsToFile(filename:string);
  public
    //Temp for fight simulator
    fHitPointRestorePace:word;
    fHitPointRestoreInFights:boolean;
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
    procedure SetBrightness(aValue:integer);
    procedure SetIsAutosave(val:boolean);
    procedure SetIsFastScroll(val:boolean);
    procedure SetIsFullScreen(val:boolean);
    property IsAutosave:boolean read fAutosave write SetIsAutosave default true;
    property IsFastScroll:boolean read fFastScroll write SetIsFastScroll default false;
    property GetSlidersMin:byte read fSlidersMin;
    property GetSlidersMax:byte read fSlidersMax;
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
    property IsVSync:boolean read fVSync;
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


procedure TGlobalSettings.SaveSettings;
begin
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
  fLocale        := f.ReadString ('Game','Locale','eng');
  fPace          := f.ReadInteger('Game','GamePace',100);
  fSpeedup       := f.ReadInteger('Game','Speedup',10);

  fSoundFXVolume := f.ReadInteger('SFX','SFXVolume',10);
  fMusicVolume   := f.ReadInteger('SFX','MusicVolume',10);
  fMusicOnOff    := f.ReadBool   ('SFX','MusicEnabled',true);

  fHitPointRestorePace := f.ReadInteger('Fights','HitPointRestorePace',0);
  fHitPointRestoreInFights := f.ReadBool('Fights','HitPointRestoreInFights',true);

  FreeAndNil(f);
  fNeedsSave:=false;
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
  f.WriteInteger('Game','GamePace',   fPace);
  f.WriteInteger('Game','Speedup',    fSpeedup);

  f.WriteInteger('SFX','SFXVolume',   fSoundFXVolume);
  f.WriteInteger('SFX','MusicVolume', fMusicVolume);
  f.WriteBool   ('SFX','MusicEnabled',fMusicOnOff);

  f.WriteInteger('Fights','HitPointRestorePace',fHitPointRestorePace);
  f.WriteBool   ('Fights','HitPointRestoreInFights',fHitPointRestoreInFights);

  FreeAndNil(f);
  fNeedsSave := false;
end;


procedure TGlobalSettings.SetBrightness(aValue:integer);
begin
  fBrightness := EnsureRange(aValue,0,20);
  fNeedsSave  := true;
end;


procedure TGlobalSettings.SetIsAutosave(val:boolean);
begin
  fAutosave:=val;
  fNeedsSave:=true;
end;


procedure TGlobalSettings.SetIsFastScroll(val:boolean);
begin
  fFastScroll:=val;
  fNeedsSave:=true;
end;


procedure TGlobalSettings.SetIsFullScreen(val:boolean);
begin
  fFullScreen:=val;
  fNeedsSave:=true;
end;


procedure TGlobalSettings.SetMouseSpeed(Value:integer);
begin
  fMouseSpeed:=EnsureRange(Value,fSlidersMin,fSlidersMax);
  fNeedsSave:=true;
end;


procedure TGlobalSettings.SetSoundFXVolume(Value:integer);
begin
  fSoundFXVolume:=EnsureRange(Value,fSlidersMin,fSlidersMax);
  UpdateSFXVolume();
  fNeedsSave:=true;
end;


procedure TGlobalSettings.SetMusicVolume(Value:integer);
begin
  fMusicVolume:=EnsureRange(Value,fSlidersMin,fSlidersMax);
  UpdateSFXVolume();
  fNeedsSave:=true;
end;


procedure TGlobalSettings.SetMusicOnOff(Value:boolean);
begin
  if fMusicOnOff <> Value then
  begin
    fMusicOnOff:=Value;
    if Value then
      fGame.fMusicLib.PlayMenuTrack(not IsMusic) //Start with the default track
    else
      fGame.fMusicLib.StopMusic;
  end;
  fNeedsSave:=true;
end;


procedure TGlobalSettings.UpdateSFXVolume();
begin
  fSoundLib.UpdateSoundVolume(fSoundFXVolume/fSlidersMax);
  if fGame<>nil then
    fGame.fMusicLib.UpdateMusicVolume(fMusicVolume/fSlidersMax);
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

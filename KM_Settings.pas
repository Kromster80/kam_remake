unit KM_Settings;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math, KM_Defaults, INIfiles;

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
    fMultiplayerName:string;
    fMultiplayerIP:string;
    fTCPPort:string;
    fMasterServerAddress:string;
    fMasterAnnounceInterval:integer;
    fMaxRooms:integer;
    fAutoKickTimeout:integer;
    fPingInterval:integer;
    fAnnounceServer:boolean;
    function LoadSettingsFromFile(FileName:string):boolean;
    procedure SaveSettingsToFile(FileName:string);

    procedure SetAutosave(aValue:boolean);
    procedure SetBrightness(aValue:byte);
    procedure SetFastScroll(aValue:boolean);
    procedure SetFullScreen(aValue:boolean);
    procedure SetLocale(aLocale:shortstring);
    procedure SetMouseSpeed(aValue:byte);
    procedure SetMusicOn(aValue:boolean);
    procedure SetMusicVolume(aValue:byte);
    procedure SetSoundFXVolume(aValue:byte);
    procedure SetMultiplayerName(aValue:string);
    procedure SetMultiplayerIP(aValue:string);
    procedure SetMasterServerAddress(aValue:string);
    procedure SetTCPPort(aValue:string);
  public
    //Temp for fight simulator
    fHitPointRestorePace:word;
    fHitPointRestoreInFights:boolean;
    constructor Create;
    destructor Destroy; override;
    procedure SaveSettings(aForce:boolean=false);

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
    property MultiplayerName:string read fMultiplayerName write SetMultiplayerName;
    property MultiplayerIP:string read fMultiplayerIP write SetMultiplayerIP;
    property TCPPort:string read fTCPPort write SetTCPPort;
    property MasterServerAddress:string read fMasterServerAddress write SetMasterServerAddress;
    property MasterAnnounceInterval:integer read fMasterAnnounceInterval;
    property AnnounceServer:boolean read fAnnounceServer;
    property MaxRooms:integer read fMaxRooms;
    property AutoKickTimeout:integer read fAutoKickTimeout;
    property PingInterval:integer read fPingInterval;
  end;


implementation
uses KM_Log;


constructor TGlobalSettings.Create;
begin
  Inherited;
  fSlidersMin := 0;
  fSlidersMax := 20;
  LoadSettingsFromFile(ExeDir+SETTINGS_FILE);
  fNeedsSave := false;
  fLog.AppendLog('Global settings loaded from '+SETTINGS_FILE);
end;


destructor TGlobalSettings.Destroy;
begin
  SaveSettingsToFile(ExeDir+SETTINGS_FILE);
  Inherited;
end;


//Save only when needed
procedure TGlobalSettings.SaveSettings(aForce:boolean=false);
begin
  if fNeedsSave or aForce then
    SaveSettingsToFile(ExeDir+SETTINGS_FILE);
end;


function TGlobalSettings.LoadSettingsFromFile(FileName:string):boolean;
var f:TMemIniFile;
begin
  Result := FileExists(FileName);

  f := TMemIniFile.Create(FileName);
  
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

  fMultiplayerName := f.ReadString('Multiplayer','Name','NoName');
  fMultiplayerIP   := f.ReadString('Multiplayer','IP','127.0.0.1');
  fTCPPort         := f.ReadString('Multiplayer','Port','56789');
  fMasterServerAddress    := f.ReadString('Multiplayer','MasterServer','http://lewin.hodgman.id.au/kam_remake_master_server/');
  fMasterAnnounceInterval := f.ReadInteger('Multiplayer','MasterServerAnnounceInterval',60);
  fAnnounceServer         := f.ReadBool('Multiplayer','AnnounceDedicatedServer',true);
  fMaxRooms               := f.ReadInteger('Multiplayer','MaxRooms',16);
  fAutoKickTimeout        := f.ReadInteger('Multiplayer','AutoKickTimeout',20);
  fPingInterval           := f.ReadInteger('Multiplayer','PingMeasurementInterval',1000);

  FreeAndNil(f);
  fNeedsSave := false;
end;


procedure TGlobalSettings.SaveSettingsToFile(FileName:string);
var f:TMemIniFile; //Don't rewrite the file for each change, do it in one batch
begin
  f := TMemIniFile.Create(FileName);

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

  f.WriteString('Multiplayer','Name',fMultiplayerName);
  f.WriteString('Multiplayer','IP',fMultiplayerIP);
  f.WriteString('Multiplayer','Port',fTCPPort);
  f.WriteString('Multiplayer','MasterServerAddress',fMasterServerAddress);
  f.WriteInteger('Multiplayer','MasterServerAnnounceInterval',fMasterAnnounceInterval);
  f.WriteBool('Multiplayer','AnnounceDedicatedServer',fAnnounceServer);
  f.WriteInteger('Multiplayer','MaxRooms',fMaxRooms);
  f.WriteInteger('Multiplayer','AutoKickTimeout',fAutoKickTimeout);
  f.WriteInteger('Multiplayer','PingMeasurementInterval',fPingInterval);

  f.UpdateFile; //Write changes to file
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
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetMultiplayerName(aValue:string);
begin
  fMultiplayerName := aValue;
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetMultiplayerIP(aValue:string);
begin
  fMultiplayerIP := aValue;
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetMasterServerAddress(aValue:string);
begin
  fMasterServerAddress := aValue;
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetTCPPort(aValue:string);
begin
  fTCPPort := aValue;
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetMusicVolume(aValue:byte);
begin
  fMusicVolume := EnsureRange(aValue,fSlidersMin,fSlidersMax);
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetMusicOn(aValue:boolean);
begin
  fMusicOn := aValue;
  fNeedsSave := true;
end;


end.

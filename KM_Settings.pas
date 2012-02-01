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
    fScrollSpeed:byte;
    fFullScreen:boolean;
    fAlphaShadows:boolean;
    fLocale:shortstring;
    fMouseSpeed:byte;
    fMusicOn:boolean;
    fShuffleOn:boolean;
    fMusicVolume:byte;
    fResolutionID:word; //ID of currently chosen resolution, it's not a fixed value
    fResolutionWidth:word;
    fResolutionHeight:word;
    fRefreshRate:word;
    fSlidersMin:byte;
    fSlidersMax:byte;
    fSoundFXVolume:byte;
    fSpeedPace:word;
    fSpeedMedium:word;
    fSpeedFast:word;
    fSpeedVeryFast:word;
    fVSync:boolean;
    fMultiplayerName:string;
    fMultiplayerIP:string;
    fLastPort:string;
    fLastRoom:string;
    fServerPort:string;
    fMasterServerAddress:string;
    fServerName:string;
    fMasterAnnounceInterval:integer;
    fMaxRooms:integer;
    fAutoKickTimeout:integer;
    fPingInterval:integer;
    fAnnounceServer:boolean;
    fHTMLStatusFile:string;
    fServerWelcomeMessage:string;
    function LoadSettingsFromFile(FileName:string):boolean;
    procedure SaveSettingsToFile(FileName:string);

    procedure SetAutosave(aValue:boolean);
    procedure SetBrightness(aValue:byte);
    procedure SetScrollSpeed(aValue:byte);
    procedure SetFullScreen(aValue:boolean);
    procedure SetAlphaShadows(aValue:boolean);
    procedure SetLocale(aLocale:shortstring);
    procedure SetMouseSpeed(aValue:byte);
    procedure SetMusicOn(aValue:boolean);
    procedure SetShuffleOn(aValue:boolean);
    procedure SetMusicVolume(aValue:byte);
    procedure SetSoundFXVolume(aValue:byte);
    procedure SetMultiplayerName(aValue:string);
    procedure SetMultiplayerIP(aValue:string);
    procedure SetMasterServerAddress(aValue:string);
    procedure SetServerName(aValue:string);
    procedure SetLastPort(aValue:string);
    procedure SetLastRoom(aValue:string);
    procedure SetServerPort(aValue:string);
  public
    //Temp for fight simulator
    fHitPointRestorePace:word;
    constructor Create;
    destructor Destroy; override;
    procedure SaveSettings(aForce:boolean=false);
    procedure ReloadSettings;

    property Autosave:boolean read fAutosave write SetAutosave default true;
    property Brightness:byte read fBrightness write SetBrightness default 1;
    property ScrollSpeed:byte read fScrollSpeed write SetScrollSpeed default 10;
    property FullScreen:boolean read fFullScreen write SetFullScreen default true;
    property AlphaShadows:boolean read fAlphaShadows write SetAlphaShadows default true;
    property Locale:shortstring read fLocale write SetLocale;
    function GetLocalID:byte;
    property MouseSpeed:byte read fMouseSpeed write SetMouseSpeed;
    property MusicOn:boolean read fMusicOn write SetMusicOn default true;
    property ShuffleOn:boolean read fShuffleOn write SetShuffleOn default false;
    property MusicVolume:byte read fMusicVolume write SetMusicVolume;
    property ResolutionID:word read fResolutionID write fResolutionID;
    property ResolutionWidth:word read fResolutionWidth write fResolutionWidth;
    property ResolutionHeight:word read fResolutionHeight write fResolutionHeight;
    property RefreshRate:word read fRefreshRate write fRefreshRate;
    property SlidersMin:byte read fSlidersMin;
    property SlidersMax:byte read fSlidersMax;
    property SoundFXVolume:byte read fSoundFXVolume write SetSoundFXVolume;
    property SpeedPace:word read fSpeedPace;
    property SpeedMedium:word read fSpeedMedium;
    property SpeedFast:word read fSpeedFast;
    property SpeedVeryFast:word read fSpeedVeryFast;
    property VSync:boolean read fVSync;
    property MultiplayerName:string read fMultiplayerName write SetMultiplayerName;
    property MultiplayerIP:string read fMultiplayerIP write SetMultiplayerIP;
    property LastPort:string read fLastPort write SetLastPort;
    property LastRoom:string read fLastRoom write SetLastRoom;
    property ServerPort:string read fServerPort write SetServerPort;
    property MasterServerAddress:string read fMasterServerAddress write SetMasterServerAddress;
    property ServerName:string read fServerName write SetServerName;
    property MasterAnnounceInterval:integer read fMasterAnnounceInterval;
    property AnnounceServer:boolean read fAnnounceServer;
    property MaxRooms:integer read fMaxRooms;
    property AutoKickTimeout:integer read fAutoKickTimeout;
    property PingInterval:integer read fPingInterval;
    property HTMLStatusFile:string read fHTMLStatusFile;
    property ServerWelcomeMessage:string read fServerWelcomeMessage;
  end;


implementation
uses KM_Log;


{ TGlobalSettings }
constructor TGlobalSettings.Create;
begin
  Inherited;
  fSlidersMin := 0;
  fSlidersMax := 20;
  LoadSettingsFromFile(ExeDir + SETTINGS_FILE);
  fNeedsSave := false;
  fLog.AppendLog('Global settings loaded from ' + SETTINGS_FILE);
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


procedure TGlobalSettings.ReloadSettings;
begin
  LoadSettingsFromFile(ExeDir+SETTINGS_FILE);
end;


function TGlobalSettings.GetLocalID:byte;
var i:integer;
begin
  Result := 0;
  for i:=1 to LOCALES_COUNT do
    if Locales[i,1] = Locale then
      Result := i;
end;


function TGlobalSettings.LoadSettingsFromFile(FileName:string):boolean;
var f:TMemIniFile; i:integer;
begin
  Result := FileExists(FileName);

  f := TMemIniFile.Create(FileName);
  
  fBrightness       := f.ReadInteger('GFX','Brightness',1);
  fFullScreen       := f.ReadBool   ('GFX','FullScreen',false);
  fVSync            := f.ReadBool   ('GFX','VSync',true);
  fAlphaShadows     := f.ReadBool   ('GFX','AlphaShadows',true);
  fResolutionWidth  := f.ReadInteger('GFX','ResolutionWidth',1024);
  fResolutionHeight := f.ReadInteger('GFX','ResolutionHeight',768);
  fRefreshRate      := f.ReadInteger('GFX','RefreshRate',60);

  fAutosave      := f.ReadBool   ('Game','Autosave',true); //Should be ON by default
  fScrollSpeed   := f.ReadInteger('Game','ScrollSpeed',10);
  fMouseSpeed    := f.ReadInteger('Game','MouseSpeed',10);
  Locale         := f.ReadString ('Game','Locale','eng'); //Wrong name will become ENG too
  fSpeedPace     := f.ReadInteger('Game','SpeedPace',100);
  fSpeedMedium   := f.ReadInteger('Game','SpeedMedium',3);
  fSpeedFast     := f.ReadInteger('Game','SpeedFast',6);
  fSpeedVeryFast := f.ReadInteger('Game','SpeedVeryFast',10);

  fSoundFXVolume := f.ReadInteger('SFX','SFXVolume',10);
  fMusicVolume   := f.ReadInteger('SFX','MusicVolume',10);
  fMusicOn       := f.ReadBool   ('SFX','MusicEnabled',true);
  fShuffleOn     := f.ReadBool   ('SFX','ShuffleEnabled',false);

  if INI_HITPOINT_RESTORE then
    fHitPointRestorePace := f.ReadInteger('Fights','HitPointRestorePace',DEFAULT_HITPOINT_RESTORE)
  else
    fHitPointRestorePace := DEFAULT_HITPOINT_RESTORE;

  fMultiplayerName        := f.ReadString ('Multiplayer','Name','NoName');
  fMultiplayerIP          := f.ReadString ('Multiplayer','LastIP','127.0.0.1');
  fLastPort               := f.ReadString ('Multiplayer','LastPort','56789');
  fLastRoom               := f.ReadString ('Multiplayer','LastRoom','0');
  fServerPort             := f.ReadString ('Server','ServerPort','56789');
  fMasterServerAddress    := f.ReadString ('Server','MasterServerAddress','http://lewin.hodgman.id.au/kam_remake_master_server/');
  fMasterAnnounceInterval := f.ReadInteger('Server','MasterServerAnnounceInterval',120);
  fAnnounceServer         := f.ReadBool   ('Server','AnnounceDedicatedServer',true);
  fServerName             := f.ReadString ('Server','ServerName','KaM Remake Server');
  fMaxRooms               := f.ReadInteger('Server','MaxRooms',16);
  fAutoKickTimeout        := f.ReadInteger('Server','AutoKickTimeout',20);
  fPingInterval           := f.ReadInteger('Server','PingMeasurementInterval',1000);
  fHTMLStatusFile         := f.ReadString ('Server','HTMLStatusFile','KaM_Remake_Server_Status.html');
  fServerWelcomeMessage   := f.ReadString ('Server','WelcomeMessage','');

  //determining ID of saved resolution, if values are incorrect, ID is 1
  fResolutionID := 1;
  for i := 1 to RESOLUTION_COUNT do
    if (ScreenRes[i].Width = fResolutionWidth) and (ScreenRes[i].Height = fResolutionHeight) then
      fResolutionID := i;

  FreeAndNil(f);
  fNeedsSave := false;
end;


procedure TGlobalSettings.SaveSettingsToFile(FileName:string);
var f:TMemIniFile; //Don't rewrite the file for each change, do it in one batch
begin
  f := TMemIniFile.Create(FileName);

  f.WriteInteger('GFX','Brightness',      fBrightness);
  f.WriteBool   ('GFX','FullScreen',      fFullScreen);
  f.WriteBool   ('GFX','VSync',           fVSync);
  f.WriteBool   ('GFX','AlphaShadows',    fAlphaShadows);
  f.WriteInteger('GFX','ResolutionWidth', fResolutionWidth);
  f.WriteInteger('GFX','ResolutionHeight',fResolutionHeight);
  f.WriteInteger('GFX','RefreshRate',     fRefreshRate);

  f.WriteBool   ('Game','Autosave',   fAutosave);
  f.WriteInteger('Game','ScrollSpeed',fScrollSpeed);
  f.WriteInteger('Game','MouseSpeed', fMouseSpeed);
  f.WriteString ('Game','Locale',     fLocale);
  f.WriteInteger('Game','SpeedPace',  fSpeedPace);
  f.WriteInteger('Game','SpeedMedium',fSpeedMedium);
  f.WriteInteger('Game','SpeedFast',  fSpeedFast);
  f.WriteInteger('Game','SpeedVeryFast',fSpeedVeryFast);

  f.WriteInteger('SFX','SFXVolume',     fSoundFXVolume);
  f.WriteInteger('SFX','MusicVolume',   fMusicVolume);
  f.WriteBool   ('SFX','MusicEnabled',  fMusicOn);
  f.WriteBool   ('SFX','ShuffleEnabled',fShuffleOn);

  if INI_HITPOINT_RESTORE then
    f.WriteInteger('Fights','HitPointRestorePace',fHitPointRestorePace);

  f.WriteString ('Multiplayer','Name',    fMultiplayerName);
  f.WriteString ('Multiplayer','LastIP',  fMultiplayerIP);
  f.WriteString ('Multiplayer','LastPort',fLastPort);
  f.WriteString ('Multiplayer','LastRoom',fLastRoom);

  f.WriteString ('Server','ServerName',fServerName);
  f.WriteString ('Server','WelcomeMessage',fServerWelcomeMessage);
  f.WriteString ('Server','ServerPort',fServerPort);
  f.WriteBool   ('Server','AnnounceDedicatedServer',fAnnounceServer);
  f.WriteInteger('Server','MaxRooms',fMaxRooms);
  f.WriteString ('Server','HTMLStatusFile',fHTMLStatusFile);
  f.WriteInteger('Server','MasterServerAnnounceInterval',fMasterAnnounceInterval);
  f.WriteString ('Server','MasterServerAddress',fMasterServerAddress);
  f.WriteInteger('Server','AutoKickTimeout',fAutoKickTimeout);
  f.WriteInteger('Server','PingMeasurementInterval',fPingInterval);

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


procedure TGlobalSettings.SetScrollSpeed(aValue:byte);
begin
  fScrollSpeed := aValue;
  fNeedsSave  := true;
end;


procedure TGlobalSettings.SetFullScreen(aValue:boolean);
begin
  fFullScreen := aValue;
  fNeedsSave  := true;
end;


procedure TGlobalSettings.SetAlphaShadows(aValue:boolean);
begin
  fAlphaShadows := aValue;
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


procedure TGlobalSettings.SetServerName(aValue:string);
begin
  fServerName := aValue;
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetLastPort(aValue:string);
begin
  fLastPort := aValue;
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetLastRoom(aValue:string);
begin
  fLastRoom := aValue;
  fNeedsSave := true;
end;


procedure TGlobalSettings.SetServerPort(aValue:string);
begin
  fServerPort := aValue;
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


procedure TGlobalSettings.SetShuffleOn(aValue:boolean);
begin
  fShuffleOn := aValue;
  fNeedsSave := true;
end;


end.

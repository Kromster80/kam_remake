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
    fMusicOn:boolean;
    fShuffleOn:boolean;
    fMusicVolume:byte;
    fResolutionID:integer; //ID of currently chosen resolution, it's not a fixed value
    fRefreshRateID:integer;
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
    function GetLocaleID:byte;
    property MusicOn:boolean read fMusicOn write SetMusicOn default true;
    property ShuffleOn:boolean read fShuffleOn write SetShuffleOn default false;
    property MusicVolume:byte read fMusicVolume write SetMusicVolume;
    property ResolutionID:integer read fResolutionID write fResolutionID;
    property RefreshRateID:integer read fRefreshRateID write fRefreshRateID;
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
uses KM_Log, KM_Main;


{ TGlobalSettings }
constructor TGlobalSettings.Create;
begin
  Inherited;
  fSlidersMin := 0;
  fSlidersMax := 20;
  LoadSettingsFromFile(ExeDir + SETTINGS_FILE);
  //verify data loaded from file
  if not fMain.Resolutions.Check(Self) then
    fMain.Resolutions.FindCorrect(Self);
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


function TGlobalSettings.GetLocaleID: byte;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Locales) to High(Locales) do
    if Locales[I, 1] = Locale then
      Result := I;
end;


function TGlobalSettings.LoadSettingsFromFile(FileName:string):boolean;
var f:TMemIniFile;
begin
  Result := FileExists(FileName);

  f := TMemIniFile.Create(FileName);

  fBrightness       := f.ReadInteger('GFX', 'Brightness',       1);
  fFullScreen       := f.ReadBool   ('GFX', 'FullScreen',       False);
  fVSync            := f.ReadBool   ('GFX', 'VSync',            True);
  fAlphaShadows     := f.ReadBool   ('GFX', 'AlphaShadows',     True);
  fResolutionWidth  := f.ReadInteger('GFX', 'ResolutionWidth',  1024);
  fResolutionHeight := f.ReadInteger('GFX', 'ResolutionHeight', 768);
  fRefreshRate      := f.ReadInteger('GFX', 'RefreshRate',      60);

  fAutosave       := f.ReadBool   ('Game', 'Autosave',       True); //Should be ON by default
  fScrollSpeed    := f.ReadInteger('Game', 'ScrollSpeed',    10);
  Locale          := f.ReadString ('Game', 'Locale',         DEFAULT_LOCALE); //Wrong name will become ENG too
  fSpeedPace      := f.ReadInteger('Game', 'SpeedPace',      100);
  fSpeedMedium    := f.ReadInteger('Game', 'SpeedMedium',    3);
  fSpeedFast      := f.ReadInteger('Game', 'SpeedFast',      6);
  fSpeedVeryFast  := f.ReadInteger('Game', 'SpeedVeryFast',  10);

  fSoundFXVolume  := f.ReadInteger('SFX',  'SFXVolume',      10);
  fMusicVolume    := f.ReadInteger('SFX',  'MusicVolume',    10);
  fMusicOn        := f.ReadBool   ('SFX',  'MusicEnabled',   True);
  fShuffleOn      := f.ReadBool   ('SFX',  'ShuffleEnabled', False);

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

  FreeAndNil(f);
  fNeedsSave := false;
end;


//Don't rewrite the file for each individual change, do it in one batch for simplicity
procedure TGlobalSettings.SaveSettingsToFile(FileName: string);
var F: TMemIniFile;
begin
  F := TMemIniFile.Create(FileName);

  F.WriteInteger('GFX','Brightness',      fBrightness);
  F.WriteBool   ('GFX','FullScreen',      fFullScreen);
  F.WriteBool   ('GFX','VSync',           fVSync);
  F.WriteBool   ('GFX','AlphaShadows',    fAlphaShadows);
  F.WriteInteger('GFX','ResolutionWidth', fResolutionWidth);
  F.WriteInteger('GFX','ResolutionHeight',fResolutionHeight);
  F.WriteInteger('GFX','RefreshRate',     fRefreshRate);

  F.WriteBool   ('Game','Autosave',   fAutosave);
  F.WriteInteger('Game','ScrollSpeed',fScrollSpeed);
  F.WriteString ('Game','Locale',     fLocale);
  F.WriteInteger('Game','SpeedPace',  fSpeedPace);
  F.WriteInteger('Game','SpeedMedium',fSpeedMedium);
  F.WriteInteger('Game','SpeedFast',  fSpeedFast);
  F.WriteInteger('Game','SpeedVeryFast',fSpeedVeryFast);

  F.WriteInteger('SFX','SFXVolume',     fSoundFXVolume);
  F.WriteInteger('SFX','MusicVolume',   fMusicVolume);
  F.WriteBool   ('SFX','MusicEnabled',  fMusicOn);
  F.WriteBool   ('SFX','ShuffleEnabled',fShuffleOn);

  if INI_HITPOINT_RESTORE then
    F.WriteInteger('Fights','HitPointRestorePace',fHitPointRestorePace);

  F.WriteString ('Multiplayer','Name',    fMultiplayerName);
  F.WriteString ('Multiplayer','LastIP',  fMultiplayerIP);
  F.WriteString ('Multiplayer','LastPort',fLastPort);
  F.WriteString ('Multiplayer','LastRoom',fLastRoom);

  F.WriteString ('Server','ServerName',fServerName);
  F.WriteString ('Server','WelcomeMessage',fServerWelcomeMessage);
  F.WriteString ('Server','ServerPort',fServerPort);
  F.WriteBool   ('Server','AnnounceDedicatedServer',fAnnounceServer);
  F.WriteInteger('Server','MaxRooms',fMaxRooms);
  F.WriteString ('Server','HTMLStatusFile',fHTMLStatusFile);
  F.WriteInteger('Server','MasterServerAnnounceInterval',fMasterAnnounceInterval);
  F.WriteString ('Server','MasterServerAddress',fMasterServerAddress);
  F.WriteInteger('Server','AutoKickTimeout',fAutoKickTimeout);
  F.WriteInteger('Server','PingMeasurementInterval',fPingInterval);

  F.UpdateFile; //Write changes to file
  FreeAndNil(F);
  fNeedsSave := false;
end;


//Scan list of available locales and pick existing one, or ignore
procedure TGlobalSettings.SetLocale(aLocale: ShortString);
var I: Integer;
begin
  fLocale := DEFAULT_LOCALE; //Default - ENG
  for I := Low(Locales) to High(Locales) do
    if Locales[I,1] = aLocale then
      fLocale := aLocale;
end;


procedure TGlobalSettings.SetBrightness(aValue: Byte);
begin
  fBrightness := EnsureRange(aValue, 0, 20);
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

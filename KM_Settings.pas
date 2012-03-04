unit KM_Settings;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math, KM_Defaults, INIfiles;


type
  //Settings that are irrelevant to the game (game does not cares about them)
  TMainSettings = class
  private
    fNeedsSave: boolean;

    fFullScreen: Boolean;
    fResolutionID: Integer; //ID of currently chosen resolution, it's not a fixed value
    fRefreshRateID: Integer;
    fResolutionWidth: Word;
    fResolutionHeight: Word;
    fRefreshRate: Word;

    fVSync: Boolean;

    function LoadFromINI(FileName: string): Boolean;
    procedure SaveToINI(FileName:string);
    procedure SetFullScreen(aValue:boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveSettings(aForce: Boolean = False);
    procedure ReloadSettings;

    property FullScreen:boolean read fFullScreen write SetFullScreen;
    property ResolutionID:integer read fResolutionID write fResolutionID;
    property RefreshRateID:integer read fRefreshRateID write fRefreshRateID;
    property ResolutionWidth:word read fResolutionWidth write fResolutionWidth;
    property ResolutionHeight:word read fResolutionHeight write fResolutionHeight;
    property RefreshRate:word read fRefreshRate write fRefreshRate;
    property VSync:boolean read fVSync;
  end;

  //Gameplay settings, those that affect the game
  TGameSettings = class
  private
    fNeedsSave: Boolean;

    fAutosave: Boolean;
    fBrightness: Byte;
    fScrollSpeed: Byte;
    fAlphaShadows: Boolean;
    fLocale: shortstring;
    fMusicOn: Boolean;
    fShuffleOn: Boolean;
    fMusicVolume: Single;
    fSoundFXVolume: Single;
    fSpeedPace: Word;
    fSpeedMedium: Word;
    fSpeedFast: Word;
    fSpeedVeryFast: Word;
    fMultiplayerName: string;
    fMultiplayerIP: string;
    fLastPort: string;
    fLastRoom: string;
    fServerPort: string;
    fMasterServerAddress: string;
    fServerName: string;
    fMasterAnnounceInterval: integer;
    fMaxRooms: integer;
    fAutoKickTimeout: integer;
    fPingInterval: integer;
    fAnnounceServer: boolean;
    fHTMLStatusFile: string;
    fServerWelcomeMessage: string;
    function LoadFromINI(FileName: string): Boolean;
    procedure SaveToINI(FileName:string);

    procedure SetAutosave(aValue:boolean);
    procedure SetBrightness(aValue:byte);
    procedure SetScrollSpeed(aValue:byte);
    procedure SetAlphaShadows(aValue:boolean);
    procedure SetLocale(aLocale:shortstring);
    procedure SetMusicOn(aValue:boolean);
    procedure SetShuffleOn(aValue:boolean);
    procedure SetMusicVolume(aValue: Single);
    procedure SetSoundFXVolume(aValue: Single);
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
    procedure SaveSettings(aForce:boolean=False);
    procedure ReloadSettings;

    property Autosave:boolean read fAutosave write SetAutosave;
    property Brightness:byte read fBrightness write SetBrightness;
    property ScrollSpeed:byte read fScrollSpeed write SetScrollSpeed;
    property AlphaShadows:boolean read fAlphaShadows write SetAlphaShadows;
    property Locale:shortstring read fLocale write SetLocale;
    property MusicOn:boolean read fMusicOn write SetMusicOn;
    property ShuffleOn:boolean read fShuffleOn write SetShuffleOn;
    property MusicVolume: Single read fMusicVolume write SetMusicVolume;
    property SoundFXVolume: Single read fSoundFXVolume write SetSoundFXVolume;
    property SpeedPace:word read fSpeedPace;
    property SpeedMedium:word read fSpeedMedium;
    property SpeedFast:word read fSpeedFast;
    property SpeedVeryFast:word read fSpeedVeryFast;
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
uses KM_Log, KM_Main, KM_Locales;


{ TMainSettings }
constructor TMainSettings.Create;
begin
  inherited;

  LoadFromINI(ExeDir + SETTINGS_FILE);
  //verify data loaded from file
  if not fMain.Resolutions.Check(Self) then
    fMain.Resolutions.FindCorrect(Self);
  fNeedsSave := False;
  fLog.AppendLog('Global settings loaded from ' + SETTINGS_FILE);
end;

destructor TMainSettings.Destroy;
begin
  SaveToINI(ExeDir+SETTINGS_FILE);
  inherited;
end;

function TMainSettings.LoadFromINI(FileName: string): Boolean;
var f:TMemIniFile;
begin
  Result := FileExists(FileName);

  f := TMemIniFile.Create(FileName);

  fFullScreen       := f.ReadBool   ('GFX', 'FullScreen',       False);
  fVSync            := f.ReadBool   ('GFX', 'VSync',            True);
  fResolutionWidth  := f.ReadInteger('GFX', 'ResolutionWidth',  1024);
  fResolutionHeight := f.ReadInteger('GFX', 'ResolutionHeight', 768);
  fRefreshRate      := f.ReadInteger('GFX', 'RefreshRate',      60);


  FreeAndNil(f);
  fNeedsSave := False;
end;


//Don't rewrite the file for each individual change, do it in one batch for simplicity
procedure TMainSettings.SaveToINI(FileName: string);
var F: TMemIniFile;
begin
  F := TMemIniFile.Create(FileName);

  F.WriteBool   ('GFX','FullScreen',      fFullScreen);
  F.WriteBool   ('GFX','VSync',           fVSync);
  F.WriteInteger('GFX','ResolutionWidth', fResolutionWidth);
  F.WriteInteger('GFX','ResolutionHeight',fResolutionHeight);
  F.WriteInteger('GFX','RefreshRate',     fRefreshRate);

  F.UpdateFile; //Write changes to file
  FreeAndNil(F);
  fNeedsSave := False;
end;


procedure TMainSettings.SetFullScreen(aValue: boolean);
begin
  fFullScreen := aValue;
  fNeedsSave  := True;
end;


procedure TMainSettings.ReloadSettings;
begin
  LoadFromINI(ExeDir + SETTINGS_FILE);
end;


procedure TMainSettings.SaveSettings(aForce: boolean);
begin
  if fNeedsSave or aForce then
    SaveToINI(ExeDir+SETTINGS_FILE);
end;


{ TGameSettings }
constructor TGameSettings.Create;
begin
  inherited;

  ReloadSettings;
end;


destructor TGameSettings.Destroy;
begin
  SaveToINI(ExeDir+SETTINGS_FILE);
  inherited;
end;


//Save only when needed
procedure TGameSettings.SaveSettings(aForce:boolean=False);
begin
  if fNeedsSave or aForce then
    SaveToINI(ExeDir + SETTINGS_FILE);
end;


procedure TGameSettings.ReloadSettings;
begin
  LoadFromINI(ExeDir + SETTINGS_FILE);
  fLog.AppendLog('Game settings loaded from ' + SETTINGS_FILE);
end;


function TGameSettings.LoadFromINI(FileName: string): Boolean;
var f:TMemIniFile;
begin
  Result := FileExists(FileName);

  f := TMemIniFile.Create(FileName);

  fBrightness       := f.ReadInteger('GFX', 'Brightness',       1);
  fAlphaShadows     := f.ReadBool   ('GFX', 'AlphaShadows',     True);

  fAutosave       := f.ReadBool   ('Game', 'Autosave',       True); //Should be ON by default
  fScrollSpeed    := f.ReadInteger('Game', 'ScrollSpeed',    10);
  Locale          := f.ReadString ('Game', 'Locale',         DEFAULT_LOCALE); //Wrong name will become ENG too
  fSpeedPace      := f.ReadInteger('Game', 'SpeedPace',      100);
  fSpeedMedium    := f.ReadInteger('Game', 'SpeedMedium',    3);
  fSpeedFast      := f.ReadInteger('Game', 'SpeedFast',      6);
  fSpeedVeryFast  := f.ReadInteger('Game', 'SpeedVeryFast',  10);

  fSoundFXVolume  := f.ReadFloat  ('SFX',  'SFXVolume',      0.5);
  fMusicVolume    := f.ReadFloat  ('SFX',  'MusicVolume',    0.5);
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
  fAnnounceServer         := f.ReadBool   ('Server','AnnounceDedicatedServer',True);
  fServerName             := f.ReadString ('Server','ServerName','KaM Remake Server');
  fMaxRooms               := f.ReadInteger('Server','MaxRooms',16);
  fAutoKickTimeout        := f.ReadInteger('Server','AutoKickTimeout',20);
  fPingInterval           := f.ReadInteger('Server','PingMeasurementInterval',1000);
  fHTMLStatusFile         := f.ReadString ('Server','HTMLStatusFile','KaM_Remake_Server_Status.html');
  fServerWelcomeMessage   := f.ReadString ('Server','WelcomeMessage','');

  FreeAndNil(f);
  fNeedsSave := False;
end;


//Don't rewrite the file for each individual change, do it in one batch for simplicity
procedure TGameSettings.SaveToINI(FileName: string);
var F: TMemIniFile;
begin
  F := TMemIniFile.Create(FileName);

  F.WriteInteger('GFX','Brightness',      fBrightness);
  F.WriteBool   ('GFX','AlphaShadows',    fAlphaShadows);

  F.WriteBool   ('Game','Autosave',   fAutosave);
  F.WriteInteger('Game','ScrollSpeed',fScrollSpeed);
  F.WriteString ('Game','Locale',     fLocale);
  F.WriteInteger('Game','SpeedPace',  fSpeedPace);
  F.WriteInteger('Game','SpeedMedium',fSpeedMedium);
  F.WriteInteger('Game','SpeedFast',  fSpeedFast);
  F.WriteInteger('Game','SpeedVeryFast',fSpeedVeryFast);

  F.WriteFloat  ('SFX','SFXVolume',     fSoundFXVolume);
  F.WriteFloat  ('SFX','MusicVolume',   fMusicVolume);
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
  fNeedsSave := False;
end;


//Scan list of available locales and pick existing one, or ignore
procedure TGameSettings.SetLocale(aLocale: ShortString);
begin
  if fLocales.GetIDFromCode(aLocale) <> -1 then
    fLocale := aLocale
  else
    fLocale := DEFAULT_LOCALE; //Default - ENG
end;


procedure TGameSettings.SetBrightness(aValue: Byte);
begin
  fBrightness := EnsureRange(aValue, 0, 20);
  fNeedsSave  := True;
end;


procedure TGameSettings.SetAutosave(aValue:boolean);
begin
  fAutosave  := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetScrollSpeed(aValue:byte);
begin
  fScrollSpeed := aValue;
  fNeedsSave  := True;
end;


procedure TGameSettings.SetAlphaShadows(aValue:boolean);
begin
  fAlphaShadows := aValue;
  fNeedsSave  := True;
end;


procedure TGameSettings.SetSoundFXVolume(aValue: Single);
begin
  fSoundFXVolume := EnsureRange(aValue, 0, 1);
  fNeedsSave := True;
end;


procedure TGameSettings.SetMultiplayerName(aValue:string);
begin
  fMultiplayerName := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetMultiplayerIP(aValue:string);
begin
  fMultiplayerIP := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetMasterServerAddress(aValue:string);
begin
  fMasterServerAddress := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetServerName(aValue:string);
begin
  fServerName := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetLastPort(aValue:string);
begin
  fLastPort := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetLastRoom(aValue:string);
begin
  fLastRoom := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetServerPort(aValue:string);
begin
  fServerPort := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetMusicVolume(aValue: Single);
begin
  fMusicVolume := EnsureRange(aValue, 0, 1);
  fNeedsSave := True;
end;


procedure TGameSettings.SetMusicOn(aValue:boolean);
begin
  fMusicOn := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetShuffleOn(aValue:boolean);
begin
  fShuffleOn := aValue;
  fNeedsSave := True;
end;


end.

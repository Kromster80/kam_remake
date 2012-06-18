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
    fResolution: TScreenRes;

    fVSync: Boolean;

    function LoadFromINI(FileName: string): Boolean;
    procedure SaveToINI(FileName: string);
    procedure SetFullScreen(aValue: Boolean);
    procedure SetResolution(const Value: TScreenRes);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveSettings(aForce: Boolean = False);
    procedure ReloadSettings;

    property FullScreen: Boolean read fFullScreen write SetFullScreen;
    property Resolution: TScreenRes read fResolution write SetResolution;
    property VSync: Boolean read fVSync;
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
    fMusicOff: Boolean;
    fShuffleOn: Boolean;
    fMusicVolume: Single;
    fSoundFXVolume: Single;
    fSpeedPace: Word;
    fSpeedMedium: Word;
    fSpeedFast: Word;
    fSpeedVeryFast: Word;
    fMultiplayerName: string;
    fLastIP: string;
    fLastPort: string;
    fLastRoom: string;
    fServerPort: string;
    fMasterServerAddress: string;
    fServerName: string;
    fMasterAnnounceInterval: Integer;
    fMaxRooms: Integer;
    fAutoKickTimeout: Integer;
    fPingInterval: Integer;
    fAnnounceServer: Boolean;
    fHTMLStatusFile: string;
    fServerWelcomeMessage: string;
    function LoadFromINI(FileName: string): Boolean;
    procedure SaveToINI(FileName: string);

    procedure SetAutosave(aValue: Boolean);
    procedure SetBrightness(aValue: Byte);
    procedure SetScrollSpeed(aValue: Byte);
    procedure SetAlphaShadows(aValue: Boolean);
    procedure SetLocale(aLocale: shortstring);
    procedure SetMusicOff(aValue: Boolean);
    procedure SetShuffleOn(aValue: Boolean);
    procedure SetMusicVolume(aValue: Single);
    procedure SetSoundFXVolume(aValue: Single);
    procedure SetMultiplayerName(aValue: string);
    procedure SetLastIP(aValue: string);
    procedure SetMasterServerAddress(aValue: string);
    procedure SetServerName(aValue: string);
    procedure SetLastPort(aValue: string);
    procedure SetLastRoom(aValue: string);
    procedure SetServerPort(aValue: string);
    procedure SetServerWelcomeMessage(aValue: string);
    procedure SetAnnounceServer(aValue: Boolean);
    procedure SetAutoKickTimeout(aValue: Integer);
    procedure SetPingInterval(aValue: Integer);
    procedure SetMasterAnnounceInterval(eValue: Integer);
    procedure SetHTMLStatusFile(eValue: string);
    procedure SetMaxRooms(eValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveSettings(aForce: Boolean=False);
    procedure ReloadSettings;

    property Autosave: Boolean read fAutosave write SetAutosave;
    property Brightness: Byte read fBrightness write SetBrightness;
    property ScrollSpeed: Byte read fScrollSpeed write SetScrollSpeed;
    property AlphaShadows: Boolean read fAlphaShadows write SetAlphaShadows;
    property Locale: shortstring read fLocale write SetLocale;
    property MusicOff: Boolean read fMusicOff write SetMusicOff;
    property ShuffleOn: Boolean read fShuffleOn write SetShuffleOn;
    property MusicVolume: Single read fMusicVolume write SetMusicVolume;
    property SoundFXVolume: Single read fSoundFXVolume write SetSoundFXVolume;
    property SpeedPace: Word read fSpeedPace;
    property SpeedMedium: Word read fSpeedMedium;
    property SpeedFast: Word read fSpeedFast;
    property SpeedVeryFast: Word read fSpeedVeryFast;
    property MultiplayerName: string read fMultiplayerName write SetMultiplayerName;
    property LastIP: string read fLastIP write SetLastIP;
    property LastPort: string read fLastPort write SetLastPort;
    property LastRoom: string read fLastRoom write SetLastRoom;
    property ServerPort: string read fServerPort write SetServerPort;
    property MasterServerAddress: string read fMasterServerAddress write SetMasterServerAddress;
    property ServerName: string read fServerName write SetServerName;
    property MasterAnnounceInterval: Integer read fMasterAnnounceInterval write SetMasterAnnounceInterval;
    property AnnounceServer: Boolean read fAnnounceServer write SetAnnounceServer;
    property MaxRooms: Integer read fMaxRooms write SetMaxRooms;
    property AutoKickTimeout: Integer read fAutoKickTimeout write SetAutoKickTimeout;
    property PingInterval: Integer read fPingInterval write SetPingInterval;
    property HTMLStatusFile: string read fHTMLStatusFile write SetHTMLStatusFile;
    property ServerWelcomeMessage: string read fServerWelcomeMessage write SetServerWelcomeMessage;
  end;


implementation
uses KM_Log, KM_Locales;


{ TMainSettings }
constructor TMainSettings.Create;
begin
  inherited;

  LoadFromINI(ExeDir + SETTINGS_FILE);
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

  fFullScreen         := f.ReadBool   ('GFX', 'FullScreen',       False);
  fVSync              := f.ReadBool   ('GFX', 'VSync',            True);
  fResolution.Width   := f.ReadInteger('GFX', 'ResolutionWidth',  1024);
  fResolution.Height  := f.ReadInteger('GFX', 'ResolutionHeight', 768);
  fResolution.RefRate := f.ReadInteger('GFX', 'RefreshRate',      60);

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
  F.WriteInteger('GFX','ResolutionWidth', fResolution.Width);
  F.WriteInteger('GFX','ResolutionHeight',fResolution.Height);
  F.WriteInteger('GFX','RefreshRate',     fResolution.RefRate);

  F.UpdateFile; //Write changes to file
  FreeAndNil(F);
  fNeedsSave := False;
end;

procedure TGameSettings.SetMaxRooms(eValue: Integer);
begin
  fMaxRooms   := eValue;
  fNeedsSave  := True;
end;

procedure TGameSettings.SetHTMLStatusFile(eValue: string);
begin
  fHTMLStatusFile   := eValue;
  fNeedsSave        := True;
end;

procedure TGameSettings.SetMasterAnnounceInterval(eValue: Integer);
begin
  fMasterAnnounceInterval := eValue;
  fNeedsSave              := True;
end;

procedure TGameSettings.SetPingInterval(aValue: Integer);
begin
  fPingInterval    := aValue;
  fNeedsSave       := True;
end;

procedure TGameSettings.SetAutoKickTimeout(aValue: Integer);
begin
  fAutoKickTimeout := aValue;
  fNeedsSave       := True;
end;

procedure TGameSettings.SetAnnounceServer(aValue: Boolean);
begin
  fAnnounceServer := aValue;
  fNeedsSave      := True;
end;

procedure TGameSettings.SetServerWelcomeMessage(aValue: string);
begin
  fServerWelcomeMessage := aValue;
  fNeedsSave            := True;
end;

procedure TMainSettings.SetFullScreen(aValue: boolean);
begin
  fFullScreen := aValue;
  fNeedsSave  := True;
end;


procedure TMainSettings.SetResolution(const Value: TScreenRes);
begin
  fResolution := Value;
  fNeedsSave  := True;
end;


procedure TMainSettings.ReloadSettings;
begin
  LoadFromINI(ExeDir + SETTINGS_FILE);
end;


procedure TMainSettings.SaveSettings(aForce: boolean);
begin
  if fNeedsSave or aForce then
    SaveToINI(ExeDir + SETTINGS_FILE);
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
procedure TGameSettings.SaveSettings(aForce: Boolean=False);
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
var f: TMemIniFile;
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
  fMusicOff       := f.ReadBool   ('SFX',  'MusicDisabled',  False);
  fShuffleOn      := f.ReadBool   ('SFX',  'ShuffleEnabled', False);

  if INI_HITPOINT_RESTORE then
    HITPOINT_RESTORE_PACE := f.ReadInteger('Fights', 'HitPointRestorePace', DEFAULT_HITPOINT_RESTORE)
  else
    HITPOINT_RESTORE_PACE := DEFAULT_HITPOINT_RESTORE;

  fMultiplayerName        := f.ReadString ('Multiplayer','Name','NoName');
  fLastIP                 := f.ReadString ('Multiplayer','LastIP','127.0.0.1');
  fLastPort               := f.ReadString ('Multiplayer','LastPort','56789');
  fLastRoom               := f.ReadString ('Multiplayer','LastRoom','0');
  fServerPort             := f.ReadString ('Server','ServerPort','56789');
  //We call it MasterServerAddressNew to force it to update in everyone's .ini file when we changed address.
  //If the key stayed the same then everyone would still be using the old value from their settings.
  fMasterServerAddress    := f.ReadString ('Server','MasterServerAddressNew','http://kam.hodgman.id.au/');
  fMasterAnnounceInterval := f.ReadInteger('Server','MasterServerAnnounceInterval',180);
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

  F.WriteBool   ('Game','Autosave',     fAutosave);
  F.WriteInteger('Game','ScrollSpeed',  fScrollSpeed);
  F.WriteString ('Game','Locale',       fLocale);
  F.WriteInteger('Game','SpeedPace',    fSpeedPace);
  F.WriteInteger('Game','SpeedMedium',  fSpeedMedium);
  F.WriteInteger('Game','SpeedFast',    fSpeedFast);
  F.WriteInteger('Game','SpeedVeryFast',fSpeedVeryFast);

  F.WriteFloat  ('SFX','SFXVolume',     fSoundFXVolume);
  F.WriteFloat  ('SFX','MusicVolume',   fMusicVolume);
  F.WriteBool   ('SFX','MusicDisabled', fMusicOff);
  F.WriteBool   ('SFX','ShuffleEnabled',fShuffleOn);

  if INI_HITPOINT_RESTORE then
    F.WriteInteger('Fights','HitPointRestorePace', HITPOINT_RESTORE_PACE);

  F.WriteString ('Multiplayer','Name',    fMultiplayerName);
  F.WriteString ('Multiplayer','LastIP',  fLastIP);
  F.WriteString ('Multiplayer','LastPort',fLastPort);
  F.WriteString ('Multiplayer','LastRoom',fLastRoom);

  F.WriteString ('Server','ServerName',fServerName);
  F.WriteString ('Server','WelcomeMessage',fServerWelcomeMessage);
  F.WriteString ('Server','ServerPort',fServerPort);
  F.WriteBool   ('Server','AnnounceDedicatedServer',fAnnounceServer);
  F.WriteInteger('Server','MaxRooms',fMaxRooms);
  F.WriteString ('Server','HTMLStatusFile',fHTMLStatusFile);
  F.WriteInteger('Server','MasterServerAnnounceInterval',fMasterAnnounceInterval);
  F.WriteString ('Server','MasterServerAddressNew',fMasterServerAddress);
  F.WriteInteger('Server','AutoKickTimeout',fAutoKickTimeout);
  F.WriteInteger('Server','PingMeasurementInterval',fPingInterval);

  F.UpdateFile; //Write changes to file
  FreeAndNil(F);
  fNeedsSave := False;
end;


//Scan list of available locales and pick existing one, or ignore
procedure TGameSettings.SetLocale(aLocale: ShortString);
begin
  //We don't know if Locales are initialized (e.g. in dedicated server)
  if (fLocales <> nil) and (fLocales.GetIDFromCode(aLocale) <> -1) then
    fLocale := aLocale
  else
    fLocale := DEFAULT_LOCALE; //Default - ENG
end;


procedure TGameSettings.SetBrightness(aValue: Byte);
begin
  fBrightness := EnsureRange(aValue, 0, 20);
  fNeedsSave  := True;
end;


procedure TGameSettings.SetAutosave(aValue: Boolean);
begin
  fAutosave  := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetScrollSpeed(aValue: Byte);
begin
  fScrollSpeed := aValue;
  fNeedsSave  := True;
end;


procedure TGameSettings.SetAlphaShadows(aValue: Boolean);
begin
  fAlphaShadows := aValue;
  fNeedsSave  := True;
end;


procedure TGameSettings.SetSoundFXVolume(aValue: Single);
begin
  fSoundFXVolume := EnsureRange(aValue, 0, 1);
  fNeedsSave := True;
end;


procedure TGameSettings.SetMultiplayerName(aValue: string);
begin
  fMultiplayerName := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetLastIP(aValue: string);
begin
  fLastIP := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetMasterServerAddress(aValue: string);
begin
  fMasterServerAddress := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetServerName(aValue: string);
begin
  fServerName := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetLastPort(aValue: string);
begin
  fLastPort := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetLastRoom(aValue: string);
begin
  fLastRoom := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetServerPort(aValue: string);
begin
  fServerPort := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetMusicVolume(aValue: Single);
begin
  fMusicVolume := EnsureRange(aValue, 0, 1);
  fNeedsSave := True;
end;


procedure TGameSettings.SetMusicOff(aValue: Boolean);
begin
  fMusicOff := aValue;
  fNeedsSave := True;
end;


procedure TGameSettings.SetShuffleOn(aValue: Boolean);
begin
  fShuffleOn := aValue;
  fNeedsSave := True;
end;


end.

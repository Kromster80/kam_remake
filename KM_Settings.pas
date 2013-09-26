unit KM_Settings;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math, KM_Defaults, INIfiles, KM_ResLocales;


type
  //Settings that are irrelevant to the game (game does not cares about them)
  //Everything gets written through setter to set fNeedsSave flag
  TMainSettings = class
  private
    fNeedsSave: Boolean;

    fFullScreen: Boolean;
    fResolution: TScreenRes;
    fVSync: Boolean;
    procedure SetFullScreen(aValue: Boolean);
    procedure SetResolution(const Value: TScreenRes);
    procedure SetVSync(aValue: Boolean);
  protected
    procedure Changed;
    function LoadFromINI(FileName: string): Boolean;
    procedure SaveToINI(FileName: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveSettings(aForce: Boolean = False);
    procedure ReloadSettings;

    property FullScreen: Boolean read fFullScreen write SetFullScreen;
    property Resolution: TScreenRes read fResolution write SetResolution;
    property VSync: Boolean read fVSync write SetVSync;
  end;

  //Gameplay settings, those that affect the game
  //Everything gets written through setter to set fNeedsSave flag
  TGameSettings = class
  private
    fNeedsSave: Boolean;

    fAutosave: Boolean;
    fBrightness: Byte;
    fScrollSpeed: Byte;
    fAlphaShadows: Boolean;
    fLocale: AnsiString;
    fMusicOff: Boolean;
    fShuffleOn: Boolean;
    fMusicVolume: Single;
    fSoundFXVolume: Single;
    fSpeedPace: Word;
    fSpeedMedium: Word;
    fSpeedFast: Word;
    fSpeedVeryFast: Word;
    fMultiplayerName: AnsiString;
    fLastIP: string;
    fLastPort: string;
    fLastRoom: string;
    fServerPort: string;
    fMasterServerAddress: string;
    fServerName: UnicodeString;
    fMasterAnnounceInterval: Integer;
    fMaxRooms: Integer;
    fAutoKickTimeout: Integer;
    fPingInterval: Integer;
    fAnnounceServer: Boolean;
    fHTMLStatusFile: UnicodeString;
    fServerWelcomeMessage: UnicodeString;
    procedure SetAutosave(aValue: Boolean);
    procedure SetBrightness(aValue: Byte);
    procedure SetScrollSpeed(aValue: Byte);
    procedure SetAlphaShadows(aValue: Boolean);
    procedure SetLocale(aLocale: AnsiString);
    procedure SetMusicOff(aValue: Boolean);
    procedure SetShuffleOn(aValue: Boolean);
    procedure SetMusicVolume(aValue: Single);
    procedure SetSoundFXVolume(aValue: Single);
    procedure SetMultiplayerName(aValue: AnsiString);
    procedure SetLastIP(aValue: string);
    procedure SetMasterServerAddress(aValue: string);
    procedure SetServerName(aValue: UnicodeString);
    procedure SetLastPort(aValue: string);
    procedure SetLastRoom(aValue: string);
    procedure SetServerPort(aValue: string);
    procedure SetServerWelcomeMessage(aValue: UnicodeString);
    procedure SetAnnounceServer(aValue: Boolean);
    procedure SetAutoKickTimeout(aValue: Integer);
    procedure SetPingInterval(aValue: Integer);
    procedure SetMasterAnnounceInterval(eValue: Integer);
    procedure SetHTMLStatusFile(eValue: UnicodeString);
    procedure SetMaxRooms(eValue: Integer);
  protected
    function LoadFromINI(FileName: UnicodeString): Boolean;
    procedure SaveToINI(FileName: UnicodeString);
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveSettings(aForce: Boolean=False);
    procedure ReloadSettings;

    property Autosave: Boolean read fAutosave write SetAutosave;
    property Brightness: Byte read fBrightness write SetBrightness;
    property ScrollSpeed: Byte read fScrollSpeed write SetScrollSpeed;
    property AlphaShadows: Boolean read fAlphaShadows write SetAlphaShadows;
    property Locale: AnsiString read fLocale write SetLocale;
    property MusicOff: Boolean read fMusicOff write SetMusicOff;
    property ShuffleOn: Boolean read fShuffleOn write SetShuffleOn;
    property MusicVolume: Single read fMusicVolume write SetMusicVolume;
    property SoundFXVolume: Single read fSoundFXVolume write SetSoundFXVolume;
    property SpeedPace: Word read fSpeedPace;
    property SpeedMedium: Word read fSpeedMedium;
    property SpeedFast: Word read fSpeedFast;
    property SpeedVeryFast: Word read fSpeedVeryFast;
    property MultiplayerName: AnsiString read fMultiplayerName write SetMultiplayerName;
    property LastIP: string read fLastIP write SetLastIP;
    property LastPort: string read fLastPort write SetLastPort;
    property LastRoom: string read fLastRoom write SetLastRoom;
    property ServerPort: string read fServerPort write SetServerPort;
    property MasterServerAddress: string read fMasterServerAddress write SetMasterServerAddress;
    property ServerName: UnicodeString read fServerName write SetServerName;
    property MasterAnnounceInterval: Integer read fMasterAnnounceInterval write SetMasterAnnounceInterval;
    property AnnounceServer: Boolean read fAnnounceServer write SetAnnounceServer;
    property MaxRooms: Integer read fMaxRooms write SetMaxRooms;
    property AutoKickTimeout: Integer read fAutoKickTimeout write SetAutoKickTimeout;
    property PingInterval: Integer read fPingInterval write SetPingInterval;
    property HTMLStatusFile: UnicodeString read fHTMLStatusFile write SetHTMLStatusFile;
    property ServerWelcomeMessage: UnicodeString read fServerWelcomeMessage write SetServerWelcomeMessage;
  end;


implementation
uses KM_Log;


{ TMainSettings }
constructor TMainSettings.Create;
begin
  inherited;

  LoadFromINI(ExeDir + SETTINGS_FILE);
  fNeedsSave := False;
  gLog.AddTime('Global settings loaded from ' + SETTINGS_FILE);
end;


destructor TMainSettings.Destroy;
begin
  SaveToINI(ExeDir+SETTINGS_FILE);
  inherited;
end;


procedure TMainSettings.Changed;
begin
  fNeedsSave := True;
end;


function TMainSettings.LoadFromINI(FileName: string): Boolean;
var
  F: TMemIniFile;
begin
  Result := FileExists(FileName);

  F := TMemIniFile.Create(FileName);

  fFullScreen         := F.ReadBool   ('GFX', 'FullScreen',       False);
  fVSync              := F.ReadBool   ('GFX', 'VSync',            True);
  fResolution.Width   := F.ReadInteger('GFX', 'ResolutionWidth',  1024);
  fResolution.Height  := F.ReadInteger('GFX', 'ResolutionHeight', 768);
  fResolution.RefRate := F.ReadInteger('GFX', 'RefreshRate',      60);

  FreeAndNil(F);
  fNeedsSave := False;
end;


//Don't rewrite the file for each individual change, do it in one batch for simplicity
procedure TMainSettings.SaveToINI(FileName: string);
var
  F: TMemIniFile;
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

procedure TMainSettings.SetFullScreen(aValue: boolean);
begin
  fFullScreen := aValue;
  Changed;
end;


procedure TMainSettings.SetResolution(const Value: TScreenRes);
begin
  fResolution := Value;
  Changed;
end;


procedure TMainSettings.SetVSync(aValue: boolean);
begin
  fVSync := aValue;
  Changed;
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
procedure TGameSettings.Changed;
begin
  fNeedsSave := True;
end;


constructor TGameSettings.Create;
begin
  inherited;

  ReloadSettings;
end;


destructor TGameSettings.Destroy;
begin
  SaveToINI(ExeDir + SETTINGS_FILE);
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
  gLog.AddTime('Game settings loaded from ' + SETTINGS_FILE);
end;


function TGameSettings.LoadFromINI(FileName: UnicodeString): Boolean;
var
  F: TMemIniFile;
begin
  Result := FileExists(FileName);

  F := TMemIniFile.Create(FileName);
  try
    fBrightness       := F.ReadInteger('GFX', 'Brightness',       1);
    fAlphaShadows     := F.ReadBool   ('GFX', 'AlphaShadows',     True);

    fAutosave       := F.ReadBool   ('Game', 'Autosave',       True); //Should be ON by default
    fScrollSpeed    := F.ReadInteger('Game', 'ScrollSpeed',    10);
    fLocale         := AnsiString(F.ReadString ('Game', 'Locale', UnicodeString(DEFAULT_LOCALE)));
    fSpeedPace      := F.ReadInteger('Game', 'SpeedPace',      100);
    fSpeedMedium    := F.ReadInteger('Game', 'SpeedMedium',    3);
    fSpeedFast      := F.ReadInteger('Game', 'SpeedFast',      6);
    fSpeedVeryFast  := F.ReadInteger('Game', 'SpeedVeryFast',  10);

    fSoundFXVolume  := F.ReadFloat  ('SFX',  'SFXVolume',      0.5);
    fMusicVolume    := F.ReadFloat  ('SFX',  'MusicVolume',    0.5);
    fMusicOff       := F.ReadBool   ('SFX',  'MusicDisabled',  False);
    fShuffleOn      := F.ReadBool   ('SFX',  'ShuffleEnabled', False);

    if INI_HITPOINT_RESTORE then
      HITPOINT_RESTORE_PACE := F.ReadInteger('Fights', 'HitPointRestorePace', DEFAULT_HITPOINT_RESTORE)
    else
      HITPOINT_RESTORE_PACE := DEFAULT_HITPOINT_RESTORE;

    fMultiplayerName        := AnsiString(F.ReadString ('Multiplayer','Name','NoName'));
    fLastIP                 := F.ReadString ('Multiplayer','LastIP','127.0.0.1');
    fLastPort               := F.ReadString ('Multiplayer','LastPort','56789');
    fLastRoom               := F.ReadString ('Multiplayer','LastRoom','0');
    fServerPort             := F.ReadString ('Server','ServerPort','56789');
    //We call it MasterServerAddressNew to force it to update in everyone's .ini file when we changed address.
    //If the key stayed the same then everyone would still be using the old value from their settings.
    fMasterServerAddress    := F.ReadString ('Server','MasterServerAddressNew','http://kam.hodgman.id.au/');
    fMasterAnnounceInterval := F.ReadInteger('Server','MasterServerAnnounceInterval',180);
    fAnnounceServer         := F.ReadBool   ('Server','AnnounceDedicatedServer',True);
    fServerName             := F.ReadString ('Server','ServerName','KaM Remake Server');
    fMaxRooms               := F.ReadInteger('Server','MaxRooms',16);
    fAutoKickTimeout        := F.ReadInteger('Server','AutoKickTimeout',20);
    fPingInterval           := F.ReadInteger('Server','PingMeasurementInterval',1000);
    fHTMLStatusFile         := F.ReadString ('Server','HTMLStatusFile','KaM_Remake_Server_Status.html');
    fServerWelcomeMessage   := F.ReadString ('Server','WelcomeMessage','');
  finally
    F.Free;
  end;

  fNeedsSave := False;
end;


//Don't rewrite the file for each individual change, do it in one batch for simplicity
procedure TGameSettings.SaveToINI(FileName: UnicodeString);
var
  F: TMemIniFile;
begin
  F := TMemIniFile.Create(FileName);
  try
    F.WriteInteger('GFX','Brightness',      fBrightness);
    F.WriteBool   ('GFX','AlphaShadows',    fAlphaShadows);

    F.WriteBool   ('Game','Autosave',     fAutosave);
    F.WriteInteger('Game','ScrollSpeed',  fScrollSpeed);
    F.WriteString ('Game','Locale',       UnicodeString(fLocale));
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

    F.WriteString ('Multiplayer','Name',    UnicodeString(fMultiplayerName));
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
  finally
    F.Free;
  end;

  fNeedsSave := False;
end;


procedure TGameSettings.SetLocale(aLocale: AnsiString);
begin
  //We can get some unsupported LocaleCode, but that is fine, it will have Eng fallback anyway
  fLocale := aLocale;
  Changed;
end;


procedure TGameSettings.SetBrightness(aValue: Byte);
begin
  fBrightness := EnsureRange(aValue, 0, 20);
  Changed;
end;


procedure TGameSettings.SetAutosave(aValue: Boolean);
begin
  fAutosave := aValue;
  Changed;
end;


procedure TGameSettings.SetScrollSpeed(aValue: Byte);
begin
  fScrollSpeed := aValue;
  Changed;
end;


procedure TGameSettings.SetAlphaShadows(aValue: Boolean);
begin
  fAlphaShadows := aValue;
  Changed;
end;


procedure TGameSettings.SetSoundFXVolume(aValue: Single);
begin
  fSoundFXVolume := EnsureRange(aValue, 0, 1);
  Changed;
end;


procedure TGameSettings.SetMultiplayerName(aValue: AnsiString);
begin
  fMultiplayerName := aValue;
  Changed;
end;


procedure TGameSettings.SetLastIP(aValue: string);
begin
  fLastIP := aValue;
  Changed;
end;


procedure TGameSettings.SetMasterServerAddress(aValue: string);
begin
  fMasterServerAddress := aValue;
  Changed;
end;


procedure TGameSettings.SetServerName(aValue: UnicodeString);
begin
  fServerName := aValue;
  Changed;
end;


procedure TGameSettings.SetLastPort(aValue: string);
begin
  fLastPort := aValue;
  Changed;
end;


procedure TGameSettings.SetLastRoom(aValue: string);
begin
  fLastRoom := aValue;
  Changed;
end;


procedure TGameSettings.SetServerPort(aValue: string);
begin
  fServerPort := aValue;
  Changed;
end;


procedure TGameSettings.SetMusicVolume(aValue: Single);
begin
  fMusicVolume := EnsureRange(aValue, 0, 1);
  Changed;
end;


procedure TGameSettings.SetMusicOff(aValue: Boolean);
begin
  fMusicOff := aValue;
  Changed;
end;


procedure TGameSettings.SetShuffleOn(aValue: Boolean);
begin
  fShuffleOn := aValue;
  Changed;
end;


procedure TGameSettings.SetMaxRooms(eValue: Integer);
begin
  fMaxRooms := eValue;
  Changed;
end;


procedure TGameSettings.SetHTMLStatusFile(eValue: UnicodeString);
begin
  fHTMLStatusFile := eValue;
  Changed;
end;


procedure TGameSettings.SetMasterAnnounceInterval(eValue: Integer);
begin
  fMasterAnnounceInterval := eValue;
  Changed;
end;


procedure TGameSettings.SetPingInterval(aValue: Integer);
begin
  fPingInterval := aValue;
  Changed;
end;


procedure TGameSettings.SetAutoKickTimeout(aValue: Integer);
begin
  fAutoKickTimeout := aValue;
  Changed;
end;


procedure TGameSettings.SetAnnounceServer(aValue: Boolean);
begin
  fAnnounceServer := aValue;
  Changed;
end;


procedure TGameSettings.SetServerWelcomeMessage(aValue: UnicodeString);
begin
  fServerWelcomeMessage := aValue;
  Changed;
end;


end.

unit KM_Settings;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  {$IFDEF FPC}Forms,{$ENDIF}   //Lazarus do not know UITypes
  {$IFDEF WDC}UITypes,{$ENDIF} //We use settings in console modules
  KM_Resolutions, KM_WareDistribution,
  KM_Defaults, KM_Points, KM_CommonTypes;


type

  TKMWindowParamsRecord = record
    Width, Height, Left, Top: SmallInt;
    State: TWindowState;
  end;

  TKMWindowParams = class
  private
    fWidth, fHeight, fLeft, fTop: SmallInt; // Window size/position on the screen
    fState: TWindowState;                   // Window state (wsNormal/wsMaximized)
    fLockParams: Boolean;                   // Lock updating window params, used when Fullscreen turned On
    fIsChanged: Boolean;
    fNeedResetToDefaults: Boolean;          // Flag, when set params should be updated with defaults
  public
    constructor Create;
    property Width: SmallInt read fWidth;
    property Height: SmallInt read fHeight;
    property Left: SmallInt read fLeft;
    property Top: SmallInt read fTop;
    property State: TWindowState read fState;
    property IsChanged: Boolean read fIsChanged;
    property NeedResetToDefaults: Boolean read fNeedResetToDefaults write fNeedResetToDefaults;

    procedure ApplyWindowParams(aParams: TKMWindowParamsRecord; aDefaults: Boolean = False);
    procedure LockParams;
    procedure UnlockParams;
    function IsValid(aMonitorsInfo: TKMPointArray): Boolean;
  end;


  TKMFavouriteMaps = class
  private
    fFavouriteMPMaps: TStringList;
    fOnMapsUpdate: TUnicodeStringEvent;

    procedure FavoriteMapsUpdated;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromString(const aString: UnicodeString);
    function PackToString: UnicodeString;

    property OnMapsUpdate: TUnicodeStringEvent read fOnMapsUpdate write fOnMapsUpdate;

    procedure RemoveMissing(aMapsCRCArray: TKMCardinalArray);
    function Contains(aMapCRC: Cardinal): Boolean;
    procedure Add(aMapCRC: Cardinal);
    procedure Remove(aMapCRC: Cardinal);
    procedure Replace(aOldCRC, aNewCRC: Cardinal);
  end;


  //Settings that are irrelevant to the game (game does not cares about them)
  //Everything gets written through setter to set fNeedsSave flag
  TMainSettings = class
  private
    fNeedsSave: Boolean;
    fFullScreen: Boolean;
    fFPSCap: Integer;
    fResolution: TKMScreenRes;
    fWindowParams: TKMWindowParams;
    fVSync: Boolean;
    procedure SetFullScreen(aValue: Boolean);
    procedure SetResolution(const Value: TKMScreenRes);
    procedure SetVSync(aValue: Boolean);
  protected
    procedure Changed;
    function LoadFromINI(const aFileName: UnicodeString): Boolean;
    procedure SaveToINI(const aFileName: UnicodeString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveSettings(aForce: Boolean = False);
    procedure ReloadSettings;

    property FPSCap: Integer read fFPSCap;
    property FullScreen: Boolean read fFullScreen write SetFullScreen;
    property Resolution: TKMScreenRes read fResolution write SetResolution;
    property WindowParams: TKMWindowParams read fWindowParams;
    property VSync: Boolean read fVSync write SetVSync;
  end;

  //Gameplay settings, those that affect the game
  //Everything gets written through setter to set fNeedsSave flag
  TGameSettings = class
  private
    fNeedsSave: Boolean;

    fAutosave: Boolean;
    fAutosaveFrequency: Integer;
    fAutosaveCount: Integer;
    fReplayAutopause: Boolean;
    fReplayShowBeacons: Boolean; //Replay variable - show beacons during replay
    fSpecShowBeacons: Boolean;   //Spectator variable - show beacons while spectating
    fBrightness: Byte;
    fScrollSpeed: Byte;
    fAlphaShadows: Boolean;
    fLoadFullFonts: Boolean;
    fLocale: AnsiString;
    fMusicOff: Boolean;
    fShuffleOn: Boolean;
    fMusicVolume: Single;
    fSoundFXVolume: Single;
    fSpeedPace: Word;
    fSpeedMedium: Single;
    fSpeedFast: Single;
    fSpeedVeryFast: Single;
    fMultiplayerName: AnsiString;
    fLastIP: string;
    fLastPort: string;
    fLastRoom: string;
    fLastPassword: string;
    fServerPort: string;
    fMasterServerAddress: string;
    fServerName: AnsiString;
    fMasterAnnounceInterval: Integer;
    fMaxRooms: Integer;
    fServerPacketsAccumulatingDelay: Integer;
    fFlashOnMessage: Boolean;
    fAutoKickTimeout: Integer;
    fPingInterval: Integer;
    fAnnounceServer: Boolean;
    fHTMLStatusFile: UnicodeString;
    fServerWelcomeMessage: UnicodeString;
    fWareDistribution: TKMWareDistribution;

    fMenu_FavouriteMPMapsStr: UnicodeString;
    fMenu_ReplaysType: Byte;
    fMenu_MapEdMapType: Byte;
    fMenu_MapEdNewMapX: Word;
    fMenu_MapEdNewMapY: Word;
    fMenu_MapEdSPMapCRC: Cardinal;
    fMenu_MapEdMPMapCRC: Cardinal;
    fMenu_MapEdMPMapName: UnicodeString;
    fMenu_CampaignName: UnicodeString;
    fMenu_ReplaySPSaveName: UnicodeString;
    fMenu_ReplayMPSaveName: UnicodeString;
    fMenu_SPMapCRC: Cardinal;
    fMenu_SPSaveFileName: UnicodeString;
    fMenu_LobbyMapType: Byte;

    fFavouriteMaps: TKMFavouriteMaps;

    procedure SetAutosave(aValue: Boolean);
    procedure SetAutosaveFrequency(aValue: Integer);
    procedure SetAutosaveCount(aValue: Integer);
    procedure SetReplayAutopause(aValue: Boolean);
    procedure SetReplayShowBeacons(aValue: Boolean);
    procedure SetSpecShowBeacons(aValue: Boolean);
    procedure SetBrightness(aValue: Byte);
    procedure SetScrollSpeed(aValue: Byte);
    procedure SetAlphaShadows(aValue: Boolean);
    procedure SetLoadFullFonts(aValue: Boolean);
    procedure SetLocale(const aLocale: AnsiString);
    procedure SetMusicOff(aValue: Boolean);
    procedure SetShuffleOn(aValue: Boolean);
    procedure SetMusicVolume(aValue: Single);
    procedure SetSoundFXVolume(aValue: Single);
    procedure SetMultiplayerName(const aValue: AnsiString);
    procedure SetLastIP(const aValue: string);
    procedure SetMasterServerAddress(const aValue: string);
    procedure SetServerName(const aValue: AnsiString);
    procedure SetLastPort(const aValue: string);
    procedure SetLastRoom(const aValue: string);
    procedure SetLastPassword(const aValue: string);
    procedure SetServerPort(const aValue: string);
    procedure SetServerWelcomeMessage(const aValue: UnicodeString);
    procedure SetAnnounceServer(aValue: Boolean);
    procedure SetAutoKickTimeout(aValue: Integer);
    procedure SetPingInterval(aValue: Integer);
    procedure SetMasterAnnounceInterval(eValue: Integer);
    procedure SetHTMLStatusFile(const eValue: UnicodeString);
    procedure SetMaxRooms(eValue: Integer);
    procedure SetServerPacketsAccumulatingDelay(aValue: Integer);
    procedure SetFlashOnMessage(aValue: Boolean);

    procedure SetMenuFavouriteMPMapsStr(const aValue: UnicodeString);
    procedure SetMenuReplaysType(aValue: Byte);
    procedure SetMenuMapEdMapType(aValue: Byte);
    procedure SetMenuMapEdNewMapX(aValue: Word);
    procedure SetMenuMapEdNewMapY(aValue: Word);
    procedure SetMenuMapEdSPMapCRC(aValue: Cardinal);
    procedure SetMenuMapEdMPMapCRC(aValue: Cardinal);
    procedure SetMenuMapEdMPMapName(const aValue: UnicodeString);
    procedure SetMenuCampaignName(const aValue: UnicodeString);
    procedure SetMenuReplaySPSaveName(const aValue: UnicodeString);
    procedure SetMenuReplayMPSaveName(const aValue: UnicodeString);
    procedure SetMenuSPMapCRC(aValue: Cardinal);
    procedure SetMenuSPSaveFileName(const aValue: UnicodeString);
    procedure SetMenuLobbyMapType(aValue: Byte);
  protected
    function LoadFromINI(const FileName: UnicodeString): Boolean;
    procedure SaveToINI(const FileName: UnicodeString);
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveSettings(aForce: Boolean=False);
    procedure ReloadSettings;

    property Autosave: Boolean read fAutosave write SetAutosave;
    property AutosaveFrequency: Integer read fAutosaveFrequency write SetAutosaveFrequency;
    property AutosaveCount: Integer read fAutosaveCount write SetAutosaveCount;
    property ReplayAutopause: Boolean read fReplayAutopause write SetReplayAutopause;
    property ReplayShowBeacons: Boolean read fReplayShowBeacons write SetReplayShowBeacons;
    property SpecShowBeacons: Boolean read fSpecShowBeacons write SetSpecShowBeacons;
    property Brightness: Byte read fBrightness write SetBrightness;
    property ScrollSpeed: Byte read fScrollSpeed write SetScrollSpeed;
    property AlphaShadows: Boolean read fAlphaShadows write SetAlphaShadows;
    property LoadFullFonts: Boolean read fLoadFullFonts write SetLoadFullFonts;
    property Locale: AnsiString read fLocale write SetLocale;
    property MusicOff: Boolean read fMusicOff write SetMusicOff;
    property ShuffleOn: Boolean read fShuffleOn write SetShuffleOn;
    property MusicVolume: Single read fMusicVolume write SetMusicVolume;
    property SoundFXVolume: Single read fSoundFXVolume write SetSoundFXVolume;
    property SpeedPace: Word read fSpeedPace;
    property SpeedMedium: Single read fSpeedMedium;
    property SpeedFast: Single read fSpeedFast;
    property SpeedVeryFast: Single read fSpeedVeryFast;
    property MultiplayerName: AnsiString read fMultiplayerName write SetMultiplayerName;
    property LastIP: string read fLastIP write SetLastIP;
    property LastPort: string read fLastPort write SetLastPort;
    property LastRoom: string read fLastRoom write SetLastRoom;
    property LastPassword: string read fLastPassword write SetLastPassword;
    property ServerPort: string read fServerPort write SetServerPort;
    property MasterServerAddress: string read fMasterServerAddress write SetMasterServerAddress;
    property ServerName: AnsiString read fServerName write SetServerName;
    property MasterAnnounceInterval: Integer read fMasterAnnounceInterval write SetMasterAnnounceInterval;
    property AnnounceServer: Boolean read fAnnounceServer write SetAnnounceServer;
    property MaxRooms: Integer read fMaxRooms write SetMaxRooms;
    property ServerPacketsAccumulatingDelay: Integer read fServerPacketsAccumulatingDelay write SetServerPacketsAccumulatingDelay;
    property FlashOnMessage: Boolean read fFlashOnMessage write SetFlashOnMessage;
    property AutoKickTimeout: Integer read fAutoKickTimeout write SetAutoKickTimeout;
    property PingInterval: Integer read fPingInterval write SetPingInterval;
    property HTMLStatusFile: UnicodeString read fHTMLStatusFile write SetHTMLStatusFile;
    property ServerWelcomeMessage: UnicodeString read fServerWelcomeMessage write SetServerWelcomeMessage;
    property WareDistribution: TKMWareDistribution read fWareDistribution;

    property MenuFavouriteMPMapsStr: UnicodeString read fMenu_FavouriteMPMapsStr write SetMenuFavouriteMPMapsStr;
    property MenuReplaysType: Byte read fMenu_ReplaysType write SetMenuReplaysType;
    property MenuMapEdMapType: Byte read fMenu_MapEdMapType write SetMenuMapEdMapType;
    property MenuMapEdNewMapX: Word read fMenu_MapEdNewMapX write SetMenuMapEdNewMapX;
    property MenuMapEdNewMapY: Word read fMenu_MapEdNewMapY write SetMenuMapEdNewMapY;
    property MenuMapEdSPMapCRC: Cardinal read fMenu_MapEdSPMapCRC write SetMenuMapEdSPMapCRC;
    property MenuMapEdMPMapCRC: Cardinal read fMenu_MapEdMPMapCRC write SetMenuMapEdMPMapCRC;
    property MenuMapEdMPMapName: UnicodeString read fMenu_MapEdMPMapName write SetMenuMapEdMPMapName;
    property MenuCampaignName: UnicodeString read fMenu_CampaignName write SetMenuCampaignName;
    property MenuReplaySPSaveName: UnicodeString read fMenu_ReplaySPSaveName write SetMenuReplaySPSaveName;
    property MenuReplayMPSaveName: UnicodeString read fMenu_ReplayMPSaveName write SetMenuReplayMPSaveName;
    property MenuSPMapCRC: Cardinal read fMenu_SPMapCRC write SetMenuSPMapCRC;
    property MenuSPSaveFileName: UnicodeString read fMenu_SPSaveFileName write SetMenuSPSaveFileName;
    property MenuLobbyMapType: Byte read fMenu_LobbyMapType write SetMenuLobbyMapType;

    property FavouriteMaps: TKMFavouriteMaps read fFavouriteMaps;
  end;


implementation
uses
  SysUtils, INIfiles, Math,
  KM_Log;

const
  FAVOURITE_MAPS_DELIMITER = ':';


{ TMainSettings }
constructor TMainSettings.Create;
begin
  inherited;
  fWindowParams := TKMWindowParams.Create;
  LoadFromINI(ExeDir + SETTINGS_FILE);
  fNeedsSave := False;
  gLog.AddTime('Global settings loaded from ' + SETTINGS_FILE);
end;


destructor TMainSettings.Destroy;
begin
  SaveToINI(ExeDir+SETTINGS_FILE);
  FreeAndNil(fWindowParams);
  inherited;
end;


procedure TMainSettings.Changed;
begin
  fNeedsSave := True;
end;


function TMainSettings.LoadFromINI(const aFileName: UnicodeString): Boolean;
var
  F: TMemIniFile;
begin
  Result := FileExists(aFileName);

  F := TMemIniFile.Create(aFileName {$IFDEF WDC}, TEncoding.UTF8 {$ENDIF} );

  fFullScreen         := F.ReadBool   ('GFX', 'FullScreen',       False);
  fVSync              := F.ReadBool   ('GFX', 'VSync',            True);
  fResolution.Width   := F.ReadInteger('GFX', 'ResolutionWidth',  MENU_DESIGN_X);
  fResolution.Height  := F.ReadInteger('GFX', 'ResolutionHeight', MENU_DESIGN_Y);
  fResolution.RefRate := F.ReadInteger('GFX', 'RefreshRate',      60);
  fFPSCap := EnsureRange(F.ReadInteger('GFX', 'FPSCap', DEF_FPS_CAP), MIN_FPS_CAP, MAX_FPS_CAP);

  // For proper window positioning we need Left and Top records
  // Otherwise reset all window params to defaults
  if F.ValueExists('Window', 'WindowLeft') and F.ValueExists('Window', 'WindowTop') then
  begin
    fWindowParams.fWidth  := F.ReadInteger('Window', 'WindowWidth',  MENU_DESIGN_X);
    fWindowParams.fHeight := F.ReadInteger('Window', 'WindowHeight', MENU_DESIGN_Y);
    fWindowParams.fLeft   := F.ReadInteger('Window', 'WindowLeft',   -1);
    fWindowParams.fTop    := F.ReadInteger('Window', 'WindowTop',    -1);
    fWindowParams.fState  := TWindowState(EnsureRange(F.ReadInteger('Window', 'WindowState', 0), 0, 2));
  end else
    fWindowParams.fNeedResetToDefaults := True;

  // Reset wsMinimized state to wsNormal
  if (fWindowParams.fState = TWindowState.wsMinimized) then
    fWindowParams.fState := TWindowState.wsNormal;

  FreeAndNil(F);
  fNeedsSave := False;
end;


//Don't rewrite the file for each individual change, do it in one batch for simplicity
procedure TMainSettings.SaveToINI(const aFileName: UnicodeString);
var
  F: TMemIniFile;
begin
  F := TMemIniFile.Create(aFileName {$IFDEF WDC}, TEncoding.UTF8 {$ENDIF} );

  F.WriteBool   ('GFX','FullScreen',      fFullScreen);
  F.WriteBool   ('GFX','VSync',           fVSync);
  F.WriteInteger('GFX','ResolutionWidth', fResolution.Width);
  F.WriteInteger('GFX','ResolutionHeight',fResolution.Height);
  F.WriteInteger('GFX','RefreshRate',     fResolution.RefRate);
  F.WriteInteger('GFX','FPSCap',          fFPSCap);

  F.WriteInteger('Window','WindowWidth',    fWindowParams.Width);
  F.WriteInteger('Window','WindowHeight',   fWindowParams.Height);
  F.WriteInteger('Window','WindowLeft',     fWindowParams.Left);
  F.WriteInteger('Window','WindowTop',      fWindowParams.Top);
  F.WriteInteger('Window','WindowState',    Ord(fWindowParams.State));

  F.UpdateFile; //Write changes to file
  FreeAndNil(F);
  fNeedsSave := False;
end;

procedure TMainSettings.SetFullScreen(aValue: boolean);
begin
  fFullScreen := aValue;
  Changed;
end;


procedure TMainSettings.SetResolution(const Value: TKMScreenRes);
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


procedure TMainSettings.SaveSettings(aForce: Boolean);
begin
  if fNeedsSave or aForce or fWindowParams.IsChanged then
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

  fWareDistribution := TKMWareDistribution.Create;
  fFavouriteMaps := TKMFavouriteMaps.Create;
  fFavouriteMaps.OnMapsUpdate := SetMenuFavouriteMPMapsStr;

  ReloadSettings;
end;


destructor TGameSettings.Destroy;
begin
  SaveToINI(ExeDir + SETTINGS_FILE);
  FreeAndNil(fWareDistribution);
  FreeAndNil(fFavouriteMaps);

  inherited;
end;


//Save only when needed
procedure TGameSettings.SaveSettings(aForce: Boolean = False);
begin
  if fNeedsSave or fWareDistribution.Changed or aForce then
    SaveToINI(ExeDir + SETTINGS_FILE);
end;


procedure TGameSettings.ReloadSettings;
begin
  LoadFromINI(ExeDir + SETTINGS_FILE);
  gLog.AddTime('Game settings loaded from ' + SETTINGS_FILE);
end;


function TGameSettings.LoadFromINI(const FileName: UnicodeString): Boolean;
var
  F: TMemIniFile;
begin
  Result := FileExists(FileName);

  F := TMemIniFile.Create(FileName {$IFDEF WDC}, TEncoding.UTF8 {$ENDIF} );
  try
    fBrightness         := F.ReadInteger  ('GFX', 'Brightness',         1);
    fAlphaShadows       := F.ReadBool     ('GFX', 'AlphaShadows',       True);
    fLoadFullFonts      := F.ReadBool     ('GFX', 'LoadFullFonts',      False);

    fAutosave           := F.ReadBool     ('Game', 'Autosave',          True); //Should be ON by default
    SetAutosaveFrequency(F.ReadInteger    ('Game', 'AutosaveFrequency', AUTOSAVE_FREQUENCY));
    SetAutosaveCount    (F.ReadInteger    ('Game', 'AutosaveCount',     AUTOSAVE_COUNT));
    fReplayAutopause    := F.ReadBool     ('Game', 'ReplayAutopause',   False); //Disabled by default
    fReplayShowBeacons  := F.ReadBool     ('Game', 'ReplayShowBeacons', False); //Disabled by default
    fSpecShowBeacons    := F.ReadBool     ('Game', 'SpecShowBeacons',   False); //Disabled by default
    fScrollSpeed        := F.ReadInteger  ('Game', 'ScrollSpeed',       10);
    fSpeedPace          := F.ReadInteger  ('Game', 'SpeedPace',         100);
    fSpeedMedium        := F.ReadFloat    ('Game', 'SpeedMedium',       3);
    fSpeedFast          := F.ReadFloat    ('Game', 'SpeedFast',         6);
    fSpeedVeryFast      := F.ReadFloat    ('Game', 'SpeedVeryFast',     10);

    fLocale             := AnsiString(F.ReadString ('Game', 'Locale', UnicodeString(DEFAULT_LOCALE)));

    fWareDistribution.LoadFromStr(F.ReadString ('Game','WareDistribution',''));

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
    fLastPassword           := F.ReadString('Multiplayer','LastPassword','');
    fFlashOnMessage         := F.ReadBool   ('Multiplayer','FlashOnMessage',True);
    fServerPort             := F.ReadString ('Server','ServerPort','56789');
    //We call it MasterServerAddressNew to force it to update in everyone's .ini file when we changed address.
    //If the key stayed the same then everyone would still be using the old value from their settings.
    fMasterServerAddress    := F.ReadString ('Server','MasterServerAddressNew','http://kam.hodgman.id.au/');
    fMasterAnnounceInterval := F.ReadInteger('Server','MasterServerAnnounceInterval',180);
    fAnnounceServer         := F.ReadBool   ('Server','AnnounceDedicatedServer',True);
    fServerName             := AnsiString(F.ReadString ('Server','ServerName','KaM Remake Server'));
    fMaxRooms               := F.ReadInteger('Server','MaxRooms',16);
    ServerPacketsAccumulatingDelay := F.ReadInteger('Server','PacketsAccumulatingDelay',20);
    fAutoKickTimeout        := F.ReadInteger('Server','AutoKickTimeout',20);
    fPingInterval           := F.ReadInteger('Server','PingMeasurementInterval',1000);
    fHTMLStatusFile         := F.ReadString ('Server','HTMLStatusFile','KaM_Remake_Server_Status.html');
    fServerWelcomeMessage   := {$IFDEF FPC} UTF8Decode {$ENDIF} (F.ReadString ('Server','WelcomeMessage',''));

    fMenu_FavouriteMPMapsStr   := F.ReadString('Menu', 'FavouriteMaps', '');
    fFavouriteMaps.LoadFromString(fMenu_FavouriteMPMapsStr);

    fMenu_ReplaysType       := F.ReadInteger('Menu', 'ReplaysType',  0);
    fMenu_MapEdMapType      := F.ReadInteger('Menu', 'MapEdMapType', 0);
    fMenu_MapEdNewMapX      := F.ReadInteger('Menu', 'MapEdNewMapX', 64);
    fMenu_MapEdNewMapY      := F.ReadInteger('Menu', 'MapEdNewMapY', 64);
    fMenu_MapEdSPMapCRC     := StrToInt64(F.ReadString('Menu', 'MapEdSPMapCRC', '0'));
    fMenu_MapEdMPMapCRC     := StrToInt64(F.ReadString('Menu', 'MapEdMPMapCRC', '0'));
    fMenu_MapEdMPMapName    := F.ReadString('Menu', 'MapEdMPMapName', '');
    fMenu_CampaignName      := F.ReadString('Menu', 'CampaignName', '');
    fMenu_ReplaySPSaveName  := F.ReadString('Menu', 'ReplaySPSaveName', '');
    fMenu_ReplayMPSaveName  := F.ReadString('Menu', 'ReplayMPSaveName', '');
    fMenu_SPMapCRC          := StrToInt64(F.ReadString('Menu', 'SPMapCRC', '0'));
    fMenu_SPSaveFileName    := F.ReadString('Menu', 'SPSaveFileName', '');
    fMenu_LobbyMapType      := F.ReadInteger('Menu', 'LobbyMapType', 0);
  finally
    F.Free;
  end;

  fNeedsSave := False;
end;


//Don't rewrite the file for each individual change, do it in one batch for simplicity
procedure TGameSettings.SaveToINI(const FileName: UnicodeString);
var
  F: TMemIniFile;
begin
  F := TMemIniFile.Create(FileName {$IFDEF WDC}, TEncoding.UTF8 {$ENDIF} );
  try
    F.WriteInteger('GFX','Brightness',    fBrightness);
    F.WriteBool   ('GFX','AlphaShadows',  fAlphaShadows);
    F.WriteBool   ('GFX','LoadFullFonts', fLoadFullFonts);

    F.WriteBool   ('Game','Autosave',           fAutosave);
    F.WriteInteger('Game','AutosaveFrequency',  fAutosaveFrequency);
    F.WriteInteger('Game','AutosaveCount',      fAutosaveCount);
    F.WriteBool   ('Game','ReplayAutopause',    fReplayAutopause);
    F.WriteBool   ('Game','ReplayShowBeacons',  fReplayShowBeacons);
    F.WriteBool   ('Game','SpecShowBeacons',    fSpecShowBeacons);
    F.WriteInteger('Game','ScrollSpeed',        fScrollSpeed);
    F.WriteInteger('Game','SpeedPace',          fSpeedPace);
    F.WriteFloat  ('Game','SpeedMedium',        fSpeedMedium);
    F.WriteFloat  ('Game','SpeedFast',          fSpeedFast);
    F.WriteFloat  ('Game','SpeedVeryFast',      fSpeedVeryFast);

    F.WriteString ('Game','Locale',          UnicodeString(fLocale));

    F.WriteString('Game','WareDistribution', fWareDistribution.PackToStr);

    F.WriteFloat  ('SFX','SFXVolume',     fSoundFXVolume);
    F.WriteFloat  ('SFX','MusicVolume',   fMusicVolume);
    F.WriteBool   ('SFX','MusicDisabled', fMusicOff);
    F.WriteBool   ('SFX','ShuffleEnabled',fShuffleOn);

    if INI_HITPOINT_RESTORE then
      F.WriteInteger('Fights','HitPointRestorePace', HITPOINT_RESTORE_PACE);

    F.WriteString ('Multiplayer','Name',            UnicodeString(fMultiplayerName));
    F.WriteString ('Multiplayer','LastIP',          fLastIP);
    F.WriteString ('Multiplayer','LastPort',        fLastPort);
    F.WriteString ('Multiplayer','LastRoom',        fLastRoom);
    F.WriteString ('Multiplayer','LastPassword',    fLastPassword);
    F.WriteBool   ('Multiplayer','FlashOnMessage',  fFlashOnMessage);

    F.WriteString ('Server','ServerName',                   UnicodeString(fServerName));
    F.WriteString ('Server','WelcomeMessage',               {$IFDEF FPC} UTF8Encode {$ENDIF}(fServerWelcomeMessage));
    F.WriteString ('Server','ServerPort',                   fServerPort);
    F.WriteBool   ('Server','AnnounceDedicatedServer',      fAnnounceServer);
    F.WriteInteger('Server','MaxRooms',                     fMaxRooms);
    F.WriteInteger('Server','PacketsAccumulatingDelay',     fServerPacketsAccumulatingDelay);
    F.WriteString ('Server','HTMLStatusFile',               fHTMLStatusFile);
    F.WriteInteger('Server','MasterServerAnnounceInterval', fMasterAnnounceInterval);
    F.WriteString ('Server','MasterServerAddressNew',       fMasterServerAddress);
    F.WriteInteger('Server','AutoKickTimeout',              fAutoKickTimeout);
    F.WriteInteger('Server','PingMeasurementInterval',      fPingInterval);

    F.WriteString ('Menu',  'FavouriteMaps',      fMenu_FavouriteMPMapsStr);
    F.WriteInteger('Menu',  'ReplaysType',        fMenu_ReplaysType);
    F.WriteInteger('Menu',  'MapEdMapType',       fMenu_MapEdMapType);
    F.WriteInteger('Menu',  'MapEdNewMapX',       fMenu_MapEdNewMapX);
    F.WriteInteger('Menu',  'MapEdNewMapY',       fMenu_MapEdNewMapY);
    F.WriteString ('Menu',  'MapEdSPMapCRC',      IntToStr(fMenu_MapEdSPMapCRC));
    F.WriteString ('Menu',  'MapEdMPMapCRC',      IntToStr(fMenu_MapEdMPMapCRC));
    F.WriteString ('Menu',  'MapEdMPMapName',     fMenu_MapEdMPMapName);
    F.WriteString ('Menu',  'CampaignName',       fMenu_CampaignName);
    F.WriteString ('Menu',  'ReplaySPSaveName',   fMenu_ReplaySPSaveName);
    F.WriteString ('Menu',  'ReplayMPSaveName',   fMenu_ReplayMPSaveName);
    F.WriteString ('Menu',  'SPMapCRC',           IntToStr(fMenu_SPMapCRC));
    F.WriteString ('Menu',  'SPSaveFileName',     fMenu_SPSaveFileName);
    F.WriteInteger('Menu',  'LobbyMapType',       fMenu_LobbyMapType);

    F.UpdateFile; //Write changes to file
  finally
    F.Free;
  end;

  fNeedsSave := False;
end;


procedure TGameSettings.SetLocale(const aLocale: AnsiString);
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


procedure TGameSettings.SetFlashOnMessage(aValue: Boolean);
begin
  fFlashOnMessage := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuFavouriteMPMapsStr(const aValue: UnicodeString);
begin
  fMenu_FavouriteMPMapsStr := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuReplaysType(aValue: Byte);
begin
  fMenu_ReplaysType := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuMapEdMapType(aValue: Byte);
begin
  fMenu_MapEdMapType := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuMapEdNewMapX(aValue: Word);
begin
  fMenu_MapEdNewMapX := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuMapEdNewMapY(aValue: Word);
begin
  fMenu_MapEdNewMapY := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuMapEdSPMapCRC(aValue: Cardinal);
begin
  fMenu_MapEdSPMapCRC := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuMapEdMPMapCRC(aValue: Cardinal);
begin
  fMenu_MapEdMPMapCRC := aValue;
  Changed;
end;



procedure TGameSettings.SetMenuMapEdMPMapName(const aValue: UnicodeString);
begin
  fMenu_MapEdMPMapName := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuCampaignName(const aValue: UnicodeString);
begin
  fMenu_CampaignName := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuReplaySPSaveName(const aValue: UnicodeString);
begin
  fMenu_ReplaySPSaveName := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuReplayMPSaveName(const aValue: UnicodeString);
begin
  fMenu_ReplayMPSaveName := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuSPMapCRC(aValue: Cardinal);
begin
  fMenu_SPMapCRC := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuSPSaveFileName(const aValue: UnicodeString);
begin
  fMenu_SPSaveFileName := aValue;
  Changed;
end;


procedure TGameSettings.SetMenuLobbyMapType(aValue: Byte);
begin
  fMenu_LobbyMapType := aValue;
  Changed;
end;


procedure TGameSettings.SetAutosave(aValue: Boolean);
begin
  fAutosave := aValue;
  Changed;
end;


procedure TGameSettings.SetAutosaveCount(aValue: Integer);
begin
  fAutosaveCount := EnsureRange(aValue, AUTOSAVE_COUNT_MIN, AUTOSAVE_COUNT_MAX);
  Changed;
end;


procedure TGameSettings.SetAutosaveFrequency(aValue: Integer);
begin
  fAutosaveFrequency := EnsureRange(aValue, AUTOSAVE_FREQUENCY_MIN, AUTOSAVE_FREQUENCY_MAX);
  Changed;
end;


procedure TGameSettings.SetReplayAutopause(aValue: Boolean);
begin
  fReplayAutopause := aValue;
  Changed;
end;


procedure TGameSettings.SetReplayShowBeacons(aValue: Boolean);
begin
  fReplayShowBeacons := aValue;
  Changed;
end;


procedure TGameSettings.SetSpecShowBeacons(aValue: Boolean);
begin
  fSpecShowBeacons := aValue;
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


procedure TGameSettings.SetLoadFullFonts(aValue: Boolean);
begin
  fLoadFullFonts := aValue;
  Changed;
end;


procedure TGameSettings.SetSoundFXVolume(aValue: Single);
begin
  fSoundFXVolume := EnsureRange(aValue, 0, 1);
  Changed;
end;


procedure TGameSettings.SetMultiplayerName(const aValue: AnsiString);
begin
  fMultiplayerName := aValue;
  Changed;
end;


procedure TGameSettings.SetLastIP(const aValue: string);
begin
  fLastIP := aValue;
  Changed;
end;


procedure TGameSettings.SetMasterServerAddress(const aValue: string);
begin
  fMasterServerAddress := aValue;
  Changed;
end;


procedure TGameSettings.SetServerName(const aValue: AnsiString);
begin
  fServerName := aValue;
  Changed;
end;


procedure TGameSettings.SetLastPort(const aValue: string);
begin
  fLastPort := aValue;
  Changed;
end;


procedure TGameSettings.SetLastRoom(const aValue: string);
begin
  fLastRoom := aValue;
  Changed;
end;


procedure TGameSettings.SetLastPassword(const aValue: string);
begin
  fLastPassword := aValue;
  Changed;
end;


procedure TGameSettings.SetServerPacketsAccumulatingDelay(aValue: Integer);
begin
  fServerPacketsAccumulatingDelay := EnsureRange(aValue, 0, 1000); //This is rough restrictions. Real one are in TKMNetServer
  Changed;
end;


procedure TGameSettings.SetServerPort(const aValue: string);
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


procedure TGameSettings.SetHTMLStatusFile(const eValue: UnicodeString);
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


procedure TGameSettings.SetServerWelcomeMessage(const aValue: UnicodeString);
begin
  fServerWelcomeMessage := aValue;
  Changed;
end;


{TKMWindowParams}
constructor TKMWindowParams.Create;
begin
  inherited;
  fIsChanged := False;
  fLockParams := False;
  fNeedResetToDefaults := False;
end;


procedure TKMWindowParams.ApplyWindowParams(aParams: TKMWindowParamsRecord; aDefaults: Boolean = False);
begin
  if not fLockParams then
  begin
    fWidth := aParams.Width;
    fHeight := aParams.Height;
    fLeft := aParams.Left;
    fTop := aParams.Top;
    fState := aParams.State;
    fIsChanged := True;
    fNeedResetToDefaults := aDefaults;
  end;
end;


procedure TKMWindowParams.LockParams;
begin
  fLockParams := True;
end;


procedure TKMWindowParams.UnlockParams;
begin
  fLockParams := False;
end;


// Check window param, with current Screen object
function TKMWindowParams.IsValid(aMonitorsInfo: TKMPointArray): Boolean;
var I, ScreenMaxWidth, ScreenMaxHeight: Integer;
begin
  ScreenMaxWidth := 0;
  ScreenMaxHeight := 0;
  // Calc Max width/height for multi screen systems
  // Assume appending monitor screens left to right, so summarise width, get max of height
  for I := Low(aMonitorsInfo) to High(aMonitorsInfo) do
  begin
    ScreenMaxWidth := ScreenMaxWidth + aMonitorsInfo[I].X;
    ScreenMaxHeight := max(ScreenMaxHeight, aMonitorsInfo[I].Y);
  end;
  // Do not let put window too much left or right. 100px is enought to get it back in that case
  Result := (fLeft > -fWidth + 100)
        and (fLeft < ScreenMaxWidth - 100)
        and (fTop >= 0)
        and (fTop < ScreenMaxHeight - 100)
        and (fWidth >= MIN_RESOLUTION_WIDTH)
        and (fWidth <= ScreenMaxWidth)
        and (fHeight >= MIN_RESOLUTION_HEIGHT)
        and (fHeight <= ScreenMaxHeight)
        and (fState in [TWindowState.wsNormal, TWindowState.wsMaximized]);
end;


{TKMFavouriteMaps}
constructor TKMFavouriteMaps.Create;
begin
  inherited;
  fFavouriteMPMaps := TStringList.Create;
  fFavouriteMPMaps.Delimiter       := FAVOURITE_MAPS_DELIMITER;
  fFavouriteMPMaps.StrictDelimiter := True; // Requires D2006 or newer.
end;


destructor TKMFavouriteMaps.Destroy;
begin
  FreeAndNil(fFavouriteMPMaps);
  inherited;
end;


procedure TKMFavouriteMaps.FavoriteMapsUpdated;
begin
  if Assigned(fOnMapsUpdate) then
    fOnMapsUpdate(PackToString);
end;


procedure TKMFavouriteMaps.LoadFromString(const aString: UnicodeString);
var I: Integer;
    MapCRC : Int64;
    StringList: TStringList;
begin
  fFavouriteMPMaps.Clear;
  StringList := TStringList.Create;
  StringList.Delimiter := FAVOURITE_MAPS_DELIMITER;
  StringList.DelimitedText   := Trim(aString);

  for I := 0 to StringList.Count - 1 do
  begin
    if TryStrToInt64(Trim(StringList[I]), MapCRC)
      and (MapCRC > 0)
      and not Contains(Cardinal(MapCRC)) then
      fFavouriteMPMaps.Add(Trim(StringList[I]));
  end;

  StringList.Free;
end;


function TKMFavouriteMaps.PackToString: UnicodeString;
begin
  Result := fFavouriteMPMaps.DelimitedText;
end;


//Remove missing Favourites Maps from list, check if are of them are presented in the given maps CRC array.
procedure TKMFavouriteMaps.RemoveMissing(aMapsCRCArray: TKMCardinalArray);
  function ArrayContains(aValue: Cardinal): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I := Low(aMapsCRCArray) to High(aMapsCRCArray) do
      if aMapsCRCArray[I] = aValue then
      begin
        Result := True;
        Break;
      end;
  end;
var I: Integer;
begin
  I := fFavouriteMPMaps.Count - 1;
  //We must check, that all values from favorites are presented in maps CRC array. If not - then remove it from favourites
  while (fFavouriteMPMaps.Count > 0) and (I >= 0) do
  begin
    if not ArrayContains(StrToInt64(fFavouriteMPMaps[I])) then
    begin
      fFavouriteMPMaps.Delete(I);
      FavoriteMapsUpdated;
    end;

    Dec(I);
  end;
end;


function TKMFavouriteMaps.Contains(aMapCRC: Cardinal): Boolean;
begin
  Result := fFavouriteMPMaps.IndexOf(IntToStr(aMapCRC)) <> -1;
end;


procedure TKMFavouriteMaps.Add(aMapCRC: Cardinal);
begin
  if not Contains(aMapCRC) then
  begin
    fFavouriteMPMaps.Add(IntToStr(aMapCRC));
    FavoriteMapsUpdated;
  end;
end;


procedure TKMFavouriteMaps.Remove(aMapCRC: Cardinal);
var Index: Integer;
begin
  Index := fFavouriteMPMaps.IndexOf(IntToStr(aMapCRC));
  if Index <> -1 then
    fFavouriteMPMaps.Delete(Index);
  FavoriteMapsUpdated;
end;


procedure TKMFavouriteMaps.Replace(aOldCRC, aNewCRC: Cardinal);
begin
  if Contains(aOldCRC) then
  begin
    Remove(aOldCRC);
    Add(aNewCRC);
  end;
end;


end.

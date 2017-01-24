﻿unit KM_Settings;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, INIfiles, System.UITypes,
  KM_Defaults, KM_Resolutions, KM_Points, KM_WareDistribution;


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


  //Settings that are irrelevant to the game (game does not cares about them)
  //Everything gets written through setter to set fNeedsSave flag
  TMainSettings = class
  private
    fNeedsSave: Boolean;
    fFullScreen: Boolean;
    fResolution: TKMScreenRes;
    fWindowParams: TKMWindowParams;
    fVSync: Boolean;
    procedure SetFullScreen(aValue: Boolean);
    procedure SetResolution(const Value: TKMScreenRes);
    procedure SetVSync(aValue: Boolean);
  protected
    procedure Changed;
    function LoadFromINI(aFileName: UnicodeString): Boolean;
    procedure SaveToINI(aFileName: UnicodeString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveSettings(aForce: Boolean = False);
    procedure ReloadSettings;

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
    fReplayAutopause: Boolean;
    fBrightness: Byte;
    fScrollSpeed: Byte;
    fAlphaShadows: Boolean;
    fLoadFullFonts: Boolean;
    fLocale: AnsiString;
    fMute: Boolean;
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
    fServerPort: string;
    fMasterServerAddress: string;
    fServerName: AnsiString;
    fMasterAnnounceInterval: Integer;
    fMaxRooms: Integer;
    fFlashOnMessage: Boolean;
    fAutoKickTimeout: Integer;
    fPingInterval: Integer;
    fAnnounceServer: Boolean;
    fHTMLStatusFile: UnicodeString;
    fServerWelcomeMessage: UnicodeString;
    fWareDistribution: TKMWareDistribution;

    procedure SetAutosave(aValue: Boolean);
    procedure SetReplayAutopause(aValue: Boolean);
    procedure SetBrightness(aValue: Byte);
    procedure SetScrollSpeed(aValue: Byte);
    procedure SetAlphaShadows(aValue: Boolean);
    procedure SetLoadFullFonts(aValue: Boolean);
    procedure SetLocale(aLocale: AnsiString);
    procedure SetMute(aValue: Boolean);
    procedure SetMusicOff(aValue: Boolean);
    procedure SetShuffleOn(aValue: Boolean);
    procedure SetMusicVolume(aValue: Single);
    procedure SetSoundFXVolume(aValue: Single);
    procedure SetMultiplayerName(aValue: AnsiString);
    procedure SetLastIP(aValue: string);
    procedure SetMasterServerAddress(aValue: string);
    procedure SetServerName(aValue: AnsiString);
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
    procedure SetFlashOnMessage(aValue: Boolean);
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
    property ReplayAutopause: Boolean read fReplayAutopause write SetReplayAutopause;
    property Brightness: Byte read fBrightness write SetBrightness;
    property ScrollSpeed: Byte read fScrollSpeed write SetScrollSpeed;
    property AlphaShadows: Boolean read fAlphaShadows write SetAlphaShadows;
    property LoadFullFonts: Boolean read fLoadFullFonts write SetLoadFullFonts;
    property Locale: AnsiString read fLocale write SetLocale;
    property Mute: Boolean read fMute write SetMute;
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
    property ServerPort: string read fServerPort write SetServerPort;
    property MasterServerAddress: string read fMasterServerAddress write SetMasterServerAddress;
    property ServerName: AnsiString read fServerName write SetServerName;
    property MasterAnnounceInterval: Integer read fMasterAnnounceInterval write SetMasterAnnounceInterval;
    property AnnounceServer: Boolean read fAnnounceServer write SetAnnounceServer;
    property MaxRooms: Integer read fMaxRooms write SetMaxRooms;
    property FlashOnMessage: Boolean read fFlashOnMessage write SetFlashOnMessage;
    property AutoKickTimeout: Integer read fAutoKickTimeout write SetAutoKickTimeout;
    property PingInterval: Integer read fPingInterval write SetPingInterval;
    property HTMLStatusFile: UnicodeString read fHTMLStatusFile write SetHTMLStatusFile;
    property ServerWelcomeMessage: UnicodeString read fServerWelcomeMessage write SetServerWelcomeMessage;
    property WareDistribution: TKMWareDistribution read fWareDistribution;
  end;


implementation
uses
  KM_Log;


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


function TMainSettings.LoadFromINI(aFileName: UnicodeString): Boolean;
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
procedure TMainSettings.SaveToINI(aFileName: UnicodeString);
var
  F: TMemIniFile;
begin
  F := TMemIniFile.Create(aFileName {$IFDEF WDC}, TEncoding.UTF8 {$ENDIF} );

  F.WriteBool   ('GFX','FullScreen',      fFullScreen);
  F.WriteBool   ('GFX','VSync',           fVSync);
  F.WriteInteger('GFX','ResolutionWidth', fResolution.Width);
  F.WriteInteger('GFX','ResolutionHeight',fResolution.Height);
  F.WriteInteger('GFX','RefreshRate',     fResolution.RefRate);

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


procedure TMainSettings.SaveSettings(aForce: boolean);
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
  ReloadSettings;
end;


destructor TGameSettings.Destroy;
begin
  SaveToINI(ExeDir + SETTINGS_FILE);
  FreeAndNil(fWareDistribution);

  inherited;
end;


//Save only when needed
procedure TGameSettings.SaveSettings(aForce: Boolean=False);
begin
  if fNeedsSave or fWareDistribution.Changed or aForce then
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

  F := TMemIniFile.Create(FileName {$IFDEF WDC}, TEncoding.UTF8 {$ENDIF} );
  try
    fBrightness       := F.ReadInteger('GFX', 'Brightness',       1);
    fAlphaShadows     := F.ReadBool   ('GFX', 'AlphaShadows',     True);
    fLoadFullFonts    := F.ReadBool   ('GFX', 'LoadFullFonts',    False);

    fAutosave       := F.ReadBool   ('Game', 'Autosave',       True); //Should be ON by default
    fReplayAutopause:= F.ReadBool   ('Game', 'ReplayAutopause', False); //Disabled by default
    fScrollSpeed    := F.ReadInteger('Game', 'ScrollSpeed',    10);
    fLocale         := AnsiString(F.ReadString ('Game', 'Locale', UnicodeString(DEFAULT_LOCALE)));
    fSpeedPace      := F.ReadInteger('Game', 'SpeedPace',      100);
    fSpeedMedium    := F.ReadFloat('Game', 'SpeedMedium',    3);
    fSpeedFast      := F.ReadFloat('Game', 'SpeedFast',      6);
    fSpeedVeryFast  := F.ReadFloat('Game', 'SpeedVeryFast',  10);

    fWareDistribution.LoadFromStr(F.ReadString ('Game','WareDistribution',''));

    fSoundFXVolume  := F.ReadFloat  ('SFX',  'SFXVolume',      0.5);
    fMusicVolume    := F.ReadFloat  ('SFX',  'MusicVolume',    0.5);
    fMute           := F.ReadBool   ('SFX', 'Mute',           False);
    fMusicOff       := F.ReadBool   ('SFX', 'MusicDisabled',  False);
    fShuffleOn      := F.ReadBool   ('SFX', 'ShuffleEnabled', False);

    if INI_HITPOINT_RESTORE then
      HITPOINT_RESTORE_PACE := F.ReadInteger('Fights', 'HitPointRestorePace', DEFAULT_HITPOINT_RESTORE)
    else
      HITPOINT_RESTORE_PACE := DEFAULT_HITPOINT_RESTORE;

    fMultiplayerName        := AnsiString(F.ReadString ('Multiplayer','Name','NoName'));
    fLastIP                 := F.ReadString ('Multiplayer','LastIP','127.0.0.1');
    fLastPort               := F.ReadString ('Multiplayer','LastPort','56789');
    fLastRoom               := F.ReadString ('Multiplayer','LastRoom','0');
    fFlashOnMessage         := F.ReadBool   ('Multiplayer','FlashOnMessage',True);
    fServerPort             := F.ReadString ('Server','ServerPort','56789');
    //We call it MasterServerAddressNew to force it to update in everyone's .ini file when we changed address.
    //If the key stayed the same then everyone would still be using the old value from their settings.
    fMasterServerAddress    := F.ReadString ('Server','MasterServerAddressNew','http://kam.hodgman.id.au/');
    fMasterAnnounceInterval := F.ReadInteger('Server','MasterServerAnnounceInterval',180);
    fAnnounceServer         := F.ReadBool   ('Server','AnnounceDedicatedServer',True);
    fServerName             := AnsiString(F.ReadString ('Server','ServerName','KaM Remake Server'));
    fMaxRooms               := F.ReadInteger('Server','MaxRooms',16);
    fAutoKickTimeout        := F.ReadInteger('Server','AutoKickTimeout',20);
    fPingInterval           := F.ReadInteger('Server','PingMeasurementInterval',1000);
    fHTMLStatusFile         := F.ReadString ('Server','HTMLStatusFile','KaM_Remake_Server_Status.html');
    fServerWelcomeMessage   := {$IFDEF FPC} UTF8Decode {$ENDIF} (F.ReadString ('Server','WelcomeMessage',''));
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
  F := TMemIniFile.Create(FileName {$IFDEF WDC}, TEncoding.UTF8 {$ENDIF} );
  try
    F.WriteInteger('GFX','Brightness',    fBrightness);
    F.WriteBool   ('GFX','AlphaShadows',  fAlphaShadows);
    F.WriteBool   ('GFX','LoadFullFonts', fLoadFullFonts);

    F.WriteBool   ('Game','Autosave',        fAutosave);
    F.WriteBool   ('Game','ReplayAutopause', fReplayAutopause);
    F.WriteInteger('Game','ScrollSpeed',     fScrollSpeed);
    F.WriteString ('Game','Locale',          UnicodeString(fLocale));
    F.WriteInteger('Game','SpeedPace',       fSpeedPace);
    F.WriteFloat('Game','SpeedMedium',       fSpeedMedium);
    F.WriteFloat('Game','SpeedFast',         fSpeedFast);
    F.WriteFloat('Game','SpeedVeryFast',     fSpeedVeryFast);

    F.WriteString('Game','WareDistribution', fWareDistribution.PackToStr);

    F.WriteFloat  ('SFX','SFXVolume',     fSoundFXVolume);
    F.WriteFloat  ('SFX','MusicVolume',   fMusicVolume);
    F.WriteBool   ('SFX','Mute',          fMute);
    F.WriteBool   ('SFX','MusicDisabled', fMusicOff);
    F.WriteBool   ('SFX','ShuffleEnabled',fShuffleOn);

    if INI_HITPOINT_RESTORE then
      F.WriteInteger('Fights','HitPointRestorePace', HITPOINT_RESTORE_PACE);

    F.WriteString ('Multiplayer','Name',            UnicodeString(fMultiplayerName));
    F.WriteString ('Multiplayer','LastIP',          fLastIP);
    F.WriteString ('Multiplayer','LastPort',        fLastPort);
    F.WriteString ('Multiplayer','LastRoom',        fLastRoom);
    F.WriteBool   ('Multiplayer','FlashOnMessage',  fFlashOnMessage);

    F.WriteString ('Server','ServerName',                   UnicodeString(fServerName));
    F.WriteString ('Server','WelcomeMessage',               {$IFDEF FPC} UTF8Encode {$ENDIF}(fServerWelcomeMessage));
    F.WriteString ('Server','ServerPort',                   fServerPort);
    F.WriteBool   ('Server','AnnounceDedicatedServer',      fAnnounceServer);
    F.WriteInteger('Server','MaxRooms',                     fMaxRooms);
    F.WriteString ('Server','HTMLStatusFile',               fHTMLStatusFile);
    F.WriteInteger('Server','MasterServerAnnounceInterval', fMasterAnnounceInterval);
    F.WriteString ('Server','MasterServerAddressNew',       fMasterServerAddress);
    F.WriteInteger('Server','AutoKickTimeout',              fAutoKickTimeout);
    F.WriteInteger('Server','PingMeasurementInterval',      fPingInterval);

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


procedure TGameSettings.SetFlashOnMessage(aValue: Boolean);
begin
  fFlashOnMessage := aValue;
  Changed;
end;


procedure TGameSettings.SetAutosave(aValue: Boolean);
begin
  fAutosave := aValue;
  Changed;
end;


procedure TGameSettings.SetReplayAutopause(aValue: Boolean);
begin
  fReplayAutopause := aValue;
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


procedure TGameSettings.SetServerName(aValue: AnsiString);
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


procedure TGameSettings.SetMute(aValue: Boolean);
begin
  fMute := aValue;
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


end.

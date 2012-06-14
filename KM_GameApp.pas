unit KM_GameApp;
{$I KaM_Remake.inc}
interface
uses Windows, Classes, Controls, Dialogs, ExtCtrls, KromUtils, Math, SysUtils,
  KM_CommonTypes,
  KM_Campaigns, KM_Game,
  KM_InterfaceDefaults, KM_InterfaceMapEditor, KM_InterfaceGamePlay, KM_InterfaceMainMenu,
  KM_Locales, KM_Music, KM_Networking, KM_Settings, KM_TextLibrary, KM_Render;

type
  TGameState = (
    gsNoGame,  //No game running at all, MainMenu
    gsPaused,  //Game is paused and responds to 'P' key only
    gsOnHold,  //Game is paused, shows victory options (resume, win) and responds to mouse clicks only
    gsRunning, //Game is running normally
    gsReplay,  //Game is showing replay, no player input allowed
    gsEditor); //Game is in MapEditor mode

  //Methods relevant to gameplay
  TKMGameApp = class
  private
    fGameState: TGameState;
    fGlobalTickCount: Cardinal; //Not affected by Pause and anything (Music, Minimap, StatusBar update)
    fScreenX, fScreenY: Word;

    fCampaigns: TKMCampaignsCollection;
    fGame: TKMGame;
    fGameSettings: TGameSettings;
    fMusicLib: TMusicLib;
    fNetworking: TKMNetworking;
    fRender: TRender;
    fTimerUI: TTimer;

    fActiveInterface: TKMUserInterface;
    fGamePlayInterface: TKMGamePlayInterface;
    fMainMenuInterface: TKMMainMenuInterface;
    fMapEditorInterface: TKMapEdInterface;

  public
    OnCursorUpdate: TIntegerStringEvent;

    constructor Create(aHandle: HWND; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TStringEvent; NoMusic: Boolean = False);
    destructor Destroy; override;
    procedure AfterConstruction(aReturnToOptions: Boolean); reintroduce;

    function CanClose: Boolean;
    procedure SetGameState(aNewState: TGameState);
    property GameState: TGameState read fGameState;
    procedure Resize(X,Y: Integer);
    procedure ToggleLocale(aLocale: ShortString);

    property GlobalSettings: TGameSettings read fGameSettings;
    property Campaigns: TKMCampaignsCollection read fCampaigns;
    property MusicLib: TMusicLib read fMusicLib;
    property Networking: TKMNetworking read fNetworking;

    procedure KeyDown(Key: Word; Shift: TShiftState);
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure UpdateState(Sender: TObject);
    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


var
  fGameApp: TKMGameApp;


implementation
uses
  KM_Defaults, KM_GameInfo, KM_RenderAux, KM_RenderPool, KM_Resource, KM_Sound, KM_Utils;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
constructor TKMGameApp.Create(aHandle: HWND; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TStringEvent; NoMusic: Boolean = False);
begin
  inherited Create;

  fGameState    := gsNoGame;
  fScreenX := aScreenX;
  fScreenY := aScreenY;

  fLocales        := TKMLocales.Create(ExeDir + 'data\locales.txt');
  fGameSettings   := TGameSettings.Create;

  //Texts must be loaded BEFORE we check OpenGL version so the message is translated!
  fTextLibrary    := TTextLibrary.Create(ExeDir + 'data\text\', fGameSettings.Locale);

  fRender         := TRender.Create(aHandle, fScreenX, fScreenY, aVSync);
  //Show the message if user has old OpenGL drivers (pre-1.4)
  if fRender.IsOldGLVersion then
    //MessageDlg works better than Application.MessageBox or others, it stays on top and
    //pauses here until the user clicks ok.
    MessageDlg(fTextLibrary[TX_GAME_ERROR_OLD_OPENGL]+eol+eol+fTextLibrary[TX_GAME_ERROR_OLD_OPENGL_2], mtWarning, [mbOk], 0);

  fRenderAux        := TRenderAux.Create;
  {$IFDEF USE_MAD_EXCEPT}fExceptions.LoadTranslation;{$ENDIF}

  fResource         := TResource.Create(fRender, aLS, aLT);
  fResource.LoadMenuResources(fGameSettings.Locale);

  fSoundLib         := TSoundLib.Create(fGameSettings.Locale, fGameSettings.SoundFXVolume); //Required for button click sounds
  fMusicLib         := TMusicLib.Create(fGameSettings.MusicVolume);
  fSoundLib.OnRequestFade := fMusicLib.FadeMusic;
  fSoundLib.OnRequestUnfade := fMusicLib.UnfadeMusic;

  fCampaigns        := TKMCampaignsCollection.Create;
  fCampaigns.ScanFolder(ExeDir + 'Campaigns\');
  fCampaigns.LoadProgress(ExeDir + 'Saves\Campaigns.dat');

  //If game was reinitialized from options menu then we should return there
  fMainMenuInterface := TKMMainMenuInterface.Create(fScreenX, fScreenY);
  fActiveInterface := fMainMenuInterface;

  fTimerUI := TTimer.Create(nil);
  fTimerUI.Interval := 100;
  fTimerUI.OnTimer := UpdateState;
  fTimerUI.Enabled := True;

  //Start the Music playback as soon as loading is complete
  if (not NoMusic) and not fGameSettings.MusicOff then
    fMusicLib.PlayMenuTrack;

  fMusicLib.ToggleShuffle(fGameSettings.ShuffleOn); //Determine track order
end;


procedure TKMGameApp.AfterConstruction(aReturnToOptions: Boolean);
begin
  if aReturnToOptions then
    fMainMenuInterface.ShowScreen(msOptions)
  else
    fMainMenuInterface.ShowScreen(msMain);
end;


{ Destroy what was created }
destructor TKMGameApp.Destroy;
begin
  fTimerUI.Enabled := False;

  fMusicLib.StopMusic; //Stop music imediently, so it doesn't keep playing and jerk while things closes

  FreeAndNil(fTimerUI);
  fCampaigns.SaveProgress(ExeDir + 'Saves\Campaigns.dat');
  FreeThenNil(fCampaigns);
  FreeThenNil(fGameSettings);
  FreeThenNil(fLocales);
  FreeThenNil(fMainMenuInterface);
  FreeThenNil(fResource);
  FreeThenNil(fSoundLib);
  FreeThenNil(fMusicLib);
  FreeThenNil(fTextLibrary);

  FreeThenNil(fRenderPool);
  FreeThenNil(fRenderAux);
  FreeThenNil(fRender);

  inherited;
end;


//Determine if the game can be closed without loosing any important progress
function TKMGameApp.CanClose: Boolean;
begin
  //There are no unsaved changes in Menu(gsNoGame) and in Replay.
  //In all other cases (maybe except gsOnHold?) there are potentially unsaved changes
  Result := fGameState in [gsNoGame, gsReplay];
end;


procedure TKMGameApp.ToggleLocale(aLocale: ShortString);
begin
  Assert(fGameG = nil, 'We don''t want to recreate whole fGame for that. Let''s limit it only to MainMenu');

  fGameSettings.Locale := aLocale; //Wrong Locale will be ignored

  //Release resources that use Locale info
  FreeAndNil(fNetworking);
  FreeAndNil(fCampaigns);
  fActiveInterface := nil; //Otherwise it will hold a pointer to freed memory
  FreeAndNil(fMainMenuInterface);
  FreeAndNil(fSoundLib);
  FreeAndNil(fTextLibrary);

  //Recreate resources that use Locale info
  fTextLibrary := TTextLibrary.Create(ExeDir + 'data\text\', fGameSettings.Locale);
  {$IFDEF USE_MAD_EXCEPT}fExceptions.LoadTranslation;{$ENDIF}
  fSoundLib := TSoundLib.Create(fGameSettings.Locale, fGameSettings.SoundFXVolume);
  fSoundLib.OnRequestFade := fMusicLib.FadeMusic;
  fSoundLib.OnRequestUnfade := fMusicLib.UnfadeMusic;
  fResource.ResourceFont.LoadFonts(fGameSettings.Locale);
  fCampaigns := TKMCampaignsCollection.Create; //Campaigns load text into TextLibrary
  fCampaigns.ScanFolder(ExeDir + 'Campaigns\');
  fCampaigns.LoadProgress(ExeDir + 'Saves\Campaigns.dat');
  fMainMenuInterface := TKMMainMenuInterface.Create(fScreenX, fScreenY);
  fMainMenuInterface.ShowScreen(msOptions);
  fActiveInterface := fMainMenuInterface;
  Resize(fScreenX, fScreenY); //Force the recreated main menu to resize to the user's screen
end;


procedure TKMGameApp.Resize(X,Y: Integer);
begin
  fScreenX := X;
  fScreenY := Y;
  fRender.Resize(fScreenX, fScreenY);

  //Main menu is invisible while in game, but it still exists and when we return to it
  //it must be properly sized (player could resize the screen while playing)
  if fMainMenuInterface<>nil then fMainMenuInterface.Resize(fScreenX, fScreenY);
  if fMapEditorInterface<>nil then fMapEditorInterface.Resize(fScreenX, fScreenY);
  if fGamePlayInterface<>nil then fGamePlayInterface.Resize(fScreenX, fScreenY);

  if fGame <> nil then fGame.Viewport.Resize(fScreenX, fScreenY);
end;


procedure TKMGameApp.SetGameState(aNewState: TGameState);
begin
  fGameState := aNewState;

  case fGameState of
    gsNoGame:  fActiveInterface := fMainMenuInterface;
    gsPaused,
    gsOnHold,
    gsRunning,
    gsReplay:  fActiveInterface := fGamePlayInterface;
    gsEditor:  fActiveInterface := fMapEditorInterface;
  end;
end;


procedure TKMGameApp.KeyDown(Key: Word; Shift: TShiftState);
begin
  fActiveInterface.KeyDown(Key, Shift);
end;


procedure TKMGameApp.KeyPress(Key: Char);
begin
  fActiveInterface.KeyPress(Key);
end;


procedure TKMGameApp.KeyUp(Key: Word; Shift: TShiftState);
begin
  //List of conflicting keys that we should try to avoid using in debug/game:
  //  F12 Pauses Execution and switches to debug
  //  F10 sets focus on MainMenu1
  //  F9 is the default key in Fraps for video capture
  //  F4 and F9 are used in debug to control run-flow
  //  others.. unknown

  //GLOBAL KEYS
  if Key = VK_F3 then SHOW_CONTROLS_OVERLAY := not SHOW_CONTROLS_OVERLAY;

  fActiveInterface.KeyUp(Key, Shift);
end;


procedure TKMGameApp.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fActiveInterface.MouseDown(Button,Shift,X,Y);
end;


procedure TKMGameApp.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if not InRange(X,1,fScreenX-1) or not InRange(Y,1,fScreenY-1) then exit; //Exit if Cursor is outside of frame

  //fActiveInterface = nil while loading a new locale
  if fActiveInterface <> nil then
    fActiveInterface.MouseMove(Shift, X,Y);

  if Assigned(OnCursorUpdate) then
    OnCursorUpdate(1, Format('Cursor: %.1f:%.1f [%d:%d]', [GameCursor.Float.X, GameCursor.Float.Y,
                                                           GameCursor.Cell.X, GameCursor.Cell.Y]));
end;


procedure TKMGameApp.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fActiveInterface.MouseUp(Button, Shift, X,Y);
end;


procedure TKMGameApp.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer);
begin
  fActiveInterface.MouseWheel(Shift, WheelDelta, X, Y);

  //This occurs when you use the mouse wheel on the window frame
  if (X < 0) or (Y < 0) then Exit;

  //Allow to zoom only when curor is over map. Controls handle zoom on their own
  if MOUSEWHEEL_ZOOM_ENABLE and (fGameState <> gsNoGame)
  and (fActiveInterface.MyControls.CtrlOver = nil) then
    fGame.MouseWheel(Shift, WheelDelta, X, Y);
end;


procedure TKMGameApp.UpdateState(Sender: TObject);
begin
  Inc(fGlobalTickCount);
  case fGameState of
    gsPaused:   ;
    gsOnHold:   ;
    gsNoGame:   begin
                  if fNetworking <> nil then fNetworking.UpdateState(fGlobalTickCount); //Measures pings
                  fMainMenuInterface.UpdateState(fGlobalTickCount);
                end;
    gsRunning:  begin
                  if fGame.MultiplayerMode then fNetworking.UpdateState(fGlobalTickCount); //Measures pings
                  if fGame.MultiplayerMode and (fGlobalTickCount mod 100 = 0) then
                    fGame.SendMPGameInfo(Self); //Send status to the server every 10 seconds

                  //Update minimap
                  fGamePlayInterface.UpdateState(fGlobalTickCount);
                end;
    gsReplay:   begin
                  //Update minimap
                  fGamePlayInterface.UpdateState(fGlobalTickCount);
                end;
    gsEditor:   begin
                  //Update minimap
                  fMapEditorInterface.UpdateState(fGlobalTickCount);
                end;
  end;

  //Every 1000ms
  if fGlobalTickCount mod 10 = 0 then
  begin
    //Music
    if not GlobalSettings.MusicOff and fMusicLib.IsMusicEnded then
      fMusicLib.PlayNextTrack; //Feed new music track

    //StatusBar
    if (fGameState in [gsRunning, gsReplay]) and Assigned(OnCursorUpdate) then
      OnCursorUpdate(2, 'Time: ' + FormatDateTime('hh:nn:ss', fGame.GetMissionTime));
  end;
end;


end.

unit KM_GameApp;
{$I KaM_Remake.inc}
interface
uses Windows, Classes, Controls, Dialogs, ExtCtrls, KromUtils, Math, SysUtils, TypInfo,
  KM_CommonTypes, KM_Defaults,
  KM_Campaigns, KM_Game,
  KM_InterfaceDefaults, KM_InterfaceMapEditor, KM_InterfaceGamePlay, KM_InterfaceMainMenu,
  KM_Locales, KM_Music, KM_Networking, KM_Settings, KM_TextLibrary, KM_Render;

type

    {gsNoGame,  //No game running at all, MainMenu
    gsPaused,  //Game is paused and responds to 'P' key only
    gsOnHold,  //Game is paused, shows victory options (resume, win) and responds to mouse clicks only
    gsRunning, //Game is running normally
    gsReplay,  //Game is showing replay, no player input allowed
    gsEditor); //Game is in MapEditor mode}

  //Methods relevant to gameplay
  TKMGameApp = class
  private
    fGlobalTickCount: Cardinal; //Not affected by Pause and anything (Music, Minimap, StatusBar update)
    fScreenX, fScreenY: Word;

    fCampaigns: TKMCampaignsCollection;
    fGame: TKMGame;
    fGameSettings: TGameSettings;
    fMusicLib: TMusicLib;
    fNetworking: TKMNetworking;
    fRender: TRender;
    fTimerUI: TTimer;

    fMainMenuInterface: TKMMainMenuInterface;

    procedure GameLoadingStep(const aText: string);
    procedure LoadGameAssets;
    procedure LoadGameFromSave(aFileName: string; aGameMode: TGameMode);
    procedure LoadGameFromScript(aMissionFile, aGameName: string; aCampaign: TKMCampaign; aMap: Byte; aGameMode: TGameMode);
    procedure LoadGameFromScratch(aSizeX, aSizeY: Integer; aGameMode: TGameMode);
  public
    OnCursorUpdate: TIntegerStringEvent;

    constructor Create(aHandle: HWND; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TStringEvent; NoMusic: Boolean = False);
    destructor Destroy; override;
    procedure AfterConstruction(aReturnToOptions: Boolean); reintroduce;

    procedure Stop(Msg: TGameResultMsg; TextMsg: string = '');
    function CanClose: Boolean;
    procedure Resize(X,Y: Integer);
    procedure ToggleLocale(aLocale: ShortString);
    procedure NetworkInit;
    procedure SendMPGameInfo(Sender: TObject);
    function AllowDebugRendering: Boolean;

    procedure StartCampaignMap(aCampaign: TKMCampaign; aMap: Byte);
    procedure StartSingleMap(aMissionFile, aGameName: string);
    procedure StartSingleSave(aFileName: string);
    procedure StartMultiplayerMap(const aFileName: string);
    procedure StartMultiplayerSave(const aFileName: string);
    procedure StartLastGame;
    procedure StartMapEditor(const aFileName: string; aSizeX, aSizeY: Integer);
    procedure StartReplay(const aSaveName: string);

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

    function RenderVersion: string;
    procedure Render;
    procedure PrintScreen(aFilename: string = '');

    procedure UpdateState(Sender: TObject);
    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


var
  fGameApp: TKMGameApp;


implementation
uses
  KM_GameInfo, KM_Log, KM_RenderAux, KM_Resource, KM_Sound, KM_Utils;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
constructor TKMGameApp.Create(aHandle: HWND; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TStringEvent; NoMusic: Boolean = False);
begin
  inherited Create;

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
  FreeAndNil(fNetworking);

  FreeThenNil(fRenderAux);
  FreeThenNil(fRender);

  inherited;
end;


//Determine if the game can be closed without loosing any important progress
function TKMGameApp.CanClose: Boolean;
begin
  //There are no unsaved changes in MainMenu and in Replay.
  //In all other cases (maybe except gsOnHold?) there are potentially unsaved changes
  Result := (fGame = nil) or (fGame.GameMode = gmReplay);
end;


procedure TKMGameApp.ToggleLocale(aLocale: ShortString);
begin
  Assert(fGameG = nil, 'We don''t want to recreate whole fGame for that. Let''s limit it only to MainMenu');

  fGameSettings.Locale := aLocale; //Wrong Locale will be ignored

  //Release resources that use Locale info
  FreeAndNil(fNetworking);
  FreeAndNil(fCampaigns);
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

  if fGame <> nil then fGame.Resize(fScreenX, fScreenY);
end;


procedure TKMGameApp.KeyDown(Key: Word; Shift: TShiftState);
begin
  if fGame <> nil then
    fGame.KeyDown(Key, Shift)
  else
    fMainMenuInterface.KeyDown(Key, Shift);
end;


procedure TKMGameApp.KeyPress(Key: Char);
begin
  if fGame <> nil then
    fGame.KeyPress(Key)
  else
    fMainMenuInterface.KeyPress(Key);
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

  if fGame <> nil then
    fGame.KeyUp(Key, Shift)
  else
    fMainMenuInterface.KeyUp(Key, Shift);
end;


procedure TKMGameApp.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if fGame <> nil then
    fGame.MouseDown(Button,Shift,X,Y)
  else
    fMainMenuInterface.MouseDown(Button,Shift,X,Y);
end;


procedure TKMGameApp.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if not InRange(X,1,fScreenX-1) or not InRange(Y,1,fScreenY-1) then Exit; //Exit if Cursor is outside of frame

  if fGame <> nil then
    fGame.MouseMove(Shift,X,Y)
  else
    //fMainMenuInterface = nil while loading a new locale
    if fMainMenuInterface <> nil then
      fMainMenuInterface.MouseMove(Shift, X,Y);

  if Assigned(OnCursorUpdate) then
    OnCursorUpdate(1, Format('Cursor: %.1f:%.1f [%d:%d]', [GameCursor.Float.X, GameCursor.Float.Y,
                                                           GameCursor.Cell.X, GameCursor.Cell.Y]));
end;


procedure TKMGameApp.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if fGame <> nil then
    fGame.MouseUp(Button,Shift,X,Y)
  else
    fMainMenuInterface.MouseUp(Button, Shift, X,Y);
end;


procedure TKMGameApp.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer);
begin
  if fGame <> nil then
    fGame.MouseWheel(Shift, WheelDelta, X, Y)
  else
    fMainMenuInterface.MouseWheel(Shift, WheelDelta, X, Y);
end;


procedure TKMGameApp.GameLoadingStep(const aText: String);
begin
  fMainMenuInterface.AppendLoadingText(aText);
  Render;
end;


procedure TKMGameApp.LoadGameAssets;
begin
  //Load the resources if necessary
  fMainMenuInterface.ShowScreen(msLoading, '');
  Render;

  GameLoadingStep(fTextLibrary[TX_MENU_LOADING_DEFINITIONS]);
  fResource.OnLoadingText := GameLoadingStep;
  fResource.LoadGameResources(fGameSettings.AlphaShadows);

  GameLoadingStep(fTextLibrary[TX_MENU_LOADING_INITIALIZING]);

  GameLoadingStep(fTextLibrary[TX_MENU_LOADING_SCRIPT]);
end;


//Game needs to be stopped
//1. Disconnect from network
//2. Save games replay
//3. Fill in game results
//4. Fill in menu message if needed
//5. Free the game object
//6. Switch to MainMenu
procedure TKMGameApp.Stop(Msg: TGameResultMsg; TextMsg: string='');
var CampName: string; CampMap: Byte;
begin
  if fGame = nil then Exit;

  if (fGame.GameMode = gmMulti) then
  begin
    if fNetworking.Connected then
      fNetworking.AnnounceDisconnect;
    fNetworking.Disconnect;
  end;

  //Take results from MyPlayer before data is flushed
  if Msg in [gr_Win, gr_Defeat, gr_Cancel, gr_ReplayEnd] then
    if fGame.GameMode = gmMulti then
      fMainMenuInterface.ResultsMP_Fill
    else
      fMainMenuInterface.Results_Fill;

  //If the game was a part of a campaign, select that campaign,
  //so we know which menu to show next and unlock next map
  fCampaigns.SetActive(fCampaigns.CampaignByTitle(fGame.CampaignName), fGame.CampaignMap);

  FreeThenNil(fGame);

  fLog.AppendLog('Gameplay ended - ' + GetEnumName(TypeInfo(TGameResultMsg), Integer(Msg)) + ' /' + TextMsg);

  case Msg of
    gr_Win:         if fGame.GameMode = gmMulti then
                      fMainMenuInterface.ShowScreen(msResultsMP, '', Msg)
                    else
                    begin
                      fMainMenuInterface.ShowScreen(msResults, '', Msg);
                      if fCampaigns.ActiveCampaign <> nil then
                        fCampaigns.UnlockNextMap;
                    end;
    gr_Defeat,
    gr_Cancel,
    gr_ReplayEnd:   if fGame.GameMode = gmMulti then
                      fMainMenuInterface.ShowScreen(msResultsMP, '', Msg)
                    else
                      fMainMenuInterface.ShowScreen(msResults, '', Msg);
    gr_Error:       fMainMenuInterface.ShowScreen(msError, TextMsg);
    gr_Disconnect:  fMainMenuInterface.ShowScreen(msError, TextMsg);
    gr_Silent:      ;//Used when loading new savegame from gameplay UI
    gr_MapEdEnd:    fMainMenuInterface.ShowScreen(msMain);
  end;
end;


procedure TKMGameApp.LoadGameFromSave(aFileName: string; aGameMode: TGameMode);
var
  LoadError: string;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  fGame := TKMGame.Create(aGameMode, fRender, fNetworking);
  try
    fGame.Load(aFileName);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], [aFileName])+'||'+E.ClassName+': '+E.Message;
      Stop(gr_Error, LoadError);
      fLog.AppendLog('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  if Assigned(OnCursorUpdate) then OnCursorUpdate(0, 'Map size: '+inttostr(fGame.MapX)+' x '+inttostr(fGame.MapY));
end;


procedure TKMGameApp.LoadGameFromScript(aMissionFile, aGameName: string; aCampaign: TKMCampaign; aMap: Byte; aGameMode: TGameMode);
var
  LoadError: string;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  fGame := TKMGame.Create(aGameMode, fRender, fNetworking);
  try
    fGame.GameStart(aMissionFile, aGameName, aCampaign.ShortTitle, aMap);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], [aMissionFile])+'||'+E.ClassName+': '+E.Message;
      Stop(gr_Error, LoadError);
      fLog.AppendLog('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  //todo: Do that in mission scripts! Hack to block the market in TSK/TPR campaigns
  {if (aCampaign.ShortTitle = 'TSK') or (aCampaign.ShortTitle = 'TPR') then
    for I:=0 to fPlayers.Count-1 do
      fPlayers[I].Stats.HouseBlocked[ht_Marketplace] := True;}

  if Assigned(OnCursorUpdate) then OnCursorUpdate(0, 'Map size: '+inttostr(fGame.MapX)+' x '+inttostr(fGame.MapY));
end;


procedure TKMGameApp.LoadGameFromScratch(aSizeX, aSizeY: Integer; aGameMode: TGameMode);
var
  LoadError: string;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  fGame := TKMGame.Create(gmMapEd, fRender, nil);
  try
    fGame.GameStart(aSizeX, aSizeY);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], ['-'])+'||'+E.ClassName+': '+E.Message;
      Stop(gr_Error, LoadError);
      fLog.AppendLog('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  //todo: Do that in mission scripts! Hack to block the market in TSK/TPR campaigns
  {if (aCampaign.ShortTitle = 'TSK') or (aCampaign.ShortTitle = 'TPR') then
    for I:=0 to fPlayers.Count-1 do
      fPlayers[I].Stats.HouseBlocked[ht_Marketplace] := True;}

  if Assigned(OnCursorUpdate) then OnCursorUpdate(0, 'Map size: '+inttostr(fGame.MapX)+' x '+inttostr(fGame.MapY));
end;


procedure TKMGameApp.StartCampaignMap(aCampaign: TKMCampaign; aMap: Byte);
begin
  LoadGameFromScript(aCampaign.MissionFile(aMap), aCampaign.MissionTitle(aMap), aCampaign, aMap, gmSingle);
end;


procedure TKMGameApp.StartSingleMap(aMissionFile, aGameName: string);
begin
  LoadGameFromScript(aMissionFile, aGameName, nil, 0, gmSingle);
end;


procedure TKMGameApp.StartSingleSave(aFileName: string);
begin
  LoadGameFromSave(aFileName, gmSingle);
end;


procedure TKMGameApp.StartMultiplayerMap(const aFileName: string);
begin
  LoadGameFromScript(aFileName, aFileName, nil, 0, gmMulti);
end;


procedure TKMGameApp.StartMultiplayerSave(const aFileName: string);
begin
  LoadGameFromSave(aFileName, gmMulti);
end;


procedure TKMGameApp.StartLastGame;
begin
  //@Lewin: Overall "basesave" is outdated and we can discard it.
  //Earlier it was used for "View last replay" mechanism, but now as we have
  //replay manager the only use for it is RestartLastMap which can be made
  //just by loading current map .bas file (or reload mission?)
  //The only other use is saving RPL info on crash,
  //which certainly can go to latest autosave.rpl

  //There are two options to restart a map:
  //1. Restart mission script
  //   - files may be gone
  //   + mission may be intentionally updated by mapmaker
  //2. Load savegame.bas
  //   - mission may be intentionally updated by mapmaker
  //   + savegame.bas always exists
  //In my opinion both have their advantages and weak points

  //@Krom: I agree basesave can be discarded. I suggest that we create a
  //"crashsave" when generating the crash report because that was the only
  //really useful feature of baseave (I used it in crash reports)
  //I don't have a strong opinion between 1. and 2. I think I prefer 1.

  //LoadGameFromSave(aFileName, gmMulti);
end;


procedure TKMGameApp.StartMapEditor(const aFileName: string; aSizeX, aSizeY: Integer);
begin
  if aFileName <> '' then
    LoadGameFromScript(aFileName, TruncateExt(ExtractFileName(aFileName)), nil, 0, gmMapEd)
  else
    LoadGameFromScratch(aSizeX, aSizeY, gmMapEd);
end;


procedure TKMGameApp.StartReplay(const aSaveName: string);
begin
  LoadGameFromSave(aSaveName, gmReplay);
end;


procedure TKMGameApp.NetworkInit;
begin
  if fNetworking = nil then
    fNetworking := TKMNetworking.Create(fGameSettings.MasterServerAddress,
                                        fGameSettings.AutoKickTimeout,
                                        fGameSettings.PingInterval,
                                        fGameSettings.MasterAnnounceInterval,
                                        fGameSettings.Locale);
  fNetworking.OnMPGameInfoChanged := SendMPGameInfo;
end;


//Called by fNetworking to access MissionTime/GameName
procedure TKMGameApp.SendMPGameInfo(Sender: TObject);
begin
  fNetworking.SendMPGameInfo(fGame.GetMissionTime, fGame.GameName);
end;


//Debug rendering may be used as a cheat in MP to see unrevealed areas, thats why we block it there
function TKMGameApp.AllowDebugRendering: Boolean;
begin
  Result := (fGame.GameMode in [gmSingle, gmMapEd, gmReplay]) or (MULTIPLAYER_CHEATS and (fGame.GameMode = gmMulti));
end;


procedure TKMGameApp.Render;
begin
  if SKIP_RENDER then Exit;
  if fRender.Blind then Exit;

  fRender.BeginFrame;

  if fGame <> nil then
    fGame.Render(fRender)
  else
    fMainMenuInterface.Paint;

  fRender.RenderBrightness(GlobalSettings.Brightness);

  fRender.EndFrame;
end;


function TKMGameApp.RenderVersion: string;
begin
  Result := 'OpenGL '+ fRender.RendererVersion;
end;


procedure TKMGameApp.PrintScreen(aFilename: string = '');
var
  s: string;
begin
  if aFilename = '' then
  begin
    DateTimeToString(s, 'yyyy-mm-dd hh-nn-ss', Now); //2007-12-23 15-24-33
    fRender.DoPrintScreen(ExeDir + 'KaM ' + s + '.jpg');
  end
  else
    fRender.DoPrintScreen(aFilename);
end;


procedure TKMGameApp.UpdateState(Sender: TObject);
begin
  Inc(fGlobalTickCount);
  if fGame <> nil then
  begin
    fGame.UpdateState(fGlobalTickCount);
    if (fGame.GameMode = gmMulti) and (fGlobalTickCount mod 100 = 0) then
      SendMPGameInfo(Self); //Send status to the server every 10 seconds
  end
  else
  begin
    if fNetworking <> nil then fNetworking.UpdateState(fGlobalTickCount); //Measures pings
    fMainMenuInterface.UpdateState(fGlobalTickCount);
  end;

  //Every 1000ms
  if fGlobalTickCount mod 10 = 0 then
  begin
    //Music
    if not GlobalSettings.MusicOff and fMusicLib.IsMusicEnded then
      fMusicLib.PlayNextTrack; //Feed new music track

    //StatusBar
    if (fGame <> nil) and not fGame.IsPaused and Assigned(OnCursorUpdate) then
      OnCursorUpdate(2, 'Time: ' + FormatDateTime('hh:nn:ss', fGame.GetMissionTime));
  end;
end;


//This is our real-time "thread", use it wisely
procedure TKMGameApp.UpdateStateIdle(aFrameTime: Cardinal);
begin
  if fGame <> nil then
    fGame.UpdateStateIdle(aFrameTime);

  if fMusicLib <> nil then fMusicLib.UpdateStateIdle;
  if fSoundLib <> nil then fSoundLib.UpdateStateIdle;
  if fNetworking <> nil then fNetworking.UpdateStateIdle;
end;


end.

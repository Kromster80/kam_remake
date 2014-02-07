unit KM_GameApp;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Controls, Dialogs, ExtCtrls, KromUtils, Math, SysUtils, TypInfo,
  KM_CommonTypes, KM_Defaults, KM_RenderControl,
  KM_Campaigns, KM_Game, KM_InterfaceMainMenu, KM_InterfaceDefaults,
  KM_Music, KM_Networking, KM_Settings, KM_ResTexts, KM_Render;

type
  //Methods relevant to gameplay
  TKMGameApp = class
  private
    fGlobalTickCount: Cardinal;
    fIsExiting: Boolean;

    fCampaigns: TKMCampaignsCollection;
    fGameSettings: TGameSettings;
    fMusicLib: TMusicLib;
    fNetworking: TKMNetworking;
    fRender: TRender;
    fTimerUI: TTimer;
    fMainMenuInterface: TKMMainMenuInterface;

    fOnCursorUpdate: TIntegerStringEvent;

    procedure GameLoadingStep(const aText: UnicodeString);
    procedure LoadGameAssets;
    procedure LoadGameFromSave(aFilePath: UnicodeString; aGameMode: TGameMode);
    procedure LoadGameFromScript(aMissionFile, aGameName: UnicodeString; aCampaignName: TKMCampaignId; aMap: Byte; aGameMode: TGameMode; aDesiredLoc: ShortInt; aDesiredColor: Cardinal);
    procedure LoadGameFromScratch(aSizeX, aSizeY: Integer; aGameMode: TGameMode);
    function SaveName(const aName, aExt: UnicodeString; aMultiPlayer: Boolean): UnicodeString;
  public
    constructor Create(aRenderControl: TKMRenderControl; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TUnicodeStringEvent; aOnCursorUpdate: TIntegerStringEvent; NoMusic: Boolean = False);
    destructor Destroy; override;
    procedure AfterConstruction(aReturnToOptions: Boolean); reintroduce;

    procedure Stop(aMsg: TGameResultMsg; aTextMsg: UnicodeString = '');
    procedure StopGameReturnToLobby(aSender: TObject);
    function CanClose: Boolean;
    procedure Resize(X,Y: Integer);
    procedure ToggleLocale(aLocale: AnsiString);
    procedure NetworkInit;
    procedure SendMPGameInfo(Sender: TObject);
    function RenderVersion: UnicodeString;
    procedure PrintScreen(aFilename: UnicodeString = '');
    procedure PauseMusicToPlayFile(aFileName: UnicodeString);
    function CheckDATConsistency: Boolean;

    //These are all different game kinds we can start
    procedure NewCampaignMap(aCampaign: TKMCampaign; aMap: Byte);
    procedure NewSingleMap(aMissionFile, aGameName: UnicodeString; aDesiredLoc: ShortInt = -1; aDesiredColor: Cardinal = $00000000);
    procedure NewSingleSave(aSaveName: UnicodeString);
    procedure NewMultiplayerMap(const aFileName: UnicodeString; Spectating: Boolean);
    procedure NewMultiplayerSave(const aSaveName: UnicodeString; Spectating: Boolean);
    procedure NewRestartLast(aGameName, aMission, aSave: UnicodeString; aCampName: TKMCampaignId; aCampMap: Byte; aLocation: Byte; aColor: Cardinal);
    procedure NewEmptyMap(aSizeX, aSizeY: Integer);
    procedure NewMapEditor(const aFileName: UnicodeString; aSizeX, aSizeY: Integer);
    procedure NewReplay(const aFilePath: UnicodeString);

    property Campaigns: TKMCampaignsCollection read fCampaigns;
    function Game: TKMGame;
    property GameSettings: TGameSettings read fGameSettings;
    property MainMenuInterface: TKMMainMenuInterface read fMainMenuInterface;
    property MusicLib: TMusicLib read fMusicLib;
    property Networking: TKMNetworking read fNetworking;
    property GlobalTickCount: Cardinal read fGlobalTickCount;

    procedure KeyDown(Key: Word; Shift: TShiftState);
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
    procedure FPSMeasurement(aFPS: Cardinal);

    procedure Render(aForPrintScreen: Boolean);
    procedure UpdateState(Sender: TObject);
    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


var
  fGameApp: TKMGameApp;


implementation
uses
  KM_Log, KM_Main, KM_GameCursor,
  {$IFDEF USE_MAD_EXCEPT} KM_Exceptions, {$ENDIF}
  KM_Maps, KM_Resource, KM_Sound, KM_Utils;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
constructor TKMGameApp.Create(aRenderControl: TKMRenderControl; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TUnicodeStringEvent; aOnCursorUpdate: TIntegerStringEvent; NoMusic: Boolean = False);
begin
  inherited Create;

  fOnCursorUpdate := aOnCursorUpdate;

  fGameSettings := TGameSettings.Create;

  fRender       := TRender.Create(aRenderControl, aScreenX, aScreenY, aVSync);

  gResource := TKMResource.Create(fRender, aLS, aLT);
  gResource.LoadMainResources(fGameSettings.Locale);

  {$IFDEF USE_MAD_EXCEPT}fExceptions.LoadTranslation;{$ENDIF}

  //Show the message if user has old OpenGL drivers (pre-1.4)
  if fRender.IsOldGLVersion then
    //MessageDlg works better than Application.MessageBox or others, it stays on top and
    //pauses here until the user clicks ok.
    MessageDlg(gResTexts[TX_GAME_ERROR_OLD_OPENGL] + EolW + EolW + gResTexts[TX_GAME_ERROR_OLD_OPENGL_2], mtWarning, [mbOk], 0);

  gSoundPlayer  := TKMSoundPlayer.Create(fGameSettings.SoundFXVolume);
  fMusicLib     := TMusicLib.Create(fGameSettings.MusicVolume);
  gSoundPlayer.OnRequestFade   := fMusicLib.FadeMusic;
  gSoundPlayer.OnRequestUnfade := fMusicLib.UnfadeMusic;

  fCampaigns    := TKMCampaignsCollection.Create;
  fCampaigns.ScanFolder(ExeDir + 'Campaigns' + PathDelim);
  fCampaigns.LoadProgress(ExeDir + 'Saves' + PathDelim + 'Campaigns.dat');

  //If game was reinitialized from options menu then we should return there
  fMainMenuInterface := TKMMainMenuInterface.Create(aScreenX, aScreenY);

  fTimerUI := TTimer.Create(nil);
  fTimerUI.Interval := 100;
  fTimerUI.OnTimer  := UpdateState;
  fTimerUI.Enabled  := True;

  //Start the Music playback as soon as loading is complete
  if (not NoMusic) and not fGameSettings.MusicOff then
    fMusicLib.PlayMenuTrack;

  fMusicLib.ToggleShuffle(fGameSettings.ShuffleOn); //Determine track order
end;


procedure TKMGameApp.AfterConstruction(aReturnToOptions: Boolean);
begin
  if aReturnToOptions then
    fMainMenuInterface.PageChange(gpOptions)
  else
    fMainMenuInterface.PageChange(gpMainMenu);
end;


{ Destroy what was created }
destructor TKMGameApp.Destroy;
begin
  //Freeing network sockets and OpenAL can result in events like Resize/Paint occuring (seen in crash reports)
  //Set fIsExiting so we know to skip them
  fIsExiting := True;

  //We might have crashed part way through .Create, so we can't assume ANYTHING exists here.
  //Doing so causes a 2nd exception which overrides 1st. Hence check <> nil on everything except Free (TObject.Free does that already)

  if fTimerUI <> nil then fTimerUI.Enabled := False;
  //Stop music imediently, so it doesn't keep playing and jerk while things closes
  if fMusicLib <> nil then fMusicLib.StopMusic;

  Stop(gr_Silent);

  FreeAndNil(fTimerUI);
  if fCampaigns <> nil then fCampaigns.SaveProgress(ExeDir + 'Saves' + PathDelim + 'Campaigns.dat');
  FreeThenNil(fCampaigns);
  FreeThenNil(fGameSettings);
  FreeThenNil(fMainMenuInterface);
  FreeThenNil(gResource);
  FreeThenNil(gSoundPlayer);
  FreeThenNil(fMusicLib);
  FreeThenNil(gResTexts);
  FreeAndNil(fNetworking);

  FreeThenNil(fRender);

  inherited;
end;


//Determine if the game can be closed without loosing any important progress
function TKMGameApp.CanClose: Boolean;
begin
  //There are no unsaved changes in MainMenu and in Replays
  //In all other cases (maybe except gsOnHold?) there are potentially unsaved changes
  Result := (gGame = nil) or (gGame.GameMode in [gmReplaySingle, gmReplayMulti]);
end;


procedure TKMGameApp.ToggleLocale(aLocale: AnsiString);
begin
  Assert(gGame = nil, 'We don''t want to recreate whole fGame for that. Let''s limit it only to MainMenu');

  fMainMenuInterface.PageChange(gpLoading, gResTexts[TX_MENU_NEW_LOCALE]);
  Render(False); //Force to repaint information screen

  fTimerUI.Enabled := False; //Disable it while switching, if an OpenAL error appears the timer should be disabled
  fGameSettings.Locale := aLocale; //Wrong Locale will be ignored

  //Release resources that use Locale info
  FreeAndNil(fNetworking);
  FreeAndNil(fCampaigns);
  FreeAndNil(fMainMenuInterface);

  //Recreate resources that use Locale info
  gResource.LoadLocaleResources(fGameSettings.Locale);

  {$IFDEF USE_MAD_EXCEPT}fExceptions.LoadTranslation;{$ENDIF}

  //Campaigns use single locale
  fCampaigns := TKMCampaignsCollection.Create;
  fCampaigns.ScanFolder(ExeDir + 'Campaigns' + PathDelim);
  fCampaigns.LoadProgress(ExeDir + 'Saves' + PathDelim + 'Campaigns.dat');
  fMainMenuInterface := TKMMainMenuInterface.Create(fRender.ScreenX, fRender.ScreenY);
  fMainMenuInterface.PageChange(gpOptions);
  Resize(fRender.ScreenX, fRender.ScreenY); //Force the recreated main menu to resize to the user's screen
  fTimerUI.Enabled := True; //Safe to enable the timer again
end;


procedure TKMGameApp.Resize(X,Y: Integer);
begin
  if fIsExiting then Exit;
  fRender.Resize(X, Y);

  //Main menu is invisible while in game, but it still exists and when we return to it
  //it must be properly sized (player could resize the screen while playing)
  fMainMenuInterface.Resize(X, Y);

  if gGame <> nil then gGame.ActiveInterface.Resize(X, Y);
end;


procedure TKMGameApp.KeyDown(Key: Word; Shift: TShiftState);
begin
  if gGame <> nil then
    gGame.ActiveInterface.KeyDown(Key, Shift)
  else
    fMainMenuInterface.KeyDown(Key, Shift);
end;


procedure TKMGameApp.KeyPress(Key: Char);
begin
  if gGame <> nil then
    gGame.ActiveInterface.KeyPress(Key)
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
  if DEBUG_CHEATS and (Key = VK_F12) then SHOW_CONTROLS_OVERLAY := not SHOW_CONTROLS_OVERLAY;

  if gGame <> nil then
    gGame.ActiveInterface.KeyUp(Key, Shift)
  else
    fMainMenuInterface.KeyUp(Key, Shift);
end;


procedure TKMGameApp.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if gGame <> nil then
    gGame.ActiveInterface.MouseDown(Button,Shift,X,Y)
  else
    fMainMenuInterface.MouseDown(Button,Shift,X,Y);
end;


procedure TKMGameApp.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if not InRange(X, 1, fRender.ScreenX - 1)
  or not InRange(Y, 1, fRender.ScreenY - 1) then
    Exit; // Exit if Cursor is outside of frame

  if gGame <> nil then
    gGame.ActiveInterface.MouseMove(Shift,X,Y)
  else
    //fMainMenuInterface = nil while loading a new locale
    if fMainMenuInterface <> nil then
      fMainMenuInterface.MouseMove(Shift, X,Y);

  if Assigned(fOnCursorUpdate) then
    fOnCursorUpdate(1, Format('Cursor: %.1f:%.1f [%d:%d]', [GameCursor.Float.X, GameCursor.Float.Y,
                                                            GameCursor.Cell.X, GameCursor.Cell.Y]));
end;


procedure TKMGameApp.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if gGame <> nil then
    gGame.ActiveInterface.MouseUp(Button,Shift,X,Y)
  else
    fMainMenuInterface.MouseUp(Button, Shift, X,Y);
end;


procedure TKMGameApp.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer);
begin
  if gGame <> nil then
    gGame.ActiveInterface.MouseWheel(Shift, WheelDelta, X, Y)
  else
    fMainMenuInterface.MouseWheel(Shift, WheelDelta, X, Y);
end;


procedure TKMGameApp.FPSMeasurement(aFPS: Cardinal);
begin
  if fNetworking <> nil then fNetworking.FPSMeasurement(aFPS);
end;


function TKMGameApp.Game: TKMGame;
begin
  Result := gGame;
end;


procedure TKMGameApp.GameLoadingStep(const aText: UnicodeString);
begin
  fMainMenuInterface.AppendLoadingText(aText);
  Render(False);
end;


procedure TKMGameApp.LoadGameAssets;
begin
  //Load the resources if necessary
  fMainMenuInterface.PageChange(gpLoading);
  Render(False);

  GameLoadingStep(gResTexts[TX_MENU_LOADING_DEFINITIONS]);
  gResource.OnLoadingText := GameLoadingStep;
  gResource.LoadGameResources(fGameSettings.AlphaShadows);

  GameLoadingStep(gResTexts[TX_MENU_LOADING_INITIALIZING]);

  GameLoadingStep(gResTexts[TX_MENU_LOADING_SCRIPT]);
end;


//Game needs to be stopped
//1. Disconnect from network
//2. Save games replay
//3. Fill in game results
//4. Fill in menu message if needed
//5. Free the game object
//6. Switch to MainMenu
procedure TKMGameApp.Stop(aMsg: TGameResultMsg; aTextMsg: UnicodeString='');
begin
  if gGame = nil then Exit;

  if gGame.IsMultiplayer then
  begin
    if fNetworking.Connected then
      fNetworking.AnnounceDisconnect;
    fNetworking.Disconnect;
  end;

  case aMsg of
    gr_Win, gr_Defeat, gr_Cancel, gr_ReplayEnd:
                    begin
                      //If the game was a part of a campaign, select that campaign,
                      //so we know which menu to show next and unlock next map
                      fCampaigns.SetActive(fCampaigns.CampaignById(gGame.CampaignName), gGame.CampaignMap);

                      if (gGame.GameMode in [gmMulti, gmMultiSpectate, gmReplayMulti]) or MP_RESULTS_IN_SP then
                        fMainMenuInterface.ShowResultsMP(aMsg)
                      else
                        fMainMenuInterface.ShowResultsSP(aMsg);

                      if (aMsg = gr_Win) and (fCampaigns.ActiveCampaign <> nil) then
                        fCampaigns.UnlockNextMap;
                    end;
    gr_Error, gr_Disconnect:
                    fMainMenuInterface.PageChange(gpError, aTextMsg);
    gr_Silent:      ;//Used when loading new savegame from gameplay UI
    gr_MapEdEnd:    fMainMenuInterface.PageChange(gpMainMenu);
  end;

  FreeThenNil(gGame);
  gLog.AddTime('Gameplay ended - ' + GetEnumName(TypeInfo(TGameResultMsg), Integer(aMsg)) + ' /' + aTextMsg);
end;


procedure TKMGameApp.StopGameReturnToLobby(aSender: TObject);
var ChatText, ChatMessages: UnicodeString;
begin
  if gGame = nil then Exit;

  //Copy text from in-game chat to lobby (save it before freeing gGame)
  ChatText := gGame.GameplayInterface.GetChatText;
  ChatMessages := gGame.GameplayInterface.GetChatMessages;

  if fNetworking.IsHost then
    gGame.Save(RETURN_TO_LOBBY_SAVE, UTCNow);
  FreeThenNil(gGame);

  fNetworking.ReturnToLobby; //Clears gGame event pointers from Networking
  fMainMenuInterface.ReturnToLobby(RETURN_TO_LOBBY_SAVE); //Assigns Lobby event pointers to Networking and selects map
  if fNetworking.IsHost then
    fNetworking.SendPlayerListAndRefreshPlayersSetup; //Call now that events are attached to lobby

  //Copy text from in-game chat to lobby
  fMainMenuInterface.SetChatText(ChatText);
  fMainMenuInterface.SetChatMessages(ChatMessages);

  gLog.AddTime('Gameplay ended - Return to lobby');
end;


procedure TKMGameApp.LoadGameFromSave(aFilePath: UnicodeString; aGameMode: TGameMode);
var
  LoadError: UnicodeString;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  //Reset controls if MainForm exists (KMR could be run without main form)
  if fMain <> nil then
    fMain.FormMain.ControlsReset;

  gGame := TKMGame.Create(aGameMode, fRender, fNetworking);
  try
    gGame.Load(aFilePath);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(gResTexts[TX_MENU_PARSE_ERROR], [aFilePath])+'||'+E.ClassName+': '+E.Message;
      Stop(gr_Error, LoadError);
      gLog.AddTime('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  if Assigned(fOnCursorUpdate) then
    fOnCursorUpdate(0, gGame.MapSizeInfo);
end;


procedure TKMGameApp.LoadGameFromScript(aMissionFile, aGameName: UnicodeString; aCampaignName: TKMCampaignId; aMap: Byte; aGameMode: TGameMode; aDesiredLoc: ShortInt; aDesiredColor: Cardinal);
var
  LoadError: UnicodeString;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  //Reset controls if MainForm exists (KMR could be run without main form)
  if fMain <> nil then
    fMain.FormMain.ControlsReset;

  gGame := TKMGame.Create(aGameMode, fRender, fNetworking);
  try
    gGame.GameStart(aMissionFile, aGameName, aCampaignName, aMap, aDesiredLoc, aDesiredColor);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(gResTexts[TX_MENU_PARSE_ERROR], [aMissionFile])+'||'+E.ClassName+': '+E.Message;
      Stop(gr_Error, LoadError);
      gLog.AddTime('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  if Assigned(fOnCursorUpdate) then
    fOnCursorUpdate(0, gGame.MapSizeInfo);
end;


procedure TKMGameApp.LoadGameFromScratch(aSizeX, aSizeY: Integer; aGameMode: TGameMode);
var
  LoadError: string;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  //Reset controls if MainForm exists (KMR could be run without main form)
  if fMain <> nil then
    fMain.FormMain.ControlsReset;

  gGame := TKMGame.Create(aGameMode, fRender, nil);
  try
    gGame.GameStart(aSizeX, aSizeY);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(gResTexts[TX_MENU_PARSE_ERROR], ['-'])+'||'+E.ClassName+': '+E.Message;
      Stop(gr_Error, LoadError);
      gLog.AddTime('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  if Assigned(fOnCursorUpdate) then
    fOnCursorUpdate(0, gGame.MapSizeInfo);
end;


procedure TKMGameApp.NewCampaignMap(aCampaign: TKMCampaign; aMap: Byte);
begin
  LoadGameFromScript(aCampaign.MissionFile(aMap), aCampaign.MissionTitle(aMap), aCampaign.CampaignId, aMap, gmSingle, -1, 0);
end;


procedure TKMGameApp.NewSingleMap(aMissionFile, aGameName: UnicodeString; aDesiredLoc: ShortInt = -1; aDesiredColor: Cardinal = $00000000);
begin
  LoadGameFromScript(aMissionFile, aGameName, NO_CAMPAIGN, 0, gmSingle, aDesiredLoc, aDesiredColor);
end;


procedure TKMGameApp.NewSingleSave(aSaveName: UnicodeString);
begin
  //Convert SaveName to local FilePath
  LoadGameFromSave(SaveName(aSaveName, 'sav', False), gmSingle);
end;


procedure TKMGameApp.NewMultiplayerMap(const aFileName: UnicodeString; Spectating: Boolean);
var GameMode: TGameMode;
begin
  if Spectating then
    GameMode := gmMultiSpectate
  else
    GameMode := gmMulti;
  LoadGameFromScript(TKMapsCollection.FullPath(aFileName, '.dat', True), aFileName, NO_CAMPAIGN, 0, GameMode, 0, 0);

  //Starting the game might have failed (e.g. fatal script error)
  if gGame <> nil then
  begin
    //Copy text from lobby to in-game chat
    gGame.GamePlayInterface.SetChatText(fMainMenuInterface.GetChatText);
    gGame.GamePlayInterface.SetChatMessages(fMainMenuInterface.GetChatMessages);
  end;
end;


procedure TKMGameApp.NewMultiplayerSave(const aSaveName: UnicodeString; Spectating: Boolean);
var GameMode: TGameMode;
begin
  if Spectating then
    GameMode := gmMultiSpectate
  else
    GameMode := gmMulti;
  //Convert SaveName to local FilePath
  //aFileName is the same for all players, but Path to it is different
  LoadGameFromSave(SaveName(aSaveName, 'sav', True), GameMode);

  //Copy the chat and typed lobby message to the in-game chat
  gGame.GamePlayInterface.SetChatText(fMainMenuInterface.GetChatText);
  gGame.GamePlayInterface.SetChatMessages(fMainMenuInterface.GetChatMessages);
end;


procedure TKMGameApp.NewRestartLast(aGameName, aMission, aSave: UnicodeString; aCampName: TKMCampaignId; aCampMap: Byte; aLocation: Byte; aColor: Cardinal);
begin
  if FileExists(ExeDir + aMission) then
    LoadGameFromScript(ExeDir + aMission, aGameName, aCampName, aCampMap, gmSingle, aLocation, aColor)
  else
  if FileExists(ChangeFileExt(ExeDir + aSave, '.bas')) then
    LoadGameFromSave(ChangeFileExt(ExeDir + aSave, '.bas'), gmSingle)
  else
    fMainMenuInterface.PageChange(gpError, 'Can not repeat last mission');
end;


procedure TKMGameApp.NewEmptyMap(aSizeX, aSizeY: Integer);
begin
  LoadGameFromScratch(aSizeX, aSizeY, gmSingle);
end;


procedure TKMGameApp.NewMapEditor(const aFileName: UnicodeString; aSizeX, aSizeY: Integer);
begin
  if aFileName <> '' then
    LoadGameFromScript(aFileName, TruncateExt(ExtractFileName(aFileName)), NO_CAMPAIGN, 0, gmMapEd, 0, 0)
  else
    LoadGameFromScratch(aSizeX, aSizeY, gmMapEd);
end;


procedure TKMGameApp.NewReplay(const aFilePath: UnicodeString);
begin
  Assert(ExtractFileExt(aFilePath) = '.bas');
  LoadGameFromSave(aFilePath, gmReplaySingle); //Will be changed to gmReplayMulti depending on save contents
end;


function TKMGameApp.SaveName(const aName, aExt: UnicodeString; aMultiPlayer: Boolean): UnicodeString;
begin
  if aMultiPlayer then
    Result := ExeDir + 'SavesMP' + PathDelim + aName + '.' + aExt
  else
    Result := ExeDir + 'Saves' + PathDelim + aName + '.' + aExt;
end;


procedure TKMGameApp.NetworkInit;
begin
  if fNetworking = nil then
    fNetworking := TKMNetworking.Create(fGameSettings.MasterServerAddress,
                                        fGameSettings.AutoKickTimeout,
                                        fGameSettings.PingInterval,
                                        fGameSettings.MasterAnnounceInterval);
  fNetworking.OnMPGameInfoChanged := SendMPGameInfo;
  fNetworking.OnStartMap := NewMultiplayerMap;
  fNetworking.OnStartSave := NewMultiplayerSave;
  fNetworking.OnReturnToLobby := StopGameReturnToLobby;
end;


//Called by fNetworking to access MissionTime/GameName if they are valid
//fNetworking knows nothing about fGame
procedure TKMGameApp.SendMPGameInfo(Sender: TObject);
begin
  if gGame <> nil then
    fNetworking.AnnounceGameInfo(gGame.MissionTime, gGame.GameName)
  else
    fNetworking.AnnounceGameInfo(-1, ''); //fNetworking will fill the details from lobby
end;


procedure TKMGameApp.Render(aForPrintScreen: Boolean);
begin
  if SKIP_RENDER then Exit;
  if fIsExiting then Exit;
  if fRender.Blind then Exit;
  if not fTimerUI.Enabled then Exit; //Don't render while toggling locale

  fRender.BeginFrame;

  if gGame <> nil then
    gGame.Render(fRender)
  else
    fMainMenuInterface.Paint;

  fRender.RenderBrightness(GameSettings.Brightness);

  if SHOW_SEL_BUFFER and (gGame <> nil) then
    //Color-code render result assigned to GameCursor.ObjectId
    gGame.RenderSelection;

  fRender.EndFrame;

  //Selection buffer
  if not aForPrintScreen and (gGame <> nil) then
  begin
    //Clear buffer
    fRender.BeginFrame;

    //Color-code render result assigned to GameCursor.ObjectId
    gGame.RenderSelection;

    if Assigned(fOnCursorUpdate) then
      fOnCursorUpdate(4, 'Objects: ' + IntToStr(GameCursor.ObjectUID));
  end;
end;


function TKMGameApp.RenderVersion: UnicodeString;
begin
  Result := 'OpenGL ' + fRender.RendererVersion;
end;


procedure TKMGameApp.PrintScreen(aFilename: UnicodeString = '');
var
  s: string;
begin
  Render(True);
  if aFilename = '' then
  begin
    DateTimeToString(s, 'yyyy-mm-dd hh-nn-ss', Now); //2007-12-23 15-24-33
    fRender.DoPrintScreen(ExeDir + 'KaM ' + s + '.jpg');
  end
  else
    fRender.DoPrintScreen(aFilename);
end;


procedure TKMGameApp.PauseMusicToPlayFile(aFileName: UnicodeString);
begin
  if not FileExists(aFileName) then Exit;
  gSoundPlayer.AbortAllFadeSounds; //Victory/defeat sounds also fade music, so stop those in the rare chance they might still be playing
  fMusicLib.PauseMusicToPlayFile(aFileName, fGameSettings.SoundFXVolume);
end;


function TKMGameApp.CheckDATConsistency: Boolean;
begin
  Result := ALLOW_MP_MODS or (gResource.GetDATCRC = $4F5458E6); //That's the magic CRC of official .dat files
end;


procedure TKMGameApp.UpdateState(Sender: TObject);
begin
  //Some PCs seem to change 8087CW randomly between events like Timers and OnMouse*,
  //so we need to set it right before we do game logic processing
  Set8087CW($133F);

  Inc(fGlobalTickCount);
  //Always update networking for auto reconnection and query timeouts
  if fNetworking <> nil then fNetworking.UpdateState(fGlobalTickCount);
  if gGame <> nil then
  begin
    gGame.UpdateState(fGlobalTickCount);
    if gGame.IsMultiplayer and (fGlobalTickCount mod 100 = 0) then
      SendMPGameInfo(Self); //Send status to the server every 10 seconds
  end
  else
    fMainMenuInterface.UpdateState(fGlobalTickCount);

  //Every 1000ms
  if fGlobalTickCount mod 10 = 0 then
  begin
    //Music
    if not GameSettings.MusicOff and fMusicLib.IsMusicEnded then
      fMusicLib.PlayNextTrack; //Feed new music track

    //StatusBar
    if (gGame <> nil) and not gGame.IsPaused and Assigned(fOnCursorUpdate) then
        fOnCursorUpdate(2, 'Time: ' + TimeToString(gGame.MissionTime));
  end;
end;


//This is our real-time "thread", use it wisely
procedure TKMGameApp.UpdateStateIdle(aFrameTime: Cardinal);
begin
  if gGame <> nil then
    gGame.UpdateStateIdle(aFrameTime);

  if fMusicLib <> nil then fMusicLib.UpdateStateIdle;
  if gSoundPlayer <> nil then gSoundPlayer.UpdateStateIdle;
  if fNetworking <> nil then fNetworking.UpdateStateIdle;
end;


end.

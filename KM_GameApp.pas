unit KM_GameApp;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Controls, Dialogs, ExtCtrls, KromUtils, Math, SysUtils, TypInfo,
  KM_CommonTypes, KM_Defaults, KM_RenderControl,
  KM_Campaigns, KM_Game,
  KM_InterfaceMainMenu,
  KM_Locales, KM_Music, KM_Networking, KM_Settings, KM_TextLibrary, KM_Render;

type
  //Methods relevant to gameplay
  TKMGameApp = class
  private
    fGlobalTickCount: Cardinal;

    //Story behind these seemingly superflous elements that
    //we need to carry on from previous Game:
    //- Mission scropt does not knows GameName
    //- Mission does not knows to which CampaignName/Map it belongs
    //- PathName to mission and savegame (incase mission is missing we can load .bas)
    fRepeatGameName: string;
    fRepeatMission: string;
    fRepeatSave: string;
    fRepeatCampName: AnsiString;
    fRepeatCampMap: Byte;
    fRepeatLocation: Byte;
    fRepeatColor: Cardinal;

    fCampaigns: TKMCampaignsCollection;
    fGameSettings: TGameSettings;
    fMusicLib: TMusicLib;
    fNetworking: TKMNetworking;
    fRender: TRender;
    fTimerUI: TTimer;
    fMainMenuInterface: TKMMainMenuInterface;

    fOnCursorUpdate: TIntegerStringEvent;

    procedure GameLoadingStep(const aText: string);
    procedure LoadGameAssets;
    procedure LoadGameFromSave(aFilePath: string; aGameMode: TGameMode);
    procedure LoadGameFromScript(aMissionFile, aGameName: string; aCampaignName: string; aMap: Byte; aGameMode: TGameMode; aDesiredLoc: ShortInt; aDesiredColor: Cardinal);
    procedure LoadGameFromScratch(aSizeX, aSizeY: Integer; aGameMode: TGameMode);
    function SaveName(const aName, aExt: string; aMultiPlayer: Boolean): string;
  public
    constructor Create(aRenderControl: TKMRenderControl; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TStringEvent; aOnCursorUpdate: TIntegerStringEvent; NoMusic: Boolean = False);
    destructor Destroy; override;
    procedure AfterConstruction(aReturnToOptions: Boolean); reintroduce;

    procedure Stop(Msg: TGameResultMsg; TextMsg: string = '');
    function CanClose: Boolean;
    procedure Resize(X,Y: Integer);
    procedure ToggleLocale(aLocale: ShortString);
    procedure NetworkInit;
    procedure SendMPGameInfo(Sender: TObject);
    function RenderVersion: string;
    procedure PrintScreen(aFilename: string = '');
    procedure PauseMusicToPlayFile(aFileName:string);
    function CheckDATConsistency: Boolean;

    //These are all different game kinds we can start
    procedure NewCampaignMap(aCampaign: TKMCampaign; aMap: Byte);
    procedure NewSingleMap(aMissionFile, aGameName: string; aDesiredLoc: ShortInt = -1; aDesiredColor: Cardinal = $00000000);
    procedure NewSingleSave(aSaveName: string);
    procedure NewMultiplayerMap(const aFileName: string);
    procedure NewMultiplayerSave(const aSaveName: string);
    procedure NewRestartLast;
    procedure NewEmptyMap(aSizeX, aSizeY: Integer);
    procedure NewMapEditor(const aFileName: string; aSizeX, aSizeY: Integer);
    procedure NewReplay(const aFilePath: string);

    property Campaigns: TKMCampaignsCollection read fCampaigns;
    function Game: TKMGame;
    property GameSettings: TGameSettings read fGameSettings;
    property MainMenuInterface: TKMMainMenuInterface read fMainMenuInterface;
    property MusicLib: TMusicLib read fMusicLib;
    property Networking: TKMNetworking read fNetworking;

    procedure KeyDown(Key: Word; Shift: TShiftState);
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
    procedure FPSMeasurement(aFPS: Cardinal);

    procedure Render;
    procedure UpdateState(Sender: TObject);
    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


var
  fGameApp: TKMGameApp;


implementation
uses
  KM_Log, KM_Main,
  {$IFDEF USE_MAD_EXCEPT} KM_Exceptions, {$ENDIF}
  KM_Maps, KM_Resource, KM_Sound, KM_Utils;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
constructor TKMGameApp.Create(aRenderControl: TKMRenderControl; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TStringEvent; aOnCursorUpdate: TIntegerStringEvent; NoMusic: Boolean = False);
begin
  inherited Create;

  fOnCursorUpdate := aOnCursorUpdate;

  fLocales      := TKMLocales.Create(ExeDir + 'data' + PathDelim + 'locales.txt');
  fGameSettings := TGameSettings.Create;
  fTextLibrary  := TTextLibrary.Create(ExeDir + 'data' + PathDelim + 'text' + PathDelim, fGameSettings.Locale);
  {$IFDEF USE_MAD_EXCEPT}fExceptions.LoadTranslation;{$ENDIF}

  fRender       := TRender.Create(aRenderControl, aScreenX, aScreenY, aVSync);

  //Show the message if user has old OpenGL drivers (pre-1.4)
  if fRender.IsOldGLVersion then
    //MessageDlg works better than Application.MessageBox or others, it stays on top and
    //pauses here until the user clicks ok.
    MessageDlg(fTextLibrary[TX_GAME_ERROR_OLD_OPENGL]+eol+eol+fTextLibrary[TX_GAME_ERROR_OLD_OPENGL_2], mtWarning, [mbOk], 0);

  fResource     := TResource.Create(fRender, aLS, aLT);
  fResource.LoadMenuResources(fGameSettings.Locale);

  fSoundLib     := TSoundLib.Create(fGameSettings.Locale, fGameSettings.SoundFXVolume, True); //Required for button click sounds
  fMusicLib     := TMusicLib.Create(fGameSettings.MusicVolume);
  fSoundLib.OnRequestFade   := fMusicLib.FadeMusic;
  fSoundLib.OnRequestUnfade := fMusicLib.UnfadeMusic;

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
    fMainMenuInterface.ShowScreen(msOptions)
  else
    fMainMenuInterface.ShowScreen(msMain);
end;


{ Destroy what was created }
destructor TKMGameApp.Destroy;
begin
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
  FreeThenNil(fLocales);
  FreeThenNil(fMainMenuInterface);
  FreeThenNil(fResource);
  FreeThenNil(fSoundLib);
  FreeThenNil(fMusicLib);
  FreeThenNil(fTextLibrary);
  FreeAndNil(fNetworking);

  FreeThenNil(fRender);

  inherited;
end;


//Determine if the game can be closed without loosing any important progress
function TKMGameApp.CanClose: Boolean;
begin
  //There are no unsaved changes in MainMenu and in Replays
  //In all other cases (maybe except gsOnHold?) there are potentially unsaved changes
  Result := (fGame = nil) or (fGame.GameMode in [gmReplaySingle, gmReplayMulti]);
end;


procedure TKMGameApp.ToggleLocale(aLocale: ShortString);
begin
  Assert(fGame = nil, 'We don''t want to recreate whole fGame for that. Let''s limit it only to MainMenu');

  fMainMenuInterface.ShowScreen(msLoading, fTextLibrary[TX_MENU_NEW_LOCALE]);
  Render; //Force to repaint information screen

  fTimerUI.Enabled := False; //Disable it while switching, if an OpenAL error appears the timer should be disabled
  fGameSettings.Locale := aLocale; //Wrong Locale will be ignored

  //Release resources that use Locale info
  FreeAndNil(fNetworking);
  FreeAndNil(fCampaigns);
  FreeAndNil(fMainMenuInterface);
  FreeAndNil(fSoundLib);
  FreeAndNil(fTextLibrary);

  //Recreate resources that use Locale info
  fTextLibrary := TTextLibrary.Create(ExeDir + 'data' + PathDelim + 'text' + PathDelim + '', fGameSettings.Locale);
  {$IFDEF USE_MAD_EXCEPT}fExceptions.LoadTranslation;{$ENDIF}
  //Don't reshow the warning dialog when initing sounds, it gets stuck behind in full screen
  //and the user already saw it when starting the game.
  fSoundLib := TSoundLib.Create(fGameSettings.Locale, fGameSettings.SoundFXVolume, False);
  fSoundLib.OnRequestFade := fMusicLib.FadeMusic;
  fSoundLib.OnRequestUnfade := fMusicLib.UnfadeMusic;
  fResource.Fonts.LoadFonts(fGameSettings.Locale);
  fCampaigns := TKMCampaignsCollection.Create; //Campaigns load text into TextLibrary
  fCampaigns.ScanFolder(ExeDir + 'Campaigns' + PathDelim);
  fCampaigns.LoadProgress(ExeDir + 'Saves' + PathDelim + 'Campaigns.dat');
  fMainMenuInterface := TKMMainMenuInterface.Create(fRender.ScreenX, fRender.ScreenY);
  fMainMenuInterface.ShowScreen(msOptions);
  Resize(fRender.ScreenX, fRender.ScreenY); //Force the recreated main menu to resize to the user's screen
  fTimerUI.Enabled := True; //Safe to enable the timer again
end;


procedure TKMGameApp.Resize(X,Y: Integer);
begin
  fRender.Resize(X, Y);

  //Main menu is invisible while in game, but it still exists and when we return to it
  //it must be properly sized (player could resize the screen while playing)
  fMainMenuInterface.Resize(X, Y);

  if fGame <> nil then fGame.Resize(X, Y);
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
  if DEBUG_CHEATS and (Key = VK_F12) then SHOW_CONTROLS_OVERLAY := not SHOW_CONTROLS_OVERLAY;

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
  if not InRange(X, 1, fRender.ScreenX - 1)
  or not InRange(Y, 1, fRender.ScreenY - 1) then
    Exit; // Exit if Cursor is outside of frame

  if fGame <> nil then
    fGame.MouseMove(Shift,X,Y)
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


procedure TKMGameApp.FPSMeasurement(aFPS: Cardinal);
begin
  if fNetworking <> nil then fNetworking.FPSMeasurement(aFPS);
end;


function TKMGameApp.Game: TKMGame;
begin
  Result := fGame;
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
begin
  if fGame = nil then Exit;

  if fGame.GameMode = gmMulti then
  begin
    if fNetworking.Connected then
      fNetworking.AnnounceDisconnect;
    fNetworking.Disconnect;
  end;

  fRepeatGameName := '';
  fRepeatMission := '';
  fRepeatSave := '';
  fRepeatCampName := '';
  fRepeatCampMap := 0;
  fRepeatLocation := 0;
  fRepeatColor := 0;

  case Msg of
    gr_Win, gr_Defeat, gr_Cancel, gr_ReplayEnd:
                    begin
                      //If the game was a part of a campaign, select that campaign,
                      //so we know which menu to show next and unlock next map
                      fCampaigns.SetActive(fCampaigns.CampaignByTitle(fGame.CampaignName), fGame.CampaignMap);

                      if (fGame.GameMode in [gmMulti, gmReplayMulti]) then
                        fMainMenuInterface.ShowResultsMP(Msg)
                      else
                      begin
                        //Remember which map we played so we could restart it
                        fRepeatGameName := fGame.GameName;
                        fRepeatMission := fGame.MissionFile;
                        fRepeatSave := fGame.SaveFile;
                        fRepeatCampName := fGame.CampaignName;
                        fRepeatCampMap := fGame.CampaignMap;
                        fRepeatLocation := fGame.PlayerLoc;
                        fRepeatColor := fGame.PlayerColor;

                        fMainMenuInterface.Results_Fill(Msg);
                        fMainMenuInterface.ShowScreen(msResults, '', Msg);
                      end;

                      if (Msg = gr_Win) and (fCampaigns.ActiveCampaign <> nil) then
                        fCampaigns.UnlockNextMap;
                    end;
    gr_Error, gr_Disconnect:
                    fMainMenuInterface.ShowScreen(msError, TextMsg);
    gr_Silent:      ;//Used when loading new savegame from gameplay UI
    gr_MapEdEnd:    fMainMenuInterface.ShowScreen(msMain);
  end;

  FreeThenNil(fGame);
  fLog.AddTime('Gameplay ended - ' + GetEnumName(TypeInfo(TGameResultMsg), Integer(Msg)) + ' /' + TextMsg);
end;


procedure TKMGameApp.LoadGameFromSave(aFilePath: string; aGameMode: TGameMode);
var
  LoadError: string;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  //Reset controls if MainForm exists (KMR could be run without main form)
  if fMain <> nil then
    fMain.FormMain.ControlsReset;

  fGame := TKMGame.Create(aGameMode, fRender, fNetworking);
  try
    fGame.Load(aFilePath);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], [aFilePath])+'||'+E.ClassName+': '+E.Message;
      Stop(gr_Error, LoadError);
      fLog.AddTime('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  if Assigned(fOnCursorUpdate) then
    fOnCursorUpdate(0, 'Map size: ' + IntToStr(fGame.MapX) + ' x ' + IntToStr(fGame.MapY));
end;


procedure TKMGameApp.LoadGameFromScript(aMissionFile, aGameName: string; aCampaignName: string; aMap: Byte; aGameMode: TGameMode; aDesiredLoc: ShortInt; aDesiredColor: Cardinal);
var
  LoadError: string;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  //Reset controls if MainForm exists (KMR could be run without main form)
  if fMain <> nil then
    fMain.FormMain.ControlsReset;

  fGame := TKMGame.Create(aGameMode, fRender, fNetworking);
  try
    fGame.GameStart(aMissionFile, aGameName, aCampaignName, aMap, aDesiredLoc, aDesiredColor);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], [aMissionFile])+'||'+E.ClassName+': '+E.Message;
      Stop(gr_Error, LoadError);
      fLog.AddTime('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  if Assigned(fOnCursorUpdate) then
    fOnCursorUpdate(0, 'Map size: '+inttostr(fGame.MapX)+' x '+inttostr(fGame.MapY));
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

  fGame := TKMGame.Create(aGameMode, fRender, nil);
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
      fLog.AddTime('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  if Assigned(fOnCursorUpdate) then
    fOnCursorUpdate(0, 'Map size: '+inttostr(fGame.MapX)+' x '+inttostr(fGame.MapY));
end;


procedure TKMGameApp.NewCampaignMap(aCampaign: TKMCampaign; aMap: Byte);
begin
  LoadGameFromScript(aCampaign.MissionFile(aMap), aCampaign.MissionTitle(aMap), aCampaign.ShortTitle, aMap, gmSingle, -1, 0);
end;


procedure TKMGameApp.NewSingleMap(aMissionFile, aGameName: string; aDesiredLoc: ShortInt = -1; aDesiredColor: Cardinal = $00000000);
begin
  LoadGameFromScript(aMissionFile, aGameName, '', 0, gmSingle, aDesiredLoc, aDesiredColor);
end;


procedure TKMGameApp.NewSingleSave(aSaveName: string);
begin
  //Convert SaveName to local FilePath
  LoadGameFromSave(SaveName(aSaveName, 'sav', False), gmSingle);
end;


procedure TKMGameApp.NewMultiplayerMap(const aFileName: string);
begin
  LoadGameFromScript(TKMapsCollection.FullPath(aFileName, '.dat', True), aFileName, '', 0, gmMulti, 0, 0);

  //Copy text from lobby to in-game chat
  if fGame <> nil then
  begin
    fGame.GamePlayInterface.SetChatText(fMainMenuInterface.GetChatText);
    fGame.GamePlayInterface.SetChatMessages(fMainMenuInterface.GetChatMessages);
  end;
end;


procedure TKMGameApp.NewMultiplayerSave(const aSaveName: string);
begin
  //Convert SaveName to local FilePath
  //aFileName is the same for all players, but Path to it is different
  LoadGameFromSave(SaveName(aSaveName, 'sav', True), gmMulti);

  //Copy the chat and typed lobby message to the in-game chat
  fGame.GamePlayInterface.SetChatText(fMainMenuInterface.GetChatText);
  fGame.GamePlayInterface.SetChatMessages(fMainMenuInterface.GetChatMessages);
end;


procedure TKMGameApp.NewRestartLast;
begin
  if FileExists(ExeDir + fRepeatMission) then
    LoadGameFromScript(ExeDir + fRepeatMission, fRepeatGameName, fRepeatCampName, fRepeatCampMap, gmSingle, fRepeatLocation, fRepeatColor)
  else
  if FileExists(ChangeFileExt(ExeDir + fRepeatSave, '.bas')) then
    LoadGameFromSave(ChangeFileExt(ExeDir + fRepeatSave, '.bas'), gmSingle)
  else
    fMainMenuInterface.ShowScreen(msError, 'Can not repeat last mission');
end;


procedure TKMGameApp.NewEmptyMap(aSizeX, aSizeY: Integer);
begin
  LoadGameFromScratch(aSizeX, aSizeY, gmSingle);
end;


procedure TKMGameApp.NewMapEditor(const aFileName: string; aSizeX, aSizeY: Integer);
begin
  if aFileName <> '' then
    LoadGameFromScript(aFileName, TruncateExt(ExtractFileName(aFileName)), '', 0, gmMapEd, 0, 0)
  else
    LoadGameFromScratch(aSizeX, aSizeY, gmMapEd);
end;


procedure TKMGameApp.NewReplay(const aFilePath: string);
begin
  Assert(ExtractFileExt(aFilePath) = '.bas');
  LoadGameFromSave(aFilePath, gmReplaySingle); //Will be changed to gmReplayMulti depending on save contents
end;


function TKMGameApp.SaveName(const aName, aExt: string; aMultiPlayer: Boolean): string;
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
                                        fGameSettings.MasterAnnounceInterval,
                                        fGameSettings.Locale);
  fNetworking.OnMPGameInfoChanged := SendMPGameInfo;
  fNetworking.OnStartMap := NewMultiplayerMap;
  fNetworking.OnStartSave := NewMultiplayerSave;
end;


//Called by fNetworking to access MissionTime/GameName if they are valid
//fNetworking knows nothing about fGame
procedure TKMGameApp.SendMPGameInfo(Sender: TObject);
begin
  if fGame <> nil then
    fNetworking.AnnounceGameInfo(fGame.MissionTime, fGame.GameName)
  else
    fNetworking.AnnounceGameInfo(-1, ''); //fNetworking will fill the details from lobby
end;


procedure TKMGameApp.Render;
begin
  if SKIP_RENDER then Exit;
  if fRender.Blind then Exit;
  if not fTimerUI.Enabled then Exit; //Don't render while toggling locale

  fRender.BeginFrame;

  if fGame <> nil then
    fGame.Render(fRender)
  else
    fMainMenuInterface.Paint;

  fRender.RenderBrightness(GameSettings.Brightness);

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


procedure TKMGameApp.PauseMusicToPlayFile(aFileName:string);
begin
  if not FileExists(aFileName) then Exit;
  fSoundLib.AbortAllFadeSounds; //Victory/defeat sounds also fade music, so stop those in the rare chance they might still be playing
  fMusicLib.PauseMusicToPlayFile(aFileName, fGameSettings.SoundFXVolume);
end;


function TKMGameApp.CheckDATConsistency: Boolean;
begin
  Result := ALLOW_MP_MODS or (fResource.GetDATCRC = $4F5458E6); //That's the magic CRC of official .dat files
end;


procedure TKMGameApp.UpdateState(Sender: TObject);
begin
  Inc(fGlobalTickCount);
  //Always update networking for auto reconnection and query timeouts
  if fNetworking <> nil then fNetworking.UpdateState(fGlobalTickCount);
  if fGame <> nil then
  begin
    fGame.UpdateState(fGlobalTickCount);
    if (fGame.GameMode = gmMulti) and (fGlobalTickCount mod 100 = 0) then
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
    if (fGame <> nil) and not fGame.IsPaused and Assigned(fOnCursorUpdate) then
        fOnCursorUpdate(2, 'Time: ' + TimeToString(fGame.MissionTime));
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

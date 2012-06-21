unit KM_GameApp;
{$I KaM_Remake.inc}
interface
uses
  Windows, Classes, Controls, Dialogs, ExtCtrls, KromUtils, Math, SysUtils, TypInfo,
  KM_CommonTypes, KM_Defaults,
  KM_Campaigns, KM_Game,
  KM_InterfaceDefaults, KM_InterfaceMainMenu,
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
    procedure LoadGameFromScript(aMissionFile, aGameName: string; aCampaignName: string; aMap: Byte; aGameMode: TGameMode);
    procedure LoadGameFromScratch(aSizeX, aSizeY: Integer; aGameMode: TGameMode);
    function SaveName(const aName, aExt: string; aMultiPlayer: Boolean): string;
  public
    constructor Create(aHandle: HWND; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TStringEvent; aOnCursorUpdate: TIntegerStringEvent; NoMusic: Boolean = False);
    destructor Destroy; override;
    procedure AfterConstruction(aReturnToOptions: Boolean); reintroduce;

    procedure Stop(Msg: TGameResultMsg; TextMsg: string = '');
    function CanClose: Boolean;
    procedure Resize(X,Y: Integer);
    procedure ToggleLocale(aLocale: ShortString);
    procedure NetworkInit;
    procedure SendMPGameInfo(Sender: TObject);
    function AllowDebugRendering: Boolean;
    function RenderVersion: string;
    procedure PrintScreen(aFilename: string = '');

    //These are all different game kinds we can start
    procedure NewCampaignMap(aCampaign: TKMCampaign; aMap: Byte);
    procedure NewSingleMap(aMissionFile, aGameName: string);
    procedure NewSingleSave(aSaveName: string);
    procedure NewMultiplayerMap(const aFileName: string);
    procedure NewMultiplayerSave(const aSaveName: string);
    procedure NewRestartLast;
    procedure NewMapEditor(const aFileName: string; aSizeX, aSizeY: Integer);
    procedure NewReplay(const aSaveName: string);

    property Campaigns: TKMCampaignsCollection read fCampaigns;
    function Game: TKMGame;
    property GlobalSettings: TGameSettings read fGameSettings;
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

    procedure Render;
    procedure UpdateState(Sender: TObject);
    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


var
  fGameApp: TKMGameApp;


implementation
uses
  KM_Log, KM_RenderAux, KM_Resource, KM_Sound;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
constructor TKMGameApp.Create(aHandle: HWND; aScreenX, aScreenY: Word; aVSync: Boolean; aLS: TEvent; aLT: TStringEvent; aOnCursorUpdate: TIntegerStringEvent; NoMusic: Boolean = False);
begin
  inherited Create;

  fOnCursorUpdate := aOnCursorUpdate;

  fLocales      := TKMLocales.Create(ExeDir + 'data\locales.txt');
  fGameSettings := TGameSettings.Create;
  fTextLibrary  := TTextLibrary.Create(ExeDir + 'data\text\', fGameSettings.Locale);
  {$IFDEF USE_MAD_EXCEPT}fExceptions.LoadTranslation;{$ENDIF}

  fRender       := TRender.Create(aHandle, aScreenX, aScreenY, aVSync);
  fRenderAux    := TRenderAux.Create;
  //Show the message if user has old OpenGL drivers (pre-1.4)
  if fRender.IsOldGLVersion then
    //MessageDlg works better than Application.MessageBox or others, it stays on top and
    //pauses here until the user clicks ok.
    MessageDlg(fTextLibrary[TX_GAME_ERROR_OLD_OPENGL]+eol+eol+fTextLibrary[TX_GAME_ERROR_OLD_OPENGL_2], mtWarning, [mbOk], 0);

  fResource     := TResource.Create(fRender, aLS, aLT);
  fResource.LoadMenuResources(fGameSettings.Locale);

  fSoundLib     := TSoundLib.Create(fGameSettings.Locale, fGameSettings.SoundFXVolume); //Required for button click sounds
  fMusicLib     := TMusicLib.Create(fGameSettings.MusicVolume);
  fSoundLib.OnRequestFade := fMusicLib.FadeMusic;
  fSoundLib.OnRequestUnfade := fMusicLib.UnfadeMusic;

  fCampaigns    := TKMCampaignsCollection.Create;
  fCampaigns.ScanFolder(ExeDir + 'Campaigns\');
  fCampaigns.LoadProgress(ExeDir + 'Saves\Campaigns.dat');

  //If game was reinitialized from options menu then we should return there
  fMainMenuInterface := TKMMainMenuInterface.Create(aScreenX, aScreenY);

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

  Stop(gr_Silent);

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
  Result := (fGameG = nil) or (fGameG.GameMode = gmReplay);
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
  fMainMenuInterface := TKMMainMenuInterface.Create(fRender.ScreenX, fRender.ScreenY);
  fMainMenuInterface.ShowScreen(msOptions);
  Resize(fRender.ScreenX, fRender.ScreenY); //Force the recreated main menu to resize to the user's screen
end;


procedure TKMGameApp.Resize(X,Y: Integer);
begin
  fRender.Resize(X, Y);

  //Main menu is invisible while in game, but it still exists and when we return to it
  //it must be properly sized (player could resize the screen while playing)
  fMainMenuInterface.Resize(X, Y);

  if fGameG <> nil then fGameG.Resize(X, Y);
end;


procedure TKMGameApp.KeyDown(Key: Word; Shift: TShiftState);
begin
  if fGameG <> nil then
    fGameG.KeyDown(Key, Shift)
  else
    fMainMenuInterface.KeyDown(Key, Shift);
end;


procedure TKMGameApp.KeyPress(Key: Char);
begin
  if fGameG <> nil then
    fGameG.KeyPress(Key)
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

  if fGameG <> nil then
    fGameG.KeyUp(Key, Shift)
  else
    fMainMenuInterface.KeyUp(Key, Shift);
end;


procedure TKMGameApp.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if fGameG <> nil then
    fGameG.MouseDown(Button,Shift,X,Y)
  else
    fMainMenuInterface.MouseDown(Button,Shift,X,Y);
end;


procedure TKMGameApp.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if not InRange(X, 1, fRender.ScreenX - 1)
  or not InRange(Y, 1, fRender.ScreenY - 1) then
    Exit; // Exit if Cursor is outside of frame

  if fGameG <> nil then
    fGameG.MouseMove(Shift,X,Y)
  else
    //fMainMenuInterface = nil while loading a new locale
    if fMainMenuInterface <> nil then
      fMainMenuInterface.MouseMove(Shift, X,Y);

  fOnCursorUpdate(1, Format('Cursor: %.1f:%.1f [%d:%d]', [GameCursor.Float.X, GameCursor.Float.Y,
                                                          GameCursor.Cell.X, GameCursor.Cell.Y]));
end;


procedure TKMGameApp.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if fGameG <> nil then
    fGameG.MouseUp(Button,Shift,X,Y)
  else
    fMainMenuInterface.MouseUp(Button, Shift, X,Y);
end;


procedure TKMGameApp.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer);
begin
  if fGameG <> nil then
    fGameG.MouseWheel(Shift, WheelDelta, X, Y)
  else
    fMainMenuInterface.MouseWheel(Shift, WheelDelta, X, Y);
end;


function TKMGameApp.Game: TKMGame;
begin
  Result := fGameG;
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
  if fGameG = nil then Exit;

  if fGameG.GameMode = gmMulti then
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

  case Msg of
    gr_Win, gr_Defeat, gr_Cancel, gr_ReplayEnd:
                    begin
                      if fGameG.GameMode = gmMulti then
                      begin
                        fMainMenuInterface.ResultsMP_Fill;
                        fMainMenuInterface.ShowScreen(msResultsMP, '', Msg);
                      end
                      else
                      begin
                        //Remember which map we played so we could restart it
                        fRepeatGameName := fGameG.GameName;
                        fRepeatMission := fGameG.MissionFile;
                        fRepeatSave := fGameG.SaveFile;
                        fRepeatCampName := fGameG.CampaignName;
                        fRepeatCampMap := fGameG.CampaignMap;

                        fMainMenuInterface.Results_Fill;
                        fMainMenuInterface.ShowScreen(msResults, '', Msg);
                      end;

                      //If the game was a part of a campaign, select that campaign,
                      //so we know which menu to show next and unlock next map
                      fCampaigns.SetActive(fCampaigns.CampaignByTitle(fGameG.CampaignName), fGameG.CampaignMap);

                      if (Msg = gr_Win) and (fCampaigns.ActiveCampaign <> nil) then
                        fCampaigns.UnlockNextMap;
                    end;
    gr_Error, gr_Disconnect:
                    fMainMenuInterface.ShowScreen(msError, TextMsg);
    gr_Silent:      ;//Used when loading new savegame from gameplay UI
    gr_MapEdEnd:    fMainMenuInterface.ShowScreen(msMain);
  end;

  FreeThenNil(fGameG);
  fLog.AppendLog('Gameplay ended - ' + GetEnumName(TypeInfo(TGameResultMsg), Integer(Msg)) + ' /' + TextMsg);
end;


procedure TKMGameApp.LoadGameFromSave(aFilePath: string; aGameMode: TGameMode);
var
  LoadError: string;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  fGameG := TKMGame.Create(aGameMode, fRender, fNetworking);
  try
    fGameG.Load(aFilePath);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], [aFilePath])+'||'+E.ClassName+': '+E.Message;
      Stop(gr_Error, LoadError);
      fLog.AppendLog('Game creation Exception: ' + LoadError);
      Exit;
    end;
  end;

  fOnCursorUpdate(0, 'Map size: '+inttostr(fGameG.MapX)+' x '+inttostr(fGameG.MapY));
end;


procedure TKMGameApp.LoadGameFromScript(aMissionFile, aGameName: string; aCampaignName: string; aMap: Byte; aGameMode: TGameMode);
var
  LoadError: string;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  fGameG := TKMGame.Create(aGameMode, fRender, fNetworking);
  try
    fGameG.GameStart(aMissionFile, aGameName, aCampaignName, aMap);
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

  fOnCursorUpdate(0, 'Map size: '+inttostr(fGameG.MapX)+' x '+inttostr(fGameG.MapY));
end;


procedure TKMGameApp.LoadGameFromScratch(aSizeX, aSizeY: Integer; aGameMode: TGameMode);
var
  LoadError: string;
begin
  Stop(gr_Silent); //Stop everything silently
  LoadGameAssets;

  fGameG := TKMGame.Create(gmMapEd, fRender, nil);
  try
    fGameG.GameStart(aSizeX, aSizeY);
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

  fOnCursorUpdate(0, 'Map size: '+inttostr(fGameG.MapX)+' x '+inttostr(fGameG.MapY));
end;


procedure TKMGameApp.NewCampaignMap(aCampaign: TKMCampaign; aMap: Byte);
begin
  LoadGameFromScript(aCampaign.MissionFile(aMap), aCampaign.MissionTitle(aMap), aCampaign.ShortTitle, aMap, gmSingle);
end;


procedure TKMGameApp.NewSingleMap(aMissionFile, aGameName: string);
begin
  LoadGameFromScript(aMissionFile, aGameName, '', 0, gmSingle);
end;


procedure TKMGameApp.NewSingleSave(aSaveName: string);
begin
  //Convert SaveName to local FilePath
  LoadGameFromSave(SaveName(aSaveName, 'sav', False), gmSingle);
end;


procedure TKMGameApp.NewMultiplayerMap(const aFileName: string);
begin
  LoadGameFromScript(aFileName, aFileName, '', 0, gmMulti);
end;


procedure TKMGameApp.NewMultiplayerSave(const aSaveName: string);
begin
  //Convert SaveName to local FilePath
  //aFileName is the same for all players, but Path to it is different
  LoadGameFromSave(SaveName(aSaveName, 'sav', False), gmMulti);
end;


procedure TKMGameApp.NewRestartLast;
begin
  if FileExists(ExeDir + fRepeatMission) then
    LoadGameFromScript(ExeDir + fRepeatMission, fRepeatGameName, fRepeatCampName, fRepeatCampMap, gmSingle)
  else
  if FileExists(ChangeFileExt(ExeDir + fRepeatSave, '.bas')) then
    LoadGameFromSave(ChangeFileExt(ExeDir + fRepeatSave, '.bas'), gmSingle)
  else
    fMainMenuInterface.ShowScreen(msError, 'Can not repeat last mission');
end;


procedure TKMGameApp.NewMapEditor(const aFileName: string; aSizeX, aSizeY: Integer);
begin
  if aFileName <> '' then
    LoadGameFromScript(aFileName, TruncateExt(ExtractFileName(aFileName)), '', 0, gmMapEd)
  else
    LoadGameFromScratch(aSizeX, aSizeY, gmMapEd);
end;


procedure TKMGameApp.NewReplay(const aSaveName: string);
begin
  //Convert SaveName to local FilePath with proper extension
  LoadGameFromSave(SaveName(aSaveName, 'rpl', False), gmReplay);
end;


function TKMGameApp.SaveName(const aName, aExt: string; aMultiPlayer: Boolean): string;
begin
  if aMultiPlayer then
    Result := ExeDir + 'SavesMP\' + aName + '.' + aExt
  else
    Result := ExeDir + 'Saves\' + aName + '.' + aExt;
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


//Called by fNetworking to access MissionTime/GameName if they are valid
//fNetworking knows nothing about fGame
procedure TKMGameApp.SendMPGameInfo(Sender: TObject);
begin
  if fGameG <> nil then
    fNetworking.AnnounceGameInfo(fGameG.MissionTime, fGameG.GameName);
end;


//Debug rendering may be used as a cheat in MP to see unrevealed areas, thats why we block it there
function TKMGameApp.AllowDebugRendering: Boolean;
begin
  Result := (fGameG.GameMode in [gmSingle, gmMapEd, gmReplay]) or (MULTIPLAYER_CHEATS and (fGameG.GameMode = gmMulti));
end;


procedure TKMGameApp.Render;
begin
  if SKIP_RENDER then Exit;
  if fRender.Blind then Exit;

  fRender.BeginFrame;

  if fGameG <> nil then
    fGameG.Render(fRender)
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
  if fGameG <> nil then
  begin
    fGameG.UpdateState(fGlobalTickCount);
    if (fGameG.GameMode = gmMulti) and (fGlobalTickCount mod 100 = 0) then
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
    if (fGameG <> nil) and not fGameG.IsPaused then
      fOnCursorUpdate(2, 'Time: ' + FormatDateTime('hh:nn:ss', fGameG.MissionTime));
  end;
end;


//This is our real-time "thread", use it wisely
procedure TKMGameApp.UpdateStateIdle(aFrameTime: Cardinal);
begin
  if fGameG <> nil then
    fGameG.UpdateStateIdle(aFrameTime);

  if fMusicLib <> nil then fMusicLib.UpdateStateIdle;
  if fSoundLib <> nil then fSoundLib.UpdateStateIdle;
  if fNetworking <> nil then fNetworking.UpdateStateIdle;
end;


end.

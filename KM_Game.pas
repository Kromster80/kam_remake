unit KM_Game;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, FileUtil, {$ENDIF}
  {$IFDEF WDC} MPlayer, {$ENDIF}
  Forms, Controls, Classes, Dialogs, SysUtils, KromUtils, Math, TypInfo, Zippit,
  KM_CommonTypes, KM_Defaults, KM_Utils,
  KM_Networking,
  KM_MapEditor, KM_Campaigns,
  KM_GameInputProcess, KM_PlayersCollection, KM_Render, KM_RenderAux, KM_TextLibrary, KM_InterfaceMapEditor, KM_InterfaceGamePlay, KM_InterfaceMainMenu,
  KM_ResourceGFX, KM_Terrain, KM_MissionScript, KM_Projectiles, KM_Sound, KM_Viewport, KM_Settings, KM_Music, KM_Points,
  KM_ArmyEvaluation, KM_GameOptions;

type TGameState = ( gsNoGame,  //No game running at all, MainMenu
                    gsPaused,  //Game is paused and responds to 'P' key only
                    gsOnHold,  //Game is paused, shows victory options (resume, win) and responds to mouse clicks only
                    gsRunning, //Game is running normally
                    gsReplay,  //Game is showing replay, no player input allowed
                    gsEditor); //Game is in MapEditor mode

type
  TKMGame = class
  private //Irrelevant to savegame
    ScreenX,ScreenY:word;
    FormControlsVisible:boolean;
    fFormPassability:integer;
    fIsExiting: boolean; //Set this to true on Exit and unit/house pointers will be released without cross-checking
    fGlobalTickCount:cardinal; //Not affected by Pause and anything (Music, Minimap, StatusBar update)
    fGameSpeed:word;
    fGameState:TGameState;
    fMultiplayerMode:boolean;
    fReplayMode:boolean;
    fReplayFile:string;
    fWaitingForNetwork:boolean;
    fGameOptions:TKMGameOptions;
    fAdvanceFrame:boolean; //Replay variable to advance 1 frame, afterwards set to false
    fGlobalSettings: TGlobalSettings;
    fCampaigns: TKMCampaignsCollection;
    fMusicLib: TMusicLib;
    fMapEditor: TKMMapEditor;
    fProjectiles:TKMProjectiles;
    fGameInputProcess:TGameInputProcess;
    fNetworking:TKMNetworking;

  //Should be saved
    fGameTickCount:cardinal;
    fGameName:string;
    fMissionMode: TKMissionMode;
    ID_Tracker:cardinal; //Mainly Units-Houses tracker, to issue unique numbers on demand

    procedure GameInit(aMultiplayerMode:boolean);
    procedure GameStart(aMissionFile, aGameName:string);
    procedure GameMPDisconnect(const aData:string);
    procedure MultiplayerRig;

    procedure Load(const aFilename: string; aReplay:boolean=false);
  public
    PlayOnState:TGameResultMsg;
    DoGameHold:boolean; //Request to run GameHold after UpdateState has finished
    DoGameHoldState:TGameResultMsg; //The type of GameHold we want to occur due to DoGameHold
    SkipReplayEndCheck:boolean;
    fGamePlayInterface: TKMGamePlayInterface;
    fMainMenuInterface: TKMMainMenuInterface;
    fMapEditorInterface: TKMapEdInterface;
    constructor Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; aVSync,aReturnToOptions:boolean; aLS:TNotifyEvent; aLT:TStringEvent; NoMusic:boolean=false);
    destructor Destroy; override;
    procedure ToggleLocale(aLocale:shortstring);
    procedure Resize(X,Y:integer);
    procedure ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean);
    function MapSizeText: string;
    procedure KeyDown(Key: Word; Shift: TShiftState);
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure StartCampaignMap(aCampaign:TKMCampaign; aMap:byte);
    procedure StartSingleMap(aMissionFile, aGameName:string);
    procedure StartSingleSave(aFilename:string);
    procedure StartLastMap;
    procedure StartMultiplayerSave(const aFilename: string);
    procedure StartMultiplayerMap(const aFilename: string);
    procedure StartMapEditor(const aFilename: string; aMultiplayer:boolean; aSizeX:integer=64; aSizeY:integer=64);
    procedure Stop(Msg:TGameResultMsg; TextMsg:string='');

    procedure GameMPPlay(Sender:TObject);
    procedure GameMPReadyToPlay(Sender:TObject);
    procedure GameError(aLoc:TKMPoint; aText:string); //Stop the game because of an error
    procedure SetGameState(aNewState:TGameState);
    procedure GameHold(DoHold:boolean; Msg:TGameResultMsg); //Hold the game to ask if player wants to play after Victory/Defeat/ReplayEnd
    procedure RequestGameHold(Msg:TGameResultMsg);
    procedure PlayerVictory(aPlayerIndex:TPlayerIndex);
    procedure PlayerDefeat(aPlayerIndex:TPlayerIndex);
    procedure GameWaitingForNetwork(aWaiting:boolean);
    procedure GameDropWaitingPlayers;
    procedure SendMPGameInfo(Sender:TObject);

    procedure AutoSave;
    procedure BaseSave;
    procedure SaveMapEditor(const aMissionName:string; aMultiplayer:boolean);

    function  ReplayExists(const aSaveName:string; aMultiplayer:boolean):boolean;
    procedure RestartReplay;
    procedure StartReplay(const aSaveName:string; aMultiplayer:boolean);

    procedure NetworkInit;

    function GetMissionTime:TDateTime;
    function GetPeacetimeRemaining:TDateTime;
    function CheckTime(aTimeTicks:cardinal):boolean;
    function IsPeaceTime:boolean;
    procedure UpdatePeaceTime;
    property GameTickCount:cardinal read fGameTickCount;
    property GlobalTickCount:cardinal read fGlobalTickCount;
    property GameName:string read fGameName;
    property MultiplayerMode:boolean read fMultiplayerMode;
    property ReplayMode:boolean read fReplayMode;
    property FormPassability:integer read fFormPassability write fFormPassability;
    property IsExiting:boolean read fIsExiting;
    property MissionMode:TKMissionMode read fMissionMode write fMissionMode;
    function AllowDebugRendering:boolean;
    function GetNewID:cardinal;
    property GameState:TGameState read fGameState;
    procedure SetGameSpeed(aSpeed:word);
    procedure StepOneFrame;
    function SaveName(const aName, aExt:string):string;

    property GlobalSettings: TGlobalSettings read fGlobalSettings;
    property Campaigns: TKMCampaignsCollection read fCampaigns;
    property MapEditor: TKMMapEditor read fMapEditor;
    property MusicLib:TMusicLib read fMusicLib;
    property Projectiles:TKMProjectiles read fProjectiles;
    property GameInputProcess:TGameInputProcess read fGameInputProcess;
    property Networking:TKMNetworking read fNetworking;
    property GameOptions:TKMGameOptions read fGameOptions;

    procedure Save(const aFilename: string);

    procedure UpdateState;
    procedure UpdateStateIdle(aFrameTime:cardinal);
    procedure PaintInterface;
  end;

  var
    fGame:TKMGame;

implementation
uses
  KM_Unit1, KM_Player, KM_GameInfo, KM_GameInputProcess_Single, KM_GameInputProcess_Multi, KM_Log;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
constructor TKMGame.Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; aVSync,aReturnToOptions:boolean; aLS:TNotifyEvent; aLT:TStringEvent; NoMusic:boolean=false);
begin
  Inherited Create;
  ScreenX := aScreenX;
  ScreenY := aScreenY;
  fAdvanceFrame := false;
  ID_Tracker    := 0;
  PlayOnState   := gr_Cancel;
  DoGameHold    := false;
  fGameSpeed    := 1;
  fGameState    := gsNoGame;
  SkipReplayEndCheck  := false;
  FormControlsVisible := false;
  fWaitingForNetwork := false;
  fGameOptions := TKMGameOptions.Create;

  fGlobalSettings   := TGlobalSettings.Create;
  fRender           := TRender.Create(RenderHandle, aVSync);
  fRenderAux        := TRenderAux.Create;
  fTextLibrary      := TTextLibrary.Create(ExeDir+'data\misc\', fGlobalSettings.Locale);
  fSoundLib         := TSoundLib.Create(fGlobalSettings.Locale, fGlobalSettings.SoundFXVolume/fGlobalSettings.SlidersMax); //Required for button click sounds
  fMusicLib         := TMusicLib.Create(fGlobalSettings.MusicVolume/fGlobalSettings.SlidersMax);
  fSoundLib.OnFadeMusic := fMusicLib.FadeMusic;
  fSoundLib.OnUnfadeMusic := fMusicLib.UnfadeMusic;
  fResource         := TResource.Create(aLS, aLT);
  fResource.LoadMenuResources(fGlobalSettings.Locale);
  fCampaigns        := TKMCampaignsCollection.Create;

  //If game was reinitialized from options menu then we should return there
  fMainMenuInterface:= TKMMainMenuInterface.Create(ScreenX,ScreenY,fGlobalSettings, aReturnToOptions);

  if (not NoMusic) and fGlobalSettings.MusicOn then fMusicLib.PlayMenuTrack; //Start the playback as soon as loading is complete
  fMusicLib.ToggleShuffle(fGlobalSettings.ShuffleOn); //Determine track order

  fLog.AppendLog('<== Game creation is done ==>');
end;


{ Destroy what was created }
destructor TKMGame.Destroy;
begin
  fMusicLib.StopMusic; //Stop music imediently, so it doesn't keep playing and jerk while things closes

  FreeThenNil(fCampaigns);
  if fNetworking <> nil then FreeAndNil(fNetworking);
  FreeThenNil(fGlobalSettings);
  FreeThenNil(fMainMenuInterface);
  FreeThenNil(fResource);
  FreeThenNil(fSoundLib);
  FreeThenNil(fMusicLib);
  FreeThenNil(fTextLibrary);
  FreeThenNil(fRenderAux);
  FreeThenNil(fRender);
  FreeAndNil(fGameInputProcess);
  FreeAndNil(fGameOptions);
  Inherited;
end;


procedure TKMGame.ToggleLocale(aLocale:shortstring);
begin
  fGlobalSettings.Locale := aLocale; //Wrong Locale will be ignored
  if fNetworking <> nil then FreeAndNil(fNetworking);
  FreeAndNil(fMainMenuInterface);
  FreeAndNil(fSoundLib);
  FreeAndNil(fTextLibrary);
  fTextLibrary := TTextLibrary.Create(ExeDir+'data\misc\', fGlobalSettings.Locale);
  fSoundLib := TSoundLib.Create(fGlobalSettings.Locale, fGlobalSettings.SoundFXVolume/fGlobalSettings.SlidersMax);
  fResource.ResourceFont.LoadFonts(fGlobalSettings.Locale);
  fMainMenuInterface := TKMMainMenuInterface.Create(ScreenX, ScreenY, fGlobalSettings, True);
end;


procedure TKMGame.Resize(X,Y:integer);
begin
  ScreenX := X;
  ScreenY := Y;
  fRender.Resize(X,Y,rm2D);

  //Main menu is invisible while in game, but it still exists and when we return to it
  //it must be properly sized (player could resize the screen while playing)
  if fMainMenuInterface<>nil then fMainMenuInterface.Resize(X,Y);
  if fMapEditorInterface<>nil then fMapEditorInterface.Resize(X,Y);
  if fGamePlayInterface<>nil then fGamePlayInterface.Resize(X,Y);
end;


procedure TKMGame.ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean);
begin
  Form1.ToggleFullScreen(aToggle, fGlobalSettings.ResolutionID, fGlobalSettings.VSync, ReturnToOptions);
end;


procedure TKMGame.KeyDown(Key: Word; Shift: TShiftState);
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.KeyDown(Key, Shift);
    gsPaused:   fGamePlayInterface.KeyDown(Key, Shift);
    gsOnHold:   fGamePlayInterface.KeyDown(Key, Shift);
    gsRunning:  fGamePlayInterface.KeyDown(Key, Shift);
    gsReplay:   fGamePlayInterface.KeyDown(Key, Shift);
    gsEditor:   fMapEditorInterface.KeyDown(Key, Shift);
  end;
end;


procedure TKMGame.KeyPress(Key: Char);
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.KeyPress(Key);
    gsPaused:   fGamePlayInterface.KeyPress(Key);
    gsOnHold:   fGamePlayInterface.KeyPress(Key);
    gsRunning:  fGamePlayInterface.KeyPress(Key);
    gsReplay:   fGamePlayInterface.KeyPress(Key);
    gsEditor:   fMapEditorInterface.KeyPress(Key);
  end;
end;


procedure TKMGame.KeyUp(Key: Word; Shift: TShiftState);
begin
  //List of conflicting keys that we should try to avoid using in debug/game:
  //  F12 Pauses Execution and switches to debug
  //  F10 sets focus on MainMenu1
  //  F9 is the default key in Fraps for video capture
  //  F4 and F9 are used in debug to control run-flow
  //  others.. unknown

  //GLOBAL KEYS
  if Key = VK_F3 then SHOW_CONTROLS_OVERLAY := not SHOW_CONTROLS_OVERLAY;
  if Key = VK_F11  then begin
    FormControlsVisible := not FormControlsVisible;
    Form1.ToggleControlsVisibility(FormControlsVisible);
  end;

  case fGameState of
    gsNoGame:   fMainMenuInterface.KeyUp(Key, Shift); //Exit if handled
    gsPaused:   fGamePlayInterface.KeyUp(Key, Shift);
    gsOnHold:   fGamePlayInterface.KeyUp(Key, Shift);
    gsRunning:  fGamePlayInterface.KeyUp(Key, Shift);
    gsReplay:   fGamePlayInterface.KeyUp(Key, Shift);
    gsEditor:   fMapEditorInterface.KeyUp(Key, Shift);
  end;
end;


procedure TKMGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseDown(Button,Shift,X,Y);
    gsPaused:   fGamePlayInterface.MouseDown(Button,Shift,X,Y);
    gsOnHold:   fGamePlayInterface.MouseDown(Button,Shift,X,Y);
    gsReplay:   fGamePlayInterface.MouseDown(Button,Shift,X,Y);
    gsRunning:  fGamePlayInterface.MouseDown(Button,Shift,X,Y);
    gsEditor:   fMapEditorInterface.MouseDown(Button,Shift,X,Y);
  end;
end;


procedure TKMGame.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if not InRange(X,1,ScreenX-1) or not InRange(Y,1,ScreenY-1) then exit; //Exit if Cursor is outside of frame

  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseMove(Shift, X,Y);
    gsPaused:   fGamePlayInterface.MouseMove(Shift, X,Y);
    gsOnHold:   fGamePlayInterface.MouseMove(Shift, X,Y);
    gsRunning:  fGamePlayInterface.MouseMove(Shift, X,Y);
    gsReplay:   fGamePlayInterface.MouseMove(Shift, X,Y);
    gsEditor:   fMapEditorInterface.MouseMove(Shift,X,Y);
  end;

Form1.StatusBar1.Panels.Items[1].Text := Format('Cursor: %.1f:%.1f [%d:%d]', [
                                         GameCursor.Float.X, GameCursor.Float.Y,
                                         GameCursor.Cell.X, GameCursor.Cell.Y]);
end;


procedure TKMGame.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseUp(Button, Shift, X,Y);
    gsPaused:   fGamePlayInterface.MouseUp(Button, Shift, X,Y);
    gsOnHold:   fGamePlayInterface.MouseUp(Button, Shift, X,Y);
    gsReplay:   fGamePlayInterface.MouseUp(Button, Shift, X,Y);
    gsRunning:  fGamePlayInterface.MouseUp(Button, Shift, X,Y);
    gsEditor:   fMapEditorInterface.MouseUp(Button, Shift, X,Y)
  end;
end;


procedure TKMGame.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer);
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsPaused:   fGamePlayInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsOnHold:   fGamePlayInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsRunning:  fGamePlayInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsReplay:   fGamePlayInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsEditor:   fMapEditorInterface.MouseWheel(Shift, WheelDelta, X, Y);
  end;
end;


procedure TKMGame.GameInit(aMultiplayerMode:boolean);
begin
  fGameSpeed := 1; //In case it was set in last run mission
  PlayOnState := gr_Cancel;
  SkipReplayEndCheck := false; //Reset before each mission
  fMultiplayerMode := aMultiplayerMode;

  if fResource.DataState<>dls_All then begin
    fMainMenuInterface.ShowScreen(msLoading, 'trees, houses and units');
    fResource.LoadGameResources;
    InitUnitStatEvals; //Army
  end;

  fMainMenuInterface.ShowScreen(msLoading, 'initializing');

  fViewport := TViewport.Create;
  fGamePlayInterface := TKMGamePlayInterface.Create(ScreenX, ScreenY);

  //Here comes terrain/mission init
  SetKaMSeed(4); //Every time the game will be the same as previous. Good for debug.
  fTerrain := TTerrain.Create;
  fProjectiles := TKMProjectiles.Create;

  fRender.Resize(ScreenX, ScreenY, rm2D);
  fViewport.Resize(ScreenX, ScreenY);

  fGameTickCount := 0; //Restart counter
end;


procedure TKMGame.StartCampaignMap(aCampaign:TKMCampaign; aMap:byte);
begin
  Stop(gr_Silent); //Stop everything silently

  fCampaigns.ActiveCampaign := aCampaign;
  fCampaigns.ActiveCampaignMap := aMap;

  GameInit(false);
  GameStart(aCampaign.Maps[aMap].ScriptPath, aCampaign.Maps[aMap].MapName);
end;


procedure TKMGame.StartSingleMap(aMissionFile, aGameName:string);
begin
  Stop(gr_Silent); //Stop everything silently

  fCampaigns.ActiveCampaign := nil;
  fCampaigns.ActiveCampaignMap := 0;

  GameInit(false);
  GameStart(aMissionFile, aGameName);
end;


procedure TKMGame.StartSingleSave(aFilename:string);
begin
  Stop(gr_Silent); //Stop everything silently

  fCampaigns.ActiveCampaign := nil;
  fCampaigns.ActiveCampaignMap := 0;

  GameInit(false);
  Load(aFilename);
  fGameState := gsRunning;
  fReplayMode := false;
end;


procedure TKMGame.StartLastMap;
begin
  StartSingleSave('basesave');
end;


procedure TKMGame.GameStart(aMissionFile, aGameName:string);
var LoadError:string; fMissionParser: TMissionParser;
begin
  fLog.AppendLog('GameStart');

  fGameName := aGameName;

  fMainMenuInterface.ShowScreen(msLoading, 'script');

  if aMissionFile <> '' then
  try //Catch exceptions
    fMissionParser := TMissionParser.Create(mpm_Single, false);
    if not fMissionParser.LoadMission(aMissionFile) then
      Raise Exception.Create(fMissionParser.ErrorMessage);
    MyPlayer := fPlayers.Player[fMissionParser.MissionInfo.HumanPlayerID];
    Assert(MyPlayer.PlayerType = pt_Human);
    fMissionMode := fMissionParser.MissionInfo.MissionMode;
    FreeAndNil(fMissionParser);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := 'An error has occured while parsing the file:| '+aMissionFile+'||'+E.ClassName+': '+E.Message;
      fMainMenuInterface.ShowScreen(msError, LoadError);
      fLog.AppendLog('DAT Load Exception: '+LoadError);
      Exit;
    end;
  end
  else
  begin
    fTerrain.MakeNewMap(64, 64); //For debug we use blank mission
    fPlayers := TKMPlayersCollection.Create;
    fPlayers.AddPlayers(MAX_PLAYERS);
    MyPlayer := fPlayers.Player[0];
  end;

  fPlayers.AfterMissionInit(true);

  fViewport.Position := KMPointF(MyPlayer.CenterScreen);
  fViewport.ResetZoom; //This ensures the viewport is centered on the map

  fGamePlayInterface.MenuIconsEnabled(fMissionMode <> mm_Tactic);
  fGamePlayInterface.UpdateMapSize(fTerrain.MapX, fTerrain.MapY);

  fLog.AppendLog('Gameplay initialized', true);

  fGameState := gsRunning;
  fReplayMode := false;

  fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);
  BaseSave;

  fLog.AppendLog('Gameplay recording initialized',true);
  SetKaMSeed(4); //Random after StartGame and ViewReplay should match
end;


procedure TKMGame.StartMultiplayerMap(const aFilename: string);
var
  i: integer;
  PlayerRemap:TPlayerArray;
  fMissionParser:TMissionParser;
  LoadError:string;
begin
  Stop(gr_Silent); //Stop everything silently

  fCampaigns.ActiveCampaign := nil;
  fCampaigns.ActiveCampaignMap := 0;

  GameInit(true);

  //Load mission file
  fGameName := aFilename;

  fMainMenuInterface.ShowScreen(msLoading, 'script');

  //Reorder start locations and players for 1-1 2-2 result
  for i:=0 to High(PlayerRemap) do PlayerRemap[i] := PLAYER_NONE; //Init with empty values
  for i:=1 to fNetworking.NetPlayers.Count do
  begin
    PlayerRemap[fNetworking.NetPlayers[i].StartLocation - 1] := i-1; //PlayerID is 0 based
    fNetworking.NetPlayers[i].StartLocation := i;
  end;

  try //Catch exceptions
    fMissionParser := TMissionParser.Create(mpm_Multi, PlayerRemap, false);
    if not fMissionParser.LoadMission(MapNameToPath(aFilename, 'dat', true)) then
      Raise Exception.Create(fMissionParser.ErrorMessage);
    fMissionMode := fMissionParser.MissionInfo.MissionMode;
    FreeAndNil(fMissionParser);
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := 'An error has occured while parsing the file '+aFilename+'||'+E.ClassName+': '+E.Message;
      fMainMenuInterface.ShowScreen(msError, LoadError);
      fLog.AppendLog('DAT Load Exception: '+LoadError);
      fNetworking.Disconnect; //Abort all network connections
      Exit;
    end;
  end;

  fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking);
  fPlayers.AfterMissionInit(true);

  MultiplayerRig;
  if fGameState = gsNoGame then exit; //Network error

  BaseSave; //Thats our base for a game record
  SetKaMSeed(4); //Random after StartGameMP and ViewReplay should match
end;


procedure TKMGame.StartMultiplayerSave(const aFilename: string);
begin
  Stop(gr_Silent); //Stop everything silently

  fCampaigns.ActiveCampaign := nil;
  fCampaigns.ActiveCampaignMap := 0;

  GameInit(true);
  Load(aFilename);
  fGamePlayInterface.LastSaveName := aFilename; //Next time they go to save it will have this name entered

  MultiplayerRig;
end;


//All setup data gets taken from fNetworking class
procedure TKMGame.MultiplayerRig;
var
  i,k:integer;
  PlayerIndex:TPlayerIndex;
  PlayerUsed:array[0..MAX_PLAYERS-1]of boolean;
begin
  fMainMenuInterface.ShowScreen(msLoading, 'multiplayer init');

  FreeAndNil(fGameOptions);
  fGameOptions := TKMGameOptions.Create;
  fGameOptions.Peacetime := Networking.NetGameOptions.Peacetime;

  FillChar(PlayerUsed, SizeOf(PlayerUsed), #0);
  //Assign existing NetPlayers(1..N) to map players(0..N-1)
  for i:=1 to fNetworking.NetPlayers.Count do
  begin
    PlayerIndex := fNetworking.NetPlayers[i].StartLocation - 1; //PlayerID is 0 based
    fNetworking.NetPlayers[i].PlayerIndex := fPlayers.Player[PlayerIndex];
    fPlayers.Player[PlayerIndex].PlayerType := fNetworking.NetPlayers[i].PlayerType;
    fPlayers.Player[PlayerIndex].PlayerName := fNetworking.NetPlayers[i].Nikname;

    //Setup alliances
    if fNetworking.SelectGameKind = ngk_Map then
      for k:=0 to fPlayers.Count-1 do
        if (fNetworking.NetPlayers[i].Team = 0) or (fNetworking.NetPlayers.StartingLocToLocal(k+1) = -1) or
          (fNetworking.NetPlayers[i].Team <> fNetworking.NetPlayers[fNetworking.NetPlayers.StartingLocToLocal(k+1)].Team) then
          fPlayers.Player[PlayerIndex].Alliances[k] := at_Enemy
        else
          fPlayers.Player[PlayerIndex].Alliances[k] := at_Ally;

    fPlayers.Player[PlayerIndex].FlagColor := fNetworking.NetPlayers[i].FlagColor;
    PlayerUsed[PlayerIndex] := true;
  end;

  //MyPlayer is a pointer to TKMPlayer
  MyPlayer := fPlayers.Player[fNetworking.NetPlayers[fNetworking.MyIndex].StartLocation-1];

  //Clear remaining players
  for i:=fPlayers.Count-1 downto 0 do
    if not PlayerUsed[i] then
      if fNetworking.SelectGameKind = ngk_Map then
        fPlayers.RemovePlayer(i)
      else
        //We cannot remove a player from a save (as they might be interacting with other players) so make them inactive (uncontrolled human)
        fPlayers[i].PlayerType := pt_Human;

  fPlayers.SyncFogOfWar; //Syncs fog of war revelation between players AFTER alliances
  fPlayers.AddDefaultMPGoals; //Multiplayer missions don't have goals yet, so add the defaults

  fViewport.Position := KMPointF(MyPlayer.CenterScreen);
  fViewport.ResetZoom; //This ensures the viewport is centered on the map
  fRender.Render;

  fGamePlayInterface.MenuIconsEnabled(fMissionMode <> mm_Tactic);
  fGamePlayInterface.UpdateMapSize(fTerrain.MapX, fTerrain.MapY);

  fLog.AppendLog('Gameplay initialized', true);

  fGameState := gsRunning;
  fReplayMode := false;

  fNetworking.OnPlay := GameMPPlay;
  fNetworking.OnReadyToPlay := GameMPReadyToPlay;
  fNetworking.OnCommands := TGameInputProcess_Multi(fGameInputProcess).RecieveCommands;
  fNetworking.OnTextMessage := fGamePlayInterface.ChatMessage;
  fNetworking.OnPlayersSetup := fGamePlayInterface.AlliesOnPlayerSetup;
  fNetworking.OnPingInfo     := fGamePlayInterface.AlliesOnPingInfo;
  fNetworking.OnDisconnect   := GameMPDisconnect; //For auto reconnecting
  fNetworking.GameCreated;

  if fNetworking.Connected and (fNetworking.NetGameState = lgs_Loading) then GameWaitingForNetwork(true); //Waiting for players
  fGamePlayInterface.SetChatText(fMainMenuInterface.GetChatText); //Copy the typed lobby message to the in-game chat
  fGamePlayInterface.SetChatMessages(fMainMenuInterface.GetChatMessages); //Copy the old chat messages to the in-game chat

  fLog.AppendLog('Gameplay recording initialized', True);
end;


//Everyone is ready to start playing
//Issued by fNetworking at the time depending on each Players lag individually
procedure TKMGame.GameMPPlay(Sender:TObject);
begin
  GameWaitingForNetwork(false); //Finished waiting for players
  fNetworking.SendMPGameInfo(GetMissionTime,GameName);
  fLog.AppendLog('Net game began');
end;


procedure TKMGame.GameMPReadyToPlay(Sender:TObject);
begin
  //Update the list of players that are ready to play
  GameWaitingForNetwork(true);
end;


procedure TKMGame.GameMPDisconnect(const aData:string);
begin
  if fNetworking.NetGameState in [lgs_Game, lgs_Reconnecting] then
  begin
    fNetworking.OnJoinFail := GameMPDisconnect; //If the connection fails (e.g. timeout) then try again
    fNetworking.AttemptReconnection;
  end
  else
  begin
    fNetworking.Disconnect;
    Stop(gr_Disconnect, fTextLibrary[TX_GAME_ERROR_NETWORK]+' '+aData)
  end;
end;


{ Set viewport and save command log }
procedure TKMGame.GameError(aLoc:TKMPoint; aText:string);
var
  PreviousState: TGameState;
  MyZip: TZippit;
  CrashFile: string;
  i: integer;
begin
  //Negotiate duplicate calls for GameError
  if fGameState = gsNoGame then exit;

  PreviousState := GameState; //Could be running, replay, map editor, etc.
  SetGameState(gsPaused);
  if not KMSamePoint(aLoc, KMPoint(0,0)) then
  begin
    fViewport.Position := KMPointF(aLoc);
    SHOW_UNIT_ROUTES := true;
    SHOW_UNIT_MOVEMENT := true;
  end;

  fLog.AppendLog('Gameplay Error: "'+aText+'" at location '+TypeToString(aLoc));

  if (fGameInputProcess <> nil) and (fGameInputProcess.ReplayState = gipRecording) then
    fGameInputProcess.SaveToFile(SaveName('basesave', 'rpl')); //Save replay data ourselves

  MyZip := TZippit.Create;
  //Include in the bug report:
  MyZip.AddFiles(SaveName('basesave', '*'), 'Replay'); //Replay files
  MyZip.AddFile(fLog.LogPath); //Log file
  //@Krom: I think you commented out the line below while refactoring. I found it very useful
  //       when people were using custom made missions, so could we have a way to remember and
  //       include both the script and map files? (.dat and .map)
  //MyZip.AddFile(fMissionFile,'Mission'); //Mission script
  for i:=1 to AUTOSAVE_COUNT do
    MyZip.AddFiles(SaveName('autosave'+int2fix(i,2), '*'), 'Autosaves'); //All autosaves

  //Save it
  CrashFile := 'KaM Crash '+GAME_REVISION+' '+FormatDateTime('yyyy-mm-dd hh-nn-ss',Now)+'.zip'; //KaM Crash r1830 2007-12-23 15-24-33.zip
  CreateDir(ExeDir+'Crash Reports');
  MyZip.SaveToFile(ExeDir+'Crash Reports\'+CrashFile);
  FreeAndNil(MyZip); //Free the memory

  if MessageDlg(
    fTextLibrary[TX_GAME_ERROR_CAPTION]+eol+aText+eol+eol+Format(fTextLibrary[TX_GAME_ERROR_SEND_REPORT],[CrashFile]),
    mtWarning, [mbYes, mbNo], 0) <> mrYes then

    Stop(gr_Error, StringReplace(aText, eol, '|', [rfReplaceAll]) )
  else
    //If they choose to play on, start the game again because the player cannot tell that the game is paused
    SetGameState(PreviousState);
end;


procedure TKMGame.SetGameState(aNewState:TGameState);
begin
  fGameState := aNewState;
end;


//Put the game on Hold for Victory screen
procedure TKMGame.GameHold(DoHold:boolean; Msg:TGameResultMsg);
begin
  DoGameHold := false;
  fGamePlayInterface.ReleaseDirectionSelector; //In case of victory/defeat while moving troops
  Screen.Cursor := c_Default;
  fViewport.ReleaseScrollKeys;
  PlayOnState := Msg;

  if DoHold then begin
    SetGameState(gsOnHold);
    fGamePlayInterface.ShowPlayMore(true, Msg);
  end else
    case Msg of
      gr_ReplayEnd:     SetGameState(gsReplay);
      gr_Win,gr_Defeat: SetGameState(gsRunning);
    end;
end;


procedure TKMGame.RequestGameHold(Msg:TGameResultMsg);
begin
  DoGameHold := true;
  DoGameHoldState := Msg;
end;


procedure TKMGame.PlayerVictory(aPlayerIndex:TPlayerIndex);
begin
  if aPlayerIndex = MyPlayer.PlayerIndex then fSoundLib.Play(sfxn_Victory, 1.0, true); //Fade music
  if MultiplayerMode then
  begin
    if aPlayerIndex = MyPlayer.PlayerIndex then
    begin
      PlayOnState := gr_Win;
      fGamePlayInterface.ShowMPPlayMore(gr_Win);
    end;
  end
  else
    RequestGameHold(gr_Win);
end;


procedure TKMGame.PlayerDefeat(aPlayerIndex:TPlayerIndex);
begin
  if aPlayerIndex = MyPlayer.PlayerIndex then fSoundLib.Play(sfxn_Defeat, 1.0, true); //Fade music
  if MultiplayerMode then
  begin
    if aPlayerIndex = MyPlayer.PlayerIndex then
    begin
      PlayOnState := gr_Defeat;
      fGamePlayInterface.ShowMPPlayMore(gr_Defeat);
    end
    else
      fNetworking.PostLocalMessage(Format(fTextLibrary[TX_MULTIPLAYER_PLAYER_DEFEATED],
                                          [fPlayers[aPlayerIndex].PlayerName]));
  end
  else
    RequestGameHold(gr_Defeat);
end;


procedure TKMGame.GameWaitingForNetwork(aWaiting:boolean);
var WaitingPlayers: TStringList;
begin
  fWaitingForNetwork := aWaiting;

  WaitingPlayers := TStringList.Create;
  case fNetworking.NetGameState of
    lgs_Game,lgs_Reconnecting:    TGameInputProcess_Multi(fGameInputProcess).GetWaitingPlayers(fGameTickCount+1, WaitingPlayers); //GIP is waiting for next tick
    lgs_Loading: fNetworking.NetPlayers.GetNotReadyToPlayPlayers(WaitingPlayers); //We are waiting during inital loading
    else assert(false, 'GameWaitingForNetwork from wrong state '+GetEnumName(TypeInfo(TNetGameState), Integer(fNetworking.NetGameState)));
  end;

  fGamePlayInterface.ShowNetworkLag(aWaiting, WaitingPlayers, fNetworking.IsHost);
  WaitingPlayers.Free;
end;


procedure TKMGame.GameDropWaitingPlayers;
var WaitingPlayers: TStringList;
begin
  WaitingPlayers := TStringList.Create;
  case fNetworking.NetGameState of
    lgs_Game,lgs_Reconnecting:    TGameInputProcess_Multi(fGameInputProcess).GetWaitingPlayers(fGameTickCount+1, WaitingPlayers); //GIP is waiting for next tick
    lgs_Loading: fNetworking.NetPlayers.GetNotReadyToPlayPlayers(WaitingPlayers); //We are waiting during inital loading
    else assert(false); //Should not be waiting for players from any other GameState
  end;
  fNetworking.DropWaitingPlayers(WaitingPlayers);
end;


procedure TKMGame.SendMPGameInfo(Sender:TObject);
begin
  fNetworking.SendMPGameInfo(GetMissionTime,GameName);
end;


procedure TKMGame.Stop(Msg:TGameResultMsg; TextMsg:string='');
begin
  if fGameState = gsNoGame then Exit;

  fIsExiting := True;
  try
    fGameState := gsNoGame;

    if fMultiplayerMode and not fReplayMode then
    begin
      if fNetworking.Connected then fNetworking.AnnounceDisconnect;
      fNetworking.Disconnect;
    end;

    //Take results from MyPlayer before data is flushed
    if Msg in [gr_Win, gr_Defeat, gr_Cancel] then
      if fMultiplayerMode then
        fMainMenuInterface.Fill_ResultsMP
      else
        fMainMenuInterface.Fill_Results;

    if (fGameInputProcess <> nil) and (fGameInputProcess.ReplayState = gipRecording) then
      fGameInputProcess.SaveToFile(SaveName('basesave', 'rpl'));

    FreeThenNil(fGameInputProcess);
    FreeThenNil(fPlayers);
    FreeThenNil(fProjectiles);
    FreeThenNil(fTerrain);
    FreeThenNil(fMapEditor);

    FreeThenNil(fGamePlayInterface);  //Free both interfaces
    FreeThenNil(fMapEditorInterface); //Free both interfaces
    FreeThenNil(fViewport);
    ID_Tracker := 0; //Reset ID tracker

    fLog.AppendLog('Gameplay ended - ' + GetEnumName(TypeInfo(TGameResultMsg), Integer(Msg)) + ' /' + TextMsg);

    case Msg of
      gr_Win:       if fMultiplayerMode then
                      fMainMenuInterface.ShowScreen(msResultsMP, '', Msg)
                    else
                    begin
                      fMainMenuInterface.ShowScreen(msResults, '', Msg);
                      if fCampaigns.ActiveCampaign <> nil then
                        fCampaigns.UnlockNextMap;
                    end;
      gr_Defeat,
      gr_Cancel:    if fMultiplayerMode then
                      fMainMenuInterface.ShowScreen(msResultsMP, '', Msg)
                    else
                      fMainMenuInterface.ShowScreen(msResults, '', Msg);
      gr_Error:     fMainMenuInterface.ShowScreen(msError, TextMsg);
      gr_Disconnect:fMainMenuInterface.ShowScreen(msError, TextMsg);
      gr_Silent:    ;//Used when loading new savegame from gameplay UI
      gr_ReplayEnd: fMainMenuInterface.ShowScreen(msMain);
      gr_MapEdEnd:  fMainMenuInterface.ShowScreen(msMain);
    end;
  finally
    fIsExiting := false;
  end;
end;


{Mission name is absolute}
procedure TKMGame.StartMapEditor(const aFilename: string; aMultiplayer:boolean; aSizeX:integer=64; aSizeY:integer=64);
var
  i: Integer;
  LoadError: string;
  fMissionParser: TMissionParser;
begin
  if not FileExists(aFilename) and (aSizeX*aSizeY=0) then exit; //Erroneous call

  Stop(gr_Silent); //Stop MapEd as we are loading from existing MapEd session

  fLog.AppendLog('Starting Map Editor');

  SetKaMSeed(4); //Every time MapEd will be the same as previous. Good for debug.
  fGameSpeed := 1; //In case it was set in last run mission

  if fResource.DataState<>dls_All then begin
    fMainMenuInterface.ShowScreen(msLoading, 'units and houses');
    fResource.LoadGameResources;
  end;

  fMainMenuInterface.ShowScreen(msLoading, 'initializing');

  fViewport := TViewport.Create;
  fMapEditor := TKMMapEditor.Create;

  //Here comes terrain/mission init
  fTerrain := TTerrain.Create;

  if aFilename <> '' then
  try //Catch exceptions
    fMissionParser := TMissionParser.Create(mpm_Editor, False);
    if not fMissionParser.LoadMission(aFilename) then
      Raise Exception.Create(fMissionParser.ErrorMessage);
    MyPlayer := fPlayers.Player[0];
    fPlayers.AddPlayers(MAX_PLAYERS - fPlayers.Count); //Activate all players
    FreeAndNil(fMissionParser);
    fGameName := TruncateExt(ExtractFileName(aFilename));
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := 'An error has occured while parsing the file:| '+aFilename+'||'+E.ClassName+': '+E.Message;
      fMainMenuInterface.ShowScreen(msError, LoadError);
      fLog.AppendLog('DAT Load Exception: '+LoadError);
      Exit;
    end;
  end
  else
  begin
    fTerrain.MakeNewMap(aSizeX, aSizeY);
    fPlayers := TKMPlayersCollection.Create;
    fPlayers.AddPlayers(MAX_PLAYERS); //Create MAX players
    MyPlayer := fPlayers.Player[0];
    MyPlayer.PlayerType := pt_Human; //Make Player1 human by default
    fGameName := 'New Mission';
  end;

  fMapEditorInterface := TKMapEdInterface.Create(ScreenX, ScreenY);
  fMapEditorInterface.Player_UpdateColors;
  fMapEditorInterface.UpdateMapSize(fTerrain.MapX, fTerrain.MapY);
  if FileExists(aFilename) then fMapEditorInterface.SetLoadMode(aMultiplayer);
  fPlayers.AfterMissionInit(false);

  for i:=0 to fPlayers.Count-1 do //Reveal all players since we'll swap between them in MapEd
    fPlayers[i].FogOfWar.RevealEverything;

  fLog.AppendLog('Gameplay initialized', True);

  fRender.Resize(ScreenX, ScreenY, rm2D);
  fViewport.Resize(ScreenX, ScreenY);

  fGameTickCount := 0; //Restart counter

  fGameState := gsEditor;
  fReplayMode := false;
end;


procedure TKMGame.AutoSave;
var i: integer;
begin
  Save('autosave'); //Temp file

  DeleteFile(SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'sav'));
  DeleteFile(SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'rpl'));
  DeleteFile(SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'bas'));
  for i:=AUTOSAVE_COUNT downto 2 do //03 to 01
  begin
    RenameFile(SaveName('autosave'+int2fix(i-1,2), 'sav'), SaveName('autosave'+int2fix(i,2), 'sav'));
    RenameFile(SaveName('autosave'+int2fix(i-1,2), 'rpl'), SaveName('autosave'+int2fix(i,2), 'rpl'));
    RenameFile(SaveName('autosave'+int2fix(i-1,2), 'bas'), SaveName('autosave'+int2fix(i,2), 'bas'));
  end;

  RenameFile(SaveName('autosave', 'sav'), SaveName('autosave01', 'sav'));
  RenameFile(SaveName('autosave', 'rpl'), SaveName('autosave01', 'rpl'));
  RenameFile(SaveName('autosave', 'bas'), SaveName('autosave01', 'bas'));
end;


procedure TKMGame.BaseSave;
begin
  Save('basesave'); //Temp file

  //In Linux CopyFile does not overwrite
  if FileExists(SaveName('basesave', 'bas')) then DeleteFile(SaveName('basesave','bas'));
  CopyFile(PChar(SaveName('basesave','sav')), PChar(SaveName('basesave','bas')), false);
end;


procedure TKMGame.SaveMapEditor(const aMissionName:string; aMultiplayer:boolean);
var i:integer; fMissionParser: TMissionParser;
begin
  if aMissionName = '' then exit;

  fPlayers.RemoveEmptyPlayers;

  CreateDir(ExeDir+'Maps');
  CreateDir(ExeDir+'Maps\'+aMissionName);
  fTerrain.SaveToFile(MapNameToPath(aMissionName, 'map', aMultiplayer));
  fMissionParser := TMissionParser.Create(mpm_Editor,false);
  fMissionParser.SaveDATFile(MapNameToPath(aMissionName, 'dat', aMultiplayer));
  FreeAndNil(fMissionParser);
  fGameName := aMissionName;

  fPlayers.AddPlayers(MAX_PLAYERS-fPlayers.Count); //Activate all players
  for i:=0 to fPlayers.Count-1 do //Reveal all players since we'll swap between them in MapEd
    fPlayers[i].FogOfWar.RevealEverything;
  if MyPlayer = nil then MyPlayer := fPlayers[0];
end;


{ Check if replay files exist at location }
function TKMGame.ReplayExists(const aSaveName:string; aMultiplayer:boolean):boolean;
var OldMultiplayerMode:boolean;
begin
  OldMultiplayerMode := fMultiplayerMode;
  fMultiplayerMode := aMultiplayer;
  Result := FileExists(SaveName(aSaveName, 'bas')) and
            FileExists(SaveName(aSaveName, 'rpl'));
  fMultiplayerMode := OldMultiplayerMode;
end;

//Restart the replay but do not change the viewport position/zoom
procedure TKMGame.RestartReplay;
var OldCenter:TKMPointF; OldZoom:single;
begin
  OldCenter := fViewport.Position;
  OldZoom := fViewport.Zoom;

  StartReplay(fReplayFile,fMultiplayerMode);

  fViewport.Position := OldCenter;
  fViewport.Zoom := OldZoom;
end;


procedure TKMGame.StartReplay(const aSaveName:string; aMultiplayer:boolean);
begin
  Stop(gr_Silent);
  fReplayMode := true;

  GameInit(aMultiplayer);

  Load(aSaveName,true); //We load what was saved right before starting Recording

  FreeAndNil(fGameInputProcess); //Override GIP from savegame
  fGameInputProcess := TGameInputProcess_Single.Create(gipReplaying);
  fGameInputProcess.LoadFromFile(SaveName(aSaveName,'rpl'));

  SetKaMSeed(4); //Random after StartGame and ViewReplay should match
  fGameState := gsReplay;
  fReplayMode := true;
  fReplayFile := aSaveName;
end;


procedure TKMGame.NetworkInit;
begin
  if fNetworking = nil then
    fNetworking := TKMNetworking.Create(fGlobalSettings.MasterServerAddress,
                                        fGlobalSettings.AutoKickTimeout,
                                        fGlobalSettings.PingInterval,
                                        fGlobalSettings.MasterAnnounceInterval,
                                        fGlobalSettings.GetLocalID);
  fNetworking.OnMPGameInfoChanged := SendMPGameInfo;
end;


//TDateTime stores days/months/years as 1 and hours/minutes/seconds as fractions of a 1
//Treat 10 ticks as 1 sec irregardless of user-set pace
function TKMGame.GetMissionTime:TDateTime;
begin
  //Convert cardinal into TDateTime, where 1hour = 1/24 and so on..
  Result := fGameTickCount/24/60/60/10;
end;


function TKMGame.GetPeacetimeRemaining:TDateTime;
begin
  Result := Max(0,Int64(fGame.GameOptions.Peacetime*600)-fGame.GameTickCount)/24/60/60/10;
end;


//Tests whether time has past
function TKMGame.CheckTime(aTimeTicks:cardinal):boolean;
begin
  Result := (fGameTickCount >= aTimeTicks);
end;


function TKMGame.IsPeaceTime:boolean;
begin
  Result := not CheckTime(fGameOptions.Peacetime*600);
end;


procedure TKMGame.UpdatePeacetime;
var PeaceTicksRemaining:cardinal;
begin
  PeaceTicksRemaining := Max(0,Int64((fGame.GameOptions.Peacetime*600))-fGame.GameTickCount);
  if (PeaceTicksRemaining = 1) and MultiplayerMode then
  begin
    Networking.PostLocalMessage(fTextLibrary[TX_MP_PEACETIME_OVER],false);
    fSoundLib.Play(sfxn_Error1);
  end;
end;


function TKMGame.AllowDebugRendering:boolean;
begin
  Result := MULTIPLAYER_CHEATS or not MultiplayerMode;
end;


function TKMGame.GetNewID:cardinal;
begin
  inc(ID_Tracker);
  Result := ID_Tracker;
end;


procedure TKMGame.SetGameSpeed(aSpeed: Word);
begin
  Assert(aSpeed > 0);

  //Make the speed toggle between 1 and desired value
  if aSpeed = fGameSpeed then
    fGameSpeed := 1
  else
    fGameSpeed := aSpeed;

  fGamePlayInterface.ShowClock(fGameSpeed <> 1); //todo: Show current speed value on clock
end;


procedure TKMGame.StepOneFrame;
begin
  Assert(fGameState in [gsPaused, gsReplay], 'We can work step-by-step only in Replay');
  SetGameSpeed(1); //Do not allow multiple updates in UpdateState loop
  fAdvanceFrame := True;
end;


//Saves the game in all its glory
//Base savegame gets copied from save99.bas
//Saves command log to RPL file
procedure TKMGame.Save(const aFilename: string);
var
  SaveStream: TKMemoryStream;
  fGameInfo: TKMGameInfo;
  i, NetIndex: integer;
begin
  fLog.AppendLog('Saving game');
  if not (fGameState in [gsPaused, gsRunning]) then begin
    Assert(false, 'Saving from wrong state?');
    Exit;
  end;

  SaveStream := TKMemoryStream.Create;

  fGameInfo := TKMGameInfo.Create;
  fGameInfo.Title := fGameName;
  fGameInfo.TickCount := fGameTickCount;
  fGameInfo.MissionMode := fMissionMode;
  fGameInfo.MapSizeX := fTerrain.MapX;
  fGameInfo.MapSizeY := fTerrain.MapY;
  fGameInfo.VictoryCondition := 'Win';
  fGameInfo.DefeatCondition := 'Lose';
  fGameInfo.PlayerCount := fPlayers.Count;
  for i:=0 to fPlayers.Count-1 do
  begin
    if fNetworking <> nil then
      NetIndex := fNetworking.NetPlayers.PlayerIndexToLocal(i)
    else
      NetIndex := -1;

    if NetIndex = -1 then begin
      fGameInfo.LocationName[i] := 'Unknown';
      fGameInfo.PlayerTypes[i] := pt_Human;
      fGameInfo.ColorID[i] := 0;
      fGameInfo.Team[i] := 0;
    end else begin
      fGameInfo.LocationName[i] := fNetworking.NetPlayers[NetIndex].Nikname;
      fGameInfo.PlayerTypes[i] := fNetworking.NetPlayers[NetIndex].PlayerType;
      fGameInfo.ColorID[i] := fNetworking.NetPlayers[NetIndex].FlagColorID;
      fGameInfo.Team[i] := fNetworking.NetPlayers[NetIndex].Team;
    end
  end;

  fGameInfo.Save(SaveStream);
  fGameInfo.Free;
  fGameOptions.Save(SaveStream);

  fCampaigns.Save(SaveStream);
  SaveStream.Write(ID_Tracker); //Units-Houses ID tracker
  SaveStream.Write(GetKaMSeed); //Include the random seed in the save file to ensure consistency in replays

  //Because the viewport is only saved in singleplayer we need to know whether it is included in this save,
  //so we can load multiplayer saves in single player and vice versa.
  SaveStream.Write(fMultiplayerMode);
  if not fMultiplayerMode then
    SaveStream.Write(PlayOnState, SizeOf(PlayOnState));

  fTerrain.Save(SaveStream); //Saves the map
  fPlayers.Save(SaveStream); //Saves all players properties individually
  fProjectiles.Save(SaveStream);

  if not fMultiplayerMode then
  begin
    fViewport.Save(SaveStream); //Saves viewed area settings
    fGamePlayInterface.Save(SaveStream); //Saves message queue and school/barracks selected units
    //Don't include fGameSettings.Save it's not required for settings are Game-global, not mission
  end;

  CreateDir(ExeDir+'Saves\'); //Makes the folder incase it was deleted
  CreateDir(ExeDir+'SavesMP\');

  SaveStream.SaveToFile(SaveName(aFilename,'sav')); //Some 70ms for TPR7 map
  SaveStream.Free;

  fLog.AppendLog('Save done');

  CopyFile(PChar(SaveName('basesave','bas')), PChar(SaveName(aFilename,'bas')), false); //replace Replay base savegame
  fGameInputProcess.SaveToFile(SaveName(aFilename,'rpl')); //Adds command queue to savegame

  fLog.AppendLog('Saving game', true);
end;


procedure TKMGame.Load(const aFilename: string; aReplay:boolean=false);
var
  LoadStream:TKMemoryStream;
  fGameInfo: TKMGameInfo;
  LoadError,LoadFileExt:string;
  LoadedSeed:Longint;
  SaveIsMultiplayer:boolean;
begin
  fLog.AppendLog('Loading game');
  if aReplay then LoadFileExt := 'bas' else LoadFileExt := 'sav';

  LoadStream := TKMemoryStream.Create; //Read data from file into stream
  try //Catch exceptions
    if not FileExists(SaveName(aFileName, LoadFileExt)) then Raise Exception.Create('Savegame could not be found');

    LoadStream.LoadFromFile(SaveName(aFileName, LoadFileExt));

    //We need only few essential parts from GameInfo, the rest is duplicate from fTerrain and fPlayers
    fGameInfo := TKMGameInfo.Create;
    try
      fGameInfo.Load(LoadStream);
      fGameName := fGameInfo.Title;
      fGameTickCount := fGameInfo.TickCount;
      fMissionMode := fGameInfo.MissionMode;
    finally //@Lewin: I'm not sure if exceptions from GameInfo will be caught here later on
            //@Krom: They won't be caught, which is bad. (try adding a raise to the line above to test)
      fGameInfo.Free;
    end;

    fGameOptions.Load(LoadStream);
    fCampaigns.Load(LoadStream);
    LoadStream.Read(ID_Tracker);
    LoadStream.Read(LoadedSeed);

    //So we can allow loading of multiplayer saves in single player and vice versa we need to know which type THIS save is
    LoadStream.Read(SaveIsMultiplayer);
    if not SaveIsMultiplayer then
      LoadStream.Read(PlayOnState, SizeOf(PlayOnState));

    fPlayers := TKMPlayersCollection.Create;

    //Load the data into the game
    fTerrain.Load(LoadStream);
    fPlayers.Load(LoadStream);
    fProjectiles.Load(LoadStream);

    //Multiplayer saves don't have this piece of information due to each player has his own version
    //@Lewin: Does this means that MessageList in UI is lost?
    //@Krom: Yes, it's lost for now. Multiplayer saves must be identical so I guess we'll have to send all message commands through
    //       the GIC (add, delete) even though they won't show to other players.
    if not SaveIsMultiplayer then
    begin
      fViewport.Load(LoadStream);
      fGamePlayInterface.Load(LoadStream);
    end;

    LoadStream.Free;

    if fMultiplayerMode and not aReplay then
      fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking)
    else
      fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);
    fGameInputProcess.LoadFromFile(SaveName(aFileName,'rpl'));

    if not aReplay then
      CopyFile(PChar(SaveName(aFileName,'bas')), PChar(SaveName('basesave','bas')), false); //replace Replay base savegame

    fGamePlayInterface.MenuIconsEnabled(fMissionMode <> mm_Tactic); //Preserve disabled icons
    fGamePlayInterface.UpdateMapSize(fTerrain.MapX, fTerrain.MapY);

    fPlayers.SyncLoad; //Should parse all Unit-House ID references and replace them with actual pointers
    fTerrain.SyncLoad; //IsUnit values should be replaced with actual pointers
    fViewport.ResetZoom; //This ensures the viewport is centered on the map (game could have been saved with a different resolution/zoom)
  except
    on E : Exception do
    begin
      //Trap the exception and show the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
      LoadError := 'An error was encountered while parsing the file '+aFileName+'.|Details of the error:|'+
                    E.ClassName+' error raised with message: '+E.Message;
      fMainMenuInterface.ShowScreen(msError, LoadError); //This will show an option to return back to menu
      Exit;
    end;
  end;

  SetKaMSeed(LoadedSeed);

  fLog.AppendLog('Loading game', True);
end;


procedure TKMGame.UpdateState;
var i:integer;
begin
  inc(fGlobalTickCount);
  case fGameState of
    gsPaused:   ; //Don't exit here as there is code afterwards to execute (e.g. play next music track)
    gsOnHold:   ; //Don't exit here as there is code afterwards to execute (e.g. play next music track)
    gsNoGame:   begin
                  if fNetworking <> nil then fNetworking.UpdateState(fGlobalTickCount); //Measures pings
                  fMainMenuInterface.UpdateState;
                end;
    gsRunning:  begin
                  if fMultiplayerMode then  fNetworking.UpdateState(fGlobalTickCount); //Measures pings
                  if fMultiplayerMode and (fGameTickCount mod 100 = 0) then
                    SendMPGameInfo(Self); //Send status to the server every 10 seconds
                  if not fMultiplayerMode or (fNetworking.NetGameState <> lgs_Loading) then
                  begin
                  try //Catch exceptions during update state

                    for i:=1 to fGameSpeed do
                    begin
                      if fGameInputProcess.CommandsConfirmed(fGameTickCount+1) then
                      begin
                        if fWaitingForNetwork then GameWaitingForNetwork(false); //No longer waiting for players
                        inc(fGameTickCount); //Thats our tick counter for gameplay events
                        if fMultiplayerMode then fNetworking.LastProcessedTick := fGameTickCount;
                        UpdatePeacetime; //Send warning messages about peacetime if required
                        fTerrain.UpdateState;
                        fPlayers.UpdateState(fGameTickCount); //Quite slow
                        if fGameState = gsNoGame then exit; //Quit the update if game was stopped by MyPlayer defeat
                        fProjectiles.UpdateState; //If game has stopped it's NIL

                        fGameInputProcess.RunningTimer(fGameTickCount); //GIP_Multi issues all commands for this tick
                        //In aggressive mode store a command every tick so we can find exactly when a replay mismatch occurs
                        if AGGRESSIVE_REPLAYS then
                          fGameInputProcess.CmdTemp(gic_TempDoNothing);

                        //Each 1min of gameplay time
                        //Don't autosave if the game was put on hold during this tick
                        if (fGameTickCount mod 600 = 0) and fGlobalSettings.Autosave and not (GameState = gsOnHold) then
                          AutoSave;

                        //During this tick we were requested to GameHold
                        if DoGameHold then break; //Break the for loop (if we are using speed up)
                      end
                      else
                      begin
                        fGameInputProcess.WaitingForConfirmation(fGameTickCount);
                        if TGameInputProcess_Multi(fGameInputProcess).GetNumberConsecutiveWaits > 5 then
                          GameWaitingForNetwork(true);
                      end;
                      fGameInputProcess.UpdateState(fGameTickCount); //Do maintenance
                    end;

                  except
                    //Trap the exception and show the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
                    on E : ELocError do GameError(E.Loc, E.ClassName+': '+E.Message);
                    on E : Exception do GameError(KMPoint(0,0), E.ClassName+': '+E.Message);
                  end;
                  end;
                  fGamePlayInterface.UpdateState;
                end;
    gsReplay:   begin
                  try //Catch exceptions during update state

                    for i:=1 to fGameSpeed do
                    begin
                      inc(fGameTickCount); //Thats our tick counter for gameplay events
                      fTerrain.UpdateState;
                      fPlayers.UpdateState(fGameTickCount); //Quite slow
                      if fGameState = gsNoGame then exit; //Quit the update if game was stopped by MyPlayer defeat
                      fProjectiles.UpdateState; //If game has stopped it's NIL

                      //Issue stored commands
                      fGameInputProcess.ReplayTimer(fGameTickCount);
                      if fGameState = gsNoGame then exit; //Quit if the game was stopped by a replay mismatch
                      if not SkipReplayEndCheck and fGameInputProcess.ReplayEnded then
                        RequestGameHold(gr_ReplayEnd);

                      if fAdvanceFrame then begin
                        fAdvanceFrame := false;
                        SetGameState(gsPaused);
                      end;

                      //During this tick we were requested to GameHold
                      if DoGameHold then break; //Break the for loop (if we are using speed up)
                    end;

                    fGamePlayInterface.UpdateState;

                  except
                    //Trap the exception and show the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
                    on E : ELocError do GameError(E.Loc, E.ClassName+': '+E.Message);
                    on E : Exception do GameError(KMPoint(0,0), E.ClassName+': '+E.Message);
                  end;
                end;
    gsEditor:   begin
                  fMapEditorInterface.UpdateState;
                  fTerrain.IncAnimStep;
                  fPlayers.IncAnimStep;
                end;
    end;

  //Every 1000ms
  if fGlobalTickCount mod 10 = 0 then
  begin
    //Minimap
    if (fGameState in [gsRunning, gsReplay, gsEditor]) then
      fTerrain.UpdateMinimapData(fGameState = gsEditor);

    //Music
    if fMusicLib.IsMusicEnded then
      fMusicLib.PlayNextTrack; //Feed new music track

    //StatusBar
    if (fGameState in [gsRunning, gsReplay]) then
      Form1.StatusBar1.Panels[2].Text := 'Time: ' + FormatDateTime('hh:nn:ss', GetMissionTime);
  end;
  if DoGameHold then GameHold(true,DoGameHoldState);
end;


{This is our real-time thread, use it wisely}
procedure TKMGame.UpdateStateIdle(aFrameTime:cardinal);
begin
  case fGameState of
    gsRunning,
    gsReplay:   fViewport.UpdateStateIdle(aFrameTime); //Check to see if we need to scroll
    gsEditor:   begin
                  fViewport.UpdateStateIdle(aFrameTime); //Check to see if we need to scroll
                  fTerrain.UpdateStateIdle;
                end;
  end;
  if fMusicLib <> nil then fMusicLib.UpdateStateIdle;
  if fSoundLib <> nil then fSoundLib.UpdateStateIdle;
  if fNetworking <> nil then
    fNetworking.UpdateStateIdle;
end;


procedure TKMGame.PaintInterface;
begin
  case fGameState of
    gsNoGame:  fMainMenuInterface.Paint;
    gsPaused:  fGamePlayInterface.Paint;
    gsOnHold:  fGamePlayInterface.Paint;
    gsRunning: fGamePlayInterface.Paint;
    gsReplay:  fGamePlayInterface.Paint;
    gsEditor:  fMapEditorInterface.Paint;
  end;
end;


function TKMGame.SaveName(const aName, aExt: string): string;
begin
  if fMultiplayerMode then
    Result := ExeDir + 'SavesMP\' + aName + '.' + aExt
  else
    Result := ExeDir + 'Saves\' + aName + '.' + aExt;
end;


function TKMGame.MapSizeText: string;
begin
  if fTerrain <> nil then
    Result := 'Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY)
  else
    Result := 'No map';
end;


end.

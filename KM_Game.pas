unit KM_Game;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, FileUtil, {$ENDIF}
  {$IFDEF WDC} MPlayer, {$ENDIF}
  Forms, Controls, Classes, Dialogs, ExtCtrls, SysUtils, KromUtils, Math, TypInfo,
  {$IFDEF USE_MAD_EXCEPT} MadExcept, KM_Exceptions, {$ENDIF}
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Utils,
  KM_Networking,
  KM_Campaigns, KM_EventProcess,
  KM_GameInputProcess, KM_PlayersCollection, KM_Render, KM_RenderAux, KM_RenderPool, KM_TextLibrary,
  KM_InterfaceDefaults, KM_InterfaceMapEditor, KM_InterfaceGamePlay, KM_InterfaceMainMenu,
  KM_Resource, KM_Terrain, KM_PathFinding, KM_MissionScript, KM_Projectiles, KM_Sound, KM_Viewport, KM_Settings, KM_Music, KM_Points,
  KM_ArmyEvaluation, KM_GameOptions, KM_PerfLog, KM_Locales;

type
  //Methods relevant to gameplay
  TKMGame = class
  private //Irrelevant to savegame
    fFormPassability: Integer;
    fIsExiting: boolean; //Set this to true on Exit and unit/house pointers will be released without cross-checking
    fTimerGame: TTimer;
    fGameSpeed: Word; //Actual speedup value
    fGameSpeedMultiplier: Word; //how many ticks are compressed in one
    fMultiplayerMode: Boolean;
    fReplayMode: Boolean; //Separate flag since Replay can change fGameState
    fReplayFile: string;
    fWaitingForNetwork: Boolean;
    fGameOptions: TKMGameOptions;
    fAdvanceFrame: Boolean; //Replay variable to advance 1 frame, afterwards set to false
    fProjectiles: TKMProjectiles;
    fGameInputProcess: TGameInputProcess;
    fPathfinding: TPathFinding;

    fViewport: TViewport;
    fPerfLog: TKMPerfLog;
    fMissionFile: string; //Path to mission we are playing, so it gets saved to crashreport

  //Should be saved
    fGameTickCount: Cardinal;
    fGameName: string;
    fMissionMode: TKMissionMode;
    ID_Tracker: Cardinal; //Mainly Units-Houses tracker, to issue unique numbers on demand


    procedure GameMPDisconnect(const aData:string);
    procedure MultiplayerRig;

  public
    PlayOnState: TGameResultMsg;
    DoGameHold:boolean; //Request to run GameHold after UpdateState has finished
    DoGameHoldState: TGameResultMsg; //The type of GameHold we want to occur due to DoGameHold
    SkipReplayEndCheck:boolean;
    constructor Create(aMultiplayerMode, aReplayMode: Boolean; aRender: TRender);
    destructor Destroy; override;

    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure GameStart(aMissionFile, aGameName:string);
    procedure Load(const aFileName: string; aReplay:boolean=false);

    procedure StartSingleMap(aMissionFile, aGameName:string; aBlockMarket:Boolean=False);
    procedure StartSingleSave(aFileName:string);
    procedure StartLastMap;
    procedure StartMultiplayerSave(const aFileName: string);
    procedure StartMultiplayerMap(const aFileName: string);
    procedure StartMapEditor(const aFileName: string; aMultiplayer:boolean; aSizeX:integer=64; aSizeY:integer=64);
    procedure Stop(Msg:TGameResultMsg; TextMsg:string='');

    procedure GameMPPlay(Sender:TObject);
    procedure GameMPReadyToPlay(Sender:TObject);
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
    property GameName:string read fGameName;
    property MultiplayerMode:boolean read fMultiplayerMode;
    property ReplayMode: Boolean read fReplayMode;
    property FormPassability:integer read fFormPassability write fFormPassability;
    property IsExiting:boolean read fIsExiting;
    property MissionMode:TKMissionMode read fMissionMode write fMissionMode;
    function AllowDebugRendering:boolean;
    function GetNewID:cardinal;
    procedure SetGameSpeed(aSpeed: word);
    procedure StepOneFrame;
    function SaveName(const aName, aExt: string; aMultiPlayer: Boolean): string;

    procedure UpdateGameCursor(X,Y: Integer; Shift: TShiftState);

    property Pathfinding: TPathFinding read fPathfinding;
    property Projectiles: TKMProjectiles read fProjectiles;
    property GameInputProcess: TGameInputProcess read fGameInputProcess;
    property GameOptions: TKMGameOptions read fGameOptions;
    property Viewport: TViewport read fViewport;

    procedure Save(const aFileName: string);
    procedure PrintScreen(aFilename: string = '');
    {$IFDEF USE_MAD_EXCEPT}
    procedure AttachCrashReport(const ExceptIntf: IMEException; aZipFile:string);
    {$ENDIF}
    procedure ReplayInconsistancy;

    procedure Render;
    procedure UpdateGame(Sender: TObject);
    procedure UpdateState(Sender: TObject);
    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


var
  fGameG: TKMGame;


implementation
uses
  KM_Player, KM_GameInfo, KM_GameInputProcess_Single, KM_GameInputProcess_Multi, KM_Log,
  KM_ResourceCursors;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
//aMultiplayer - is this a multiplayer game
//aRender - who will be rendering the Game session
constructor TKMGame.Create(aMultiplayerMode, aReplayMode: Boolean; aRender: TRender);
begin
  inherited Create;

  fMultiplayerMode := aMultiplayerMode;
  fReplayMode := aReplayMode;
  fAdvanceFrame := False;
  ID_Tracker    := 0;
  PlayOnState   := gr_Cancel;
  DoGameHold    := False;
  SkipReplayEndCheck := False;
  fWaitingForNetwork := False;
  fGameOptions  := TKMGameOptions.Create;

  //todo: Maybe we should reset the GameCursor? If I play 192x192 map, quit, and play a 64x64 map
  //      my cursor could be at (190,190) if the player starts with his cursor over the controls panel...
  //      This caused a crash in RenderCursors which I fixed by adding range checking to CheckTileRevelation
  //      (good idea anyway) There could be other crashes caused by this.
  fViewport := TViewport.Create(aRender.ScreenX, aRender.ScreenY);

  fTimerGame := TTimer.Create(nil);
  SetGameSpeed(1); //Initialize relevant variables
  fTimerGame.OnTimer := UpdateGame;
  fTimerGame.Enabled := True;

  //Here comes terrain/mission init
  SetKaMSeed(4); //Every time the game will be the same as previous. Good for debug.
  fTerrain := TTerrain.Create;

  InitUnitStatEvals; //Army

  fPerfLog := TKMPerfLog.Create;
  fLog.AppendLog('<== Game creation is done ==>');
  fPathfinding := TPathfinding.Create;
  fProjectiles := TKMProjectiles.Create;
  fEventsManager := TKMEventsManager.Create;

  fRenderPool := TRenderPool.Create(aRender);

  fGameTickCount := 0; //Restart counter
end;


{ Destroy what was created }
destructor TKMGame.Destroy;
begin
  fTimerGame.Enabled := False;

  if (fGameInputProcess <> nil) and (fGameInputProcess.ReplayState = gipRecording) then
    fGameInputProcess.SaveToFile(SaveName('basesave', 'rpl', fMultiplayerMode));

  fPerfLog.SaveToFile(ExeDir + 'Logs\PerfLog.txt');

  FreeAndNil(fTimerGame);

  FreeAndNil(fGameInputProcess);
  FreeAndNil(fRenderPool);
  FreeAndNil(fGameOptions);
  fPerfLog.Free;
  inherited;
end;


procedure TKMGame.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer);
var PrevCursor: TKMPointF;
begin
    UpdateGameCursor(X, Y, Shift); //Make sure we have the correct cursor position to begin with
    PrevCursor := GameCursor.Float;
    fViewport.Zoom := fViewport.Zoom + WheelDelta / 2000;
    UpdateGameCursor(X, Y, Shift); //Zooming changes the cursor position
    //Move the center of the screen so the cursor stays on the same tile, thus pivoting the zoom around the cursor
    fViewport.Position := KMPointF(fViewport.Position.X + PrevCursor.X-GameCursor.Float.X,
                                   fViewport.Position.Y + PrevCursor.Y-GameCursor.Float.Y);
    UpdateGameCursor(X, Y, Shift); //Recentering the map changes the cursor position
end;


procedure TKMGame.StartLastMap;
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

  StartSingleSave('basesave');
end;


procedure TKMGame.GameStart(aMissionFile, aGameName: string);
  function MapSizeText: string;
  begin
    if fTerrain <> nil then
      Result := 'Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY)
    else
      Result := 'No map';
  end;
var
  LoadError: string;
  fMissionParser: TMissionParserStandard;
begin
  fLog.AppendLog('GameStart');

  fGameName := aGameName;
  fMissionFile := aMissionFile;


  if aMissionFile <> '' then
  try //Catch exceptions
    fLog.AppendLog('Loading DAT file for singleplayer: '+aMissionFile);
    fMissionParser := TMissionParserStandard.Create(mpm_Single, false);
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
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], [aMissionFile])+'||'+E.ClassName+': '+E.Message;
      fMainMenuInterface.ShowScreen(msError, LoadError);
      fLog.AppendLog('DAT Load Exception: '+LoadError);
      Exit;
    end;
  end
  else
  begin
    fTerrain.MakeNewMap(64, 64, False); //For debug we use blank mission
    fPlayers := TKMPlayersCollection.Create;
    fPlayers.AddPlayers(MAX_PLAYERS);
    MyPlayer := fPlayers.Player[0];
  end;

  fEventsManager.LoadFromFile(ChangeFileExt(aMissionFile, '.evt'));
  fTextLibrary.LoadMissionStrings(ChangeFileExt(aMissionFile, '.%s.libx'));

  fPlayers.AfterMissionInit(true);

  fViewport.ResizeMap(fTerrain.MapX, fTerrain.MapY);
  fViewport.Position := KMPointF(MyPlayer.CenterScreen);
  fViewport.ResetZoom; //This ensures the viewport is centered on the map

  fGamePlayInterface.UpdateMapSize(fTerrain.MapX, fTerrain.MapY);

  fLog.AppendLog('Gameplay initialized', true);


  fGamePlayInterface.UpdateMenuState(fMissionMode = mm_Tactic, False);

  fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);
  BaseSave;

  if Assigned(OnCursorUpdate) then
    OnCursorUpdate(0, MapSizeText);

  fLog.AppendLog('Gameplay recording initialized',true);
  SetKaMSeed(4); //Random after StartGame and ViewReplay should match
end;


procedure TKMGame.StartMultiplayerMap(const aFileName: string);
var
  i: integer;
  PlayerRemap:TPlayerArray;
  fMissionParser:TMissionParserStandard;
  LoadError:string;
begin
  Stop(gr_Silent); //Stop everything silently

  fCampaigns.SetActive(nil, 0);

  GameInit(true);

  //Load mission file
  fGameName := aFileName;
  fReplayMode := False;

  GameLoadingStep(fTextLibrary[TX_MENU_LOADING_SCRIPT]);

  //Reorder start locations and players for 1-1 2-2 result
  for i:=0 to High(PlayerRemap) do PlayerRemap[i] := PLAYER_NONE; //Init with empty values
  for i:=1 to fNetworking.NetPlayers.Count do
  begin
    PlayerRemap[fNetworking.NetPlayers[i].StartLocation - 1] := i-1; //PlayerID is 0 based
    fNetworking.NetPlayers[i].StartLocation := i;
  end;

  try //Catch exceptions
    fLog.AppendLog('Loading DAT file for multiplayer: '+MapNameToPath(aFileName, 'dat', true));
    fMissionParser := TMissionParserStandard.Create(mpm_Multi, PlayerRemap, false);
    if not fMissionParser.LoadMission(MapNameToPath(aFileName, 'dat', true)) then
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
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], [aFileName])+'||'+E.ClassName+': '+E.Message;
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


procedure TKMGame.StartMultiplayerSave(const aFileName: string);
begin
  Stop(gr_Silent); //Stop everything silently

  fCampaigns.SetActive(nil, 0);

  GameInit(true);
  Load(aFileName);
  fReplayMode := False;
  fGamePlayInterface.LastSaveName := aFileName; //Next time they go to save it will have this name entered

  MultiplayerRig;
end;


//All setup data gets taken from fNetworking class
procedure TKMGame.MultiplayerRig;
var
  i,k:integer;
  PlayerIndex:TPlayerIndex;
  PlayerUsed:array[0..MAX_PLAYERS-1]of boolean;
begin
  GameLoadingStep(fTextLibrary[TX_MENU_LOADING_MP_INITIALIZING]);

  //Copy all game options from lobby to this game
  fGameOptions.Peacetime := Networking.NetGameOptions.Peacetime;

  FillChar(PlayerUsed, SizeOf(PlayerUsed), #0);
  //Assign existing NetPlayers(1..N) to map players(0..N-1)
  for i:=1 to fNetworking.NetPlayers.Count do
  begin
    PlayerIndex := fNetworking.NetPlayers[i].StartLocation - 1; //PlayerID is 0 based
    fNetworking.NetPlayers[i].PlayerIndex := fPlayers.Player[PlayerIndex];
    fPlayers.Player[PlayerIndex].PlayerType := fNetworking.NetPlayers[i].GetPlayerType;
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
  if fNetworking.SelectGameKind = ngk_Map then
    fPlayers.AddDefaultMPGoals(fMissionMode); //Multiplayer missions don't have goals yet, so add the defaults

  fViewport.ResizeMap(fTerrain.MapX, fTerrain.MapY);
  fViewport.Position := KMPointF(MyPlayer.CenterScreen);
  fViewport.ResetZoom; //This ensures the viewport is centered on the map
  Render;

  fGamePlayInterface.UpdateMapSize(fTerrain.MapX, fTerrain.MapY);

  fLog.AppendLog('Gameplay initialized', true);

  SetGameState(gsRunning);
  fGamePlayInterface.UpdateMenuState(fMissionMode = mm_Tactic, False);

  fNetworking.OnPlay           := GameMPPlay;
  fNetworking.OnReadyToPlay    := GameMPReadyToPlay;
  fNetworking.OnCommands       := TGameInputProcess_Multi(fGameInputProcess).RecieveCommands;
  fNetworking.OnTextMessage    := fGamePlayInterface.ChatMessage;
  fNetworking.OnPlayersSetup   := fGamePlayInterface.AlliesOnPlayerSetup;
  fNetworking.OnPingInfo       := fGamePlayInterface.AlliesOnPingInfo;
  fNetworking.OnDisconnect     := GameMPDisconnect; //For auto reconnecting
  fNetworking.OnReassignedHost := nil; //So it is no longer assigned to a lobby event
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
  SendMPGameInfo(Self);
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
    if WRITE_RECONNECT_LOG then fLog.AppendLog('GameMPDisconnect: '+aData);
    fNetworking.PostLocalMessage('Connection failed: '+aData,false); //Debugging that should be removed later
    fNetworking.OnJoinFail := GameMPDisconnect; //If the connection fails (e.g. timeout) then try again
    fNetworking.OnJoinAssignedHost := nil;
    fNetworking.OnJoinSucc := nil;
    fNetworking.AttemptReconnection;
  end
  else
  begin
    fNetworking.Disconnect;
    Stop(gr_Disconnect, fTextLibrary[TX_GAME_ERROR_NETWORK]+' '+aData)
  end;
end;

{$IFDEF USE_MAD_EXCEPT}
procedure TKMGame.AttachCrashReport(const ExceptIntf: IMEException; aZipFile:string);

  procedure AttachFile(const aFile:string);
  begin
    if (aFile = '') or not FileExists(aFile) then Exit;
    ExceptIntf.AdditionalAttachments.Add(aFile, '', aZipFile);
  end;

var I: Integer;
begin
  fLog.AppendLog('Creating crash report...');

  if (fGameInputProcess <> nil) and (fGameInputProcess.ReplayState = gipRecording) then
    fGameInputProcess.SaveToFile(SaveName('basesave', 'rpl', fMultiplayerMode)); //Save replay data ourselves

  AttachFile(SaveName('basesave', 'rpl'));
  AttachFile(SaveName('basesave', 'bas'));
  AttachFile(SaveName('basesave', 'sav'));

  AttachFile(fMissionFile);

  for I := 1 to AUTOSAVE_COUNT do //All autosaves
  begin
    AttachFile(SaveName('autosave' + Int2Fix(I, 2), 'rpl'));
    AttachFile(SaveName('autosave' + Int2Fix(I, 2), 'bas'));
    AttachFile(SaveName('autosave' + Int2Fix(I, 2), 'sav'));
  end;

  fLog.AppendLog('Crash report created');
end;
{$ENDIF}


//Occasional replay inconsistencies are a known bug, we don't need reports of it
procedure TKMGame.ReplayInconsistancy;
var PreviousState: TGameState;
begin
  PreviousState := GameState;
  SetGameState(gsPaused); //Stop game from executing while the user views the message
  fLog.AppendLog('Replay failed a consistency check at tick '+IntToStr(fGameTickCount));
  if MessageDlg(fTextLibrary[TX_REPLAY_FAILED], mtWarning, [mbYes, mbNo], 0) <> mrYes then
    Stop(gr_Error, '')
  else
    SetGameState(PreviousState);
end;


//Put the game on Hold for Victory screen
procedure TKMGame.GameHold(DoHold: Boolean; Msg: TGameResultMsg);
begin
  DoGameHold := false;
  fGamePlayInterface.ReleaseDirectionSelector; //In case of victory/defeat while moving troops
  fResource.Cursors.Cursor := kmc_Default;
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
  if aPlayerIndex = MyPlayer.PlayerIndex then
    fSoundLib.Play(sfxn_Victory, 1.0, true); //Fade music

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
    fNetworking.PostLocalMessage(Format(fTextLibrary[TX_MULTIPLAYER_PLAYER_DEFEATED],
                                        [fPlayers[aPlayerIndex].PlayerName]));
    if aPlayerIndex = MyPlayer.PlayerIndex then
    begin
      PlayOnState := gr_Defeat;
      fGamePlayInterface.ShowMPPlayMore(gr_Defeat);
    end;
  end
  else
    RequestGameHold(gr_Defeat);
end;


//Display the overlay "Waiting for players"
//todo: Move to fNetworking and query GIP from there
procedure TKMGame.GameWaitingForNetwork(aWaiting: Boolean);
var WaitingPlayers: TStringList;
begin
  fWaitingForNetwork := aWaiting;

  WaitingPlayers := TStringList.Create;
  case fNetworking.NetGameState of
    lgs_Game, lgs_Reconnecting:
        //GIP is waiting for next tick
        TGameInputProcess_Multi(fGameInputProcess).GetWaitingPlayers(fGameTickCount+1, WaitingPlayers);
    lgs_Loading:
        //We are waiting during inital loading
        fNetworking.NetPlayers.GetNotReadyToPlayPlayers(WaitingPlayers);
    else
        Assert(false, 'GameWaitingForNetwork from wrong state '+GetEnumName(TypeInfo(TNetGameState), Integer(fNetworking.NetGameState)));
  end;

  fGamePlayInterface.ShowNetworkLag(aWaiting, WaitingPlayers, fNetworking.IsHost);
  WaitingPlayers.Free;
end;


//todo: Move to fNetworking and query GIP from there
procedure TKMGame.GameDropWaitingPlayers;
var WaitingPlayers: TStringList;
begin
  WaitingPlayers := TStringList.Create;
  case fNetworking.NetGameState of
    lgs_Game,lgs_Reconnecting:
        TGameInputProcess_Multi(fGameInputProcess).GetWaitingPlayers(fGameTickCount+1, WaitingPlayers); //GIP is waiting for next tick
    lgs_Loading:
        fNetworking.NetPlayers.GetNotReadyToPlayPlayers(WaitingPlayers); //We are waiting during inital loading
    else
        Assert(False); //Should not be waiting for players from any other GameState
  end;
  fNetworking.DropWaitingPlayers(WaitingPlayers);
  WaitingPlayers.Free;
end;


//Called by fNetworking to access MissionTime/GameName
procedure TKMGame.SendMPGameInfo(Sender: TObject);
begin
  fNetworking.SendMPGameInfo(GetMissionTime, GameName);
end;


procedure TKMGame.Stop(Msg: TGameResultMsg; TextMsg: string='');
begin
  fIsExiting := True;
  try
    if fMultiplayerMode and not fReplayMode then
    begin
      if fNetworking.Connected then fNetworking.AnnounceDisconnect;
      fNetworking.Disconnect;
    end;

    SetGameState(gsNoGame);

    //Take results from MyPlayer before data is flushed
    if Msg in [gr_Win, gr_Defeat, gr_Cancel, gr_ReplayEnd] then
      if fMultiplayerMode then
        fMainMenuInterface.ResultsMP_Fill
      else
        fMainMenuInterface.Results_Fill;

    if (fGameInputProcess <> nil) and (fGameInputProcess.ReplayState = gipRecording) then
      fGameInputProcess.SaveToFile(SaveName('basesave', 'rpl', fMultiplayerMode));

    FreeThenNil(fEventsManager);
    FreeThenNil(fGameInputProcess);
    FreeThenNil(fPlayers);
    FreeThenNil(fProjectiles);
    FreeThenNil(fRenderPool);
    FreeThenNil(fPathfinding);
    FreeThenNil(fTerrain);

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
      gr_Cancel,
      gr_ReplayEnd: if fMultiplayerMode then
                      fMainMenuInterface.ShowScreen(msResultsMP, '', Msg)
                    else
                      fMainMenuInterface.ShowScreen(msResults, '', Msg);
      gr_Error:     fMainMenuInterface.ShowScreen(msError, TextMsg);
      gr_Disconnect:fMainMenuInterface.ShowScreen(msError, TextMsg);
      gr_Silent:    ;//Used when loading new savegame from gameplay UI
      gr_MapEdEnd:  fMainMenuInterface.ShowScreen(msMain);
    end;
  finally
    fIsExiting := false;
  end;
end;


{Mission name is absolute}
procedure TKMGame.StartMapEditor(const aFileName: string; aMultiplayer:boolean; aSizeX:integer=64; aSizeY:integer=64);
var
  i: Integer;
  LoadError: string;
  fMissionParser: TMissionParserStandard;
begin
  if not FileExists(aFileName) and (aSizeX*aSizeY=0) then exit; //Erroneous call

  Stop(gr_Silent); //Stop MapEd as we are loading from existing MapEd session

  fLog.AppendLog('Starting Map Editor');

  SetKaMSeed(4); //Every time MapEd will be the same as previous. Good for debug.
  SetGameSpeed(1); //In case it was set in last run mission
  fMultiplayerMode := False;
  fReplayMode := False;

  //Load the resources if necessary
  fMainMenuInterface.ShowScreen(msLoading, '');
  GameLoadingStep(fTextLibrary[TX_MENU_LOADING_DEFINITIONS]);
  fResource.OnLoadingText := GameLoadingStep;
  fResource.LoadGameResources(fGameSettings.AlphaShadows);

  GameLoadingStep(fTextLibrary[TX_MENU_LOADING_INITIALIZING]);

  fViewport := TViewport.Create(fScreenX, fScreenY);

  //Here comes terrain/mission init
  fTerrain := TTerrain.Create;
  fRenderPool := TRenderPool.Create(fRender);

  //Set the state to gsEditor early, so the MissionParser knows we must not flatten house areas, give units random condition, etc.
  fMapEditorInterface := TKMapEdInterface.Create(fScreenX, fScreenY);
  SetGameState(gsEditor);

  if aFileName <> '' then
  try //Catch exceptions
    fLog.AppendLog('Loading DAT file for editor: '+aFileName);
    fMissionParser := TMissionParserStandard.Create(mpm_Editor, False);
    if not fMissionParser.LoadMission(aFileName) then
      Raise Exception.Create(fMissionParser.ErrorMessage);
    MyPlayer := fPlayers.Player[0];
    fPlayers.AddPlayers(MAX_PLAYERS - fPlayers.Count); //Activate all players
    FreeAndNil(fMissionParser);
    fGameName := TruncateExt(ExtractFileName(aFileName));
  except
    on E : Exception do
    begin
      //Trap the exception and show it to the user in nicer form.
      //Note: While debugging, Delphi will still stop execution for the exception,
      //unless Tools > Debugger > Exception > "Stop on Delphi Exceptions" is unchecked.
      //But to normal player the dialog won't show.
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], [aFileName])+'||'+E.ClassName+': '+E.Message;
      fMainMenuInterface.ShowScreen(msError, LoadError);
      fLog.AppendLog('DAT Load Exception: '+LoadError);
      Exit;
    end;
  end
  else
  begin
    fTerrain.MakeNewMap(aSizeX, aSizeY, True);
    fPlayers := TKMPlayersCollection.Create;
    fPlayers.AddPlayers(MAX_PLAYERS); //Create MAX players
    MyPlayer := fPlayers.Player[0];
    MyPlayer.PlayerType := pt_Human; //Make Player1 human by default
    fGameName := fTextLibrary[TX_MAP_ED_NEW_MISSION];
  end;

  fViewport.ResizeMap(fTerrain.MapX, fTerrain.MapY);
  fViewport.ResetZoom;

  fMapEditorInterface.Player_UpdateColors;
  fMapEditorInterface.UpdateMapSize(fTerrain.MapX, fTerrain.MapY);
  fMapEditorInterface.UpdateMapName(fGameName);
  if FileExists(aFileName) then fMapEditorInterface.SetLoadMode(aMultiplayer);
  fPlayers.AfterMissionInit(false);

  for i:=0 to fPlayers.Count-1 do //Reveal all players since we'll swap between them in MapEd
    fPlayers[i].FogOfWar.RevealEverything;

  fLog.AppendLog('Gameplay initialized', True);

  fGameTickCount := 0; //Restart counter
end;


procedure TKMGame.AutoSave;
var i: integer;
begin
  Save('autosave'); //Save to temp file

  //Delete last autosave and shift remaining by 1 position back
  DeleteFile(SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'sav', fMultiplayerMode));
  DeleteFile(SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'rpl', fMultiplayerMode));
  DeleteFile(SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'bas', fMultiplayerMode));
  for i:=AUTOSAVE_COUNT downto 2 do //03 to 01
  begin
    RenameFile(SaveName('autosave'+int2fix(i-1,2), 'sav', fMultiplayerMode), SaveName('autosave'+int2fix(i,2), 'sav', fMultiplayerMode));
    RenameFile(SaveName('autosave'+int2fix(i-1,2), 'rpl', fMultiplayerMode), SaveName('autosave'+int2fix(i,2), 'rpl', fMultiplayerMode));
    RenameFile(SaveName('autosave'+int2fix(i-1,2), 'bas', fMultiplayerMode), SaveName('autosave'+int2fix(i,2), 'bas', fMultiplayerMode));
  end;

  //Rename temp to be first in list
  RenameFile(SaveName('autosave', 'sav', fMultiplayerMode), SaveName('autosave01', 'sav', fMultiplayerMode));
  RenameFile(SaveName('autosave', 'rpl', fMultiplayerMode), SaveName('autosave01', 'rpl', fMultiplayerMode));
  RenameFile(SaveName('autosave', 'bas', fMultiplayerMode), SaveName('autosave01', 'bas', fMultiplayerMode));
end;


procedure TKMGame.BaseSave;
begin
  Save('basesave'); //Temp file

  //In Linux CopyFile does not overwrite
  if FileExists(SaveName('basesave', 'bas', fMultiplayerMode)) then
    DeleteFile(SaveName('basesave','bas', fMultiplayerMode));
  CopyFile(PChar(SaveName('basesave','sav', fMultiplayerMode)), PChar(SaveName('basesave','bas', fMultiplayerMode)), False);
end;


procedure TKMGame.SaveMapEditor(const aMissionName:string; aMultiplayer:boolean);
var
  i: integer;
  fMissionParser: TMissionParserStandard;
begin
  if aMissionName = '' then exit;

  //Prepare and save
  fPlayers.RemoveEmptyPlayers;
  ForceDirectories(ExeDir + 'Maps\' + aMissionName);
  fTerrain.SaveToFile(MapNameToPath(aMissionName, 'map', aMultiplayer));
  fMissionParser := TMissionParserStandard.Create(mpm_Editor, false);
  fMissionParser.SaveDATFile(MapNameToPath(aMissionName, 'dat', aMultiplayer));
  FreeAndNil(fMissionParser);

  fGameName := aMissionName;
  fPlayers.AddPlayers(MAX_PLAYERS - fPlayers.Count); // Activate all players

  //Reveal all players since we'll swap between them in MapEd
  for i := 0 to fPlayers.Count - 1 do
    fPlayers[i].FogOfWar.RevealEverything;

  if MyPlayer = nil then
    MyPlayer := fPlayers[0];
end;


procedure TKMGame.Render;
begin
  fRenderPool.Render;
end;


function TKMGame.RenderVersion: string;
begin
  Result := 'OpenGL '+ fRender.RendererVersion;
end;


//Check if replay files exist at location
function TKMGame.ReplayExists(const aSaveName: string; aMultiplayer:boolean):boolean;
begin
  Result := FileExists(SaveName(aSaveName, 'bas', aMultiplayer)) and
            FileExists(SaveName(aSaveName, 'rpl', aMultiplayer));
end;


//Restart the replay but do not change the viewport position/zoom
procedure TKMGame.RestartReplay;
var OldCenter: TKMPointF; OldZoom: single;
begin
  OldCenter := fViewport.Position;
  OldZoom := fViewport.Zoom;

  StartReplay(fReplayFile,fMultiplayerMode);

  fViewport.Position := OldCenter;
  fViewport.Zoom := OldZoom;
end;


procedure TKMGame.StartReplay(const aSaveName: string; aMultiplayer: boolean);
begin
  Stop(gr_Silent);

  GameInit(aMultiplayer);

  Load(aSaveName, True); //We load what was saved right before starting Recording

  FreeAndNil(fGameInputProcess); //Override GIP from savegame
  fGameInputProcess := TGameInputProcess_Single.Create(gipReplaying);
  fGameInputProcess.LoadFromFile(SaveName(aSaveName, 'rpl', fMultiplayerMode));

  SetKaMSeed(4); //Random after StartGame and ViewReplay should match
  SetGameState(gsReplay);
  fReplayMode := True;
  fGamePlayInterface.UpdateMenuState(fMissionMode = mm_Tactic, fReplayMode);
  fReplayFile := aSaveName;
end;


procedure TKMGame.NetworkInit;
begin
  if fNetworking = nil then
    fNetworking := TKMNetworking.Create(fGameSettings.MasterServerAddress,
                                        fGameSettings.AutoKickTimeout,
                                        fGameSettings.PingInterval,
                                        fGameSettings.MasterAnnounceInterval,
                                        fGameSettings.Locale);
  fNetworking.OnMPGameInfoChanged := SendMPGameInfo;
end;


//TDateTime stores days/months/years as 1 and hours/minutes/seconds as fractions of a 1
//Treat 10 ticks as 1 sec irregardless of user-set pace
function TKMGame.GetMissionTime: TDateTime;
begin
  //Convert cardinal into TDateTime, where 1hour = 1/24 and so on..
  Result := fGameTickCount/24/60/60/10;
end;


function TKMGame.GetPeacetimeRemaining: TDateTime;
begin
  Result := Max(0, Int64(fGame.GameOptions.Peacetime*600)-fGame.GameTickCount)/24/60/60/10;
end;


//Tests whether time has past
function TKMGame.CheckTime(aTimeTicks: Cardinal): Boolean;
begin
  Result := (fGameTickCount >= aTimeTicks);
end;


function TKMGame.IsPeaceTime:boolean;
begin
  Result := not CheckTime(fGameOptions.Peacetime * 600);
end;


//Compute cursor position and store it in global variables
procedure TKMGame.UpdateGameCursor(X, Y: Integer; Shift: TShiftState);
begin
  with GameCursor do
  begin
    Float.X := fViewport.Position.X + (X-fViewport.ViewRect.Right/2-TOOLBAR_WIDTH/2)/CELL_SIZE_PX/fViewport.Zoom;
    Float.Y := fViewport.Position.Y + (Y-fViewport.ViewRect.Bottom/2)/CELL_SIZE_PX/fViewport.Zoom;
    Float.Y := fTerrain.ConvertCursorToMapCoord(Float.X,Float.Y);

    //Cursor cannot reach row MapY or column MapX, they're not part of the map (only used for vertex height)
    Cell.X := EnsureRange(round(Float.X+0.5), 1, fTerrain.MapX-1); //Cell below cursor in map bounds
    Cell.Y := EnsureRange(round(Float.Y+0.5), 1, fTerrain.MapY-1);

    SState := Shift;
  end;
end;


procedure TKMGame.UpdatePeacetime;
var PeaceTicksRemaining:cardinal;
begin
  PeaceTicksRemaining := Max(0,Int64((fGame.GameOptions.Peacetime*600))-fGame.GameTickCount);
  if (PeaceTicksRemaining = 1) and MultiplayerMode then
  begin
    Networking.PostLocalMessage(fTextLibrary[TX_MP_PEACETIME_OVER],false);
    fSoundLib.Play(sfxn_Peacetime,1.0,true); //Fades music
  end;
end;


function TKMGame.AllowDebugRendering:boolean;
begin
  Result := not (fGameState in [gsRunning,gsPaused,gsOnHold]) or
            MULTIPLAYER_CHEATS or not MultiplayerMode;
end;


function TKMGame.GetNewID:cardinal;
begin
  Inc(ID_Tracker);
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


  if fGameSpeed > 5 then
  begin
    fGameSpeedMultiplier := Round(fGameSpeed / 4);
    fTimerGame.Interval := Round(fGameSettings.SpeedPace / fGameSpeed * fGameSpeedMultiplier);
  end
  else
  begin
    fGameSpeedMultiplier := 1;
    fTimerGame.Interval := Round(fGameSettings.SpeedPace / fGameSpeed);
  end;

  if fGamePlayInterface <> nil then
    fGamePlayInterface.ShowClock(fGameSpeed);
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
procedure TKMGame.Save(const aFileName: string);
var
  SaveStream: TKMemoryStream;
  fGameInfo: TKMGameInfo;
  i, NetIndex: integer;
  s: string;
begin
  fLog.AppendLog('Saving game');
  if not (fGameState in [gsPaused, gsRunning]) then begin
    Assert(false, 'Saving from wrong state?');
    Exit;
  end;

  //Makes the folders incase they were deleted
  CreateDir(ExeDir + 'Saves\');
  CreateDir(ExeDir + 'SavesMP\');

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
      fGameInfo.PlayerTypes[i] := fNetworking.NetPlayers[NetIndex].GetPlayerType;
      fGameInfo.ColorID[i] := fNetworking.NetPlayers[NetIndex].FlagColorID;
      fGameInfo.Team[i] := fNetworking.NetPlayers[NetIndex].Team;
    end
  end;

  fGameInfo.Save(SaveStream);
  fGameInfo.Free;
  fGameOptions.Save(SaveStream);

  //Because some stuff is only saved in singleplayer we need to know whether it is included in this save,
  //so we can load multiplayer saves in single player and vice versa.
  SaveStream.Write(fMultiplayerMode);

  if not fMultiplayerMode then
    fGamePlayInterface.SaveMapview(SaveStream); //Minimap is near the start so it can be accessed quickly

  fCampaigns.Save(SaveStream);
  SaveStream.Write(ID_Tracker); //Units-Houses ID tracker
  SaveStream.Write(GetKaMSeed); //Include the random seed in the save file to ensure consistency in replays

  if not fMultiplayerMode then
    SaveStream.Write(PlayOnState, SizeOf(PlayOnState));

  fTerrain.Save(SaveStream); //Saves the map
  fPlayers.Save(SaveStream, fMultiplayerMode); //Saves all players properties individually
  fProjectiles.Save(SaveStream);
  fEventsManager.Save(SaveStream);

  //Relative path to strings will be the same for all MP players
  s := ExtractRelativePath(ExeDir, ChangeFileExt(fMissionFile, '.%s.libx'));
  SaveStream.Write(s);

  //Parameters that are not identical for all players should not be saved as we need saves to be
  //created identically on all player's computers. Eventually these things can go through the GIP

  //For multiplayer consistency we compare all saves CRCs, they should be created identical on all player's computers.
  if not fMultiplayerMode then
  begin
    //Viewport settings are unique for each player
    fViewport.Save(SaveStream);
    fGamePlayInterface.Save(SaveStream); //Saves message queue and school/barracks selected units
    //Don't include fGameSettings.Save it's not required for settings are Game-global, not mission
  end;

  //If we want stuff like the MessageStack and screen center to be stored in multiplayer saves,
  //we must send those "commands" through the GIP so all players know about them and they're in sync.
  //There is a comment in fGame.Load about MessageList on this topic.

  SaveStream.SaveToFile(SaveName(aFileName,'sav', fMultiplayerMode)); //Some 70ms for TPR7 map
  SaveStream.Free;

  fLog.AppendLog('Save done');

  CopyFile(PChar(SaveName('basesave','bas', fMultiplayerMode)), PChar(SaveName(aFileName,'bas', fMultiplayerMode)), false); //replace Replay base savegame
  fGameInputProcess.SaveToFile(SaveName(aFileName,'rpl', fMultiplayerMode)); //Adds command queue to savegame

  fLog.AppendLog('Saving game', true);
end;


procedure TKMGame.Load(const aFileName: string; aReplay: Boolean = False);
var
  LoadStream: TKMemoryStream;
  GameInfo: TKMGameInfo;
  LoadError,LoadFileExt: string;
  LibxPath: AnsiString;
  LoadedSeed: Longint;
  SaveIsMultiplayer: Boolean;
begin
  fLog.AppendLog('Loading game: ' + aFileName);
  if aReplay then
    LoadFileExt := 'bas'
  else
    LoadFileExt := 'sav';

  LoadStream := TKMemoryStream.Create;
  GameInfo := TKMGameInfo.Create;
  try //Catch exceptions
    if not FileExists(SaveName(aFileName, LoadFileExt, fMultiplayerMode)) then
      raise Exception.Create('Savegame could not be found');

    LoadStream.LoadFromFile(SaveName(aFileName, LoadFileExt, fMultiplayerMode));

    //We need only few essential parts from GameInfo, the rest is duplicate from fTerrain and fPlayers

    GameInfo.Load(LoadStream);
    fGameName := GameInfo.Title;
    fGameTickCount := GameInfo.TickCount;
    fMissionMode := GameInfo.MissionMode;
    FreeAndNil(GameInfo);
    fGameOptions.Load(LoadStream);

    //So we can allow loading of multiplayer saves in single player and vice versa we need to know which type THIS save is
    LoadStream.Read(SaveIsMultiplayer);

    if not SaveIsMultiplayer then
      fGamePlayInterface.LoadMapview(LoadStream); //Not used, (only stored for preview) but it's easiest way to skip past it

    fCampaigns.Load(LoadStream);
    LoadStream.Read(ID_Tracker);
    LoadStream.Read(LoadedSeed);

    if not SaveIsMultiplayer then
      LoadStream.Read(PlayOnState, SizeOf(PlayOnState));

    //Load the data into the game
    fTerrain.Load(LoadStream);

    fPlayers := TKMPlayersCollection.Create;
    fPlayers.Load(LoadStream);
    fProjectiles.Load(LoadStream);
    fEventsManager.Load(LoadStream);

    //Load LIBX strings used in a mission by their relative path to ExeDir
    //Relative path should be the same across all MP players,
    //locale info shuold not be a problem as it is represented by %s
    LoadStream.Read(LibxPath);
    fTextLibrary.LoadMissionStrings(ExeDir + LibxPath);

    //Multiplayer saves don't have this piece of information. Its valid only for MyPlayer
    //todo: Send all message commands through GIP
    if not SaveIsMultiplayer then
    begin
      fViewport.Load(LoadStream);
      fGamePlayInterface.Load(LoadStream);
    end;

    FreeAndNil(LoadStream);

    if fMultiplayerMode and not aReplay then
      fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking)
    else
      fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);
    fGameInputProcess.LoadFromFile(SaveName(aFileName, 'rpl', fMultiplayerMode));

    if not aReplay then
      CopyFile(PChar(SaveName(aFileName,'bas', fMultiplayerMode)), PChar(SaveName('basesave','bas', fMultiplayerMode)), false); //replace Replay base savegame

    fPlayers.SyncLoad; //Should parse all Unit-House ID references and replace them with actual pointers
    fTerrain.SyncLoad; //IsUnit values should be replaced with actual pointers

    fGamePlayInterface.UpdateMapSize(fTerrain.MapX, fTerrain.MapY); //Must be updated after SyncLoad so IsUnit pointers are correct for minimap update
    fViewport.ResizeMap(fTerrain.MapX, fTerrain.MapY);
    fViewport.ResetZoom; //This ensures the viewport is centered on the map (game could have been saved with a different resolution/zoom)
  except
    on E : Exception do
    begin
      if GameInfo <> nil   then GameInfo.Free;
      if LoadStream <> nil then LoadStream.Free;

      //Trap the exception and show the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
      LoadError := Format(fTextLibrary[TX_MENU_PARSE_ERROR], [aFileName])+'||'+E.ClassName+': '+E.Message;
      fMainMenuInterface.ShowScreen(msError, LoadError); //This will show an option to return back to menu
      Exit;
    end;
  end;

  SetKaMSeed(LoadedSeed);

  fLog.AppendLog('Loading game', True);
end;


procedure TKMGame.UpdateGame(Sender: TObject);
var I: Integer; T: Cardinal;
begin
  case fGameState of
    gsPaused:   ; //Don't exit here as there is code afterwards to execute (e.g. play next music track)
    gsOnHold:   ; //Don't exit here as there is code afterwards to execute (e.g. play next music track)
    gsNoGame:   ;
    gsRunning:  if not fMultiplayerMode or (fNetworking.NetGameState <> lgs_Loading) then
                  for I := 1 to fGameSpeedMultiplier do
                  begin
                    if fGameInputProcess.CommandsConfirmed(fGameTickCount+1) then
                    begin
                      T := TimeGet;

                      if fWaitingForNetwork then GameWaitingForNetwork(false); //No longer waiting for players
                      inc(fGameTickCount); //Thats our tick counter for gameplay events
                      if fMultiplayerMode then fNetworking.LastProcessedTick := fGameTickCount;
                      //Tell the master server about our game on the specific tick (host only)
                      if fMultiplayerMode and fNetworking.IsHost and (
                         ((fMissionMode = mm_Normal) and (fGameTickCount = ANNOUNCE_BUILD_MAP)) or
                         ((fMissionMode = mm_Tactic) and (fGameTickCount = ANNOUNCE_BATTLE_MAP))) then
                        fNetworking.ServerQuery.SendMapInfo(fGameName, fNetworking.NetPlayers.GetConnectedCount);

                      fEventsManager.ProcTime(fGameTickCount);
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
                      if (fGameTickCount mod 600 = 0) and fGameSettings.Autosave and not (GameState = gsOnHold) then
                        AutoSave;

                      fPerfLog.AddTime(TimeGet - T);

                      //Break the for loop (if we are using speed up)
                      if DoGameHold then break;
                    end
                    else
                    begin
                      fGameInputProcess.WaitingForConfirmation(fGameTickCount);
                      if TGameInputProcess_Multi(fGameInputProcess).GetNumberConsecutiveWaits > 5 then
                        GameWaitingForNetwork(true);
                    end;
                    fGameInputProcess.UpdateState(fGameTickCount); //Do maintenance
                  end;
    gsReplay:     for I := 1 to fGameSpeedMultiplier do
                  begin
                    Inc(fGameTickCount); //Thats our tick counter for gameplay events
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

                    //Break the for loop (if we are using speed up)
                    if DoGameHold then break;
                  end;
    gsEditor:   begin
                  fTerrain.IncAnimStep;
                  fPlayers.IncAnimStep;
                end;
  end;

  if DoGameHold then GameHold(true,DoGameHoldState);
end;


//This is our real-time "thread", use it wisely
procedure TKMGame.UpdateStateIdle(aFrameTime: Cardinal);
begin
  case fGameState of
    gsRunning,
    gsReplay:   fViewport.UpdateStateIdle(aFrameTime); //Check to see if we need to scroll
    gsEditor:   begin
                  fViewport.UpdateStateIdle(aFrameTime); //Check to see if we need to scroll
                  fTerrain.UpdateStateIdle;
                end;
  end;
end;


function TKMGame.SaveName(const aName, aExt: string; aMultiPlayer: Boolean): string;
begin
  if aMultiPlayer then
    Result := ExeDir + 'SavesMP\' + aName + '.' + aExt
  else
    Result := ExeDir + 'Saves\' + aName + '.' + aExt;
end;


procedure TKMGame.PrintScreen(aFilename: string = '');
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


end.

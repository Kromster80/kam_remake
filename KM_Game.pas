unit KM_Game;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, FileUtil, {$ENDIF}
  Forms, Controls, Classes, Dialogs, ExtCtrls, SysUtils, KromUtils, Math, TypInfo,
  {$IFDEF USE_MAD_EXCEPT} MadExcept, KM_Exceptions, {$ENDIF}
  KM_CommonTypes, KM_Defaults, KM_Points,
  KM_Alerts, KM_GameInputProcess, KM_GameOptions,
  KM_InterfaceDefaults, KM_InterfaceMapEditor, KM_InterfaceGamePlay,
  KM_MapEditor, KM_Minimap, KM_Networking,
  KM_PathFinding, KM_PathFindingAstarOld, KM_PathFindingAStarNew, KM_PathFindingJPS,
  KM_PerfLog, KM_Projectiles, KM_Render, KM_Viewport;

type
  TGameMode = (
    gmSingle,
    gmMulti,        //Different GIP, networking,
    gmMapEd,        //Army handling, lite updates,
    gmReplaySingle, //No input, different results screen to gmReplayMulti
    gmReplayMulti   //No input, different results screen to gmReplaySingle
    );

  ///	<summary>
  ///	  Class that manages single game session
  ///	</summary>
  TKMGame = class
  private //Irrelevant to savegame
    fTimerGame: TTimer;
    fAlerts: TKMAlerts;
    fGameOptions: TKMGameOptions;
    fNetworking: TKMNetworking;
    fGameInputProcess: TGameInputProcess;
    fMinimap: TKMMinimap;
    fPathfinding: TPathFinding;
    fViewport: TViewport;
    fPerfLog: TKMPerfLog;
    fActiveInterface: TKMUserInterface; //Shortcut for both of UI
    fGamePlayInterface: TKMGamePlayInterface;
    fMapEditorInterface: TKMapEdInterface;
    fMapEditor: TKMMapEditor;

    fIsExiting: Boolean; //Set this to true on Exit and unit/house pointers will be released without cross-checking
    fIsPaused: Boolean;
    fGameSpeed: Word; //Actual speedup value
    fGameSpeedMultiplier: Word; //How many ticks are compressed into one
    fGameMode: TGameMode;
    fWaitingForNetwork: Boolean;
    fAdvanceFrame: Boolean; //Replay variable to advance 1 frame, afterwards set to false
    fSaveFile: AnsiString;  //Relative pathname to savegame we are playing, so it gets saved to crashreport
    fShowTeamNames: Boolean;
    fGameLockedMutex: Boolean;

  //Should be saved
    fCampaignMap: Byte;         //Which campaign map it is, so we can unlock next one on victory
    fCampaignName: AnsiString;  //Is this a game part of some campaign
    fGameName: string;
    fGameTickCount: Cardinal;
    fIDTracker: Cardinal;       //Units-Houses tracker, to issue unique IDs
    fMissionFile: AnsiString;   //Relative pathname to mission we are playing, so it gets saved to crashreport
    fMissionMode: TKMissionMode;

    procedure GameMPDisconnect(const aData:string);
    procedure MultiplayerRig;
    procedure SaveGame(const aPathName: string);
    procedure UpdatePeaceTime;
    procedure UpdateUI;
  public
    PlayOnState: TGameResultMsg;
    DoGameHold: Boolean; //Request to run GameHold after UpdateState has finished
    DoGameHoldState: TGameResultMsg; //The type of GameHold we want to occur due to DoGameHold
    SkipReplayEndCheck: Boolean;

    ///	<param name="aRender">
    ///	  Pointer to Render class, that will execute our rendering requests
    ///	  performed via RenderPool we create.
    ///	</param>
    ///	<param name="aNetworking">
    ///	  Pointer to networking class, required if this is a multiplayer game.
    ///	</param>
    constructor Create(aGameMode: TGameMode; aRender: TRender; aNetworking: TKMNetworking);
    destructor Destroy; override;

    procedure KeyDown(Key: Word; Shift: TShiftState);
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure GameStart(aMissionFile, aGameName, aCampName: string; aCampMap: Byte; aLocation: Byte; aColor: Cardinal); overload;
    procedure GameStart(aSizeX, aSizeY: Integer); overload;
    procedure Load(const aPathName: string);

    function MapX: Word;
    function MapY: Word;

    procedure Resize(X,Y: Integer);

    procedure GameMPPlay(Sender:TObject);
    procedure GameMPReadyToPlay(Sender:TObject);
    procedure GameHold(DoHold:boolean; Msg:TGameResultMsg); //Hold the game to ask if player wants to play after Victory/Defeat/ReplayEnd
    procedure RequestGameHold(Msg:TGameResultMsg);
    procedure PlayerVictory(aPlayerIndex:TPlayerIndex);
    procedure PlayerDefeat(aPlayerIndex:TPlayerIndex);
    procedure GameWaitingForNetwork(aWaiting:boolean);
    procedure GameDropWaitingPlayers;

    procedure AutoSave;
    procedure SaveMapEditor(const aMissionName:string; aMultiplayer:boolean);
    procedure RestartReplay; //Restart the replay but keep current viewport position/zoom

    function MissionTime: TDateTime;
    function GetPeacetimeRemaining: TDateTime;
    function CheckTime(aTimeTicks: Cardinal): Boolean;
    function IsPeaceTime: Boolean;
    function IsMapEditor: Boolean;
    function IsMultiplayer: Boolean;
    function IsReplay: Boolean;
    procedure ShowMessage(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
    property GameTickCount:cardinal read fGameTickCount;
    property GameName: string read fGameName;
    property CampaignName: AnsiString read fCampaignName;
    property CampaignMap: Byte read fCampaignMap;
    function PlayerLoc: Byte;
    function PlayerColor: Cardinal;

    property GameMode: TGameMode read fGameMode;
    property MissionFile: AnsiString read fMissionFile;
    property SaveFile: AnsiString read fSaveFile;
    property ShowTeamNames: Boolean read fShowTeamNames write fShowTeamNames;

    property IsExiting: Boolean read fIsExiting;
    property IsPaused: Boolean read fIsPaused write fIsPaused;
    property MissionMode: TKMissionMode read fMissionMode write fMissionMode;
    function GetNewID: Cardinal;
    procedure SetGameSpeed(aSpeed: Word);
    procedure StepOneFrame;
    function SaveName(const aName, aExt: string; aMultiPlayer: Boolean): string;
    procedure UpdateMultiplayerTeams;

    property PerfLog: TKMPerfLog read fPerfLog;
    procedure UpdateGameCursor(X,Y: Integer; Shift: TShiftState);

    property Alerts: TKMAlerts read fAlerts;
    property Minimap: TKMMinimap read fMinimap;
    property Networking: TKMNetworking read fNetworking;
    property Pathfinding: TPathFinding read fPathfinding;
    property GameInputProcess: TGameInputProcess read fGameInputProcess;
    property GameOptions: TKMGameOptions read fGameOptions;
    property GamePlayInterface: TKMGamePlayInterface read fGamePlayInterface;
    property MapEditorInterface: TKMapEdInterface read fMapEditorInterface;
    property MapEditor: TKMMapEditor read fMapEditor;
    property Viewport: TViewport read fViewport;

    procedure Save(const aName: string);
    {$IFDEF USE_MAD_EXCEPT}
    procedure AttachCrashReport(const ExceptIntf: IMEException; aZipFile: string);
    {$ENDIF}
    procedure ReplayInconsistancy;

    procedure Render(aRender: TRender);
    procedure UpdateGame(Sender: TObject);
    procedure UpdateState(aGlobalTickCount: Cardinal);
    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


var
  fGame: TKMGame;


implementation
uses
  KM_CommonClasses, KM_Log, KM_Utils,
  KM_ArmyEvaluation, KM_GameApp, KM_GameInfo, KM_MissionScript, KM_MissionScript_Standard,
  KM_Player, KM_PlayersCollection, KM_RenderPool, KM_Resource, KM_ResourceCursors,
  KM_Sound, KM_Terrain, KM_TerrainPainter, KM_TextLibrary, KM_AIFields, KM_Maps,
  KM_Scripting, KM_GameInputProcess_Single, KM_GameInputProcess_Multi, KM_Main;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
//aMultiplayer - is this a multiplayer game
//aRender - who will be rendering the Game session
constructor TKMGame.Create(aGameMode: TGameMode; aRender: TRender; aNetworking: TKMNetworking);
begin
  inherited Create;

  fGameMode := aGameMode;
  fNetworking := aNetworking;

  fAdvanceFrame := False;
  fIDTracker    := 0;
  PlayOnState   := gr_Cancel;
  DoGameHold    := False;
  SkipReplayEndCheck := False;
  fWaitingForNetwork := False;
  fGameOptions  := TKMGameOptions.Create;

  //Create required UI (gameplay or MapEd)
  if fGameMode = gmMapEd then
  begin
    fMinimap := TKMMinimap.Create(False, True, False);
    fMapEditorInterface := TKMapEdInterface.Create(aRender.ScreenX, aRender.ScreenY);
    fActiveInterface := fMapEditorInterface;
  end
  else
  begin
    fMinimap := TKMMinimap.Create(False, False, False);
    fGamePlayInterface := TKMGamePlayInterface.Create(aRender.ScreenX, aRender.ScreenY, IsMultiplayer, IsReplay);
    fActiveInterface := fGamePlayInterface;
  end;

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
  fTerrain := TKMTerrain.Create;
  fPlayers := TKMPlayersCollection.Create;
  fAIFields := TKMAIFields.Create;

  InitUnitStatEvals; //Army

  if DO_PERF_LOGGING then fPerfLog := TKMPerfLog.Create;
  fLog.AddTime('<== Game creation is done ==>');
  fAlerts := TKMAlerts.Create(@fGameTickCount, fViewport);
  fScripting := TKMScripting.Create;

  case PathFinderToUse of
    0:  fPathfinding := TPathfindingAStarOld.Create;
    1:  fPathfinding := TPathfindingAStarNew.Create;
    2:  fPathfinding := TPathfindingJPS.Create;
  else  fPathfinding := TPathfindingAStarOld.Create;

  end;
  fProjectiles := TKMProjectiles.Create;

  fRenderPool := TRenderPool.Create(aRender);

  fGameTickCount := 0; //Restart counter
end;


{ Destroy what was created }
destructor TKMGame.Destroy;
begin
  //We might have crashed part way through .Create, so we can't assume ANYTHING exists here.
  //Doing so causes a 2nd exception which overrides 1st. Hence check <> nil on everything except Frees, TObject.Free does that already.

  if fGameLockedMutex then fMain.UnlockMutex;
  if fTimerGame <> nil then fTimerGame.Enabled := False;
  fIsExiting := True;

  //if (fGameInputProcess <> nil) and (fGameInputProcess.ReplayState = gipRecording) then
  //  fGameInputProcess.SaveToFile(SaveName('basesave', 'rpl', fGameMode = gmMulti));

  if DO_PERF_LOGGING and (fPerfLog <> nil) then fPerfLog.SaveToFile(ExeDir + 'Logs\PerfLog.txt');

  FreeAndNil(fTimerGame);

  FreeThenNil(fMapEditor);
  FreeThenNil(fPlayers);
  FreeThenNil(fTerrainPainter);
  FreeThenNil(fTerrain);
  FreeAndNil(fAIFields);
  FreeAndNil(fProjectiles);
  FreeAndNil(fPathfinding);
  FreeAndNil(fScripting);
  FreeAndNil(fAlerts);

  FreeThenNil(fGamePlayInterface);
  FreeThenNil(fMapEditorInterface);
  FreeAndNil(fMinimap);
  FreeAndNil(fViewport);

  FreeAndNil(fGameInputProcess);
  FreeAndNil(fRenderPool);
  FreeAndNil(fGameOptions);
  FreeAndNil(fAlerts);
  if DO_PERF_LOGGING then fPerfLog.Free;

  //When leaving the game we should always reset the cursor in case the user had beacon or linking selected
  fResource.Cursors.Cursor := kmc_Default;

  inherited;
end;


procedure TKMGame.Resize(X,Y: Integer);
begin
  fActiveInterface.Resize(X, Y);

  fViewport.Resize(X, Y);
end;


function TKMGame.MapX: Word;
begin
  Result := fTerrain.MapX;
end;


function TKMGame.MapY: Word;
begin
  Result := fTerrain.MapY;
end;


procedure TKMGame.KeyDown(Key: Word; Shift: TShiftState);
begin
  fActiveInterface.KeyDown(Key, Shift);
end;


procedure TKMGame.KeyPress(Key: Char);
begin
  fActiveInterface.KeyPress(Key);
end;


procedure TKMGame.KeyUp(Key: Word; Shift: TShiftState);
begin
  fActiveInterface.KeyUp(Key, Shift);
end;


procedure TKMGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fActiveInterface.MouseDown(Button,Shift,X,Y);
end;


procedure TKMGame.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  fActiveInterface.MouseMove(Shift, X,Y);end;


procedure TKMGame.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fActiveInterface.MouseUp(Button, Shift, X,Y);
end;


procedure TKMGame.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer);
var PrevCursor: TKMPointF;
begin
  fActiveInterface.MouseWheel(Shift, WheelDelta, X, Y);

  if (X < 0) or (Y < 0) then Exit; //This occours when you use the mouse wheel on the window frame

  //Allow to zoom only when curor is over map. Controls handle zoom on their own
  if MOUSEWHEEL_ZOOM_ENABLE and (fActiveInterface.MyControls.CtrlOver = nil) then
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
end;


procedure TKMGame.GameStart(aMissionFile, aGameName, aCampName: string; aCampMap: Byte; aLocation: Byte; aColor: Cardinal);
var
  I: Integer;
  ParseMode: TMissionParsingMode;
  PlayerEnabled: TPlayerEnabledArray;
  Parser: TMissionParserStandard;
begin
  fLog.AddTime('GameStart');
  Assert(fGameMode in [gmMulti, gmMapEd, gmSingle]);

  fGameName := aGameName;
  fCampaignName := aCampName;
  fCampaignMap := aCampMap;
  fMissionFile := ExtractRelativePath(ExeDir, aMissionFile);
  fSaveFile := '';

  fLog.AddTime('Loading DAT file: ' + aMissionFile);

  //Disable players in MP to skip their assets from loading by MissionParser
  //In SP all players are enabled by default
  case fGameMode of
    gmMulti:  begin
                FillChar(PlayerEnabled, SizeOf(PlayerEnabled), #0);
                for I := 1 to fNetworking.NetPlayers.Count do
                  //PlayerID is 0 based
                  PlayerEnabled[fNetworking.NetPlayers[I].StartLocation - 1] := True;
              end;
    gmSingle: //Setup should tell us which player is AI and which not
              for I := 0 to MAX_PLAYERS - 1 do
                PlayerEnabled[I] := True;
    else      FillChar(PlayerEnabled, SizeOf(PlayerEnabled), #255);
  end;


  //Choose how we will parse the script
  case fGameMode of
    gmMulti:  ParseMode := mpm_Multi;
    gmMapEd:  ParseMode := mpm_Editor;
    gmSingle: ParseMode := mpm_Single;
    else      ParseMode := mpm_Single; //To make compiler happy
  end;

  if fGameMode = gmMapEd then
  begin
    //Mission loader needs to read the data into MapEd (e.g. FOW revealers)
    fMapEditor := TKMMapEditor.Create;
    fTerrainPainter := TKMTerrainPainter.Create;
  end;

  Parser := TMissionParserStandard.Create(ParseMode, PlayerEnabled, False);
  try
    if not Parser.LoadMission(aMissionFile) then
      raise Exception.Create(Parser.FatalErrors);

    MyPlayer := nil;

    if fGameMode = gmMapEd then
    begin
      fPlayers.AddPlayers(MAX_PLAYERS - fPlayers.Count); //Activate all players
      MyPlayer := fPlayers[0];
      for I := 0 to fPlayers.Count - 1 do
        fPlayers[I].FogOfWar.RevealEverything;
    end
    else
    if fGameMode = gmSingle then
    begin
      for I := 0 to fPlayers.Count - 1 do
        fPlayers[I].PlayerType := pt_Computer;

      Assert(InRange(aLocation, 0, fPlayers.Count - 1), 'No human player detected');
      fPlayers[aLocation].PlayerType := pt_Human;
      MyPlayer := fPlayers[aLocation];
      MyPlayer.FlagColor := aColor;
    end;

    if (Parser.MinorErrors <> '') and (fGameMode <> gmMapEd) then
      fGamePlayInterface.MessageIssue(mkQuill, 'Warnings in mission script:|' + Parser.MinorErrors);
  finally
    Parser.Free;
  end;

  if fGameMode <> gmMapEd then
  begin
    fScripting.LoadFromFile(ChangeFileExt(aMissionFile, '.script'));
    if (fScripting.ErrorString <> '') then
      fGamePlayInterface.MessageIssue(mkQuill, 'Warnings in script:|' + fScripting.ErrorString);
  end;

  fTextLibrary.LoadMissionStrings(ChangeFileExt(aMissionFile, '.%s.libx'));

  if fGameMode = gmMulti then
    fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking)
  else
    fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);
  fLog.AddTime('Gameplay recording initialized', True);

  if fGameMode = gmMulti then
    MultiplayerRig;

  fPlayers.AfterMissionInit(True);

  SetKaMSeed(4); //Random after StartGame and ViewReplay should match

  //We need to make basesave.bas since we don't know the savegame name
  //until after user saves it, but we need to attach replay base to it.
  //Basesave is sort of temp we save to HDD instead of keeping in RAM
  if fGameMode in [gmSingle, gmMulti] then
    SaveGame(SaveName('basesave', 'bas', IsMultiplayer));

  //When everything is ready we can update UI
  UpdateUI;
  fViewport.Position := KMPointF(MyPlayer.CenterScreen);

  fLog.AddTime('Gameplay initialized', true);
end;


//All setup data gets taken from fNetworking class
procedure TKMGame.MultiplayerRig;
var
  I: Integer;
  PlayerIndex: TPlayerIndex;
  PlayerEnabled: TPlayerEnabledArray;
begin
  //Copy game options from lobby to this game
  fGameOptions.Peacetime := fNetworking.NetGameOptions.Peacetime;

  FillChar(PlayerEnabled, SizeOf(PlayerEnabled), #0);

  //Assign existing NetPlayers(1..N) to map players(0..N-1)
  for I := 1 to fNetworking.NetPlayers.Count do
  begin
    PlayerIndex := fNetworking.NetPlayers[I].StartLocation - 1; //PlayerID is 0 based
    fPlayers[PlayerIndex].PlayerType := fNetworking.NetPlayers[I].GetPlayerType;
    fPlayers[PlayerIndex].PlayerName := fNetworking.NetPlayers[I].Nikname;
    fPlayers[PlayerIndex].FlagColor := fNetworking.NetPlayers[I].FlagColor;
    PlayerEnabled[PlayerIndex] := True;
  end;

  //Setup alliances
  //We mirror Lobby team setup on to alliances. Savegame has the setup already
  if fNetworking.SelectGameKind = ngk_Map then
    UpdateMultiplayerTeams;

  //MyPlayer is a pointer to TKMPlayer
  MyPlayer := fPlayers[fNetworking.NetPlayers[fNetworking.MyIndex].StartLocation-1];

  //We cannot remove a player from a save (as they might be interacting with other players)
  //so make them inactive (uncontrolled human)
  if fNetworking.SelectGameKind = ngk_Save then
    for I := 0 to fPlayers.Count - 1 do
      if not PlayerEnabled[I] then
        fPlayers[I].PlayerType := pt_Human; //Without controlling player it will be idling

  fPlayers.SyncFogOfWar; //Syncs fog of war revelation between players AFTER alliances
  if fNetworking.SelectGameKind = ngk_Map then
    fPlayers.AddDefaultMPGoals(fMissionMode); //Multiplayer missions don't have goals yet, so add the defaults

  fNetworking.OnPlay           := GameMPPlay;
  fNetworking.OnReadyToPlay    := GameMPReadyToPlay;
  fNetworking.OnCommands       := TGameInputProcess_Multi(fGameInputProcess).RecieveCommands;
  fNetworking.OnTextMessage    := fGamePlayInterface.ChatMessage;
  fNetworking.OnPlayersSetup   := fGamePlayInterface.AlliesOnPlayerSetup;
  fNetworking.OnPingInfo       := fGamePlayInterface.AlliesOnPingInfo;
  fNetworking.OnDisconnect     := GameMPDisconnect; //For auto reconnecting
  fNetworking.OnReassignedHost := nil; //So it is no longer assigned to a lobby event
  fNetworking.GameCreated;

  if fNetworking.Connected and (fNetworking.NetGameState = lgs_Loading) then GameWaitingForNetwork(True); //Waiting for players
end;


procedure TKMGame.UpdateMultiplayerTeams;
var
  I, K: Integer;
  PlayerI: TKMPlayer;
  PlayerK: Integer;
begin
  for I := 1 to fNetworking.NetPlayers.Count do
  begin
    PlayerI := fPlayers[fNetworking.NetPlayers[I].StartLocation - 1]; //PlayerID is 0 based
    for K := 1 to fNetworking.NetPlayers.Count do
    begin
      PlayerK := fNetworking.NetPlayers[K].StartLocation - 1; //PlayerID is 0 based

      //Players are allies if they belong to same team (team 0 means free-for-all)
      if (fNetworking.NetPlayers[I].Team <> 0)
      and (fNetworking.NetPlayers[I].Team = fNetworking.NetPlayers[K].Team) then
        PlayerI.Alliances[PlayerK] := at_Ally
      else
        PlayerI.Alliances[PlayerK] := at_Enemy;
    end;
  end;
end;


//Everyone is ready to start playing
//Issued by fNetworking at the time depending on each Players lag individually
procedure TKMGame.GameMPPlay(Sender:TObject);
begin
  GameWaitingForNetwork(false); //Finished waiting for players
  fNetworking.AnnounceGameInfo(MissionTime, GameName);
  fLog.AddTime('Net game began');
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
    if WRITE_RECONNECT_LOG then fLog.AddTime('GameMPDisconnect: '+aData);
    fNetworking.PostLocalMessage('Connection failed: '+aData,false); //Debugging that should be removed later
    fNetworking.OnJoinFail := GameMPDisconnect; //If the connection fails (e.g. timeout) then try again
    fNetworking.OnJoinAssignedHost := nil;
    fNetworking.OnJoinSucc := nil;
    fNetworking.AttemptReconnection;
  end
  else
  begin
    fNetworking.Disconnect;
    fGameApp.Stop(gr_Disconnect, fTextLibrary[TX_GAME_ERROR_NETWORK]+' '+aData)
  end;
end;

{$IFDEF USE_MAD_EXCEPT}
procedure TKMGame.AttachCrashReport(const ExceptIntf: IMEException; aZipFile:string);

  procedure AttachFile(const aFile: string);
  begin
    if (aFile = '') or not FileExists(aFile) then Exit;
    ExceptIntf.AdditionalAttachments.Add(aFile, '', aZipFile);
  end;

var I: Integer;
begin
  fLog.AppendLog('Creating crash report...');

  //Attempt to save the game, but if the state is too messed up it might fail
  try
    if fGameMode in [gmSingle, gmMulti] then
    begin
      Save('crashreport');
      AttachFile(SaveName('crashreport', 'sav', IsMultiplayer));
      AttachFile(SaveName('crashreport', 'bas', IsMultiplayer));
      AttachFile(SaveName('crashreport', 'rpl', IsMultiplayer));
    end;
  except
    on E : Exception do
      fLog.AppendLog('Exception while trying to save game for crash report: '+E.ClassName+': '+E.Message);
  end;

  AttachFile(ExeDir + fMissionFile);
  AttachFile(ExeDir + ChangeFileExt(fMissionFile, '.map')); //Try to attach the map too if it's named like that

  for I := 1 to AUTOSAVE_COUNT do //All autosaves
  begin
    AttachFile(SaveName('autosave' + Int2Fix(I, 2), 'rpl', IsMultiplayer));
    AttachFile(SaveName('autosave' + Int2Fix(I, 2), 'bas', IsMultiplayer));
    AttachFile(SaveName('autosave' + Int2Fix(I, 2), 'sav', IsMultiplayer));
  end;

  fLog.AppendLog('Crash report created');
end;
{$ENDIF}


//Occasional replay inconsistencies are a known bug, we don't need reports of it
procedure TKMGame.ReplayInconsistancy;
begin
  //Stop game from executing while the user views the message
  fIsPaused := True;
  fLog.AddTime('Replay failed a consistency check at tick '+IntToStr(fGameTickCount));
  if MessageDlg(fTextLibrary[TX_REPLAY_FAILED], mtWarning, [mbYes, mbNo], 0) <> mrYes then
    fGameApp.Stop(gr_Error, '')
  else
    fIsPaused := False;
end;


//Put the game on Hold for Victory screen
procedure TKMGame.GameHold(DoHold: Boolean; Msg: TGameResultMsg);
begin
  DoGameHold := false;
  fGamePlayInterface.ReleaseDirectionSelector; //In case of victory/defeat while moving troops
  fResource.Cursors.Cursor := kmc_Default;
  fViewport.ReleaseScrollKeys;
  PlayOnState := Msg;

  if DoHold then
  begin
    fIsPaused := True;
    fGamePlayInterface.ShowPlayMore(true, Msg);
  end else
    fIsPaused := False;
end;


procedure TKMGame.RequestGameHold(Msg:TGameResultMsg);
begin
  DoGameHold := true;
  DoGameHoldState := Msg;
end;


procedure TKMGame.PlayerVictory(aPlayerIndex: TPlayerIndex);
begin
  if aPlayerIndex = MyPlayer.PlayerIndex then
    fSoundLib.Play(sfxn_Victory, 1.0, true); //Fade music

  if fGameMode = gmMulti then
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


function TKMGame.PlayerColor: Cardinal;
begin
  Result := MyPlayer.FlagColor;
end;


procedure TKMGame.PlayerDefeat(aPlayerIndex:TPlayerIndex);
begin
  if aPlayerIndex = MyPlayer.PlayerIndex then fSoundLib.Play(sfxn_Defeat, 1.0, true); //Fade music
  if fGameMode = gmMulti then
  begin
    fNetworking.PostLocalMessage(Format(fTextLibrary[TX_MULTIPLAYER_PLAYER_DEFEATED],
                                        [fPlayers[aPlayerIndex].PlayerName]));
    if aPlayerIndex = MyPlayer.PlayerIndex then
    begin
      PlayOnState := gr_Defeat;
      fGamePlayInterface.ShowMPPlayMore(gr_Defeat);
      fGamePlayInterface.SetMenuState(fMissionMode = mm_Tactic); //Refresh it so that menu buttons become disabled
      fGamePlayInterface.ClearOpenMenu; //Close e.g. the build menu if it was open
    end;
  end
  else
    RequestGameHold(gr_Defeat);
end;


function TKMGame.PlayerLoc: Byte;
begin
  Result := MyPlayer.PlayerIndex;
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


procedure TKMGame.GameStart(aSizeX, aSizeY: Integer);
var
  I: Integer;
begin
  fGameName := fTextLibrary[TX_MAP_ED_NEW_MISSION];

  fMissionFile := '';
  fSaveFile := '';

  fTerrain.MakeNewMap(aSizeX, aSizeY, True);
  fTerrainPainter := TKMTerrainPainter.Create;

  fMapEditor := TKMMapEditor.Create;
  fPlayers.AddPlayers(MAX_PLAYERS); //Create MAX players
  MyPlayer := fPlayers[0];
  MyPlayer.PlayerType := pt_Human; //Make Player1 human by default

  fPlayers.AfterMissionInit(false);

  if fGameMode = gmSingle then
    fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);

  for I := 0 to fPlayers.Count - 1 do //Reveal all players since we'll swap between them in MapEd
    fPlayers[I].FogOfWar.RevealEverything;

  //When everything is ready we can update UI
  UpdateUI;
  fViewport.Position := KMPointF(aSizeX/2, aSizeY/2);

  fLog.AddTime('Gameplay initialized', True);
end;


procedure TKMGame.AutoSave;
var I: Integer;
begin
  Save('autosave'); //Save to temp file

  //Delete last autosave and shift remaining by 1 position back
  DeleteFile(SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'sav', fGameMode = gmMulti));
  DeleteFile(SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'rpl', fGameMode = gmMulti));
  DeleteFile(SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'bas', fGameMode = gmMulti));
  for i:=AUTOSAVE_COUNT downto 2 do //03 to 01
  begin
    RenameFile(SaveName('autosave'+int2fix(i-1,2), 'sav', fGameMode = gmMulti), SaveName('autosave'+int2fix(i,2), 'sav', fGameMode = gmMulti));
    RenameFile(SaveName('autosave'+int2fix(i-1,2), 'rpl', fGameMode = gmMulti), SaveName('autosave'+int2fix(i,2), 'rpl', fGameMode = gmMulti));
    RenameFile(SaveName('autosave'+int2fix(i-1,2), 'bas', fGameMode = gmMulti), SaveName('autosave'+int2fix(i,2), 'bas', fGameMode = gmMulti));
  end;

  //Rename temp to be first in list
  RenameFile(SaveName('autosave', 'sav', fGameMode = gmMulti), SaveName('autosave01', 'sav', fGameMode = gmMulti));
  RenameFile(SaveName('autosave', 'rpl', fGameMode = gmMulti), SaveName('autosave01', 'rpl', fGameMode = gmMulti));
  RenameFile(SaveName('autosave', 'bas', fGameMode = gmMulti), SaveName('autosave01', 'bas', fGameMode = gmMulti));
end;


procedure TKMGame.SaveMapEditor(const aMissionName: string; aMultiplayer: Boolean);
var
  i: integer;
  fMissionParser: TMissionParserStandard;
  MapName: string;
begin
  if aMissionName = '' then exit;

  //Prepare and save
  fPlayers.RemoveEmptyPlayers;

  MapName := TKMapsCollection.FullPath(aMissionName, '.map', aMultiplayer);
  ForceDirectories(ExtractFilePath(MapName));
  fLog.AddTime('Saving from map editor: '+MapName);

  fTerrain.SaveToFile(MapName);
  fTerrainPainter.SaveToFile(MapName);
  fMissionParser := TMissionParserStandard.Create(mpm_Editor, false);
  fMissionParser.SaveDATFile(ChangeFileExt(MapName, '.dat'));
  FreeAndNil(fMissionParser);

  fGameName := aMissionName;
  fPlayers.AddPlayers(MAX_PLAYERS - fPlayers.Count); // Activate all players

  //Reveal all players since we'll swap between them in MapEd
  for i := 0 to fPlayers.Count - 1 do
    fPlayers[i].FogOfWar.RevealEverything;

  if MyPlayer = nil then
    MyPlayer := fPlayers[0];
end;


procedure TKMGame.Render(aRender: TRender);
begin
  fRenderPool.Render;

  aRender.SetRenderMode(rm2D);
  fActiveInterface.Paint;
end;


//Restart the replay but keep the viewport position/zoom
procedure TKMGame.RestartReplay;
var
  OldCenter: TKMPointF;
  OldZoom: Single;
begin
  OldCenter := fViewport.Position;
  OldZoom := fViewport.Zoom;

  fGameApp.NewReplay(ChangeFileExt(ExeDir + fSaveFile, '.bas'));

  //Self is now destroyed, so we must access the NEW fGame object
  fGame.Viewport.Position := OldCenter;
  fGame.Viewport.Zoom := OldZoom;
end;


//TDateTime stores days/months/years as 1 and hours/minutes/seconds as fractions of a 1
//Treat 10 ticks as 1 sec irregardless of user-set pace
function TKMGame.MissionTime: TDateTime;
begin
  //Convert cardinal into TDateTime, where 1hour = 1/24 and so on..
  Result := fGameTickCount/24/60/60/10;
end;


function TKMGame.GetPeacetimeRemaining: TDateTime;
begin
  Result := Max(0, Int64(fGameOptions.Peacetime * 600) - fGameTickCount) / 24 / 60 / 60 / 10;
end;


//Tests whether time has past
function TKMGame.CheckTime(aTimeTicks: Cardinal): Boolean;
begin
  Result := (fGameTickCount >= aTimeTicks);
end;


function TKMGame.IsMapEditor: Boolean;
begin
  Result := fGameMode = gmMapEd;
end;


//We often need to see if game is MP
function TKMGame.IsMultiplayer: Boolean;
begin
  Result := fGameMode = gmMulti;
end;


function TKMGame.IsReplay: Boolean;
begin
  Result := fGameMode in [gmReplaySingle, gmReplayMulti];
end;


procedure TKMGame.ShowMessage(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
begin
  fGamePlayInterface.MessageIssue(aKind, aText, aLoc);
end;


function TKMGame.IsPeaceTime: Boolean;
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


procedure TKMGame.UpdatePeaceTime;
var
  PeaceTicksRemaining: Cardinal;
begin
  PeaceTicksRemaining := Max(0, Int64((fGameOptions.Peacetime * 600)) - fGameTickCount);
  if (PeaceTicksRemaining = 1) and (fGameMode in [gmMulti,gmReplayMulti]) then
  begin
    fSoundLib.Play(sfxn_Peacetime, 1.0, True); //Fades music
    if fGameMode = gmMulti then
      fNetworking.PostLocalMessage(fTextLibrary[TX_MP_PEACETIME_OVER], false);
  end;
end;


function TKMGame.GetNewID: Cardinal;
begin
  Inc(fIDTracker);
  Result := fIDTracker;
end;


procedure TKMGame.SetGameSpeed(aSpeed: Word);
begin
  Assert(aSpeed > 0);

  //MapEd always runs at x1
  if (IsMapEditor) or (IsMultiplayer and not MULTIPLAYER_SPEEDUP) then
  begin
    fGameSpeed := 1;
    fGameSpeedMultiplier := 1;
    fTimerGame.Interval := Round(fGameApp.GameSettings.SpeedPace / fGameSpeed);
    Exit;
  end;

  //Make the speed toggle between 1 and desired value
  if aSpeed = fGameSpeed then
    fGameSpeed := 1
  else
    fGameSpeed := aSpeed;

  //When speed is above x5 we start to skip rendering frames
  //by doing several updates per timer tick
  if fGameSpeed > 5 then
  begin
    fGameSpeedMultiplier := Round(fGameSpeed / 4);
    fTimerGame.Interval := Round(fGameApp.GameSettings.SpeedPace / fGameSpeed * fGameSpeedMultiplier);
  end
  else
  begin
    fGameSpeedMultiplier := 1;
    fTimerGame.Interval := Round(fGameApp.GameSettings.SpeedPace / fGameSpeed);
  end;

  if fGamePlayInterface <> nil then
    fGamePlayInterface.ShowClock(fGameSpeed);
end;


//In replay mode we can step the game by exactly one frame and then pause again
procedure TKMGame.StepOneFrame;
begin
  Assert(fGameMode in [gmReplaySingle,gmReplayMulti], 'We can work step-by-step only in Replay');
  SetGameSpeed(1); //Make sure we step only one tick. Do not allow multiple updates in UpdateState loop
  fAdvanceFrame := True;
end;


//Saves the game in all its glory
procedure TKMGame.SaveGame(const aPathName: string);
var
  SaveStream: TKMemoryStream;
  fGameInfo: TKMGameInfo;
  i, NetIndex: integer;
  s: string;
begin
  fLog.AddTime('Saving game: ' + aPathName);

  if fGameMode in [gmMapEd, gmReplaySingle, gmReplayMulti] then
  begin
    Assert(false, 'Saving from wrong state');
    Exit;
  end;

  SaveStream := TKMemoryStream.Create;
  try
    fGameInfo := TKMGameInfo.Create;
    fGameInfo.Title := fGameName;
    fGameInfo.TickCount := fGameTickCount;
    fGameInfo.MissionMode := fMissionMode;
    fGameInfo.MapSizeX := fTerrain.MapX;
    fGameInfo.MapSizeY := fTerrain.MapY;
    fGameInfo.VictoryCondition := 'Win';
    fGameInfo.DefeatCondition := 'Lose';
    fGameInfo.PlayerCount := fPlayers.Count;
    for I := 0 to fPlayers.Count - 1 do
    begin
      if fNetworking <> nil then
        NetIndex := fNetworking.NetPlayers.PlayerIndexToLocal(I)
      else
        NetIndex := -1;

      if NetIndex = -1 then
      begin
        fGameInfo.CanBeHuman[I] := False;
        fGameInfo.LocationName[I] := 'Unknown ' + IntToStr(I + 1);
        fGameInfo.PlayerTypes[I] := pt_Human;
        fGameInfo.ColorID[I] := 0;
        fGameInfo.Team[I] := 0;
      end else
      begin
        fGameInfo.CanBeHuman[I] := fNetworking.NetPlayers[NetIndex].IsHuman;
        fGameInfo.LocationName[I] := fNetworking.NetPlayers[NetIndex].Nikname;
        fGameInfo.PlayerTypes[I] := fNetworking.NetPlayers[NetIndex].GetPlayerType;
        fGameInfo.ColorID[I] := fNetworking.NetPlayers[NetIndex].FlagColorID;
        fGameInfo.Team[I] := fNetworking.NetPlayers[NetIndex].Team;
      end;
    end;

    fGameInfo.Save(SaveStream);
    fGameInfo.Free;
    fGameOptions.Save(SaveStream);

    //Because some stuff is only saved in singleplayer we need to know whether it is included in this save,
    //so we can load multiplayer saves in single player and vice versa.
    SaveStream.Write(fGameMode = gmMulti);

    //Minimap is near the start so it can be accessed quickly
    //Each player in MP has his own minimap version ..
    if fGameMode <> gmMulti then
      fMinimap.SaveToStream(SaveStream);

    //We need to know which campaign to display after victory
    SaveStream.Write(fCampaignName);
    SaveStream.Write(fCampaignMap);

    //We need to know which mission/savegame to try to restart
    //(paths are relative and thus - MP safe)
    SaveStream.Write(fMissionFile);

    SaveStream.Write(fIDTracker); //Units-Houses ID tracker
    SaveStream.Write(GetKaMSeed); //Include the random seed in the save file to ensure consistency in replays

    if fGameMode <> gmMulti then
      SaveStream.Write(PlayOnState, SizeOf(PlayOnState));

    fTerrain.Save(SaveStream); //Saves the map
    fPlayers.Save(SaveStream, fGameMode = gmMulti); //Saves all players properties individually
    fAIFields.Save(SaveStream);
    fPathfinding.Save(SaveStream);
    fProjectiles.Save(SaveStream);
    fScripting.Save(SaveStream);

    //Relative path to strings will be the same for all MP players
    s := ExtractRelativePath(ExeDir, ChangeFileExt(fMissionFile, '.%s.libx'));
    SaveStream.Write(s);

    //Parameters that are not identical for all players should not be saved as we need saves to be
    //created identically on all player's computers. Eventually these things can go through the GIP

    //For multiplayer consistency we compare all saves CRCs, they should be created identical on all player's computers.
    if fGameMode <> gmMulti then
    begin
      //Viewport settings are unique for each player
      fViewport.Save(SaveStream);
      fGamePlayInterface.Save(SaveStream); //Saves message queue and school/barracks selected units
      //Don't include fGameSettings.Save it's not required for settings are Game-global, not mission
    end;

    //If we want stuff like the MessageStack and screen center to be stored in multiplayer saves,
    //we must send those "commands" through the GIP so all players know about them and they're in sync.
    //There is a comment in fGame.Load about MessageList on this topic.

    //Makes the folders incase they were deleted
    ForceDirectories(ExtractFilePath(aPathName));
    SaveStream.SaveToFile(aPathName); //Some 70ms for TPR7 map
  finally
    SaveStream.Free;
  end;

  fLog.AddTime('Saving game: ' + aPathName);
end;


//Saves game by provided name
procedure TKMGame.Save(const aName: string);
var
  PathName: string;
begin
  //Convert name to full path+name
  PathName := SaveName(aName, 'sav', IsMultiplayer);

  SaveGame(PathName);

  //Remember which savegame to try to restart (if game was not saved before)
  fSaveFile := ExtractRelativePath(ExeDir, PathName);

  //Copy basesave so we have a starting point for replay
  DeleteFile(SaveName(aName, 'bas', IsMultiplayer));
  CopyFile(PChar(SaveName('basesave', 'bas', IsMultiplayer)), PChar(SaveName(aName, 'bas', IsMultiplayer)), False);

  //Save replay queue
  fLog.AddTime('Saving replay info');
  fGameInputProcess.SaveToFile(ChangeFileExt(PathName, '.rpl'));

  fLog.AddTime('Saving game', True);
end;


procedure TKMGame.Load(const aPathName: string);
var
  LoadStream: TKMemoryStream;
  GameInfo: TKMGameInfo;
  LibxPath: AnsiString;
  LoadedSeed: Longint;
  SaveIsMultiplayer: Boolean;
begin
  fSaveFile := ChangeFileExt(ExtractRelativePath(ExeDir, aPathName), '.sav');

  fLog.AddTime('Loading game from: ' + aPathName);

  LoadStream := TKMemoryStream.Create;
  try

  if not FileExists(aPathName) then
    raise Exception.Create('Savegame could not be found');

  LoadStream.LoadFromFile(aPathName);

  //We need only few essential parts from GameInfo, the rest is duplicate from fTerrain and fPlayers
  GameInfo := TKMGameInfo.Create;
  try
    GameInfo.Load(LoadStream);
    fGameName := GameInfo.Title;
    fGameTickCount := GameInfo.TickCount;
    fMissionMode := GameInfo.MissionMode;
  finally
    FreeAndNil(GameInfo);
  end;

  fGameOptions.Load(LoadStream);

  //So we can allow loading of multiplayer saves in single player and vice versa we need to know which type THIS save is
  LoadStream.Read(SaveIsMultiplayer);
  if SaveIsMultiplayer and (fGameMode = gmReplaySingle) then
    fGameMode := gmReplayMulti; //We only know which it is once we've read the save file, so update it now

  //If the player loads a multiplayer save in singleplayer or replay mode, we require a mutex lock to prevent cheating
  //If we're loading in multiplayer mode we have already locked the mutex when entering multiplayer menu,
  //which is better than aborting loading in a multiplayer game (spoils it for everyone else too)
  if SaveIsMultiplayer and (fGameMode in [gmSingle, gmReplaySingle, gmReplayMulti]) then
    if fMain.LockMutex then
      fGameLockedMutex := True //Remember so we unlock it in Destroy
    else
      //Abort loading (exception will be caught in fGameApp and shown to the user)
      raise Exception.Create(fTextLibrary[TX_MULTIPLE_INSTANCES]);

  //Not used, (only stored for preview) but it's easiest way to skip past it
  if not SaveIsMultiplayer then
    fMinimap.LoadFromStream(LoadStream);

  //We need to know which campaign to display after victory
  LoadStream.Read(fCampaignName);
  LoadStream.Read(fCampaignMap);

  //We need to know which mission/savegame to try to restart
  //(paths are relative and thus - MP safe)
  LoadStream.Read(fMissionFile);

  LoadStream.Read(fIDTracker);
  LoadStream.Read(LoadedSeed);

  if not SaveIsMultiplayer then
    LoadStream.Read(PlayOnState, SizeOf(PlayOnState));

  //Load the data into the game
  fTerrain.Load(LoadStream);

  fPlayers.Load(LoadStream);
  fAIFields.Load(LoadStream);
  fPathfinding.Load(LoadStream);
  fProjectiles.Load(LoadStream);
  fScripting.Load(LoadStream);

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


  if IsReplay then
    fGameInputProcess := TGameInputProcess_Single.Create(gipReplaying) //Replay
  else
    if fGameMode = gmMulti then
      fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking) //Multiplayer
    else
      fGameInputProcess := TGameInputProcess_Single.Create(gipRecording); //Singleplayer

  fGameInputProcess.LoadFromFile(ChangeFileExt(aPathName, '.rpl'));

  fPlayers.SyncLoad; //Should parse all Unit-House ID references and replace them with actual pointers
  fTerrain.SyncLoad; //IsUnit values should be replaced with actual pointers

  if fGameMode = gmMulti then
    MultiplayerRig;

  SetKaMSeed(LoadedSeed);

  if fGameMode in [gmSingle, gmMulti] then
  begin
    DeleteFile(SaveName('basesave', 'bas', IsMultiplayer));
    CopyFile(PChar(ChangeFileExt(aPathName, '.bas')), PChar(SaveName('basesave', 'bas', IsMultiplayer)), False);
  end;

  //When everything is ready we can update UI
  UpdateUI;
  if SaveIsMultiplayer then //MP does not saves view position cos of save identity for all players
    fViewport.Position := KMPointF(MyPlayer.CenterScreen);

  fLog.AddTime('Loading game', True);

  finally
    FreeAndNil(LoadStream);
  end;
end;


procedure TKMGame.UpdateGame(Sender: TObject);
var I: Integer;
begin
  if fIsPaused then Exit;

  case fGameMode of
    gmSingle, gmMulti:
                  if not (fGameMode = gmMulti) or (fNetworking.NetGameState <> lgs_Loading) then
                  for I := 1 to fGameSpeedMultiplier do
                  begin
                    if fGameInputProcess.CommandsConfirmed(fGameTickCount+1) then
                    begin
                      if DO_PERF_LOGGING then fPerfLog.EnterSection(psTick);

                      if fWaitingForNetwork then GameWaitingForNetwork(false); //No longer waiting for players
                      inc(fGameTickCount); //Thats our tick counter for gameplay events
                      if (fGameMode = gmMulti) then fNetworking.LastProcessedTick := fGameTickCount;
                      //Tell the master server about our game on the specific tick (host only)
                      if (fGameMode = gmMulti) and fNetworking.IsHost and (
                         ((fMissionMode = mm_Normal) and (fGameTickCount = ANNOUNCE_BUILD_MAP)) or
                         ((fMissionMode = mm_Tactic) and (fGameTickCount = ANNOUNCE_BATTLE_MAP))) then
                        fNetworking.ServerQuery.SendMapInfo(fGameName, fNetworking.NetPlayers.GetConnectedCount);

                      fScripting.UpdateState;
                      UpdatePeacetime; //Send warning messages about peacetime if required
                      fTerrain.UpdateState;
                      fAIFields.UpdateState(fGameTickCount);
                      fPlayers.UpdateState(fGameTickCount); //Quite slow
                      if fGame = nil then Exit; //Quit the update if game was stopped for some reason
                      fPathfinding.UpdateState;
                      fProjectiles.UpdateState; //If game has stopped it's NIL

                      //The selected object could have died during this fProjectiles or fPlayers update
                      fPlayers.UpdateSelection;

                      fGameInputProcess.RunningTimer(fGameTickCount); //GIP_Multi issues all commands for this tick
                      //In aggressive mode store a command every tick so we can find exactly when a replay mismatch occurs
                      if AGGRESSIVE_REPLAYS then
                        fGameInputProcess.CmdTemp(gic_TempDoNothing);

                      //Each 1min of gameplay time
                      //Don't autosave if the game was put on hold during this tick
                      if (fGameTickCount mod 600 = 0) and fGameApp.GameSettings.Autosave then
                        AutoSave;

                      if DO_PERF_LOGGING then fPerfLog.LeaveSection(psTick);

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
    gmReplaySingle,gmReplayMulti:
                  for I := 1 to fGameSpeedMultiplier do
                  begin
                    Inc(fGameTickCount); //Thats our tick counter for gameplay events
                    fScripting.UpdateState;
                    UpdatePeacetime; //Send warning messages about peacetime if required (peacetime sound should still be played in replays)
                    fTerrain.UpdateState;
                    fPlayers.UpdateState(fGameTickCount); //Quite slow
                    if fGame = nil then Exit; //Quit the update if game was stopped for some reason
                    fPathfinding.UpdateState;
                    fProjectiles.UpdateState; //If game has stopped it's NIL

                    //The selected object could have died during this fProjectiles or fPlayers update
                    fPlayers.UpdateSelection;

                    //Issue stored commands
                    fGameInputProcess.ReplayTimer(fGameTickCount);
                    if fGame = nil then Exit; //Quit if the game was stopped by a replay mismatch
                    if not SkipReplayEndCheck and fGameInputProcess.ReplayEnded then
                      RequestGameHold(gr_ReplayEnd);

                    if fAdvanceFrame then begin
                      fAdvanceFrame := False;
                      fIsPaused := True;
                    end;

                    //Break the for loop (if we are using speed up)
                    if DoGameHold then break;
                  end;
    gmMapEd:   begin
                  fTerrain.IncAnimStep;
                  fPlayers.IncAnimStep;
                end;
  end;

  fAlerts.UpdateState;

  if DoGameHold then GameHold(True, DoGameHoldState);
end;


procedure TKMGame.UpdateState(aGlobalTickCount: Cardinal);
begin
  if not fIsPaused then
    fActiveInterface.UpdateState(aGlobalTickCount);

  //Update minimap every 1000ms
  if aGlobalTickCount mod 10 = 0 then
    fMinimap.Update(False);

  if (aGlobalTickCount mod 10 = 0) and (fMapEditor <> nil) then
    fMapEditor.Update;
end;


//This is our real-time "thread", use it wisely
procedure TKMGame.UpdateStateIdle(aFrameTime: Cardinal);
begin
  if (not fIsPaused) or IsReplay then
    fViewport.UpdateStateIdle(aFrameTime); //Check to see if we need to scroll

  //Terrain should be updated in real time when user applies brushes
  if fTerrainPainter <> nil then
    fTerrainPainter.UpdateStateIdle;
end;


procedure TKMGame.UpdateUI;
begin
  fMinimap.LoadFromTerrain(fAlerts);
  fMinimap.Update(False);

  if fGameMode = gmMapEd then
  begin
    fViewport.ResizeMap(fTerrain.MapX, fTerrain.MapY, 100/CELL_SIZE_PX);
    fViewport.ResetZoom;

    fMapEditorInterface.Player_UpdateColors;
    fMapEditorInterface.SetMapName(fGameName);
    fMapEditorInterface.SetMinimap;
  end
  else
  begin
    fViewport.ResizeMap(fTerrain.MapX, fTerrain.MapY, fTerrain.TopHill/CELL_SIZE_PX);
    fViewport.ResetZoom;

    fGamePlayInterface.SetMinimap;
    fGamePlayInterface.SetMenuState(fMissionMode = mm_Tactic);
  end;
end;


function TKMGame.SaveName(const aName, aExt: string; aMultiPlayer: Boolean): string;
begin
  if aMultiPlayer then
    Result := ExeDir + 'SavesMP\' + aName + '.' + aExt
  else
    Result := ExeDir + 'Saves\' + aName + '.' + aExt;
end;


end.

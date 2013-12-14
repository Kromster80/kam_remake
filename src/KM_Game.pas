unit KM_Game;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, FileUtil, {$ENDIF}
  Forms, Controls, Classes, Dialogs, ExtCtrls, SysUtils, KromUtils, Math, TypInfo,
  {$IFDEF USE_MAD_EXCEPT} MadExcept, KM_Exceptions, {$ENDIF}
  KM_CommonTypes, KM_Defaults, KM_Points,
  KM_GameInputProcess, KM_GameOptions,
  KM_InterfaceDefaults, KM_InterfaceGame, KM_InterfaceMapEditor, KM_InterfaceGamePlay,
  KM_MapEditor, KM_Networking, KM_Scripting, KM_Campaigns,
  KM_PathFinding, KM_PathFindingAStarOld, KM_PathFindingAStarNew, KM_PathFindingJPS,
  KM_PerfLog, KM_Projectiles, KM_Render, KM_ResTexts;

type
  TGameMode = (
    gmSingle,
    gmMulti,        //Different GIP, networking,
    gmMapEd,        //Army handling, lite updates,
    gmReplaySingle, //No input, different results screen to gmReplayMulti
    gmReplayMulti   //No input, different results screen to gmReplaySingle
    );

  //Class that manages single game session
  TKMGame = class
  private //Irrelevant to savegame
    fTimerGame: TTimer;
    fGameOptions: TKMGameOptions;
    fNetworking: TKMNetworking;
    fGameInputProcess: TGameInputProcess;
    fTextMission: TKMTextLibraryMulti;
    fPathfinding: TPathFinding;
    fPerfLog: TKMPerfLog;
    fActiveInterface: TKMUserInterfaceGame; //Shortcut for both of UI
    fGamePlayInterface: TKMGamePlayInterface;
    fMapEditorInterface: TKMapEdInterface;
    fMapEditor: TKMMapEditor;
    fScripting: TKMScripting;

    fIsExiting: Boolean; //Set this to true on Exit and unit/house pointers will be released without cross-checking
    fIsPaused: Boolean;
    fGameSpeed: Single; //Actual speedup value
    fGameSpeedMultiplier: Word; //How many ticks are compressed into one
    fGameMode: TGameMode;
    fWaitingForNetwork: Boolean; //Indicates that we are waiting for other players commands in MP
    fAdvanceFrame: Boolean; //Replay variable to advance 1 frame, afterwards set to false
    fSaveFile: UnicodeString;  //Relative pathname to savegame we are playing, so it gets saved to crashreport
    fGameLockedMutex: Boolean;

  //Should be saved
    fCampaignMap: Byte;         //Which campaign map it is, so we can unlock next one on victory
    fCampaignName: TKMCampaignId;  //Is this a game part of some campaign
    fGameName: UnicodeString;
    fGameTickCount: Cardinal;
    fUIDTracker: Cardinal;       //Units-Houses tracker, to issue unique IDs
    fMissionFile: UnicodeString;   //Relative pathname to mission we are playing, so it gets saved to crashreport
    fMissionMode: TKMissionMode;

    function ParseTextMarkup(aText: UnicodeString): UnicodeString;
    procedure GameMPDisconnect(const aData: UnicodeString);
    procedure MultiplayerRig;
    procedure SaveGame(const aPathName: UnicodeString; aTimestamp: TDateTime);
    procedure UpdatePeaceTime;
    function WaitingPlayersList: TKMByteArray;
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

    procedure GameStart(aMissionFile, aGameName: UnicodeString; aCampName: TKMCampaignId; aCampMap: Byte; aLocation: ShortInt; aColor: Cardinal); overload;
    procedure GameStart(aSizeX, aSizeY: Integer); overload;
    procedure Load(const aPathName: UnicodeString);

    function MapSizeInfo: UnicodeString;

    procedure GameMPPlay(Sender: TObject);
    procedure GameMPReadyToPlay(Sender: TObject);
    procedure GameHold(DoHold: Boolean; Msg: TGameResultMsg); //Hold the game to ask if player wants to play after Victory/Defeat/ReplayEnd
    procedure RequestGameHold(Msg: TGameResultMsg);
    procedure PlayerVictory(aPlayerIndex: THandIndex);
    procedure PlayerDefeat(aPlayerIndex: THandIndex);
    procedure WaitingPlayersDisplay(aWaiting: Boolean);
    procedure WaitingPlayersDrop;

    procedure AutoSave(aTimestamp: TDateTime);
    procedure SaveMapEditor(const aPathName: UnicodeString);
    procedure RestartReplay; //Restart the replay but keep current viewport position/zoom

    function MissionTime: TDateTime;
    function GetPeacetimeRemaining: TDateTime;
    function CheckTime(aTimeTicks: Cardinal): Boolean;
    function IsPeaceTime: Boolean;
    function IsMapEditor: Boolean;
    function IsMultiplayer: Boolean;
    function IsReplay: Boolean;
    procedure ShowMessage(aKind: TKMMessageKind; aText: UnicodeString; aLoc: TKMPoint);
    procedure ShowMessageFormatted(aKind: TKMMessageKind; aText: UnicodeString; aLoc: TKMPoint; aParams: array of const);
    procedure ShowOverlay(aText: UnicodeString);
    procedure ShowOverlayFormatted(aText: UnicodeString; aParams: array of const);
    procedure OverlayAppend(aText: UnicodeString);
    procedure OverlayAppendFormatted(aText: UnicodeString; aParams: array of const);
    property GameTickCount:cardinal read fGameTickCount;
    property GameName: UnicodeString read fGameName;
    property CampaignName: TKMCampaignId read fCampaignName;
    property CampaignMap: Byte read fCampaignMap;
    property GameSpeed: Single read fGameSpeed;
    function PlayerLoc: Byte;
    function PlayerColor: Cardinal;

    property GameMode: TGameMode read fGameMode;
    property MissionFile: UnicodeString read fMissionFile;
    property SaveFile: UnicodeString read fSaveFile;

    property IsExiting: Boolean read fIsExiting;
    property IsPaused: Boolean read fIsPaused write fIsPaused;
    property MissionMode: TKMissionMode read fMissionMode write fMissionMode;
    function GetNewUID: Integer;
    procedure SetGameSpeed(aSpeed: Single; aToggle: Boolean);
    procedure StepOneFrame;
    function SaveName(const aName, aExt: UnicodeString; aMultiPlayer: Boolean): UnicodeString;
    procedure UpdateMultiplayerTeams;

    property PerfLog: TKMPerfLog read fPerfLog;

    property Networking: TKMNetworking read fNetworking;
    property Pathfinding: TPathFinding read fPathfinding;
    property GameInputProcess: TGameInputProcess read fGameInputProcess;
    property GameOptions: TKMGameOptions read fGameOptions;
    property ActiveInterface: TKMUserInterfaceGame read fActiveInterface;
    property GamePlayInterface: TKMGamePlayInterface read fGamePlayInterface;
    property MapEditor: TKMMapEditor read fMapEditor;
    property TextMission: TKMTextLibraryMulti read fTextMission;

    procedure Save(const aSaveName: UnicodeString; aTimestamp: TDateTime);
    {$IFDEF USE_MAD_EXCEPT}
    procedure AttachCrashReport(const ExceptIntf: IMEException; aZipFile: UnicodeString);
    {$ENDIF}
    procedure ReplayInconsistancy;

    procedure Render(aRender: TRender);
    procedure RenderSelection;
    procedure UpdateGame(Sender: TObject);
    procedure UpdateState(aGlobalTickCount: Cardinal);
    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


const
  UID_NONE: Integer = -1;

var
  gGame: TKMGame;


implementation
uses
  KM_CommonClasses, KM_Log, KM_Utils,
  KM_ArmyEvaluation, KM_GameApp, KM_GameInfo, KM_MissionScript, KM_MissionScript_Standard,
  KM_Hand, KM_HandSpectator, KM_HandsCollection, KM_RenderPool, KM_Resource, KM_ResCursors,
  KM_ResSound, KM_Terrain, KM_AIFields, KM_Maps, KM_Sound, KM_ScriptingESA,
  KM_GameInputProcess_Single, KM_GameInputProcess_Multi, KM_Main;


//Create template for the Game
//aRender - who will be rendering the Game session
//aNetworking - access to MP stuff
constructor TKMGame.Create(aGameMode: TGameMode; aRender: TRender; aNetworking: TKMNetworking);
begin
  inherited Create;

  fGameMode := aGameMode;
  fNetworking := aNetworking;

  fAdvanceFrame := False;
  fUIDTracker   := 0;
  PlayOnState   := gr_Cancel;
  DoGameHold    := False;
  SkipReplayEndCheck := False;
  fWaitingForNetwork := False;
  fGameOptions  := TKMGameOptions.Create;

  //UserInterface is different between Gameplay and MapEd
  if fGameMode = gmMapEd then
  begin
    fMapEditorInterface := TKMapEdInterface.Create(aRender);
    fActiveInterface := fMapEditorInterface;
  end
  else
  begin
    fGamePlayInterface := TKMGamePlayInterface.Create(aRender, IsMultiplayer, IsReplay);
    fActiveInterface := fGamePlayInterface;
  end;

  fTimerGame := TTimer.Create(nil);
  SetGameSpeed(1, False); //Initialize relevant variables
  fTimerGame.OnTimer := UpdateGame;
  fTimerGame.Enabled := True;

  //Here comes terrain/mission init
  SetKaMSeed(4); //Every time the game will be the same as previous. Good for debug.
  gTerrain := TKMTerrain.Create;
  gHands := TKMHandsCollection.Create;
  fAIFields := TKMAIFields.Create;

  InitUnitStatEvals; //Army

  if DO_PERF_LOGGING then fPerfLog := TKMPerfLog.Create;
  gLog.AddTime('<== Game creation is done ==>');
  fScripting := TKMScripting.Create;

  case PathFinderToUse of
    0:    fPathfinding := TPathfindingAStarOld.Create;
    1:    fPathfinding := TPathfindingAStarNew.Create;
    2:    fPathfinding := TPathfindingJPS.Create;
    else  fPathfinding := TPathfindingAStarOld.Create;
  end;
  gProjectiles := TKMProjectiles.Create;

  fGameTickCount := 0; //Restart counter
end;


//Destroy what was created
destructor TKMGame.Destroy;
begin
  //We might have crashed part way through .Create, so we can't assume ANYTHING exists here.
  //Doing so causes a 2nd exception which overrides 1st. Hence check <> nil on everything except Frees, TObject.Free does that already.

  if fGameLockedMutex then fMain.UnlockMutex;
  if fTimerGame <> nil then fTimerGame.Enabled := False;
  fIsExiting := True;

  //if (fGameInputProcess <> nil) and (fGameInputProcess.ReplayState = gipRecording) then
  //  fGameInputProcess.SaveToFile(SaveName('basesave', 'rpl', fGameMode = gmMulti));

  if DO_PERF_LOGGING and (fPerfLog <> nil) then
    fPerfLog.SaveToFile(ExeDir + 'Logs' + PathDelim + 'PerfLog.txt');

  FreeAndNil(fTimerGame);

  FreeThenNil(fMapEditor);
  FreeThenNil(gHands);
  FreeThenNil(gTerrain);
  FreeAndNil(fAIFields);
  FreeAndNil(gProjectiles);
  FreeAndNil(fPathfinding);
  FreeAndNil(fScripting);

  FreeThenNil(fGamePlayInterface);
  FreeThenNil(fMapEditorInterface);

  FreeAndNil(fGameInputProcess);
  FreeAndNil(fRenderPool);
  FreeAndNil(fGameOptions);

  if DO_PERF_LOGGING then fPerfLog.Free;

  //When leaving the game we should always reset the cursor in case the user had beacon or linking selected
  gResource.Cursors.Cursor := kmc_Default;

  FreeAndNil(MySpectator);

  inherited;
end;


function TKMGame.MapSizeInfo: UnicodeString;
begin
  Result := 'Map size: ' + IntToStr(gTerrain.MapX) + ' x ' + IntToStr(gTerrain.MapY);
end;


//New mission
procedure TKMGame.GameStart(aMissionFile, aGameName: UnicodeString; aCampName: TKMCampaignId; aCampMap: Byte; aLocation: ShortInt; aColor: Cardinal);
const
  GAME_PARSE: array [TGameMode] of TMissionParsingMode = (
    mpm_Single, mpm_Multi, mpm_Editor, mpm_Single, mpm_Single);
var
  I: Integer;
  ParseMode: TMissionParsingMode;
  PlayerEnabled: TPlayerEnabledArray;
  Parser: TMissionParserStandard;
begin
  gLog.AddTime('GameStart');
  Assert(fGameMode in [gmMulti, gmMapEd, gmSingle]);

  fGameName := aGameName;
  fCampaignName := aCampName;
  fCampaignMap := aCampMap;
  fMissionFile := ExtractRelativePath(ExeDir, aMissionFile);
  fSaveFile := '';
  MySpectator := nil; //In case somebody looks at it while parsing DAT, e.g. destroyed houses

  gLog.AddTime('Loading DAT file: ' + aMissionFile);

  //Disable players in MP to skip their assets from loading by MissionParser
  //In SP all players are enabled by default
  case fGameMode of
    gmMulti:  begin
                FillChar(PlayerEnabled, SizeOf(PlayerEnabled), #0);
                for I := 1 to fNetworking.NetPlayers.Count do
                  //PlayerID is 0 based
                  PlayerEnabled[fNetworking.NetPlayers[I].StartLocation - 1] := True;

                //Fixed AIs are always enabled (e.g. coop missions)
                for I := 0 to fNetworking.MapInfo.LocCount - 1 do
                  if fNetworking.MapInfo.CanBeAI[I] and not fNetworking.MapInfo.CanBeHuman[I] then
                    PlayerEnabled[I] := True;
              end;
    gmSingle: //Setup should tell us which player is AI and which not
              for I := 0 to MAX_HANDS - 1 do
                PlayerEnabled[I] := True;
    else      FillChar(PlayerEnabled, SizeOf(PlayerEnabled), #255);
  end;

  //Choose how we will parse the script
  ParseMode := GAME_PARSE[fGameMode];

  if fGameMode = gmMapEd then
  begin
    //Mission loader needs to read the data into MapEd (e.g. FOW revealers)
    fMapEditor := TKMMapEditor.Create;
  end;

  Parser := TMissionParserStandard.Create(ParseMode, PlayerEnabled, False);
  try
    if not Parser.LoadMission(aMissionFile) then
      raise Exception.Create(Parser.FatalErrors);

    if fGameMode = gmMapEd then
    begin
      gHands.AddPlayers(MAX_HANDS - gHands.Count); //Activate all players
      for I := 0 to gHands.Count - 1 do
        gHands[I].FogOfWar.RevealEverything;
      MySpectator := TKMSpectator.Create(0);
      MySpectator.FOWIndex := PLAYER_NONE;
    end
    else
    if fGameMode = gmSingle then
    begin
      for I := 0 to gHands.Count - 1 do
        gHands[I].PlayerType := hndComputer;

      //-1 means automatically detect the location (used for tutorials and campaigns)
      if aLocation = -1 then
        aLocation := Parser.DefaultLocation;

      Assert(InRange(aLocation, 0, gHands.Count - 1), 'No human player detected');
      gHands[aLocation].PlayerType := hndHuman;
      MySpectator := TKMSpectator.Create(aLocation);
      if aColor <> $00000000 then //If no color specified use default from mission file (don't overwrite it)
        gHands[MySpectator.HandIndex].FlagColor := aColor;
    end;

    if (Parser.MinorErrors <> '') and (fGameMode <> gmMapEd) then
      fGamePlayInterface.MessageIssue(mkQuill, 'Warnings in mission script:|' + Parser.MinorErrors);

    if (Parser.MinorErrors <> '') and (fGameMode = gmMapEd) then
      fMapEditorInterface.ShowMessage('Warnings in mission script:|' + Parser.MinorErrors);
  finally
    Parser.Free;
  end;

  if fGameMode <> gmMapEd then
  begin
    fScripting.LoadFromFile(ChangeFileExt(aMissionFile, '.script'));
    if (fScripting.ErrorString <> '') then
      fGamePlayInterface.MessageIssue(mkQuill, 'Warnings in script:|' + fScripting.ErrorString);
  end;


  case fGameMode of
    gmMulti:  begin
                fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking);
                fTextMission := TKMTextLibraryMulti.Create;
                fTextMission.LoadLocale(ChangeFileExt(aMissionFile, '.%s.libx'));
              end;
    gmSingle: begin
                fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);
                fTextMission := TKMTextLibraryMulti.Create;
                fTextMission.LoadLocale(ChangeFileExt(aMissionFile, '.%s.libx'));
              end;
    gmMapEd:  ;
  end;

  gLog.AddTime('Gameplay recording initialized', True);

  if fGameMode = gmMulti then
    MultiplayerRig;

  gHands.AfterMissionInit(True);

  //Random after StartGame and ViewReplay should match
  if IsMultiplayer then
    SetKaMSeed(fNetworking.NetGameOptions.RandomSeed)
  else
    SetKaMSeed(RandomRange(1, 2147483646));

  //We need to make basesave.bas since we don't know the savegame name
  //until after user saves it, but we need to attach replay base to it.
  //Basesave is sort of temp we save to HDD instead of keeping in RAM
  if fGameMode in [gmSingle, gmMulti] then
    SaveGame(SaveName('basesave', 'bas', IsMultiplayer), UTCNow);

  //MissionStart goes after basesave to keep it pure (repeats on Load of basesave)
  gScriptEvents.ProcMissionStart;

  //When everything is ready we can update UI
  fActiveInterface.SyncUI;
  if IsMapEditor then
    fActiveInterface.SyncUIView(KMPointF(gTerrain.MapX / 2, gTerrain.MapY / 2))
  else
    fActiveInterface.SyncUIView(KMPointF(gHands[MySpectator.HandIndex].CenterScreen));

  gLog.AddTime('Gameplay initialized', True);
end;


//All setup data gets taken from fNetworking class
procedure TKMGame.MultiplayerRig;
var
  I: Integer;
  handIndex: THandIndex;
begin
  //Copy game options from lobby to this game
  fGameOptions.Peacetime := fNetworking.NetGameOptions.Peacetime;
  fGameOptions.SpeedPT := fNetworking.NetGameOptions.SpeedPT;
  fGameOptions.SpeedAfterPT := fNetworking.NetGameOptions.SpeedAfterPT;

  if IsPeaceTime then
    SetGameSpeed(fGameOptions.SpeedPT, False)
  else
    SetGameSpeed(fGameOptions.SpeedAfterPT, False);

  //Assign existing NetPlayers(1..N) to map players(0..N-1)
  for I := 1 to fNetworking.NetPlayers.Count do
  begin
    handIndex := fNetworking.NetPlayers[I].StartLocation - 1;
    gHands[handIndex].PlayerType := fNetworking.NetPlayers[I].GetPlayerType;
    gHands[handIndex].FlagColor := fNetworking.NetPlayers[I].FlagColor;

    //Set owners name so we can write it into savegame/replay
    gHands[handIndex].SetOwnerNikname(fNetworking.NetPlayers[I].Nikname);
  end;

  //Setup alliances
  //We mirror Lobby team setup on to alliances. Savegame and coop has the setup already
  if (fNetworking.SelectGameKind = ngk_Map) and not fNetworking.MapInfo.IsCoop then
    UpdateMultiplayerTeams;

  MySpectator := TKMSpectator.Create(fNetworking.NetPlayers[fNetworking.MyIndex].StartLocation - 1);

  //We cannot remove a player from a save (as they might be interacting with other players)

  gHands.SyncFogOfWar; //Syncs fog of war revelation between players AFTER alliances
  //Multiplayer missions don't have goals yet, so add the defaults (except for special/coop missions)
  if (fNetworking.SelectGameKind = ngk_Map)
  and not fNetworking.MapInfo.IsSpecial and not fNetworking.MapInfo.IsCoop then
    gHands.AddDefaultGoalsToAll(fMissionMode);

  fNetworking.OnPlay           := GameMPPlay;
  fNetworking.OnReadyToPlay    := GameMPReadyToPlay;
  fNetworking.OnCommands       := TGameInputProcess_Multi(fGameInputProcess).RecieveCommands;
  fNetworking.OnTextMessage    := fGamePlayInterface.ChatMessage;
  fNetworking.OnPlayersSetup   := fGamePlayInterface.AlliesOnPlayerSetup;
  fNetworking.OnPingInfo       := fGamePlayInterface.AlliesOnPingInfo;
  fNetworking.OnDisconnect     := GameMPDisconnect; //For auto reconnecting
  fNetworking.OnReassignedHost := nil; //So it is no longer assigned to a lobby event
  fNetworking.GameCreated;

  if fNetworking.Connected and (fNetworking.NetGameState = lgs_Loading) then
    WaitingPlayersDisplay(True); //Waiting for players
end;


procedure TKMGame.UpdateMultiplayerTeams;
var
  I, K: Integer;
  PlayerI: TKMHand;
  PlayerK: Integer;
begin
  for I := 1 to fNetworking.NetPlayers.Count do
  begin
    PlayerI := gHands[fNetworking.NetPlayers[I].StartLocation - 1]; //PlayerID is 0 based
    for K := 1 to fNetworking.NetPlayers.Count do
    begin
      PlayerK := fNetworking.NetPlayers[K].StartLocation - 1; //PlayerID is 0 based

      //Players are allies if they belong to same team (team 0 means free-for-all)
      if (I = K)
      or ((fNetworking.NetPlayers[I].Team <> 0)
      and (fNetworking.NetPlayers[I].Team = fNetworking.NetPlayers[K].Team)) then
        PlayerI.Alliances[PlayerK] := at_Ally
      else
        PlayerI.Alliances[PlayerK] := at_Enemy;
    end;
  end;
end;


//Everyone is ready to start playing
//Issued by fNetworking at the time depending on each Players lag individually
procedure TKMGame.GameMPPlay(Sender: TObject);
begin
  WaitingPlayersDisplay(False); //Finished waiting for players
  fNetworking.AnnounceGameInfo(MissionTime, GameName);
  gLog.AddTime('Net game began');
end;


procedure TKMGame.GameMPReadyToPlay(Sender: TObject);
begin
  //Update the list of players that are ready to play
  WaitingPlayersDisplay(True);
end;


procedure TKMGame.GameMPDisconnect(const aData: UnicodeString);
begin
  if fNetworking.NetGameState in [lgs_Game, lgs_Reconnecting] then
  begin
    if WRITE_RECONNECT_LOG then gLog.AddTime('GameMPDisconnect: '+aData);
    fNetworking.OnJoinFail := GameMPDisconnect; //If the connection fails (e.g. timeout) then try again
    fNetworking.OnJoinAssignedHost := nil;
    fNetworking.OnJoinSucc := nil;
    fNetworking.AttemptReconnection;
  end
  else
  begin
    fNetworking.Disconnect;
    fGameApp.Stop(gr_Disconnect, gResTexts[TX_GAME_ERROR_NETWORK] + ' ' + aData)
  end;
end;


{$IFDEF USE_MAD_EXCEPT}
procedure TKMGame.AttachCrashReport(const ExceptIntf: IMEException; aZipFile: UnicodeString);

  procedure AttachFile(const aFile: UnicodeString);
  begin
    if (aFile = '') or not FileExists(aFile) then Exit;
    ExceptIntf.AdditionalAttachments.Add(aFile, '', aZipFile);
  end;

var I: Integer;
begin
  gLog.AddTime('Creating crash report...');

  //Attempt to save the game, but if the state is too messed up it might fail
  try
    if fGameMode in [gmSingle, gmMulti] then
    begin
      Save('crashreport', UTCNow);
      AttachFile(SaveName('crashreport', 'sav', IsMultiplayer));
      AttachFile(SaveName('crashreport', 'bas', IsMultiplayer));
      AttachFile(SaveName('crashreport', 'rpl', IsMultiplayer));
    end;
  except
    on E : Exception do
      gLog.AddTime('Exception while trying to save game for crash report: ' + E.ClassName + ': ' + E.Message);
  end;

  AttachFile(ExeDir + fMissionFile);
  AttachFile(ExeDir + ChangeFileExt(fMissionFile, '.map')); //Try to attach the map
  AttachFile(ExeDir + ChangeFileExt(fMissionFile, '.script')); //Try to attach the script

  for I := 1 to AUTOSAVE_COUNT do //All autosaves
  begin
    AttachFile(SaveName('autosave' + Int2Fix(I, 2), 'rpl', IsMultiplayer));
    AttachFile(SaveName('autosave' + Int2Fix(I, 2), 'bas', IsMultiplayer));
    AttachFile(SaveName('autosave' + Int2Fix(I, 2), 'sav', IsMultiplayer));
  end;

  gLog.AddTime('Crash report created');
end;
{$ENDIF}


//Occasional replay inconsistencies are a known bug, we don't need reports of it
procedure TKMGame.ReplayInconsistancy;
begin
  //Stop game from executing while the user views the message
  fIsPaused := True;
  gLog.AddTime('Replay failed a consistency check at tick ' + IntToStr(fGameTickCount));
  if MessageDlg(gResTexts[TX_REPLAY_FAILED], mtWarning, [mbYes, mbNo], 0) <> mrYes then
    fGameApp.Stop(gr_Error, '')
  else
    fIsPaused := False;
end;


//Put the game on Hold for Victory screen
procedure TKMGame.GameHold(DoHold: Boolean; Msg: TGameResultMsg);
begin
  DoGameHold := false;
  fGamePlayInterface.ReleaseDirectionSelector; //In case of victory/defeat while moving troops
  gResource.Cursors.Cursor := kmc_Default;

  fGamePlayInterface.Viewport.ReleaseScrollKeys;
  PlayOnState := Msg;

  if DoHold then
  begin
    fIsPaused := True;
    fGamePlayInterface.ShowPlayMore(true, Msg);
  end else
    fIsPaused := False;
end;


procedure TKMGame.RequestGameHold(Msg: TGameResultMsg);
begin
  DoGameHold := true;
  DoGameHoldState := Msg;
end;


procedure TKMGame.PlayerVictory(aPlayerIndex: THandIndex);
begin
  if aPlayerIndex = MySpectator.HandIndex then
    gSoundPlayer.Play(sfxn_Victory, 1, True); //Fade music

  if fGameMode = gmMulti then
  begin
    if aPlayerIndex = MySpectator.HandIndex then
    begin
      PlayOnState := gr_Win;
      fGamePlayInterface.ShowMPPlayMore(gr_Win);
    end;
  end
  else
    RequestGameHold(gr_Win);
end;


//Wrap for GameApp to access player color (needed for restart mission)
function TKMGame.PlayerColor: Cardinal;
begin
  Result := gHands[MySpectator.HandIndex].FlagColor;
end;


procedure TKMGame.PlayerDefeat(aPlayerIndex: THandIndex);
begin
  //We have not thought of anything to display on players defeat in Replay
  if IsReplay then
    Exit;

  if aPlayerIndex = MySpectator.HandIndex then
    gSoundPlayer.Play(sfxn_Defeat, 1, True); //Fade music

  if fGameMode = gmMulti then
  begin
    fNetworking.PostLocalMessage(Format(gResTexts[TX_MULTIPLAYER_PLAYER_DEFEATED],
                                        [gHands[aPlayerIndex].OwnerName]));
    if aPlayerIndex = MySpectator.HandIndex then
    begin
      PlayOnState := gr_Defeat;
      fGamePlayInterface.ShowMPPlayMore(gr_Defeat);
    end;
  end
  else
  if aPlayerIndex = MySpectator.HandIndex then
    RequestGameHold(gr_Defeat);
end;


function TKMGame.PlayerLoc: Byte;
begin
  Result := MySpectator.HandIndex;
end;


//Get list of players we are waiting for. We do it here because fNetworking does not knows about GIP
function TKMGame.WaitingPlayersList: TKMByteArray;
begin
  case fNetworking.NetGameState of
    lgs_Game, lgs_Reconnecting:
        //GIP is waiting for next tick
        Result := TGameInputProcess_Multi(fGameInputProcess).GetWaitingPlayers(fGameTickCount + 1);
    lgs_Loading:
        //We are waiting during inital loading
        Result := fNetworking.NetPlayers.GetNotReadyToPlayPlayers;
    else
        Assert(False, 'WaitingPlayersList from wrong state');
  end;
end;


procedure TKMGame.WaitingPlayersDisplay(aWaiting: Boolean);
begin
  fWaitingForNetwork := aWaiting;
  fGamePlayInterface.ShowNetworkLag(aWaiting, WaitingPlayersList, fNetworking.IsHost);
end;


procedure TKMGame.WaitingPlayersDrop;
begin
  fNetworking.DropPlayers(WaitingPlayersList);
end;


//Start MapEditor (empty map)
procedure TKMGame.GameStart(aSizeX, aSizeY: Integer);
var
  I: Integer;
begin
  fGameName := gResTexts[TX_MAPED_NEW_MISSION];

  fMissionFile := '';
  fSaveFile := '';

  fMapEditor := TKMMapEditor.Create;
  gTerrain.MakeNewMap(aSizeX, aSizeY, True);
  fMapEditor.TerrainPainter.InitEmpty;
  fMapEditor.TerrainPainter.MakeCheckpoint;

  gHands.AddPlayers(MAX_HANDS); //Create MAX players
  gHands[0].PlayerType := hndHuman; //Make Player1 human by default
  for I := 0 to gHands.Count - 1 do
    gHands[I].FogOfWar.RevealEverything;

  MySpectator := TKMSpectator.Create(0);
  MySpectator.FOWIndex := PLAYER_NONE;

  gHands.AfterMissionInit(false);

  if fGameMode = gmSingle then
    fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);

  //When everything is ready we can update UI
  fActiveInterface.SyncUI;
  fActiveInterface.SyncUIView(KMPointF(gTerrain.MapX / 2, gTerrain.MapY / 2));

  gLog.AddTime('Gameplay initialized', True);
end;


procedure TKMGame.AutoSave(aTimestamp: TDateTime);
var
  I: Integer;
begin
  Save('autosave', aTimestamp); //Save to temp file

  //Delete last autosave and shift remaining by 1 position back
  DeleteFile(SaveName('autosave' + Int2Fix(AUTOSAVE_COUNT, 2), 'sav', IsMultiplayer));
  DeleteFile(SaveName('autosave' + Int2Fix(AUTOSAVE_COUNT, 2), 'rpl', IsMultiplayer));
  DeleteFile(SaveName('autosave' + Int2Fix(AUTOSAVE_COUNT, 2), 'bas', IsMultiplayer));
  for I := AUTOSAVE_COUNT downto 2 do // 03 to 01
  begin
    RenameFile(SaveName('autosave' + Int2Fix(I - 1, 2), 'sav', IsMultiplayer), SaveName('autosave' + Int2Fix(I, 2), 'sav', IsMultiplayer));
    RenameFile(SaveName('autosave' + Int2Fix(I - 1, 2), 'rpl', IsMultiplayer), SaveName('autosave' + Int2Fix(I, 2), 'rpl', IsMultiplayer));
    RenameFile(SaveName('autosave' + Int2Fix(I - 1, 2), 'bas', IsMultiplayer), SaveName('autosave' + Int2Fix(I, 2), 'bas', IsMultiplayer));
  end;

  //Rename temp to be first in list
  RenameFile(SaveName('autosave', 'sav', IsMultiplayer), SaveName('autosave01', 'sav', IsMultiplayer));
  RenameFile(SaveName('autosave', 'rpl', IsMultiplayer), SaveName('autosave01', 'rpl', IsMultiplayer));
  RenameFile(SaveName('autosave', 'bas', IsMultiplayer), SaveName('autosave01', 'bas', IsMultiplayer));
end;


//aPathName - full path to DAT file
procedure TKMGame.SaveMapEditor(const aPathName: UnicodeString);
var
  I: Integer;
  fMissionParser: TMissionParserStandard;
begin
  if aPathName = '' then exit;

  //Prepare and save
  gHands.RemoveEmptyPlayers;

  ForceDirectories(ExtractFilePath(aPathName));
  gLog.AddTime('Saving from map editor: ' + aPathName);

  gTerrain.SaveToFile(ChangeFileExt(aPathName, '.map'));
  fMapEditor.TerrainPainter.SaveToFile(ChangeFileExt(aPathName, '.map'));
  fMissionParser := TMissionParserStandard.Create(mpm_Editor, false);
  fMissionParser.SaveDATFile(ChangeFileExt(aPathName, '.dat'));
  FreeAndNil(fMissionParser);

  fGameName := TruncateExt(ExtractFileName(aPathName));

  //Append empty players in place of removed ones
  gHands.AddPlayers(MAX_HANDS - gHands.Count);
  for I := 0 to gHands.Count - 1 do
    gHands[I].FogOfWar.RevealEverything;
end;


procedure TKMGame.Render(aRender: TRender);
begin
  fRenderPool.Render;

  aRender.SetRenderMode(rm2D);
  fActiveInterface.Paint;
end;


procedure TKMGame.RenderSelection;
begin
  fRenderPool.RenderSelection;
end;


procedure TKMGame.RestartReplay;
begin
  fGameApp.NewReplay(ChangeFileExt(ExeDir + fSaveFile, '.bas'));
end;


//TDateTime stores days/months/years as 1 and hours/minutes/seconds as fractions of a 1
//Treat 10 ticks as 1 sec irregardless of user-set pace
function TKMGame.MissionTime: TDateTime;
begin
  //Convert cardinal into TDateTime, where 1hour = 1/24 and so on..
  Result := fGameTickCount / 24 / 60 / 60 / 10;
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


function TKMGame.ParseTextMarkup(aText: UnicodeString): UnicodeString;
begin
  Result := fTextMission.ParseTextMarkup(aText, '$');
  Result := gResTexts.ParseTextMarkup(Result, '%');
end;


procedure TKMGame.ShowMessage(aKind: TKMMessageKind; aText: UnicodeString; aLoc: TKMPoint);
begin
  fGamePlayInterface.MessageIssue(aKind, ParseTextMarkup(aText), aLoc);
end;


procedure TKMGame.ShowMessageFormatted(aKind: TKMMessageKind; aText: UnicodeString; aLoc: TKMPoint; aParams: array of const);
var S: UnicodeString;
begin
  //We must parse for text markup before AND after running Format, since individual format
  //parameters can contain strings that need parsing (see Annie's Garden for an example)
  S := ParseTextMarkup(Format(ParseTextMarkup(aText), aParams));
  fGamePlayInterface.MessageIssue(aKind, S, aLoc);
end;


procedure TKMGame.ShowOverlay(aText: UnicodeString);
begin
  fGamePlayInterface.SetScriptedOverlay(ParseTextMarkup(aText));
end;


procedure TKMGame.ShowOverlayFormatted(aText: UnicodeString; aParams: array of const);
var S: UnicodeString;
begin
  //We must parse for text markup before AND after running Format, since individual format
  //parameters can contain strings that need parsing (see Annie's Garden for an example)
  S := ParseTextMarkup(Format(ParseTextMarkup(aText), aParams));
  fGamePlayInterface.SetScriptedOverlay(S);
end;


procedure TKMGame.OverlayAppend(aText: UnicodeString);
begin
  fGamePlayInterface.AppendScriptedOverlay(ParseTextMarkup(aText));
end;


procedure TKMGame.OverlayAppendFormatted(aText: UnicodeString; aParams: array of const);
var S: UnicodeString;
begin
  //We must parse for text markup before AND after running Format, since individual format
  //parameters can contain strings that need parsing (see Annie's Garden for an example)
  S := ParseTextMarkup(Format(ParseTextMarkup(aText), aParams));
  fGamePlayInterface.AppendScriptedOverlay(S);
end;


function TKMGame.IsPeaceTime: Boolean;
begin
  Result := not CheckTime(fGameOptions.Peacetime * 600);
end;


procedure TKMGame.UpdatePeaceTime;
var
  PeaceTicksRemaining: Cardinal;
begin
  PeaceTicksRemaining := Max(0, Int64((fGameOptions.Peacetime * 600)) - fGameTickCount);
  if (PeaceTicksRemaining = 1) and (fGameMode in [gmMulti,gmReplayMulti]) then
  begin
    gSoundPlayer.Play(sfxn_Peacetime, 1, True); //Fades music
    if fGameMode = gmMulti then
    begin
      SetGameSpeed(fGameOptions.SpeedAfterPT, False);
      fNetworking.PostLocalMessage(gResTexts[TX_MP_PEACETIME_OVER], false);
    end;
  end;
end;


function TKMGame.GetNewUID: Integer;
const
  //Prime numbers let us generate sequence of non-repeating values of max_value length
  max_value = 16777213;
  step = 8765423;
begin
  //UIDs have the following properties:
  // - allow -1 to indicate no UID (const UID_NONE = 0)
  // - fit within 24bit (we can use that much for RGB colorcoding in unit picking)
  // - Start from 1, so that black colorcode can be detected in render and then re-mapped to -1

  fUIDTracker := (fUIDTracker + step) mod max_value + 1; //1..N range, 0 is nothing for colorpicker
  Result := fUIDTracker;
end;


procedure TKMGame.SetGameSpeed(aSpeed: Single; aToggle: Boolean);
begin
  Assert(aSpeed > 0);

  //MapEd always runs at x1
  if IsMapEditor then
  begin
    fGameSpeed := 1;
    fGameSpeedMultiplier := 1;
    fTimerGame.Interval := Round(fGameApp.GameSettings.SpeedPace / fGameSpeed);
    Exit;
  end;

  //Make the speed toggle between 1 and desired value
  if (aSpeed = fGameSpeed) and aToggle then
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

  //don't show speed clock in MP since you can't turn it on/off
  if (fGamePlayInterface <> nil) and not IsMultiplayer then
    fGamePlayInterface.ShowClock(fGameSpeed);

  //Need to adjust the delay immediately in MP
  if IsMultiplayer and (fGameInputProcess <> nil) then
    TGameInputProcess_Multi(fGameInputProcess).AdjustDelay(fGameSpeed);
end;


//In replay mode we can step the game by exactly one frame and then pause again
procedure TKMGame.StepOneFrame;
begin
  Assert(fGameMode in [gmReplaySingle,gmReplayMulti], 'We can work step-by-step only in Replay');
  SetGameSpeed(1, False); //Make sure we step only one tick. Do not allow multiple updates in UpdateState loop
  fAdvanceFrame := True;
end;


//Saves the game in all its glory
procedure TKMGame.SaveGame(const aPathName: UnicodeString; aTimestamp: TDateTime);
var
  SaveStream: TKMemoryStream;
  fGameInfo: TKMGameInfo;
  I, netIndex: Integer;
begin
  gLog.AddTime('Saving game: ' + aPathName);

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
    fGameInfo.SaveTimestamp := aTimestamp;
    fGameInfo.MissionMode := fMissionMode;
    fGameInfo.MapSizeX := gTerrain.MapX;
    fGameInfo.MapSizeY := gTerrain.MapY;
    fGameInfo.VictoryCondition := 'Win';
    fGameInfo.DefeatCondition := 'Lose';
    fGameInfo.PlayerCount := gHands.Count;
    for I := 0 to gHands.Count - 1 do
    begin
      if fNetworking = nil then
      begin
        fGameInfo.Enabled[I] := False;
        fGameInfo.CanBeHuman[I] := False;
        fGameInfo.OwnerNikname[I] := '';
        fGameInfo.PlayerTypes[I] := hndHuman;
        fGameInfo.ColorID[I] := 0;
        fGameInfo.Team[I] := 0;
      end else
      begin
        netIndex := fNetworking.NetPlayers.PlayerIndexToLocal(I);
        if netIndex <> -1 then
        begin
          fGameInfo.Enabled[I] := True;
          fGameInfo.CanBeHuman[I] := fNetworking.NetPlayers[netIndex].IsHuman;
          fGameInfo.OwnerNikname[I] := fNetworking.NetPlayers[netIndex].Nikname;
          fGameInfo.PlayerTypes[I] := fNetworking.NetPlayers[netIndex].GetPlayerType;
          fGameInfo.ColorID[I] := fNetworking.NetPlayers[netIndex].FlagColorID;
          fGameInfo.Team[I] := fNetworking.NetPlayers[netIndex].Team;
        end;
      end;
    end;

    fGameInfo.Save(SaveStream);
    fGameInfo.Free;
    fGameOptions.Save(SaveStream);

    //Because some stuff is only saved in singleplayer we need to know whether it is included in this save,
    //so we can load multiplayer saves in single player and vice versa.
    SaveStream.Write(IsMultiplayer);

    //In SinglePlayer we want to show player a preview of what the game looked like when he saved
    //Save Minimap is near the start so it can be accessed quickly
    //In MP each player has his own perspective, hence we dont save minimaps to avoid cheating
    if not IsMultiplayer then
      fGamePlayInterface.SaveMinimap(SaveStream);

    //We need to know which campaign to display after victory
    SaveStream.Write(fCampaignName, SizeOf(TKMCampaignId));
    SaveStream.Write(fCampaignMap);

    //We need to know which mission/savegame to try to restart
    //(paths are relative and thus - MP safe)
    SaveStream.WriteW(fMissionFile);

    SaveStream.Write(fUIDTracker); //Units-Houses ID tracker
    SaveStream.Write(GetKaMSeed); //Include the random seed in the save file to ensure consistency in replays

    if not IsMultiplayer then
      SaveStream.Write(PlayOnState, SizeOf(PlayOnState));

    gTerrain.Save(SaveStream); //Saves the map
    gHands.Save(SaveStream, fGameMode = gmMulti); //Saves all players properties individually
    if not IsMultiplayer then
      MySpectator.Save(SaveStream);
    fAIFields.Save(SaveStream);
    fPathfinding.Save(SaveStream);
    gProjectiles.Save(SaveStream);
    fScripting.Save(SaveStream);

    fTextMission.Save(SaveStream);

    //Parameters that are not identical for all players should not be saved as we need saves to be
    //created identically on all player's computers. Eventually these things can go through the GIP

    //For multiplayer consistency we compare all saves CRCs, they should be created identical on all player's computers.
    if not IsMultiplayer then
      fGamePlayInterface.Save(SaveStream); //Saves message queue and school/barracks selected units

    //If we want stuff like the MessageStack and screen center to be stored in multiplayer saves,
    //we must send those "commands" through the GIP so all players know about them and they're in sync.
    //There is a comment in fGame.Load about MessageList on this topic.

    //Makes the folders incase they were deleted
    ForceDirectories(ExtractFilePath(aPathName));
    SaveStream.SaveToFile(aPathName); //Some 70ms for TPR7 map
  finally
    SaveStream.Free;
  end;

  gLog.AddTime('Saving game: ' + aPathName);
end;


//Saves game by provided name
procedure TKMGame.Save(const aSaveName: UnicodeString; aTimestamp: TDateTime);
var
  fullPath: UnicodeString;
begin
  //Convert name to full path+name
  fullPath := SaveName(aSaveName, 'sav', IsMultiplayer);

  SaveGame(fullPath, aTimestamp);

  //Remember which savegame to try to restart (if game was not saved before)
  fSaveFile := ExtractRelativePath(ExeDir, fullPath);

  //Copy basesave so we have a starting point for replay
  DeleteFile(SaveName(aSaveName, 'bas', IsMultiplayer));
  CopyFile(PChar(SaveName('basesave', 'bas', IsMultiplayer)), PChar(SaveName(aSaveName, 'bas', IsMultiplayer)), False);

  //Save replay queue
  gLog.AddTime('Saving replay info');
  fGameInputProcess.SaveToFile(ChangeFileExt(fullPath, '.rpl'));

  gLog.AddTime('Saving game', True);
end;


procedure TKMGame.Load(const aPathName: UnicodeString);
var
  LoadStream: TKMemoryStream;
  GameInfo: TKMGameInfo;
  LoadedSeed: LongInt;
  SaveIsMultiplayer: Boolean;
begin
  fSaveFile := ChangeFileExt(ExtractRelativePath(ExeDir, aPathName), '.sav');

  gLog.AddTime('Loading game from: ' + aPathName);

  LoadStream := TKMemoryStream.Create;
  try

  if not FileExists(aPathName) then
    raise Exception.Create('Savegame could not be found');

  LoadStream.LoadFromFile(aPathName);

  //We need only few essential parts from GameInfo, the rest is duplicate from gTerrain and fPlayers
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
      raise Exception.Create(gResTexts[TX_MULTIPLE_INSTANCES]);

  //Not used, (only stored for SP preview) but it's easiest way to skip past it
  if not SaveIsMultiplayer then
    fGamePlayInterface.LoadMinimap(LoadStream);

  //We need to know which campaign to display after victory
  LoadStream.Read(fCampaignName, SizeOf(TKMCampaignId));
  LoadStream.Read(fCampaignMap);

  //We need to know which mission/savegame to try to restart
  //(paths are relative and thus - MP safe)
  LoadStream.ReadW(fMissionFile);

  LoadStream.Read(fUIDTracker);
  LoadStream.Read(LoadedSeed);

  if not SaveIsMultiplayer then
    LoadStream.Read(PlayOnState, SizeOf(PlayOnState));

  //Load the data into the game
  gTerrain.Load(LoadStream);

  gHands.Load(LoadStream);
  MySpectator := TKMSpectator.Create(0);
  if not SaveIsMultiplayer then
    MySpectator.Load(LoadStream);
  fAIFields.Load(LoadStream);
  fPathfinding.Load(LoadStream);
  gProjectiles.Load(LoadStream);
  fScripting.Load(LoadStream);

  fTextMission := TKMTextLibraryMulti.Create;
  fTextMission.Load(LoadStream);

  if IsReplay then
    MySpectator.FOWIndex := PLAYER_NONE; //Show all by default in replays

  //Multiplayer saves don't have this piece of information. Its valid only for MyPlayer
  //todo: Send all message commands through GIP (note: that means there will be a delay when you press delete)
  if not SaveIsMultiplayer then
    fGamePlayInterface.Load(LoadStream);

  if IsReplay then
    fGameInputProcess := TGameInputProcess_Single.Create(gipReplaying) //Replay
  else
    if fGameMode = gmMulti then
      fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking) //Multiplayer
    else
      fGameInputProcess := TGameInputProcess_Single.Create(gipRecording); //Singleplayer

  fGameInputProcess.LoadFromFile(ChangeFileExt(aPathName, '.rpl'));

   //Should check all Unit-House ID references and replace them with actual pointers
  gHands.SyncLoad;
  gTerrain.SyncLoad;
  gProjectiles.SyncLoad;

  if fGameMode = gmMulti then
    MultiplayerRig;

  SetKaMSeed(LoadedSeed);

  if fGameMode in [gmSingle, gmMulti] then
  begin
    DeleteFile(SaveName('basesave', 'bas', IsMultiplayer));
    CopyFile(PChar(ChangeFileExt(aPathName, '.bas')), PChar(SaveName('basesave', 'bas', IsMultiplayer)), False);
  end;

  //Repeat mission init if necessary
  if fGameTickCount = 0 then
    gScriptEvents.ProcMissionStart;

  //When everything is ready we can update UI
  fActiveInterface.SyncUI;

  //MP does not saves view position cos of save identity for all players
  if SaveIsMultiplayer then
    fActiveInterface.SyncUIView(KMPointF(gHands[MySpectator.HandIndex].CenterScreen));

  gLog.AddTime('Loading game', True);

  finally
    FreeAndNil(LoadStream);
  end;
end;


procedure TKMGame.UpdateGame(Sender: TObject);
var
  I: Integer;
begin
  //Some PCs seem to change 8087CW randomly between events like Timers and OnMouse*,
  //so we need to set it right before we do game logic processing
  Set8087CW($133F);

  if fIsPaused then Exit;

  case fGameMode of
    gmSingle, gmMulti:
                  if not (fGameMode = gmMulti) or (fNetworking.NetGameState <> lgs_Loading) then
                  for I := 1 to fGameSpeedMultiplier do
                  begin
                    if fGameInputProcess.CommandsConfirmed(fGameTickCount+1) then
                    begin
                      if DO_PERF_LOGGING then fPerfLog.EnterSection(psTick);

                      //As soon as next command arrives we are longer in a waiting state
                      if fWaitingForNetwork then
                        WaitingPlayersDisplay(False);

                      Inc(fGameTickCount); //Thats our tick counter for gameplay events
                      if (fGameMode = gmMulti) then fNetworking.LastProcessedTick := fGameTickCount;
                      //Tell the master server about our game on the specific tick (host only)
                      if (fGameMode = gmMulti) and fNetworking.IsHost and (
                         ((fMissionMode = mm_Normal) and (fGameTickCount = ANNOUNCE_BUILD_MAP)) or
                         ((fMissionMode = mm_Tactic) and (fGameTickCount = ANNOUNCE_BATTLE_MAP))) then
                        fNetworking.ServerQuery.SendMapInfo(fGameName, fNetworking.NetPlayers.GetConnectedCount);

                      fScripting.UpdateState;
                      UpdatePeacetime; //Send warning messages about peacetime if required
                      gTerrain.UpdateState;
                      fAIFields.UpdateState(fGameTickCount);
                      gHands.UpdateState(fGameTickCount); //Quite slow
                      if gGame = nil then Exit; //Quit the update if game was stopped for some reason
                      MySpectator.UpdateState(fGameTickCount);
                      fPathfinding.UpdateState;
                      gProjectiles.UpdateState; //If game has stopped it's NIL

                      fGameInputProcess.RunningTimer(fGameTickCount); //GIP_Multi issues all commands for this tick
                      //In aggressive mode store a command every tick so we can find exactly when a replay mismatch occurs
                      if AGGRESSIVE_REPLAYS then
                        fGameInputProcess.CmdTemp(gic_TempDoNothing);

                      //Each 1min of gameplay time
                      //Don't autosave if the game was put on hold during this tick
                      if fGameTickCount mod 600 = 0 then
                      begin
                        if IsMultiplayer then
                        begin
                          if fNetworking.IsHost then
                            fGameInputProcess.CmdGame(gic_GameAutoSave, UTCNow) //Timestamp must be synchronised
                        end
                        else
                          if fGameApp.GameSettings.Autosave then
                            fGameInputProcess.CmdGame(gic_GameAutoSave, UTCNow);
                      end;

                      //if (fGameTickCount mod 10 = 0) then
                      //  SaveGame(ExeDir + 'SavesLog'+PathDelim + int2fix(fGameTickCount, 6));

                      if DO_PERF_LOGGING then fPerfLog.LeaveSection(psTick);

                      //Break the for loop (if we are using speed up)
                      if DoGameHold then break;
                    end
                    else
                    begin
                      fGameInputProcess.WaitingForConfirmation(fGameTickCount);
                      if TGameInputProcess_Multi(fGameInputProcess).GetNumberConsecutiveWaits > 10 then
                        WaitingPlayersDisplay(True);
                    end;
                    fGameInputProcess.UpdateState(fGameTickCount); //Do maintenance
                  end;
    gmReplaySingle,gmReplayMulti:
                  for I := 1 to fGameSpeedMultiplier do
                  begin
                    Inc(fGameTickCount); //Thats our tick counter for gameplay events
                    fScripting.UpdateState;
                    UpdatePeacetime; //Send warning messages about peacetime if required (peacetime sound should still be played in replays)
                    gTerrain.UpdateState;
                    fAIFields.UpdateState(fGameTickCount);
                    gHands.UpdateState(fGameTickCount); //Quite slow
                    if gGame = nil then Exit; //Quit the update if game was stopped for some reason
                    MySpectator.UpdateState(fGameTickCount);
                    fPathfinding.UpdateState;
                    gProjectiles.UpdateState; //If game has stopped it's NIL

                    //Issue stored commands
                    fGameInputProcess.ReplayTimer(fGameTickCount);
                    if gGame = nil then Exit; //Quit if the game was stopped by a replay mismatch
                    if not SkipReplayEndCheck and fGameInputProcess.ReplayEnded then
                      RequestGameHold(gr_ReplayEnd);

                    if fAdvanceFrame then
                    begin
                      fAdvanceFrame := False;
                      fIsPaused := True;
                    end;

                    //Break the for loop (if we are using speed up)
                    if DoGameHold then break;
                  end;
    gmMapEd:   begin
                  gTerrain.IncAnimStep;
                  gHands.IncAnimStep;
                end;
  end;

  if DoGameHold then GameHold(True, DoGameHoldState);
end;


procedure TKMGame.UpdateState(aGlobalTickCount: Cardinal);
begin
  if not fIsPaused then
    fActiveInterface.UpdateState(aGlobalTickCount);

  if (aGlobalTickCount mod 10 = 0) and (fMapEditor <> nil) then
    fMapEditor.Update;
end;


//This is our real-time "thread", use it wisely
procedure TKMGame.UpdateStateIdle(aFrameTime: Cardinal);
begin
  if (not fIsPaused) or IsReplay then
    fActiveInterface.UpdateStateIdle(aFrameTime);

  //Terrain should be updated in real time when user applies brushes
  if fMapEditor <> nil then
    fMapEditor.UpdateStateIdle;
end;


function TKMGame.SaveName(const aName, aExt: UnicodeString; aMultiPlayer: Boolean): UnicodeString;
begin
  if aMultiPlayer then
    Result := ExeDir + 'SavesMP' + PathDelim + aName + '.' + aExt
  else
    Result := ExeDir + 'Saves' + PathDelim + aName + '.' + aExt;
end;


end.

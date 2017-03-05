unit KM_Game;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, FileUtil, {$ENDIF}
  {$IFDEF WDC} UITypes, {$ENDIF}
  Forms, Controls, Classes, Dialogs, ExtCtrls, SysUtils, KromUtils, Math, TypInfo,
  {$IFDEF USE_MAD_EXCEPT} MadExcept, KM_Exceptions, {$ENDIF}
  KM_CommonTypes, KM_Defaults, KM_Points, KM_FileIO,
  KM_GameInputProcess, KM_GameOptions,
  KM_InterfaceDefaults, KM_InterfaceGame, KM_InterfaceMapEditor, KM_InterfaceGamePlay,
  KM_MapEditor, KM_Networking, KM_Scripting, KM_Campaigns,
  KM_PathFinding, KM_PathFindingAStarOld, KM_PathFindingAStarNew, KM_PathFindingJPS,
  KM_PerfLog, KM_Projectiles, KM_Render, KM_ResTexts, KM_CommonClasses, KM_Sound;

type
  TGameMode = (
    gmSingle,
    gmCampaign,
    gmMulti,        //Different GIP, networking,
    gmMultiSpectate,
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
    fOverlayText: array[0..MAX_HANDS] of UnicodeString; //Needed for replays. Not saved since it's translated
    fIgnoreConsistencyCheckErrors: Boolean; // User can ignore all consistency check errors while watching SP replay

    //Should be saved
    fCampaignMap: Byte;         //Which campaign map it is, so we can unlock next one on victory
    fCampaignName: TKMCampaignId;  //Is this a game part of some campaign
    fGameName: UnicodeString;
    fGameMapCRC: Cardinal; //CRC of map for reporting stats to master server. Also used in MapEd
    fGameTickCount: Cardinal;
    fUIDTracker: Cardinal;       //Units-Houses tracker, to issue unique IDs
    fMissionFileSP: UnicodeString; //Relative pathname to mission we are playing, so it gets saved to crashreport. SP only, see GetMissionFile.
    fMissionMode: TKMissionMode;

    procedure GameMPDisconnect(const aData: UnicodeString);
    procedure MultiplayerRig;
    procedure SaveGame(const aPathName: UnicodeString; aTimestamp: TDateTime; const aMinimapPathName: UnicodeString = '');
    procedure UpdatePeaceTime;
    function WaitingPlayersList: TKMByteArray;
    function FindHandToSpec: Integer;
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

    procedure GameStart(aMissionFile, aGameName: UnicodeString; aCRC: Cardinal; aCampaign: TKMCampaign; aCampMap: Byte; aLocation: ShortInt; aColor: Cardinal); overload;
    procedure GameStart(aSizeX, aSizeY: Integer); overload;
    procedure Load(const aPathName: UnicodeString);

    function MapSizeInfo: UnicodeString;

    procedure GameMPPlay(Sender: TObject);
    procedure GameMPReadyToPlay(Sender: TObject);
    procedure GameHold(DoHold: Boolean; Msg: TGameResultMsg); //Hold the game to ask if player wants to play after Victory/Defeat/ReplayEnd
    procedure RequestGameHold(Msg: TGameResultMsg);
    procedure PlayerVictory(aPlayerIndex: TKMHandIndex);
    procedure PlayerDefeat(aPlayerIndex: TKMHandIndex);
    procedure WaitingPlayersDisplay(aWaiting: Boolean);
    procedure WaitingPlayersDrop;
    procedure ShowScriptError(const aMsg: UnicodeString);

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
    function IsSpeedUpAllowed: Boolean;
    function IsMPGameSpeedUpAllowed: Boolean;
    procedure ShowMessage(aKind: TKMMessageKind; aTextID: Integer; aLoc: TKMPoint; aHandIndex: TKMHandIndex);
    procedure ShowMessageLocal(aKind: TKMMessageKind; aText: UnicodeString; aLoc: TKMPoint);
    procedure OverlayUpdate;
    procedure OverlaySet(const aText: UnicodeString; aPlayer: Shortint);
    procedure OverlayAppend(const aText: UnicodeString; aPlayer: Shortint);
    property GameTickCount:cardinal read fGameTickCount;
    property GameName: UnicodeString read fGameName;
    property CampaignName: TKMCampaignId read fCampaignName;
    property CampaignMap: Byte read fCampaignMap;
    property GameSpeed: Single read fGameSpeed;
    function PlayerLoc: Byte;
    function PlayerColor: Cardinal;

    property GameMode: TGameMode read fGameMode;
    property SaveFile: UnicodeString read fSaveFile;
    function GetMissionFile: UnicodeString;
    function GetScriptSoundFile(const aSound: AnsiString; aAudioFormat: TKMAudioFormat): UnicodeString;

    property IsExiting: Boolean read fIsExiting;
    property IsPaused: Boolean read fIsPaused write fIsPaused;
    property MissionMode: TKMissionMode read fMissionMode write fMissionMode;
    function GetNewUID: Integer;
    function GetNormalGameSpeed: Single;
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
    procedure SaveCampaignScriptData(SaveStream: TKMemoryStream);

    procedure Render(aRender: TRender);
    procedure UpdateGame(Sender: TObject);
    procedure UpdateState(aGlobalTickCount: Cardinal);
    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


const
  UID_NONE: Integer = -1; //Would be better to have it 0. But now it's -1 for backwards compatibility

var
  gGame: TKMGame;


implementation
uses
  KM_Log, KM_Utils,
  KM_AIArmyEvaluation, KM_GameApp, KM_GameInfo, KM_MissionScript, KM_MissionScript_Standard,
  KM_Hand, KM_HandSpectator, KM_HandsCollection, KM_RenderPool, KM_Resource, KM_ResCursors,
  KM_ResSound, KM_Terrain, KM_AIFields, KM_Maps, KM_Saves, KM_ScriptingEvents,
  KM_GameInputProcess_Single, KM_GameInputProcess_Multi, KM_Main, KM_AI;


//Create template for the Game
//aRender - who will be rendering the Game session
//aNetworking - access to MP stuff
constructor TKMGame.Create(aGameMode: TGameMode; aRender: TRender; aNetworking: TKMNetworking);
const UIMode: array[TGameMode] of TUIMode = (umSP, umSP, umMP, umSpectate, umSP, umReplay, umReplay);
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
    fGamePlayInterface := TKMGamePlayInterface.Create(aRender, UIMode[fGameMode]);
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
  gAIFields := TKMAIFields.Create;

  InitUnitStatEvals; //Army

  if DO_PERF_LOGGING then fPerfLog := TKMPerfLog.Create;
  gLog.AddTime('<== Game creation is done ==>');

  gLoopSounds := TKMLoopSoundsManager.Create; //Currently only used by scripting
  fScripting := TKMScripting.Create(ShowScriptError);

  fIgnoreConsistencyCheckErrors := False;

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
  //  fGameInputProcess.SaveToFile(SaveName('basesave', 'rpl', fGameMode in [gmMulti, gmMultiSpectate]));

  if DO_PERF_LOGGING and (fPerfLog <> nil) then
    fPerfLog.SaveToFile(ExeDir + 'Logs' + PathDelim + 'PerfLog.txt');

  FreeAndNil(fTimerGame);

  FreeThenNil(fMapEditor);
  FreeThenNil(gHands);
  FreeThenNil(gTerrain);
  FreeAndNil(gAIFields);
  FreeAndNil(gProjectiles);
  FreeAndNil(fPathfinding);
  FreeAndNil(fScripting);
  FreeAndNil(gLoopSounds);

  FreeThenNil(fGamePlayInterface);
  FreeThenNil(fMapEditorInterface);

  FreeAndNil(fGameInputProcess);
  FreeAndNil(fGameOptions);
  FreeAndNil(fTextMission);

  if DO_PERF_LOGGING then fPerfLog.Free;

  //When leaving the game we should always reset the cursor in case the user had beacon or linking selected
  gRes.Cursors.Cursor := kmc_Default;

  FreeAndNil(gMySpectator);

  inherited;
end;


function TKMGame.MapSizeInfo: UnicodeString;
begin
  Result := 'Map size: ' + IntToStr(gTerrain.MapX) + ' x ' + IntToStr(gTerrain.MapY);
end;


//New mission
procedure TKMGame.GameStart(aMissionFile, aGameName: UnicodeString; aCRC: Cardinal; aCampaign: TKMCampaign; aCampMap: Byte; aLocation: ShortInt; aColor: Cardinal);
const
  GAME_PARSE: array [TGameMode] of TMissionParsingMode = (
    mpm_Single, mpm_Single, mpm_Multi, mpm_Multi, mpm_Editor, mpm_Single, mpm_Single);
var
  I: Integer;
  ParseMode: TMissionParsingMode;
  PlayerEnabled: TKMHandEnabledArray;
  Parser: TMissionParserStandard;
  CampaignData: TKMemoryStream;
  CampaignDataTypeFile: UnicodeString;
begin
  gLog.AddTime('GameStart');
  Assert(fGameMode in [gmMulti, gmMultiSpectate, gmMapEd, gmSingle, gmCampaign]);

  fGameName := aGameName;
  fGameMapCRC := aCRC;
  if aCampaign <> nil then
    fCampaignName := aCampaign.CampaignId
  else
    fCampaignName := NO_CAMPAIGN;
  fCampaignMap := aCampMap;

  if IsMultiplayer then
    fMissionFileSP := '' //In MP map could be in DL or MP folder, so don't store path
  else
    fMissionFileSP := ExtractRelativePath(ExeDir, aMissionFile);

  fSaveFile := '';
  FreeAndNil(gMySpectator); //In case somebody looks at it while parsing DAT, e.g. destroyed houses

  gLog.AddTime('Loading DAT file: ' + aMissionFile);

  //Disable players in MP to skip their assets from loading by MissionParser
  //In SP all players are enabled by default
  case fGameMode of
    gmMulti, gmMultiSpectate:
              begin
                FillChar(PlayerEnabled, SizeOf(PlayerEnabled), #0);
                for I := 1 to fNetworking.NetPlayers.Count do
                  if not fNetworking.NetPlayers[I].IsSpectator then
                    //PlayerID is 0 based
                    PlayerEnabled[fNetworking.NetPlayers[I].StartLocation - 1] := True;

                //Fixed AIs are always enabled (e.g. coop missions)
                for I := 0 to fNetworking.MapInfo.LocCount - 1 do
                  if fNetworking.MapInfo.CanBeAI[I] and not fNetworking.MapInfo.CanBeHuman[I] then
                    PlayerEnabled[I] := True;
              end;
    gmSingle, gmCampaign: //Setup should tell us which player is AI and which not
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
    fMapEditor.DetectAttachedFiles(aMissionFile);
  end;

  Parser := TMissionParserStandard.Create(ParseMode, PlayerEnabled);
  try
    if not Parser.LoadMission(aMissionFile) then
      raise Exception.Create(Parser.FatalErrors);

    if fGameMode = gmMapEd then
    begin
      // Activate all players
      gHands.AddPlayers(MAX_HANDS - gHands.Count);

      for I := 0 to gHands.Count - 1 do
        gHands[I].FogOfWar.RevealEverything;

      gMySpectator := TKMSpectator.Create(0);
      gMySpectator.FOWIndex := PLAYER_NONE;
    end
    else
    if fGameMode in [gmSingle, gmCampaign] then
    begin
      for I := 0 to gHands.Count - 1 do
        gHands[I].HandType := hndComputer;

      // -1 means automatically detect the location (used for tutorials and campaigns)
      if aLocation = -1 then
        aLocation := Parser.DefaultLocation;

      Assert(InRange(aLocation, 0, gHands.Count - 1), 'No human player detected');
      gHands[aLocation].HandType := hndHuman;
      gMySpectator := TKMSpectator.Create(aLocation);

      // If no color specified use default from mission file (don't overwrite it)
      if aColor <> $00000000 then
        gMySpectator.Hand.FlagColor := aColor;
    end;

    if Parser.MinorErrors <> '' then
      if IsMapEditor then
        fMapEditorInterface.ShowMessage('Warnings in mission script:|' + Parser.MinorErrors)
      else
        fGamePlayInterface.MessageIssue(mkQuill, 'Warnings in mission script:|' + Parser.MinorErrors);

    if fGameMode <> gmMapEd then
    begin
      if aCampaign <> nil then
      begin
        CampaignData := aCampaign.ScriptData;
        CampaignData.Seek(0, soBeginning); //Seek to the beginning before we read it
        CampaignDataTypeFile := aCampaign.ScriptDataTypeFile;
      end
      else
      begin
        CampaignData := nil;
        CampaignDataTypeFile := '';
      end;

      fScripting.LoadFromFile(ChangeFileExt(aMissionFile, '.script'), CampaignDataTypeFile, CampaignData);
      //fScripting reports compile errors itself now
    end;


    case fGameMode of
      gmMulti, gmMultiSpectate:
                begin
                  fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking);
                  fTextMission := TKMTextLibraryMulti.Create;
                  fTextMission.LoadLocale(ChangeFileExt(aMissionFile, '.%s.libx'));
                end;
      gmSingle, gmCampaign:
                begin
                  fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);
                  fTextMission := TKMTextLibraryMulti.Create;
                  fTextMission.LoadLocale(ChangeFileExt(aMissionFile, '.%s.libx'));
                end;
      gmMapEd:  ;
    end;

    gLog.AddTime('Gameplay recording initialized', True);

    if fGameMode in [gmMulti, gmMultiSpectate] then
      MultiplayerRig;

    //some late operations for parser (f.e. ProcessAttackPositions, which should be done after MultiplayerRig)
    Parser.PostLoadMission;
  finally
    Parser.Free;
  end;

  gHands.AfterMissionInit(fGameMode <> gmMapEd); //Don't flatten roads in MapEd

  //Random after StartGame and ViewReplay should match
  if IsMultiplayer then
    SetKaMSeed(fNetworking.NetGameOptions.RandomSeed)
  else
    SetKaMSeed(RandomRange(1, 2147483646));

  //We need to make basesave.bas since we don't know the savegame name
  //until after user saves it, but we need to attach replay base to it.
  //Basesave is sort of temp we save to HDD instead of keeping in RAM
  if fGameMode in [gmSingle, gmCampaign, gmMulti, gmMultiSpectate] then
    SaveGame(SaveName('basesave', 'bas', IsMultiplayer), UTCNow);

  // Update our ware distributions from settings
  if fGameMode in [gmSingle, gmMulti] then
    GameInputProcess.CmdWareDistribution(gic_WareDistributions, gGameApp.GameSettings.WareDistribution.PackToStr);

  //MissionStart goes after basesave to keep it pure (repeats on Load of basesave)
  gScriptEvents.ProcMissionStart;

  //When everything is ready we can update UI
  fActiveInterface.SyncUI;
  if IsMapEditor then
    fActiveInterface.SyncUIView(KMPointF(gTerrain.MapX / 2, gTerrain.MapY / 2))
  else
    fActiveInterface.SyncUIView(KMPointF(gMySpectator.Hand.CenterScreen));

  gLog.AddTime('Gameplay initialized', True);
end;


function TKMGame.FindHandToSpec: Integer;
var I: Integer;
    handIndex, humanPlayerHandIndex: TKMHandIndex;
begin
  //Find the 1st enabled human hand to be spectating initially.
  //If there is no enabled human hands, then find the 1st enabled hand
  handIndex := -1;
  humanPlayerHandIndex := -1;
  for I := 0 to gHands.Count - 1 do
    if gHands[I].Enabled then
    begin
      if handIndex = -1 then  // save only first index
        handIndex := I;
      if gHands[I].IsHuman then
      begin
        humanPlayerHandIndex := I;
        Break;
      end;
    end;
  if humanPlayerHandIndex <> -1 then
    handIndex := humanPlayerHandIndex
  else if handIndex = -1 then // Should never happen, cause there should be at least 1 enabled hand.
    handIndex := 0;
  Result := handIndex;
end;


//All setup data gets taken from fNetworking class
procedure TKMGame.MultiplayerRig;
var
  I: Integer;
  handIndex: TKMHandIndex;
begin
  //Copy game options from lobby to this game
  fGameOptions.Peacetime := fNetworking.NetGameOptions.Peacetime;
  fGameOptions.SpeedPT := fNetworking.NetGameOptions.SpeedPT;
  fGameOptions.SpeedAfterPT := fNetworking.NetGameOptions.SpeedAfterPT;

  SetGameSpeed(GetNormalGameSpeed, False);

  //Assign existing NetPlayers(1..N) to map players(0..N-1)
  for I := 1 to fNetworking.NetPlayers.Count do
    if not fNetworking.NetPlayers[I].IsSpectator then
    begin
      handIndex := fNetworking.NetPlayers[I].StartLocation - 1;
      gHands[handIndex].HandType := fNetworking.NetPlayers[I].GetPlayerType;
      gHands[handIndex].FlagColor := fNetworking.NetPlayers[I].FlagColor;

      //In saves players can be changed to AIs, which needs to be stored in the replay
      if fNetworking.SelectGameKind = ngk_Save then
        TGameInputProcess_Multi(GameInputProcess).PlayerTypeChange(handIndex, gHands[handIndex].HandType);

      //Set owners name so we can write it into savegame/replay
      gHands[handIndex].SetOwnerNikname(fNetworking.NetPlayers[I].Nikname);
    end;

  //Setup alliances
  //We mirror Lobby team setup on to alliances. Savegame and coop has the setup already
  if (fNetworking.SelectGameKind = ngk_Map) and not fNetworking.MapInfo.BlockTeamSelection then
    UpdateMultiplayerTeams;

  FreeAndNil(gMySpectator); //May have been created earlier
  if fNetworking.MyNetPlayer.IsSpectator then
  begin
    gMySpectator := TKMSpectator.Create(FindHandToSpec);
    gMySpectator.FOWIndex := PLAYER_NONE; //Show all by default while spectating
  end
  else
    gMySpectator := TKMSpectator.Create(fNetworking.MyNetPlayer.StartLocation - 1);

  //We cannot remove a player from a save (as they might be interacting with other players)

  //FOW should never be synced for saves, it should be left like it was when the save was
  //created otherwise it can cause issues in special maps using PlayerShareFog
  if fNetworking.SelectGameKind <> ngk_Save then
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
  fNetworking.OnReassignedJoiner := nil; //So it is no longer assigned to a lobby event
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
    if not fNetworking.NetPlayers[I].IsSpectator then
    begin
      PlayerI := gHands[fNetworking.NetPlayers[I].StartLocation - 1]; //PlayerID is 0 based
      for K := 1 to fNetworking.NetPlayers.Count do
        if not fNetworking.NetPlayers[K].IsSpectator then
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
    gLog.LogNetConnection('GameMPDisconnect: ' + aData);
    fNetworking.OnJoinFail := GameMPDisconnect; //If the connection fails (e.g. timeout) then try again
    fNetworking.OnJoinAssignedHost := nil;
    fNetworking.OnJoinSucc := nil;
    fNetworking.AttemptReconnection;
  end
  else
  begin
    fNetworking.Disconnect;
    gGameApp.Stop(gr_Disconnect, gResTexts[TX_GAME_ERROR_NETWORK] + ' ' + aData)
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
    if fGameMode in [gmSingle, gmCampaign, gmMulti, gmMultiSpectate] then
    begin
      Save('crashreport', UTCNow);
      AttachFile(SaveName('crashreport', 'sav', IsMultiplayer));
      AttachFile(SaveName('crashreport', 'bas', IsMultiplayer));
      AttachFile(SaveName('crashreport', 'rpl', IsMultiplayer));
      AttachFile(SaveName('crashreport', MP_MINIMAP_SAVE_EXT, IsMultiplayer));
    end;
  except
    on E : Exception do
      gLog.AddTime('Exception while trying to save game for crash report: ' + E.ClassName + ': ' + E.Message);
  end;

  AttachFile(ExeDir + GetMissionFile);
  AttachFile(ExeDir + ChangeFileExt(GetMissionFile, '.map')); //Try to attach the map
  AttachFile(ExeDir + ChangeFileExt(GetMissionFile, '.script')); //Try to attach the script

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
  gLog.AddTime('Replay failed a consistency check at tick ' + IntToStr(fGameTickCount));
  if not fIgnoreConsistencyCheckErrors then
  begin
    //Stop game from executing while the user views the message
    fIsPaused := True;
    case MessageDlg(gResTexts[TX_REPLAY_FAILED], mtWarning, [mbYes, mbYesToAll, mbNo], 0) of
      mrYes:      fIsPaused := False;
      mrYesToAll: begin
                    fIgnoreConsistencyCheckErrors := True;  // Ignore these errors in future while watching this replay
                    fIsPaused := False;
                  end
      else        gGameApp.Stop(gr_Error, '');
    end;
  end;
end;


//Put the game on Hold for Victory screen
procedure TKMGame.GameHold(DoHold: Boolean; Msg: TGameResultMsg);
begin
  DoGameHold := false;
  fGamePlayInterface.ReleaseDirectionSelector; //In case of victory/defeat while moving troops
  gRes.Cursors.Cursor := kmc_Default;

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


procedure TKMGame.PlayerVictory(aPlayerIndex: TKMHandIndex);
begin
  if aPlayerIndex = gMySpectator.HandIndex then
    gSoundPlayer.Play(sfxn_Victory, 1, True); //Fade music

  if fGameMode = gmMulti then
  begin
    if aPlayerIndex = gMySpectator.HandIndex then
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
  Result := gMySpectator.Hand.FlagColor;
end;


procedure TKMGame.PlayerDefeat(aPlayerIndex: TKMHandIndex);
begin
  case GameMode of
    gmSingle, gmCampaign:
              if aPlayerIndex = gMySpectator.HandIndex then
              begin
                gSoundPlayer.Play(sfxn_Defeat, 1, True); //Fade music
                RequestGameHold(gr_Defeat);
              end;
    gmMulti:  begin
                if fNetworking.GetNetPlayerByHandIndex(aPlayerIndex) <> nil then
                  fNetworking.PostLocalMessage(Format(gResTexts[TX_MULTIPLAYER_PLAYER_DEFEATED],
                    [fNetworking.GetNetPlayerByHandIndex(aPlayerIndex).NiknameColoredU]), csSystem);
                if aPlayerIndex = gMySpectator.HandIndex then
                begin
                  gSoundPlayer.Play(sfxn_Defeat, 1, True); //Fade music
                  PlayOnState := gr_Defeat;
                  fGamePlayInterface.ShowMPPlayMore(gr_Defeat);
                end;
              end;
    gmMultiSpectate:  if fNetworking.GetNetPlayerByHandIndex(aPlayerIndex) <> nil then
                        fNetworking.PostLocalMessage(Format(gResTexts[TX_MULTIPLAYER_PLAYER_DEFEATED],
                          [fNetworking.GetNetPlayerByHandIndex(aPlayerIndex).NiknameColoredU]), csSystem);
    //We have not thought of anything to display on players defeat in Replay
  end;
end;


function TKMGame.PlayerLoc: Byte;
begin
  Result := gMySpectator.HandIndex;
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
        raise Exception.Create('WaitingPlayersList from wrong state');
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

  fMissionFileSP := '';
  fSaveFile := '';

  fMapEditor := TKMMapEditor.Create;
  gTerrain.MakeNewMap(aSizeX, aSizeY, True);
  fMapEditor.TerrainPainter.InitEmpty;
  fMapEditor.TerrainPainter.MakeCheckpoint;

  gHands.AddPlayers(MAX_HANDS); //Create MAX players
  gHands[0].HandType := hndHuman; //Make Player1 human by default
  for I := 0 to gHands.Count - 1 do
  begin
    gHands[I].FogOfWar.RevealEverything;
    gHands[I].CenterScreen := KMPoint(aSizeX div 2, aSizeY div 2);
  end;

  gMySpectator := TKMSpectator.Create(0);
  gMySpectator.FOWIndex := PLAYER_NONE;

  gHands.AfterMissionInit(false);

  if fGameMode in [gmSingle, gmCampaign] then
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
  DeleteFile(SaveName('autosave' + Int2Fix(AUTOSAVE_COUNT, 2), MP_MINIMAP_SAVE_EXT, IsMultiplayer));
  for I := AUTOSAVE_COUNT downto 2 do // 03 to 01
  begin
    RenameFile(SaveName('autosave' + Int2Fix(I - 1, 2), 'sav', IsMultiplayer), SaveName('autosave' + Int2Fix(I, 2), 'sav', IsMultiplayer));
    RenameFile(SaveName('autosave' + Int2Fix(I - 1, 2), 'rpl', IsMultiplayer), SaveName('autosave' + Int2Fix(I, 2), 'rpl', IsMultiplayer));
    RenameFile(SaveName('autosave' + Int2Fix(I - 1, 2), 'bas', IsMultiplayer), SaveName('autosave' + Int2Fix(I, 2), 'bas', IsMultiplayer));
    RenameFile(SaveName('autosave' + Int2Fix(I - 1, 2), MP_MINIMAP_SAVE_EXT, IsMultiplayer), SaveName('autosave' + Int2Fix(I, 2), MP_MINIMAP_SAVE_EXT, IsMultiplayer));
  end;

  //Rename temp to be first in list
  RenameFile(SaveName('autosave', 'sav', IsMultiplayer), SaveName('autosave01', 'sav', IsMultiplayer));
  RenameFile(SaveName('autosave', 'rpl', IsMultiplayer), SaveName('autosave01', 'rpl', IsMultiplayer));
  RenameFile(SaveName('autosave', 'bas', IsMultiplayer), SaveName('autosave01', 'bas', IsMultiplayer));
  RenameFile(SaveName('autosave', MP_MINIMAP_SAVE_EXT, IsMultiplayer), SaveName('autosave01', MP_MINIMAP_SAVE_EXT, IsMultiplayer));
end;


//aPathName - full path to DAT file
procedure TKMGame.SaveMapEditor(const aPathName: UnicodeString);
var
  I: Integer;
  fMissionParser: TMissionParserStandard;
  MapInfo: TKMapInfo;
  MapFolder: TMapFolder;
begin
  if aPathName = '' then exit;

  //Prepare and save
  gHands.RemoveEmptyPlayers;

  ForceDirectories(ExtractFilePath(aPathName));
  gLog.AddTime('Saving from map editor: ' + aPathName);

  fMapEditor.SaveAttachements(aPathName);
  gTerrain.SaveToFile(ChangeFileExt(aPathName, '.map'));
  fMapEditor.TerrainPainter.SaveToFile(ChangeFileExt(aPathName, '.map'));
  fMissionParser := TMissionParserStandard.Create(mpm_Editor);
  fMissionParser.SaveDATFile(ChangeFileExt(aPathName, '.dat'));
  FreeAndNil(fMissionParser);

  // Update GameSettings for saved maps positions in list on MapEd menu
  if DetermineMapFolder(GetFileDirName(ExtractFileDir(aPathName)), MapFolder) then
  begin
    // Update GameSettings for saved maps positions in list on MapEd menu
    MapInfo := TKMapInfo.Create(GetFileDirName(aPathName), True, MapFolder); //Force recreate map CRC
    case MapInfo.MapFolder of
      mfSP:       begin
                    gGameApp.GameSettings.MenuMapEdSPMapCRC := MapInfo.CRC;
                    gGameApp.GameSettings.MenuMapEdMapType := 0;
                  end;
      mfMP,mfDL:  begin
                    gGameApp.GameSettings.MenuMapEdMPMapCRC := MapInfo.CRC;
                    gGameApp.GameSettings.MenuMapEdMPMapName := MapInfo.FileName;
                    gGameApp.GameSettings.MenuMapEdMapType := 1;
                    // Update favorite map CRC if we resave favourite map with the same name
                    if fGameName = MapInfo.FileName then
                      gGameApp.GameSettings.FavouriteMaps.Replace(fGameMapCRC, MapInfo.CRC);
                  end;
    end;
    MapInfo.Free;
  end;

  fGameName := TruncateExt(ExtractFileName(aPathName));

  //Append empty players in place of removed ones
  gHands.AddPlayers(MAX_HANDS - gHands.Count);
  for I := 0 to gHands.Count - 1 do
    gHands[I].FogOfWar.RevealEverything;
end;


procedure TKMGame.Render(aRender: TRender);
begin
  gRenderPool.Render;

  aRender.SetRenderMode(rm2D);
  fActiveInterface.Paint;
end;


procedure TKMGame.RestartReplay;
begin
  gGameApp.NewReplay(ChangeFileExt(ExeDir + fSaveFile, '.bas'));
end;


function TKMGame.GetMissionFile: UnicodeString;
begin
  if not IsMultiplayer then
    Result := fMissionFileSP //In SP we store it
  else
    //In MP we can't store it since it will be MapsMP or MapsDL on different clients
    Result := TKMapsCollection.GuessMPPath(fGameName, '.dat', fGameMapCRC);
end;


function TKMGame.GetScriptSoundFile(const aSound: AnsiString; aAudioFormat: TKMAudioFormat): UnicodeString;
var Ext: UnicodeString;
begin
  case aAudioFormat of
    af_Wav: Ext := WAV_FILE_EXT;
    af_Ogg: Ext := OGG_FILE_EXT;
  end;
  Result := ChangeFileExt(GetMissionFile, '.' + UnicodeString(aSound) + Ext)
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


function TKMGame.IsSpeedUpAllowed: Boolean;
begin
  Result := not IsMultiplayer or IsMPGameSpeedUpAllowed;
end;


function TKMGame.IsMPGameSpeedUpAllowed: Boolean;
begin
  Result := (fGameMode in [gmMulti, gmMultiSpectate])
        and (fNetworking.NetPlayers.GetNotDroppedCount = 1);
end;


// We often need to see if game is MP
function TKMGame.IsMultiplayer: Boolean;
begin
  Result := fGameMode in [gmMulti, gmMultiSpectate];
end;


function TKMGame.IsReplay: Boolean;
begin
  Result := fGameMode in [gmReplaySingle, gmReplayMulti];
end;


procedure TKMGame.ShowMessage(aKind: TKMMessageKind; aTextID: Integer; aLoc: TKMPoint; aHandIndex: TKMHandIndex);
begin
  //Once you have lost no messages can be received
  if gHands[aHandIndex].AI.WonOrLost = wol_Lost then Exit;

  //Store it in hand so it can be included in MP save file
  gHands[aHandIndex].MessageLog.Add(aKind, aTextID, aLoc);

  //Don't play sound in replays or spectator
  if (aHandIndex = gMySpectator.HandIndex) and (fGameMode in [gmSingle, gmCampaign, gmMulti]) then
    gSoundPlayer.Play(sfx_MessageNotice, 2);
end;


procedure TKMGame.ShowMessageLocal(aKind: TKMMessageKind; aText: UnicodeString; aLoc: TKMPoint);
begin
  fGamePlayInterface.MessageIssue(aKind, aText, aLoc);
end;


procedure TKMGame.ShowScriptError(const aMsg: UnicodeString);
begin
  fGamePlayInterface.MessageIssue(mkQuill, aMsg);
end;


procedure TKMGame.OverlayUpdate;
begin
  fGamePlayInterface.SetScriptedOverlay(fOverlayText[gMySpectator.HandIndex]);
  fGamePlayInterface.UpdateOverlayControls;
end;


procedure TKMGame.OverlaySet(const aText: UnicodeString; aPlayer: Shortint);
var
  I: Integer;
begin
  if aPlayer = PLAYER_NONE then
    for I := 0 to MAX_HANDS do
      fOverlayText[I] := aText
  else
    fOverlayText[aPlayer] := aText;

  OverlayUpdate;
end;


procedure TKMGame.OverlayAppend(const aText: UnicodeString; aPlayer: Shortint);
var
  I: Integer;
begin
  if aPlayer = PLAYER_NONE then
    for I := 0 to MAX_HANDS do
      fOverlayText[I] := fOverlayText[I] + aText
  else
    fOverlayText[aPlayer] := fOverlayText[aPlayer] + aText;

  OverlayUpdate;
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
  if (PeaceTicksRemaining = 1) and (fGameMode in [gmMulti, gmMultiSpectate, gmReplayMulti]) then
  begin
    gSoundPlayer.Play(sfxn_Peacetime, 1, True); //Fades music
    if fGameMode in [gmMulti, gmMultiSpectate] then
    begin
      SetGameSpeed(fGameOptions.SpeedAfterPT, False);
      fNetworking.PostLocalMessage(gResTexts[TX_MP_PEACETIME_OVER], csNone);
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
  // - allow -1 to indicate no UID (const UID_NONE = -1)
  // - fit within 24bit (we can use that much for RGB colorcoding in unit picking)
  // - Start from 1, so that black colorcode can be detected in render and then re-mapped to -1

  fUIDTracker := (fUIDTracker + step) mod max_value + 1; //1..N range, 0 is nothing for colorpicker
  Result := fUIDTracker;
end;


function TKMGame.GetNormalGameSpeed: Single;
begin
  if IsMultiplayer then
  begin
    if IsPeaceTime then
      Result := fGameOptions.SpeedPT
    else
      Result := fGameOptions.SpeedAfterPT;
  end else
    Result := 1;
end;


procedure TKMGame.SetGameSpeed(aSpeed: Single; aToggle: Boolean);
begin
  Assert(aSpeed > 0);

  //MapEd always runs at x1
  if IsMapEditor then
  begin
    fGameSpeed := 1;
    fGameSpeedMultiplier := 1;
    fTimerGame.Interval := Round(gGameApp.GameSettings.SpeedPace / fGameSpeed);
    Exit;
  end;

  //Make the speed toggle between normal speed and desired value
  if (aSpeed = fGameSpeed) and aToggle then
    fGameSpeed := GetNormalGameSpeed
  else
    fGameSpeed := aSpeed;

  //When speed is above x5 we start to skip rendering frames
  //by doing several updates per timer tick
  if fGameSpeed > 5 then
  begin
    fGameSpeedMultiplier := Round(fGameSpeed / 4);
    fTimerGame.Interval := Round(gGameApp.GameSettings.SpeedPace / fGameSpeed * fGameSpeedMultiplier);
  end
  else
  begin
    fGameSpeedMultiplier := 1;
    fTimerGame.Interval := Round(gGameApp.GameSettings.SpeedPace / fGameSpeed);
  end;

  //don't show speed clock in MP since you can't turn it on/off
  if (fGamePlayInterface <> nil) and IsSpeedUpAllowed then
    fGamePlayInterface.ShowClock(fGameSpeed);

  //Need to adjust the delay immediately in MP
  if IsMultiplayer and (fGameInputProcess <> nil) then
    TGameInputProcess_Multi(fGameInputProcess).AdjustDelay(fGameSpeed);

  if Assigned(gGameApp.OnGameSpeedChange) then
    gGameApp.OnGameSpeedChange(fGameSpeed);
end;


//In replay mode we can step the game by exactly one frame and then pause again
procedure TKMGame.StepOneFrame;
begin
  Assert(fGameMode in [gmReplaySingle,gmReplayMulti], 'We can work step-by-step only in Replay');
  SetGameSpeed(1, False); //Make sure we step only one tick. Do not allow multiple updates in UpdateState loop
  fAdvanceFrame := True;
end;


//Saves the game in all its glory
procedure TKMGame.SaveGame(const aPathName: UnicodeString; aTimestamp: TDateTime; const aMinimapPathName: UnicodeString = '');
var
  SaveStream, MnmSaveStream: TKMemoryStream;
  gameInfo: TKMGameInfo;
  I, netIndex: Integer;
begin
  gLog.AddTime('Saving game: ' + aPathName);

  if fGameMode in [gmMapEd, gmReplaySingle, gmReplayMulti] then
    raise Exception.Create('Saving from wrong state');

  SaveStream := TKMemoryStream.Create;
  try
    gameInfo := TKMGameInfo.Create;
    try
      gameInfo.Title := fGameName;
      gameInfo.MapCRC := fGameMapCRC;
      gameInfo.TickCount := fGameTickCount;
      gameInfo.SaveTimestamp := aTimestamp;
      gameInfo.MissionMode := fMissionMode;
      gameInfo.MapSizeX := gTerrain.MapX;
      gameInfo.MapSizeY := gTerrain.MapY;

      gameInfo.PlayerCount := gHands.Count;
      for I := 0 to gHands.Count - 1 do
      begin
        if fNetworking = nil then
        begin
          gameInfo.Enabled[I] := False;
          gameInfo.CanBeHuman[I] := False;
          gameInfo.OwnerNikname[I] := '';
          gameInfo.HandTypes[I] := hndHuman;
          gameInfo.ColorID[I] := 0;
          gameInfo.Team[I] := 0;
        end else
        begin
          netIndex := fNetworking.NetPlayers.PlayerIndexToLocal(I);
          if netIndex <> -1 then
          begin
            gameInfo.Enabled[I] := True;
            gameInfo.CanBeHuman[I] := fNetworking.NetPlayers[netIndex].IsHuman;
            gameInfo.OwnerNikname[I] := fNetworking.NetPlayers[netIndex].Nikname;
            gameInfo.HandTypes[I] := fNetworking.NetPlayers[netIndex].GetPlayerType;
            gameInfo.ColorID[I] := fNetworking.NetPlayers[netIndex].FlagColorID;
            gameInfo.Team[I] := fNetworking.NetPlayers[netIndex].Team;
          end
          else
          begin
            gameInfo.Enabled[I] := gHands[I].Enabled;
            gameInfo.CanBeHuman[I] := gHands[I].HandType = hndHuman;
            gameInfo.OwnerNikname[I] := gHands[I].OwnerNikname; //MP nikname, not translated OwnerName
            gameInfo.HandTypes[I] := gHands[I].HandType;
            gameInfo.ColorID[I] := FindMPColor(gHands[I].FlagColor);
            gameInfo.Team[I] := 0;
          end;
        end;
      end;

      gameInfo.Save(SaveStream);
    finally
      gameInfo.Free;
    end;

    fGameOptions.Save(SaveStream);

    //Because some stuff is only saved in singleplayer we need to know whether it is included in this save,
    //so we can load multiplayer saves in single player and vice versa.
    SaveStream.Write(IsMultiplayer);

    //In SinglePlayer we want to show player a preview of what the game looked like when he saved
    //Save Minimap is near the start so it can be accessed quickly
    //In MP each player has his own perspective, hence we dont save minimaps in the main save file to avoid cheating,
    //but save minimap in separate file with smm extension
    if not IsMultiplayer then
      fGamePlayInterface.SaveMinimap(SaveStream)
    else
      if aMinimapPathName <> '' then
      begin
        MnmSaveStream := TKMemoryStream.Create;
        try
          try
            fGamePlayInterface.SaveMinimap(MnmSaveStream);
            MnmSaveStream.SaveToFile(aMinimapPathName);
          except
            //Ignore any errors while saving minimap, because its optional for MP games
          end;
        finally
          MnmSaveStream.Free;
        end;
      end;

    //We need to know which campaign to display after victory
    SaveStream.Write(fCampaignName, SizeOf(TKMCampaignId));
    SaveStream.Write(fCampaignMap);

    //We need to know which mission/savegame to try to restart. This is unused in MP
    if not IsMultiplayer then
      SaveStream.WriteW(fMissionFileSP);

    SaveStream.Write(fUIDTracker); //Units-Houses ID tracker
    SaveStream.Write(GetKaMSeed); //Include the random seed in the save file to ensure consistency in replays

    if not IsMultiplayer then
      SaveStream.Write(PlayOnState, SizeOf(PlayOnState));

    gTerrain.Save(SaveStream); //Saves the map
    gHands.Save(SaveStream, fGameMode in [gmMulti, gmMultiSpectate]); //Saves all players properties individually
    if not IsMultiplayer then
      gMySpectator.Save(SaveStream);
    gAIFields.Save(SaveStream);
    fPathfinding.Save(SaveStream);
    gProjectiles.Save(SaveStream);
    fScripting.Save(SaveStream);
    gLoopSounds.Save(SaveStream);

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
  fullPath, minimapPath: UnicodeString;
  SaveInfo: TKMSaveInfo;
begin
  //Convert name to full path+name
  fullPath := SaveName(aSaveName, 'sav', IsMultiplayer);
  minimapPath := SaveName(aSaveName, MP_MINIMAP_SAVE_EXT, IsMultiplayer);

  SaveGame(fullPath, aTimestamp, minimapPath);

  if not IsMultiplayer then
  begin
    // Update GameSettings for saved positions in lists of saves and replays
    SaveInfo := TKMSaveInfo.Create(ExtractFilePath(fullPath), aSaveName);
    gGameApp.GameSettings.MenuSPSaveCRC := SaveInfo.CRC; // Update save position for SP game
    SaveInfo.Free;
  end;

  //Remember which savegame to try to restart (if game was not saved before)
  fSaveFile := ExtractRelativePath(ExeDir, fullPath);

  //Copy basesave so we have a starting point for replay
  DeleteFile(SaveName(aSaveName, 'bas', IsMultiplayer));
  KMCopyFile(SaveName('basesave', 'bas', IsMultiplayer), SaveName(aSaveName, 'bas', IsMultiplayer));

  //Save replay queue
  gLog.AddTime('Saving replay info');
  fGameInputProcess.SaveToFile(ChangeFileExt(fullPath, '.rpl'));

  gLog.AddTime('Saving game', True);
end;


procedure TKMGame.SaveCampaignScriptData(SaveStream: TKMemoryStream);
begin
  fScripting.SaveCampaignData(SaveStream);
end;


procedure TKMGame.Load(const aPathName: UnicodeString);
var
  LoadStream: TKMemoryStream;
  GameInfo: TKMGameInfo;
  LoadedSeed: LongInt;
  SaveIsMultiplayer, IsCampaign: Boolean;
  I: Integer;
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
      fGameMapCRC := GameInfo.MapCRC;
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
    if SaveIsMultiplayer and (fGameMode in [gmSingle, gmCampaign, gmReplaySingle, gmReplayMulti]) then
      if fMain.LockMutex then
        fGameLockedMutex := True //Remember so we unlock it in Destroy
      else
        //Abort loading (exception will be caught in gGameApp and shown to the user)
        raise Exception.Create(gResTexts[TX_MULTIPLE_INSTANCES]);

    //Not used, (only stored for SP preview) but it's easiest way to skip past it
    if not SaveIsMultiplayer then
      fGamePlayInterface.LoadMinimap(LoadStream);

    //We need to know which campaign to display after victory
    LoadStream.Read(fCampaignName, SizeOf(TKMCampaignId));
    LoadStream.Read(fCampaignMap);

    //Check if this save is Campaign game save
    IsCampaign := False;
    for I := Low(TKMCampaignId) to High(TKMCampaignId) do
      if fCampaignName[I] <> NO_CAMPAIGN[I] then
        IsCampaign := True;
    //If there is Campaign Name in save then change GameMode to gmCampaign, because GameMode is not stored in Save
    if IsCampaign then
      fGameMode := gmCampaign;

    //We need to know which mission/savegame to try to restart. This is unused in MP.
    if not SaveIsMultiplayer then
      LoadStream.ReadW(fMissionFileSP);

    LoadStream.Read(fUIDTracker);
    LoadStream.Read(LoadedSeed);

    if not SaveIsMultiplayer then
      LoadStream.Read(PlayOnState, SizeOf(PlayOnState));

    //Load the data into the game
    gTerrain.Load(LoadStream);

    gHands.Load(LoadStream);
    gMySpectator := TKMSpectator.Create(0);
    if not SaveIsMultiplayer then
      gMySpectator.Load(LoadStream);
    gAIFields.Load(LoadStream);
    fPathfinding.Load(LoadStream);
    gProjectiles.Load(LoadStream);
    fScripting.Load(LoadStream);
    gLoopSounds.Load(LoadStream);

    fTextMission := TKMTextLibraryMulti.Create;
    fTextMission.Load(LoadStream);

    if gGame.GameMode in [gmMultiSpectate, gmReplaySingle, gmReplayMulti] then
    begin
      gMySpectator.FOWIndex := PLAYER_NONE; //Show all by default in replays
      //HandIndex is the first enabled player
      gMySpectator.HandIndex := FindHandToSpec;
    end;

    //Multiplayer saves don't have this piece of information. Its valid only for MyPlayer
    //todo: Send all message commands through GIP (note: that means there will be a delay when you press delete)
    if not SaveIsMultiplayer then
      fGamePlayInterface.Load(LoadStream);

    if IsReplay then
      fGameInputProcess := TGameInputProcess_Single.Create(gipReplaying) //Replay
    else
      if fGameMode in [gmMulti, gmMultiSpectate] then
        fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking) //Multiplayer
      else
        fGameInputProcess := TGameInputProcess_Single.Create(gipRecording); //Singleplayer

    fGameInputProcess.LoadFromFile(ChangeFileExt(aPathName, '.rpl'));

     //Should check all Unit-House ID references and replace them with actual pointers
    gHands.SyncLoad;
    gTerrain.SyncLoad;
    gProjectiles.SyncLoad;

    SetKaMSeed(LoadedSeed); //Seed is used in MultiplayerRig when changing humans to AIs through GIP for replay

    if fGameMode in [gmMulti, gmMultiSpectate] then
      MultiplayerRig;

    if fGameMode in [gmSingle, gmCampaign, gmMulti, gmMultiSpectate] then
    begin
      DeleteFile(SaveName('basesave', 'bas', IsMultiplayer));
      KMCopyFile(ChangeFileExt(aPathName, '.bas'), SaveName('basesave', 'bas', IsMultiplayer));
    end;

    //Repeat mission init if necessary
    if fGameTickCount = 0 then
      gScriptEvents.ProcMissionStart;

    //When everything is ready we can update UI
    fActiveInterface.SyncUI;

    if SaveIsMultiplayer then
    begin
      //MP does not saves view position cos of save identity for all players
      fActiveInterface.SyncUIView(KMPointF(gMySpectator.Hand.CenterScreen));
      //In MP saves hotkeys can't be saved by UI, they must be network synced
      if fGameMode in [gmSingle, gmCampaign, gmMulti] then
        fGamePlayInterface.LoadHotkeysFromHand;
    end;

    gLog.AddTime('Loading game', True);
  finally
    FreeAndNil(LoadStream);
  end;
end;


procedure TKMGame.UpdateGame(Sender: TObject);
var
  I: Integer;
  PeaceTimeLeft: Cardinal;
begin
  //Some PCs seem to change 8087CW randomly between events like Timers and OnMouse*,
  //so we need to set it right before we do game logic processing
  Set8087CW($133F);

  if fIsPaused then Exit;

  case fGameMode of
    gmSingle, gmCampaign, gmMulti, gmMultiSpectate:
                  if not (fGameMode in [gmMulti, gmMultiSpectate]) or (fNetworking.NetGameState <> lgs_Loading) then
                  for I := 1 to fGameSpeedMultiplier do
                  begin
                    if fGameInputProcess.CommandsConfirmed(fGameTickCount+1) then
                    begin
                      if DO_PERF_LOGGING then fPerfLog.EnterSection(psTick);

                      //As soon as next command arrives we are longer in a waiting state
                      if fWaitingForNetwork then
                        WaitingPlayersDisplay(False);

                      Inc(fGameTickCount); //Thats our tick counter for gameplay events
                      if (fGameMode in [gmMulti, gmMultiSpectate]) then
                        fNetworking.LastProcessedTick := fGameTickCount;
                      //Tell the master server about our game on the specific tick (host only)
                      if (fGameMode in [gmMulti, gmMultiSpectate]) and fNetworking.IsHost
                      and (((fMissionMode = mm_Normal) and (fGameTickCount = ANNOUNCE_BUILD_MAP))
                      or ((fMissionMode = mm_Tactic) and (fGameTickCount = ANNOUNCE_BATTLE_MAP))) then
                        fNetworking.ServerQuery.SendMapInfo(fGameName, fGameMapCRC, fNetworking.NetPlayers.GetConnectedCount);

                      fScripting.UpdateState;
                      UpdatePeacetime; //Send warning messages about peacetime if required
                      gTerrain.UpdateState;
                      gAIFields.UpdateState(fGameTickCount);
                      gHands.UpdateState(fGameTickCount); //Quite slow
                      if gGame = nil then Exit; //Quit the update if game was stopped for some reason
                      gMySpectator.UpdateState(fGameTickCount);
                      fPathfinding.UpdateState;
                      gProjectiles.UpdateState; //If game has stopped it's NIL

                      fGameInputProcess.RunningTimer(fGameTickCount); //GIP_Multi issues all commands for this tick
                      //Returning to the lobby (through MP GIP) ends the game
                      if gGame = nil then Exit;

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
                          if gGameApp.GameSettings.Autosave then
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
                    gAIFields.UpdateState(fGameTickCount);
                    gHands.UpdateState(fGameTickCount); //Quite slow
                    if gGame = nil then Exit; //Quit the update if game was stopped for some reason
                    gMySpectator.UpdateState(fGameTickCount);
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
                    if DoGameHold then
                      Break;

                    if fGameOptions.Peacetime * 600 < fGameTickCount then
                      PeaceTimeLeft := 0
                    else
                      PeaceTimeLeft := fGameOptions.Peacetime * 600 - fGameTickCount;

                    if (PeaceTimeLeft = 1)
                    and (fGameMode = gmReplayMulti)
                    and (gGameApp.GameSettings.ReplayAutopause) then
                    begin
                      fIsPaused := True;
                      //Set replay UI to paused state, sync replay timer and other UI elements
                      fGamePlayInterface.SetButtons(False);
                      fGamePlayInterface.UpdateState(fGameTickCount);
                      //Break the for loop
                      Break;
                    end;

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
  gLoopSounds.UpdateState;

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
    Result := ExeDir + SAVES_MP_FOLDER_NAME + PathDelim + aName + '.' + aExt
  else
    Result := ExeDir + SAVES_FOLDER_NAME + PathDelim + aName + '.' + aExt;
end;


end.

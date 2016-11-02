unit KM_GameInputProcess;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Controls, KM_CommonClasses, KM_Defaults,
  KM_Houses, KM_Units, KM_UnitGroups, KM_Points,
  KM_ResHouses, KM_ResWares;

{ A. This unit takes and adjoins players input from TGame and TGamePlayInterfaces clicks and keys
  Then passes it on to game events.
  E.g. there are 2 ways player can place an order to selected Warrior:
  1. Click on map
  2. Click on minimap

  B. And most important, it accumulates and feeds player input to the game.
  Thus making possible to:
   - record gameplay
   - playback replays
   - send input through LAN to make multiplayer games

  This is a polymorphic unit which is only used as the parent of TGameInputProcess_Single for single
  player or TGameInputProcess_Multi for multiplayer
  It contains a few common methods such as replays as well as abstract methods for the child classes to handle.
  Most importantly it converts all Cmd____ methods called by TGamePlayInterfaces into one procedure
  ProcessCommandFromPlayer. Single and Multi then use this according to their needs.
  Replays are stored and managed here, hidden from the child classes by private. They add new replay
  commands with StoreCommand, and in gipReplaying state commands are executed on Tick
  }

const MAX_PARAMS = 4; //There are maximum of 4 integers passed along with a command

type
  TGIPReplayState = (gipRecording, gipReplaying);

  TGameInputCommandType = (
    gic_None,
    //I.      Army commands, only warriors (TKMUnitWarrior, OrderInfo)
    gic_ArmyFeed,
    gic_ArmySplit,
    gic_ArmySplitSingle,
    gic_ArmyLink,
    gic_ArmyAttackUnit,
    gic_ArmyAttackHouse,
    gic_ArmyHalt,
    gic_ArmyFormation,         //Formation commands
    gic_ArmyWalk,         //Walking
    gic_ArmyStorm,        //StormAttack

    //II.     Building/road plans (what to build and where)
    gic_BuildAddFieldPlan,
    gic_BuildRemoveFieldPlan,  //Removal of a plan
    gic_BuildRemoveHouse,     //Removal of house
    gic_BuildRemoveHousePlan,
    gic_BuildHousePlan,   //Build HouseType

    //III.    House repair/delivery/orders (TKMHouse, Toggle(repair, delivery, orders))
    gic_HouseRepairToggle,
    gic_HouseDeliveryToggle,      //Including storehouse. (On/Off, ResourceType)
    gic_HouseOrderProduct,        //Place an order to manufacture warfare
    gic_HouseMarketFrom,          //Select wares to trade in marketplace
    gic_HouseMarketTo,            //Select wares to trade in marketplace
    gic_HouseWoodcutterMode,      //Switch the woodcutter mode
    gic_HouseStoreAcceptFlag,     //Control wares delivery to store
    gic_HouseSchoolTrain,         //Place an order to train citizen
    gic_HouseSchoolTrainChOrder,  //Change school training order
    gic_HouseSchoolTrainChLastUOrder,  //Change school training order for last unit in queue
    gic_HouseBarracksAcceptFlag,  //Control wares delivery to barracks
    gic_HouseBarracksEquip,       //Place an order to train warrior
    gic_HouseBarracksRally,       //Set the rally point for the barracks
    gic_HouseRemoveTrain,         //Remove unit being trained from School    
    gic_HouseWoodcuttersCutting,  //Set the cutting point for the Woodcutters

    //IV.     Delivery ratios changes (and other game-global settings)
    gic_RatioChange,

    //V.      Game changes
    gic_GameAlertBeacon,            //Signal alert (beacon)
    gic_GamePause,
    gic_GameAutoSave,
    gic_GameSaveReturnLobby,
    gic_GameTeamChange,
    gic_GameHotkeySet,      //Hotkeys are synced for MP saves (UI keeps local copy to avoid GIP delays)
    gic_GameMessageLogRead, //Player marks a message in their log as read
    gic_GamePlayerTypeChange, //Players can be changed to AI when loading a save

    //VI.      Cheatcodes affecting gameplay (props)

    //VII. Temporary and debug commands
    gic_TempAddScout,
    gic_TempRevealMap, //Revealing the map can have an impact on the game. Events happen based on tiles being revealed
    gic_TempVictory,
    gic_TempDefeat,
    gic_TempDoNothing //Used for "aggressive" replays that store a command every tick

    { Optional input }
    //VI.     Viewport settings for replay (location, zoom)
    //VII.    Message queue handling in gameplay interface
    //IX.     Text messages for multiplayer (moved to Networking)
    );
const
  BlockedByPeaceTime: set of TGameInputCommandType = [gic_ArmySplit, gic_ArmySplitSingle,
    gic_ArmyLink, gic_ArmyAttackUnit, gic_ArmyAttackHouse, gic_ArmyHalt,
    gic_ArmyFormation,  gic_ArmyWalk, gic_ArmyStorm, gic_HouseBarracksEquip];
  AllowedAfterDefeat: set of TGameInputCommandType = [gic_GameAlertBeacon, gic_GameAutoSave, gic_GameSaveReturnLobby, gic_GameMessageLogRead, gic_TempDoNothing];
  AllowedInCinematic: set of TGameInputCommandType = [gic_GameAlertBeacon, gic_GameAutoSave, gic_GameSaveReturnLobby, gic_GameMessageLogRead, gic_TempDoNothing];
  AllowedBySpectators: set of TGameInputCommandType = [gic_GameAlertBeacon, gic_GameAutoSave, gic_GameSaveReturnLobby, gic_TempDoNothing];

type
  TGameInputCommand = record
    CommandType: TGameInputCommandType;
    Params: array[1..MAX_PARAMS]of integer;
    TextParam: UnicodeString;
    DateTimeParam: TDateTime;
    HandIndex: TKMHandIndex; //Player for which the command is to be issued. (Needed for multiplayer and other reasons)
  end;

  //As TGameInputCommand is no longer fixed size (due to the string) we cannot simply read/write it as a block
  procedure SaveCommandToMemoryStream(aCommand: TGameInputCommand; aMemoryStream: TKMemoryStream);
  procedure LoadCommandFromMemoryStream(out aCommand: TGameInputCommand; aMemoryStream: TKMemoryStream);

type
  TGameInputProcess = class
  private
    fCount: Integer;
    fReplayState: TGIPReplayState;
  protected
    fCursor: Integer; //Used only in gipReplaying
    fQueue: array of packed record
      Tick: Cardinal;
      Command: TGameInputCommand;
      Rand: Cardinal; //acts as CRC check
    end;

    function MakeCommand(aGIC: TGameInputCommandType; const aParam: array of integer): TGameInputCommand; overload;
    function MakeCommand(aGIC: TGameInputCommandType; const aTextParam: UnicodeString; aDateTimeParam: TDateTime): TGameInputCommand; overload;
    procedure TakeCommand(aCommand: TGameInputCommand); virtual; abstract;
    procedure ExecCommand(aCommand: TGameInputCommand);
    procedure StoreCommand(aCommand: TGameInputCommand);
  public
    constructor Create(aReplayState: TGIPReplayState);
    destructor Destroy; override;

    procedure CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup); overload;
    procedure CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aUnit: TKMUnit); overload;
    procedure CmdArmy(aCommandType: TGameInputCommandType; aGroup1, aGroup2: TKMUnitGroup); overload;
    procedure CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aHouse: TKMHouse); overload;
    procedure CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aTurnAmount: TKMTurnDirection; aLineAmount:shortint); overload;
    procedure CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aLoc: TKMPoint; aDirection: TKMDirection); overload;

    procedure CmdBuild(aCommandType: TGameInputCommandType; aLoc: TKMPoint); overload;
    procedure CmdBuild(aCommandType: TGameInputCommandType; aLoc: TKMPoint; aFieldType: TFieldType); overload;
    procedure CmdBuild(aCommandType: TGameInputCommandType; aLoc: TKMPoint; aHouseType: THouseType); overload;

    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem, aAmountChange: Integer); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem: TWareType); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aWoodcutterMode: TWoodcutterMode); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aUnitType: TUnitType; aCount:byte); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem: Integer); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aLoc: TKMPoint); overload;

    procedure CmdRatio(aCommandType: TGameInputCommandType; aWare: TWareType; aHouseType: THouseType; aValue:integer);

    procedure CmdGame(aCommandType: TGameInputCommandType; aValue:boolean); overload;
    procedure CmdGame(aCommandType: TGameInputCommandType; aDateTime: TDateTime); overload;
    procedure CmdGame(aCommandType: TGameInputCommandType; aParam1, aParam2: Integer); overload;
    procedure CmdGame(aCommandType: TGameInputCommandType; aLoc: TKMPointF; aOwner: TKMHandIndex; aColor: Cardinal); overload;
    procedure CmdGame(aCommandType: TGameInputCommandType; aValue: Integer); overload;

    procedure CmdTemp(aCommandType: TGameInputCommandType; aLoc: TKMPoint); overload;
    procedure CmdTemp(aCommandType: TGameInputCommandType); overload;

    function CommandsConfirmed(aTick:cardinal):boolean; virtual;
    procedure WaitingForConfirmation(aTick:cardinal); virtual;
    procedure ReplayTimer(aTick:cardinal); virtual;
    procedure RunningTimer(aTick:cardinal); virtual;
    procedure UpdateState(aTick:cardinal); virtual;

    //Replay methods
    procedure SaveToFile(aFileName: UnicodeString);
    procedure LoadFromFile(aFileName: UnicodeString);
    property Count:integer read fCount;
    property ReplayState: TGIPReplayState read fReplayState;
    function GetLastTick:Cardinal;
    function ReplayEnded:boolean;
  end;


implementation
uses
  KM_Game, KM_HouseMarket, KM_HandsCollection, KM_Hand, KM_ResTexts, KM_Utils, KM_AI,
  KM_HouseBarracks, KM_HouseSchool, KM_Alerts, KM_GameApp, KM_Networking, KM_ScriptingEvents;


procedure SaveCommandToMemoryStream(aCommand: TGameInputCommand; aMemoryStream: TKMemoryStream);
begin
  with aCommand do
  begin
    aMemoryStream.Write(CommandType, SizeOf(CommandType));
    aMemoryStream.Write(Params, SizeOf(Params));
    aMemoryStream.WriteW(TextParam);
    aMemoryStream.Write(DateTimeParam);
    aMemoryStream.Write(HandIndex);
  end;
end;


procedure LoadCommandFromMemoryStream(out aCommand: TGameInputCommand; aMemoryStream: TKMemoryStream);
begin
  with aCommand do
  begin
    aMemoryStream.Read(CommandType, SizeOf(CommandType));
    aMemoryStream.Read(Params, SizeOf(Params));
    aMemoryStream.ReadW(TextParam);
    aMemoryStream.Read(DateTimeParam);
    aMemoryStream.Read(HandIndex);
  end;
end;


{ TGameInputProcess }
constructor TGameInputProcess.Create(aReplayState: TGIPReplayState);
begin
  inherited Create;
  setlength(fQueue, 128);
  fCount := 0;
  fCursor := 1;
  fReplayState := aReplayState;
end;


destructor TGameInputProcess.Destroy;
begin
  inherited;
end;


function TGameInputProcess.MakeCommand(aGIC: TGameInputCommandType; const aParam: array of integer): TGameInputCommand;
var
  I: Integer;
begin
  Result.CommandType := aGIC;
  Result.HandIndex := gMySpectator.HandIndex;

  for I := Low(aParam) to High(aParam) do
    Result.Params[I+1] := aParam[I];
  for I := High(aParam) + 1 to High(Result.Params) - 1 do
    Result.Params[I+1] := MaxInt;

  Result.TextParam := '';
  Result.DateTimeParam := 0;
end;


function TGameInputProcess.MakeCommand(aGIC: TGameInputCommandType; const aTextParam: UnicodeString; aDateTimeParam: TDateTime): TGameInputCommand;
var
  I: Integer;
begin
  Result.CommandType := aGIC;
  Result.HandIndex := gMySpectator.HandIndex;

  for I := Low(Result.Params) to High(Result.Params) do
    Result.Params[I] := MaxInt;

  Result.TextParam := aTextParam;
  Result.DateTimeParam := aDateTimeParam;
end;


procedure TGameInputProcess.ExecCommand(aCommand: TGameInputCommand);
var
  P: TKMHand;
  IsSilent: boolean;
  SrcGroup, TgtGroup: TKMUnitGroup;
  TgtUnit: TKMUnit;
  SrcHouse, TgtHouse: TKMHouse;
begin
  //NOTE: gMySpectator.PlayerIndex should not be used for important stuff here, use P instead (commands must be executed the same for all players)
  IsSilent := (aCommand.HandIndex <> gMySpectator.HandIndex);
  P := gHands[aCommand.HandIndex];
  SrcGroup := nil;
  TgtGroup := nil;
  SrcHouse := nil;
  TgtHouse := nil;
  TgtUnit := nil;

  with aCommand do
  begin
    //It is possible that units/houses have died by now
    if CommandType in [gic_ArmyFeed, gic_ArmySplit, gic_ArmySplitSingle, gic_ArmyLink,
                       gic_ArmyAttackUnit, gic_ArmyAttackHouse, gic_ArmyHalt,
                       gic_ArmyFormation, gic_ArmyWalk, gic_ArmyStorm]
    then
    begin
      SrcGroup := gHands.GetGroupByUID(Params[1]);
      if (SrcGroup = nil) or SrcGroup.IsDead //Group has died before command could be executed
      or (SrcGroup.Owner <> aCommand.HandIndex) then //Potential exploit
        Exit;
    end;
    if CommandType in [gic_ArmyLink] then
    begin
      TgtGroup := gHands.GetGroupByUID(Params[2]);
      if (TgtGroup = nil) or TgtGroup.IsDead //Unit has died before command could be executed
      or (TgtGroup.Owner <> aCommand.HandIndex) then //Potential exploit
        Exit;
    end;
    if CommandType in [gic_ArmyAttackUnit] then
    begin
      TgtUnit := gHands.GetUnitByUID(Params[2]);
      if (TgtUnit = nil) or TgtUnit.IsDeadOrDying then //Unit has died before command could be executed
        Exit;
    end;
    if CommandType in [gic_HouseRepairToggle, gic_HouseDeliveryToggle, gic_HouseWoodcuttersCutting,
      gic_HouseOrderProduct, gic_HouseMarketFrom, gic_HouseMarketTo, gic_HouseBarracksRally,
      gic_HouseStoreAcceptFlag, gic_HouseBarracksAcceptFlag, gic_HouseBarracksEquip,
      gic_HouseSchoolTrain, gic_HouseSchoolTrainChOrder, gic_HouseSchoolTrainChLastUOrder, gic_HouseRemoveTrain,
      gic_HouseWoodcutterMode] then
    begin
      SrcHouse := gHands.GetHouseByUID(Params[1]);
      if (SrcHouse = nil) or SrcHouse.IsDestroyed //House has been destroyed before command could be executed
      or (SrcHouse.Owner <> aCommand.HandIndex) then //Potential exploit
        Exit;
    end;
    if CommandType in [gic_ArmyAttackHouse] then
    begin
      TgtHouse := gHands.GetHouseByUID(Params[2]);
      if (TgtHouse = nil) or TgtHouse.IsDestroyed then Exit; //House has been destroyed before command could be executed
    end;

    //Some commands are blocked by peacetime (this is a fall back in case players try to cheat)
    if gGame.IsPeaceTime and (CommandType in BlockedByPeaceTime) then
       Exit;

    //No commands allowed after a player has lost (this is a fall back in case players try to cheat)
    if not (aCommand.CommandType in AllowedAfterDefeat) and gGame.IsMultiplayer and (P.AI.WonOrLost = wol_Lost) then
      Exit;

    //Most commands blocked during cinematic (this is a fall back in case players try to cheat)
    if not (aCommand.CommandType in AllowedInCinematic) and (P.InCinematic) then
      Exit;

    case CommandType of
      gic_ArmyFeed:         SrcGroup.OrderFood(True);
      gic_ArmySplit:        SrcGroup.OrderSplit(True);
      gic_ArmySplitSingle:  SrcGroup.OrderSplit(True, True);
      gic_ArmyStorm:        SrcGroup.OrderStorm(True);
      gic_ArmyLink:         SrcGroup.OrderLinkTo(TgtGroup, True);
      gic_ArmyAttackUnit:   SrcGroup.OrderAttackUnit(TgtUnit, True);
      gic_ArmyAttackHouse:  SrcGroup.OrderAttackHouse(TgtHouse, True);
      gic_ArmyHalt:         SrcGroup.OrderHalt(True);
      gic_ArmyFormation:    SrcGroup.OrderFormation(TKMTurnDirection(Params[2]),Params[3], True);
      gic_ArmyWalk:         SrcGroup.OrderWalk(KMPoint(Params[2],Params[3]), True, TKMDirection(Params[4]));

      gic_BuildAddFieldPlan:      P.ToggleFieldPlan(KMPoint(Params[1],Params[2]), TFieldType(Params[3]), not gGame.IsMultiplayer); //Make sound in singleplayer mode only
      gic_BuildRemoveFieldPlan:   P.RemFieldPlan(KMPoint(Params[1],Params[2]), not gGame.IsMultiplayer); //Make sound in singleplayer mode only
      gic_BuildRemoveHouse:       P.RemHouse(KMPoint(Params[1],Params[2]), IsSilent);
      gic_BuildRemoveHousePlan:   P.RemHousePlan(KMPoint(Params[1],Params[2]));
      gic_BuildHousePlan:         if P.CanAddHousePlan(KMPoint(Params[2],Params[3]), THouseType(Params[1])) then
                                    P.AddHousePlan(THouseType(Params[1]), KMPoint(Params[2],Params[3]));

      gic_HouseRepairToggle:      SrcHouse.BuildingRepair := not SrcHouse.BuildingRepair;
      gic_HouseDeliveryToggle:    SrcHouse.WareDelivery := not SrcHouse.WareDelivery;
      gic_HouseOrderProduct:      SrcHouse.ResOrder[Params[2]] := SrcHouse.ResOrder[Params[2]] + Params[3];
      gic_HouseMarketFrom:        TKMHouseMarket(SrcHouse).ResFrom := TWareType(Params[2]);
      gic_HouseMarketTo:          TKMHouseMarket(SrcHouse).ResTo := TWareType(Params[2]);
      gic_HouseStoreAcceptFlag:   TKMHouseStore(SrcHouse).ToggleAcceptFlag(TWareType(Params[2]));
      gic_HouseWoodcutterMode:    TKMHouseWoodcutters(SrcHouse).WoodcutterMode := TWoodcutterMode(Params[2]);
      gic_HouseBarracksAcceptFlag:TKMHouseBarracks(SrcHouse).ToggleAcceptFlag(TWareType(Params[2]));
      gic_HouseBarracksEquip:     TKMHouseBarracks(SrcHouse).Equip(TUnitType(Params[2]), Params[3]);
      gic_HouseBarracksRally:     TKMHouseBarracks(SrcHouse).RallyPoint := KMPoint(Params[2], Params[3]);
      gic_HouseSchoolTrain:       TKMHouseSchool(SrcHouse).AddUnitToQueue(TUnitType(Params[2]), Params[3]);
      gic_HouseSchoolTrainChOrder:TKMHouseSchool(SrcHouse).ChangeUnitTrainOrder(Params[2], Params[3]);
      gic_HouseSchoolTrainChLastUOrder: TKMHouseSchool(SrcHouse).ChangeUnitTrainOrder(Params[2]);
      gic_HouseRemoveTrain:       TKMHouseSchool(SrcHouse).RemUnitFromQueue(Params[2]);
      gic_HouseWoodcuttersCutting: TKMHouseWoodcutters(SrcHouse).CuttingPoint := KMPoint(Params[2], Params[3]);

      gic_RatioChange:            begin
                                    P.Stats.Ratio[TWareType(Params[1]), THouseType(Params[2])] := Params[3];
                                    P.Houses.UpdateResRequest
                                  end;

      gic_TempAddScout:           if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not gGame.IsMultiplayer) then
                                    //Place a warrior
                                    P.AddUnit(ut_HorseScout, KMPoint(Params[1], Params[2]), True, 0, True);
      gic_TempRevealMap:          if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not gGame.IsMultiplayer) then
                                    P.FogOfWar.RevealEverything;
      gic_TempVictory:            if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not gGame.IsMultiplayer) then
                                    P.AI.Victory;
      gic_TempDefeat:             if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not gGame.IsMultiplayer) then
                                    P.AI.Defeat;
      gic_TempDoNothing:          ;

      gic_GamePause:              ;//if fReplayState = gipRecording then fGame.fGamePlayInterface.SetPause(boolean(Params[1]));
      gic_GameAutoSave:           if (fReplayState = gipRecording) and gGameApp.GameSettings.Autosave then
                                    gGame.AutoSave(DateTimeParam); //Timestamp is synchronised
      gic_GameSaveReturnLobby:    if fReplayState = gipRecording then
                                  begin
                                    gGameApp.PrepareReturnToLobby(DateTimeParam); //Timestamp is synchronised
                                    Exit;
                                  end;
      gic_GameTeamChange:         begin
                                    //Currently unused, disabled to prevent potential exploitation
                                    {fGame.Networking.NetPlayers[Params[1]].Team := Params[2];
                                    fGame.UpdateMultiplayerTeams;
                                    fPlayers.SyncFogOfWar;
                                    if fGame.Networking.IsHost then
                                      fGame.Networking.SendPlayerListAndRefreshPlayersSetup;}
                                  end;
      gic_GameAlertBeacon:        begin
                                    //Beacon script event must always be run by all players for consistency
                                    gScriptEvents.ProcBeacon(Params[3], 1 + (Params[1] div 10), 1 + (Params[2] div 10));
                                    //However, beacons don't show in replays
                                    if fReplayState = gipRecording then
                                      if ((Params[3] = PLAYER_NONE) and (gGame.GameMode = gmMultiSpectate))  // PLAYER_NONE means it is for spectators
                                      or ((Params[3] <> PLAYER_NONE) and (gGame.GameMode <> gmMultiSpectate) // Spectators shouldn't see player beacons
                                      and (gHands.CheckAlliance(Params[3], gMySpectator.HandIndex) = at_Ally)
                                      and (gHands[Params[3]].ShareBeacons[gMySpectator.HandIndex])) then
                                        gGame.GamePlayInterface.Alerts.AddBeacon(KMPointF(Params[1]/10,Params[2]/10), Params[3], (Params[4] or $FF000000), gGameApp.GlobalTickCount + ALERT_DURATION[atBeacon]);
                                  end;
      gic_GameHotkeySet:          P.SelectionHotkeys[Params[1]] := Params[2];
      gic_GameMessageLogRead:     P.MessageLog[Params[1]].IsReadGIP := True;
      gic_GamePlayerTypeChange:   begin
                                    Assert(fReplayState <> gipRecording); //Should only occur in replays
                                    gHands[Params[1]].HandType := THandType(Params[2]);
                                  end;
      else                        Assert(false);
    end;
  end;
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup);
begin
  Assert(aCommandType in [gic_ArmyFeed, gic_ArmySplit, gic_ArmySplitSingle, gic_ArmyStorm, gic_ArmyHalt]);
  TakeCommand(MakeCommand(aCommandType, [aGroup.UID]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aUnit: TKMUnit);
begin
  Assert(aCommandType in [gic_ArmyAttackUnit]);
  TakeCommand(MakeCommand(aCommandType, [aGroup.UID, aUnit.UID]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup1, aGroup2: TKMUnitGroup);
begin
  Assert(aCommandType in [gic_ArmyLink]);
  TakeCommand(MakeCommand(aCommandType, [aGroup1.UID, aGroup2.UID]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aHouse: TKMHouse);
begin
  Assert(aCommandType = gic_ArmyAttackHouse);
  TakeCommand(MakeCommand(aCommandType, [aGroup.UID, aHouse.UID]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aTurnAmount: TKMTurnDirection; aLineAmount:shortint);
begin
  Assert(aCommandType = gic_ArmyFormation);
  TakeCommand(MakeCommand(aCommandType, [aGroup.UID, byte(aTurnAmount), aLineAmount]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aLoc: TKMPoint; aDirection: TKMDirection);
begin
  Assert(aCommandType = gic_ArmyWalk);
  TakeCommand(MakeCommand(aCommandType, [aGroup.UID, aLoc.X, aLoc.Y, byte(aDirection)]));
end;


procedure TGameInputProcess.CmdBuild(aCommandType: TGameInputCommandType; aLoc: TKMPoint);
begin
  Assert(aCommandType in [gic_BuildRemoveFieldPlan, gic_BuildRemoveHouse, gic_BuildRemoveHousePlan]);

  //Remove fake markup that will be visible only to gMySpectator until Server verifies it.
  //Must go before TakeCommand as it could execute command immediately (in singleplayer)
  //and the fake markup must be added first otherwise our logic in FieldsList fails
  if gGame.IsMultiplayer and (aCommandType = gic_BuildRemoveFieldPlan) then
    gMySpectator.Hand.RemFakeFieldPlan(aLoc);

  TakeCommand(MakeCommand(aCommandType, [aLoc.X, aLoc.Y]));
end;


procedure TGameInputProcess.CmdBuild(aCommandType: TGameInputCommandType; aLoc: TKMPoint; aFieldType: TFieldType);
begin
  Assert(aCommandType in [gic_BuildAddFieldPlan]);

  //Add fake markup that will be visible only to gMySpectator until Server verifies it.
  //Must go before TakeCommand as it could execute command immediately (in singleplayer)
  //and the fake markup must be added first otherwise our logic in FieldsList fails
  if gGame.IsMultiplayer then
    gMySpectator.Hand.ToggleFakeFieldPlan(aLoc, aFieldType);

  TakeCommand(MakeCommand(aCommandType, [aLoc.X, aLoc.Y, Byte(aFieldType)]));
end;


procedure TGameInputProcess.CmdBuild(aCommandType: TGameInputCommandType; aLoc: TKMPoint; aHouseType: THouseType);
begin
  Assert(aCommandType = gic_BuildHousePlan);
  TakeCommand(MakeCommand(aCommandType, [byte(aHouseType), aLoc.X, aLoc.Y]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse);
begin
  Assert(aCommandType in [gic_HouseRepairToggle, gic_HouseDeliveryToggle]);
  TakeCommand(MakeCommand(aCommandType, aHouse.UID));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem, aAmountChange: Integer);
begin
  Assert(aCommandType in [gic_HouseOrderProduct, gic_HouseSchoolTrainChOrder]);
  TakeCommand(MakeCommand(aCommandType, [aHouse.UID, aItem, aAmountChange]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem: TWareType);
begin
  Assert(aCommandType in [gic_HouseStoreAcceptFlag, gic_HouseBarracksAcceptFlag, gic_HouseMarketFrom, gic_HouseMarketTo]);
  TakeCommand(MakeCommand(aCommandType, [aHouse.UID, byte(aItem)]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aWoodcutterMode: TWoodcutterMode);
begin
  Assert(aCommandType = gic_HouseWoodcutterMode);
  TakeCommand(MakeCommand(aCommandType, [aHouse.UID, byte(aWoodcutterMode)]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aUnitType: TUnitType; aCount: Byte);
begin
  Assert(aCommandType in [gic_HouseSchoolTrain, gic_HouseBarracksEquip]);
  TakeCommand(MakeCommand(aCommandType, [aHouse.UID, byte(aUnitType), aCount]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem:integer);
begin
  Assert(aCommandType in [gic_HouseRemoveTrain, gic_HouseSchoolTrainChLastUOrder]);
  Assert(aHouse is TKMHouseSchool);
  TakeCommand(MakeCommand(aCommandType, [aHouse.UID, aItem]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aLoc: TKMPoint);
begin
  Assert((aCommandType = gic_HouseBarracksRally) or (aCommandType = gic_HouseWoodcuttersCutting));
  Assert((aHouse is TKMHouseBarracks) or (aHouse is TKMHouseWoodcutters));
  TakeCommand(MakeCommand(aCommandType, [aHouse.UID, aLoc.X, aLoc.Y]));
end;


procedure TGameInputProcess.CmdRatio(aCommandType: TGameInputCommandType; aWare: TWareType; aHouseType: THouseType; aValue:integer);
begin
  Assert(aCommandType = gic_RatioChange);
  TakeCommand(MakeCommand(aCommandType, [byte(aWare), byte(aHouseType), aValue]));
end;


procedure TGameInputProcess.CmdGame(aCommandType: TGameInputCommandType; aValue: Boolean);
begin
  Assert(aCommandType = gic_GamePause);
  TakeCommand(MakeCommand(aCommandType, [integer(aValue)]));
end;


procedure TGameInputProcess.CmdGame(aCommandType: TGameInputCommandType; aDateTime: TDateTime);
begin
  Assert(aCommandType in [gic_GameAutoSave, gic_GameSaveReturnLobby]);
  TakeCommand(MakeCommand(aCommandType, '', aDateTime));
end;


procedure TGameInputProcess.CmdGame(aCommandType: TGameInputCommandType; aParam1, aParam2: Integer);
begin
  Assert(aCommandType in [gic_GameTeamChange, gic_GameHotkeySet]);
  TakeCommand(MakeCommand(aCommandType, [aParam1, aParam2]));
end;


procedure TGameInputProcess.CmdGame(aCommandType: TGameInputCommandType; aValue: Integer);
begin
  Assert(aCommandType in [gic_GameMessageLogRead]);
  TakeCommand(MakeCommand(aCommandType, [aValue]));
end;


procedure TGameInputProcess.CmdGame(aCommandType: TGameInputCommandType; aLoc: TKMPointF; aOwner: TKMHandIndex; aColor: Cardinal);
begin
  Assert(aCommandType = gic_GameAlertBeacon);
  TakeCommand(MakeCommand(aCommandType, [Round(aLoc.X * 10), Round(aLoc.Y * 10), aOwner, (aColor and $FFFFFF)]));
end;


procedure TGameInputProcess.CmdTemp(aCommandType: TGameInputCommandType; aLoc: TKMPoint);
begin
  Assert(aCommandType = gic_TempAddScout);
  TakeCommand(MakeCommand(aCommandType, [aLoc.X, aLoc.Y]));
end;


procedure TGameInputProcess.CmdTemp(aCommandType: TGameInputCommandType);
begin
  Assert(aCommandType in [gic_TempRevealMap, gic_TempVictory, gic_TempDefeat, gic_TempDoNothing]);
  TakeCommand(MakeCommand(aCommandType, []));
end;


procedure TGameInputProcess.SaveToFile(aFileName: UnicodeString);
var
  I: integer;
  S: TKMemoryStream;
begin
  S := TKMemoryStream.Create;
  S.WriteA(GAME_REVISION);
  S.Write(fCount);
  for I := 1 to fCount do
  begin
    S.Write(fQueue[I].Tick);
    SaveCommandToMemoryStream(fQueue[I].Command, S);
    S.Write(fQueue[I].Rand);
  end;

  S.SaveToFile(aFileName);
  S.Free;
end;


procedure TGameInputProcess.LoadFromFile(aFileName: UnicodeString);
var
  FileVersion: AnsiString;
  I: Integer;
  S: TKMemoryStream;
begin
  if not FileExists(aFileName) then exit;
  S := TKMemoryStream.Create;
  S.LoadFromFile(aFileName);
  S.ReadA(FileVersion);
  Assert(FileVersion = GAME_REVISION, 'Old or unexpected replay file. '+GAME_REVISION+' is required.');
  S.Read(fCount);
  setlength(fQueue, fCount+1);
  for I := 1 to fCount do
  begin
    S.Read(fQueue[I].Tick);
    LoadCommandFromMemoryStream(fQueue[I].Command, S);
    S.Read(fQueue[I].Rand);
  end;

  S.Free;
end;


{ Return last recorded tick }
function TGameInputProcess.GetLastTick:Cardinal;
begin
  Result := fQueue[fCount].Tick;
end;


{ See if replay has ended (no more commands in queue) }
function TGameInputProcess.ReplayEnded:boolean;
begin
  if ReplayState = gipReplaying then
    Result := fCursor > fCount
  else
    Result := false;
end;


//Store commands for the replay
//While in replay there are no commands to process, but for debug we might allow ChangePlayer
procedure TGameInputProcess.StoreCommand(aCommand: TGameInputCommand);
begin
  if ReplayState = gipReplaying then
    Exit;

  Assert(ReplayState = gipRecording);
  inc(fCount);
  if Length(fQueue) <= fCount then SetLength(fQueue, fCount+128);

  fQueue[fCount].Tick    := gGame.GameTickCount;
  fQueue[fCount].Command := aCommand;
  fQueue[fCount].Rand    := Cardinal(KaMRandom(maxint)); //This will be our check to ensure everything is consistent
end;


function TGameInputProcess.CommandsConfirmed(aTick:cardinal):boolean;
begin
  Result := true;
end;


procedure TGameInputProcess.WaitingForConfirmation(aTick:cardinal);
begin
end;


procedure TGameInputProcess.ReplayTimer(aTick:cardinal);
begin
end;


procedure TGameInputProcess.RunningTimer(aTick:cardinal);
begin
end;


procedure TGameInputProcess.UpdateState(aTick:cardinal);
begin
  //Only used in GIP_Multi
end;


end.

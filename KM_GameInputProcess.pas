unit KM_GameInputProcess;
{$I KaM_Remake.inc}
interface
uses SysUtils, Controls, KM_CommonClasses, KM_Defaults,
    KM_Houses, KM_Units, KM_UnitGroups, KM_Points;

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
    gic_HouseBarracksAcceptFlag,  //Control wares delivery to barracks
    gic_HouseBarracksEquip,       //Place an order to train warrior
    gic_HouseRemoveTrain,         //Remove unit being trained from School

    //IV.     Delivery ratios changes (and other game-global settings)
    gic_RatioChange,

    //V.      Game changes
    gic_GameAlertBeacon,            //Signal alert (beacon)
    gic_GamePause,
    gic_GameSave,
    gic_GameTeamChange,

    //VI.      Cheatcodes affecting gameplay (props)

    //VII. Temporary and debug commands
    gic_TempAddScout,
    gic_TempRevealMap, //Revealing the map can have an impact on the game. Events happen based on tiles being revealed
    gic_TempDoNothing //Used for "aggressive" replays that store a command every tick

    { Optional input }
    //VI.     Viewport settings for replay (location, zoom)
    //VII.    Message queue handling in gameplay interface
    //IX.     Text messages for multiplayer (moved to Networking)
    );
const
  BlockedByPeaceTime: set of TGameInputCommandType = [gic_ArmySplit, gic_ArmyLink,
    gic_ArmyAttackUnit, gic_ArmyAttackHouse, gic_ArmyHalt, gic_ArmyFormation,
    gic_ArmyWalk, gic_ArmyStorm, gic_HouseBarracksEquip];
  AllowedAfterDefeat: set of TGameInputCommandType = [gic_GameAlertBeacon, gic_GameSave];

type
  TGameInputCommand = record
    CommandType: TGameInputCommandType;
    Params:array[1..MAX_PARAMS]of integer;
    TextParam: AnsiString;
    PlayerIndex: TPlayerIndex; //Player for which the command is to be issued. (Needed for multiplayer and other reasons)
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

    function MakeCommand(aGIC: TGameInputCommandType; const aParam:array of integer): TGameInputCommand; overload;
    function MakeCommand(aGIC: TGameInputCommandType; const aTextParam: AnsiString): TGameInputCommand; overload;
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
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem, aAmount:integer); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem: TResourceType); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aWoodcutterMode: TWoodcutterMode); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aUnitType: TUnitType; aCount:byte); overload;
    procedure CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem:integer); overload;

    procedure CmdRatio(aCommandType: TGameInputCommandType; aRes: TResourceType; aHouseType: THouseType; aValue:integer);

    procedure CmdGame(aCommandType: TGameInputCommandType; aValue:boolean); overload;
    procedure CmdGame(aCommandType: TGameInputCommandType; aValue: AnsiString); overload;
    procedure CmdGame(aCommandType: TGameInputCommandType; aPlayer, aTeam:integer); overload;
    procedure CmdGame(aCommandType: TGameInputCommandType; aLoc: TKMPointF; aPlayer: TPlayerIndex); overload;

    procedure CmdTemp(aCommandType: TGameInputCommandType; aLoc: TKMPoint); overload;
    procedure CmdTemp(aCommandType: TGameInputCommandType); overload;

    function CommandsConfirmed(aTick:cardinal):boolean; virtual;
    procedure WaitingForConfirmation(aTick:cardinal); virtual;
    procedure ReplayTimer(aTick:cardinal); virtual;
    procedure RunningTimer(aTick:cardinal); virtual;
    procedure UpdateState(aTick:cardinal); virtual;

    //Replay methods
    procedure SaveToFile(aFileName:string);
    procedure LoadFromFile(aFileName:string);
    property Count:integer read fCount;
    property ReplayState: TGIPReplayState read fReplayState;
    function GetLastTick:Cardinal;
    function ReplayEnded:boolean;
  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_Player, KM_TextLibrary, KM_Utils, KM_AI;


procedure SaveCommandToMemoryStream(aCommand: TGameInputCommand; aMemoryStream: TKMemoryStream);
begin
  with aCommand do
  begin
    aMemoryStream.Write(CommandType, SizeOf(CommandType));
    aMemoryStream.Write(Params, SizeOf(Params));
    aMemoryStream.Write(TextParam);
    aMemoryStream.Write(PlayerIndex);
  end;
end;


procedure LoadCommandFromMemoryStream(out aCommand: TGameInputCommand; aMemoryStream: TKMemoryStream);
begin
  with aCommand do
  begin
    aMemoryStream.Read(CommandType, SizeOf(CommandType));
    aMemoryStream.Read(Params, SizeOf(Params));
    aMemoryStream.Read(TextParam);
    aMemoryStream.Read(PlayerIndex);
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
  Result.PlayerIndex := MyPlayer.PlayerIndex;

  for I := Low(aParam) to High(aParam) do
    Result.Params[I+1] := aParam[I];
  for I := High(aParam) + 1 to High(Result.Params) - 1 do
    Result.Params[I+1] := MaxInt;

  Result.TextParam := '';
end;


function TGameInputProcess.MakeCommand(aGIC: TGameInputCommandType; const aTextParam: AnsiString): TGameInputCommand;
var i:integer;
begin
  Result.CommandType := aGIC;
  Result.PlayerIndex := MyPlayer.PlayerIndex;

  for i:=Low(Result.Params) to High(Result.Params) do
    Result.Params[i] := maxint;

  Result.TextParam := aTextParam;
end;


procedure TGameInputProcess.ExecCommand(aCommand: TGameInputCommand);
var
  P: TKMPlayer;
  IsSilent: boolean;
  Group1, Group2: TKMUnitGroup;
  U: TKMUnit;
  H, H2: TKMHouse;
begin
  //NOTE: MyPlayer should not be used for important stuff here, use P instead (commands must be executed the same for all players)
  IsSilent := (aCommand.PlayerIndex <> MyPlayer.PlayerIndex);
  P := fPlayers[aCommand.PlayerIndex];
  Group1 := nil;
  Group2 := nil;
  H := nil;
  H2 := nil;
  U := nil;

  with aCommand do
  begin
    //It is possible that units/houses have died by now
    if CommandType in [gic_ArmyFeed,gic_ArmySplit,gic_ArmyLink,gic_ArmyAttackUnit,gic_ArmyAttackHouse,gic_ArmyHalt,gic_ArmyFormation,gic_ArmyWalk,gic_ArmyStorm] then
    begin
      Group1 := fPlayers.GetGroupByID(Params[1]);
      if (Group1 = nil) or Group1.IsDead //Group has died before command could be executed
      or (Group1.Owner <> aCommand.PlayerIndex) then Exit; //Potential exploit
    end;
    if CommandType in [gic_ArmyLink] then
    begin
      Group2 := fPlayers.GetGroupByID(Params[2]);
      if (Group2 = nil) or Group2.IsDead //Unit has died before command could be executed
      or (Group2.Owner <> aCommand.PlayerIndex) then Exit; //Potential exploit
    end;
    if CommandType in [gic_ArmyAttackUnit] then
    begin
      U := fPlayers.GetUnitByID(Params[2]);
      if (U = nil) or U.IsDeadOrDying //Unit has died before command could be executed
      or (U.Owner <> aCommand.PlayerIndex) then Exit; //Potential exploit
    end;
    if CommandType in [gic_HouseRepairToggle, gic_HouseDeliveryToggle,
      gic_HouseOrderProduct, gic_HouseMarketFrom, gic_HouseMarketTo,
      gic_HouseStoreAcceptFlag, gic_HouseBarracksAcceptFlag, gic_HouseBarracksEquip,
      gic_HouseSchoolTrain, gic_HouseRemoveTrain, gic_HouseWoodcutterMode] then
    begin
      H := fPlayers.GetHouseByID(Params[1]);
      if (H = nil) or H.IsDestroyed //House has been destroyed before command could be executed
      or (H.Owner <> aCommand.PlayerIndex) then Exit; //Potential exploit
    end;
    if CommandType in [gic_ArmyAttackHouse] then
    begin
      H2 := fPlayers.GetHouseByID(Params[2]);
      if (H2 = nil) or H2.IsDestroyed //House has been destroyed before command could be executed
      or (H2.Owner <> aCommand.PlayerIndex) then Exit; //Potential exploit
    end;

    //Some commands are blocked by peacetime (this is a fall back in case players try to cheat)
    if fGame.IsPeaceTime and (CommandType in BlockedByPeaceTime) then
       Exit;

    //No commands allowed after a player has lost (this is a fall back in case players try to cheat)
    if not (aCommand.CommandType in AllowedAfterDefeat) and fGame.IsMultiplayer and (P.AI.WonOrLost = wol_Lost) then
      Exit;

    case CommandType of
      gic_ArmyFeed:         Group1.OrderFood(True);
      gic_ArmySplit:        Group1.OrderSplit(True);
      gic_ArmyStorm:        Group1.OrderStorm(True);
      gic_ArmyLink:         Group1.OrderLinkTo(Group2, True);
      gic_ArmyAttackUnit:   Group1.OrderAttackUnit(U, True);
      gic_ArmyAttackHouse:  Group1.OrderAttackHouse(H2, True);
      gic_ArmyHalt:         Group1.OrderHalt(True);
      gic_ArmyFormation:    Group1.OrderFormation(TKMTurnDirection(Params[2]),Params[3], True);
      gic_ArmyWalk:         Group1.OrderWalk(KMPoint(Params[2],Params[3]), True, TKMDirection(Params[4]));

      gic_BuildAddFieldPlan:      P.ToggleFieldPlan(KMPoint(Params[1],Params[2]), TFieldType(Params[3]), not fGame.IsMultiplayer); //Make sound in singleplayer mode only
      gic_BuildRemoveFieldPlan:   P.RemFieldPlan(KMPoint(Params[1],Params[2]), not fGame.IsMultiplayer); //Make sound in singleplayer mode only
      gic_BuildRemoveHouse:       P.RemHouse(KMPoint(Params[1],Params[2]), IsSilent);
      gic_BuildRemoveHousePlan:   P.RemHousePlan(KMPoint(Params[1],Params[2]));
      gic_BuildHousePlan:         if P.CanAddHousePlan(KMPoint(Params[2],Params[3]), THouseType(Params[1])) then
                                    P.AddHousePlan(THouseType(Params[1]), KMPoint(Params[2],Params[3]));

      gic_HouseRepairToggle:      H.RepairToggle;
      gic_HouseDeliveryToggle:    H.WareDelivery := not H.WareDelivery;
      gic_HouseOrderProduct:      H.ResEditOrder(Params[2], Params[3]);
      gic_HouseMarketFrom:        TKMHouseMarket(H).ResFrom := TResourceType(Params[2]);
      gic_HouseMarketTo:          TKMHouseMarket(H).ResTo := TResourceType(Params[2]);
      gic_HouseStoreAcceptFlag:   TKMHouseStore(H).ToggleAcceptFlag(TResourceType(Params[2]));
      gic_HouseWoodcutterMode:    TKMHouseWoodcutters(H).WoodcutterMode := TWoodcutterMode(Params[2]);
      gic_HouseBarracksAcceptFlag:TKMHouseBarracks(H).ToggleAcceptFlag(TResourceType(Params[2]));
      gic_HouseBarracksEquip:     TKMHouseBarracks(H).Equip(TUnitType(Params[2]), Params[3]);
      gic_HouseSchoolTrain:       TKMHouseSchool(H).AddUnitToQueue(TUnitType(Params[2]), Params[3]);
      gic_HouseRemoveTrain:       TKMHouseSchool(H).RemUnitFromQueue(Params[2]);

      gic_RatioChange:            begin
                                    P.Stats.Ratio[TResourceType(Params[1]), THouseType(Params[2])] := Params[3];
                                    P.Houses.UpdateResRequest
                                  end;

      gic_TempAddScout:           if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not fGame.IsMultiplayer) then
                                    P.AddUnit(ut_HorseScout, KMPoint(Params[1],Params[2]));
      gic_TempRevealMap:          if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not fGame.IsMultiplayer) then
                                    P.FogOfWar.RevealEverything;
      gic_TempDoNothing:          ;

      gic_GamePause:              ;//if fReplayState = gipRecording then fGame.fGamePlayInterface.SetPause(boolean(Params[1]));
      gic_GameSave:               if fReplayState = gipRecording then
                                  begin
                                    fGame.Save(TextParam);
                                    if fGame.IsMultiplayer then
                                      //Tell the player we have saved the game
                                      fGame.Networking.PostLocalMessage(fTextLibrary[TX_MULTIPLAYER_SAVING_GAME]);
                                  end;
      gic_GameTeamChange:         begin
                                    //Currently unused, disabled to prevent potential exploitation
                                    {fGame.Networking.NetPlayers[Params[1]].Team := Params[2];
                                    fGame.UpdateMultiplayerTeams;
                                    fPlayers.SyncFogOfWar;
                                    if fGame.Networking.IsHost then
                                      fGame.Networking.SendPlayerListAndRefreshPlayersSetup;}
                                  end;
      gic_GameAlertBeacon:        if fReplayState = gipRecording then //Beacons don't show up in replay
                                    //Beacons are only for allies
                                    if fPlayers.CheckAlliance(Params[3], MyPlayer.PlayerIndex) = at_Ally then
                                      fGame.Alerts.AddBeacon(KMPointF(Params[1]/10,Params[2]/10), Params[3]);
      else                        Assert(false);
    end;
  end;
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup);
begin
  Assert(aCommandType in [gic_ArmyFeed, gic_ArmySplit, gic_ArmyStorm, gic_ArmyHalt]);
  TakeCommand(MakeCommand(aCommandType, [aGroup.ID]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aUnit: TKMUnit);
begin
  Assert(aCommandType in [gic_ArmyAttackUnit]);
  TakeCommand(MakeCommand(aCommandType, [aGroup.ID, aUnit.ID]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup1, aGroup2: TKMUnitGroup);
begin
  Assert(aCommandType in [gic_ArmyLink]);
  TakeCommand(MakeCommand(aCommandType, [aGroup1.ID, aGroup2.ID]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aHouse: TKMHouse);
begin
  Assert(aCommandType = gic_ArmyAttackHouse);
  TakeCommand(MakeCommand(aCommandType, [aGroup.ID, aHouse.ID]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aTurnAmount: TKMTurnDirection; aLineAmount:shortint);
begin
  Assert(aCommandType = gic_ArmyFormation);
  TakeCommand(MakeCommand(aCommandType, [aGroup.ID, byte(aTurnAmount), aLineAmount]));
end;


procedure TGameInputProcess.CmdArmy(aCommandType: TGameInputCommandType; aGroup: TKMUnitGroup; aLoc: TKMPoint; aDirection: TKMDirection);
begin
  Assert(aCommandType = gic_ArmyWalk);
  TakeCommand(MakeCommand(aCommandType, [aGroup.ID, aLoc.X, aLoc.Y, byte(aDirection)]));
end;


procedure TGameInputProcess.CmdBuild(aCommandType: TGameInputCommandType; aLoc: TKMPoint);
begin
  Assert(aCommandType in [gic_BuildRemoveFieldPlan, gic_BuildRemoveHouse, gic_BuildRemoveHousePlan]);

  //Remove fake markup that will be visible only to MyPlayer until Server verifies it.
  //Must go before TakeCommand as it could execute command immediately (in singleplayer)
  //and the fake markup must be added first otherwise our logic in FieldsList fails
  if fGame.IsMultiplayer and (aCommandType = gic_BuildRemoveFieldPlan) then
    MyPlayer.RemFakeFieldPlan(aLoc);

  TakeCommand(MakeCommand(aCommandType, [aLoc.X, aLoc.Y]));
end;


procedure TGameInputProcess.CmdBuild(aCommandType: TGameInputCommandType; aLoc: TKMPoint; aFieldType: TFieldType);
begin
  Assert(aCommandType in [gic_BuildAddFieldPlan]);

  //Add fake markup that will be visible only to MyPlayer until Server verifies it.
  //Must go before TakeCommand as it could execute command immediately (in singleplayer)
  //and the fake markup must be added first otherwise our logic in FieldsList fails
  if fGame.IsMultiplayer then
    MyPlayer.ToggleFakeFieldPlan(aLoc, aFieldType);

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
  TakeCommand(MakeCommand(aCommandType, aHouse.ID));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem, aAmount:integer);
begin
  Assert(aCommandType = gic_HouseOrderProduct);
  TakeCommand(MakeCommand(aCommandType, [aHouse.ID, aItem, aAmount]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem: TResourceType);
begin
  Assert(aCommandType in [gic_HouseStoreAcceptFlag, gic_HouseBarracksAcceptFlag, gic_HouseMarketFrom, gic_HouseMarketTo]);
  TakeCommand(MakeCommand(aCommandType, [aHouse.ID, byte(aItem)]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aWoodcutterMode: TWoodcutterMode);
begin
  Assert(aCommandType = gic_HouseWoodcutterMode);
  TakeCommand(MakeCommand(aCommandType, [aHouse.ID, byte(aWoodcutterMode)]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aUnitType: TUnitType; aCount:byte);
begin
  Assert(aCommandType in [gic_HouseSchoolTrain, gic_HouseBarracksEquip]);
  TakeCommand(MakeCommand(aCommandType, [aHouse.ID, byte(aUnitType), aCount]));
end;


procedure TGameInputProcess.CmdHouse(aCommandType: TGameInputCommandType; aHouse: TKMHouse; aItem:integer);
begin
  Assert(aCommandType = gic_HouseRemoveTrain);
  Assert(aHouse is TKMHouseSchool);
  TakeCommand(MakeCommand(aCommandType, [aHouse.ID, aItem]));
end;


procedure TGameInputProcess.CmdRatio(aCommandType: TGameInputCommandType; aRes: TResourceType; aHouseType: THouseType; aValue:integer);
begin
  Assert(aCommandType = gic_RatioChange);
  TakeCommand(MakeCommand(aCommandType, [byte(aRes), byte(aHouseType), aValue]));
end;


procedure TGameInputProcess.CmdGame(aCommandType: TGameInputCommandType; aValue: Boolean);
begin
  Assert(aCommandType = gic_GamePause);
  TakeCommand(MakeCommand(aCommandType, [integer(aValue)]));
end;


procedure TGameInputProcess.CmdGame(aCommandType: TGameInputCommandType; aValue: AnsiString);
begin
  Assert(aCommandType = gic_GameSave);
  TakeCommand(MakeCommand(aCommandType, aValue));
end;


procedure TGameInputProcess.CmdGame(aCommandType: TGameInputCommandType; aPlayer, aTeam: integer);
begin
  Assert(aCommandType = gic_GameTeamChange);
  TakeCommand(MakeCommand(aCommandType, [aPlayer,aTeam]));
end;


procedure TGameInputProcess.CmdGame(aCommandType: TGameInputCommandType; aLoc: TKMPointF; aPlayer: TPlayerIndex);
begin
  Assert(aCommandType = gic_GameAlertBeacon);
  TakeCommand(MakeCommand(aCommandType, [Round(aLoc.X * 10), Round(aLoc.Y * 10), aPlayer]));
end;


procedure TGameInputProcess.CmdTemp(aCommandType: TGameInputCommandType; aLoc: TKMPoint);
begin
  Assert(aCommandType = gic_TempAddScout);
  TakeCommand(MakeCommand(aCommandType, [aLoc.X, aLoc.Y]));
end;


procedure TGameInputProcess.CmdTemp(aCommandType: TGameInputCommandType);
begin
  Assert(aCommandType in [gic_TempRevealMap, gic_TempDoNothing]);
  TakeCommand(MakeCommand(aCommandType, []));
end;


procedure TGameInputProcess.SaveToFile(aFileName: string);
var i:integer; S: TKMemoryStream;
begin
  S := TKMemoryStream.Create;
  S.Write(AnsiString(GAME_VERSION));
  S.Write(fCount);
  for i:=1 to fCount do
  begin
    S.Write(fQueue[i].Tick);
    SaveCommandToMemoryStream(fQueue[i].Command, S);
    S.Write(fQueue[i].Rand);
  end;

  S.SaveToFile(aFileName);
  S.Free;
end;


procedure TGameInputProcess.LoadFromFile(aFileName: string);
var
  FileVersion: AnsiString;
  I: Integer;
  S: TKMemoryStream;
begin
  if not FileExists(aFileName) then exit;
  S := TKMemoryStream.Create;
  S.LoadFromFile(aFileName);
  S.Read(FileVersion);
  Assert(FileVersion=GAME_VERSION, 'Old or unexpected replay file. '+GAME_VERSION+' is required.');
  S.Read(fCount);
  setlength(fQueue, fCount+1);
  for i:=1 to fCount do
  begin
    S.Read(fQueue[i].Tick);
    LoadCommandFromMemoryStream(fQueue[i].Command, S);
    S.Read(fQueue[i].Rand);
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
  begin
    //Changing MyPlayer affect AI replay which leads to replay mismatch errors soon after
    //if aCommand.CommandType = gic_TempChangeMyPlayer then
      //MyPlayer := fPlayers[aCommand.Params[1]];
    Exit;
  end;

  Assert(ReplayState = gipRecording);
  inc(fCount);
  if Length(fQueue) <= fCount then SetLength(fQueue, fCount+128);

  fQueue[fCount].Tick    := fGame.GameTickCount;
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

unit KM_GameInputProcess;
{$I KaM_Remake.inc}
interface
uses SysUtils, Controls, KromUtils, KM_CommonTypes, KM_Defaults, KM_Utils,
    KM_Houses, KM_Units, KM_Units_Warrior, KM_PlayersCollection, KM_Player;

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
    gic_ArmyHalt,         //Formation commands
    gic_ArmyWalk,         //Walking
    gic_ArmyStorm,        //StormAttack

    //II.     Building/road plans (what to build and where)
    gic_BuildRoadPlan,
    gic_BuildFieldPlan,
    gic_BuildWinePlan,
    gic_BuildWallPlan,
    gic_BuildRemovePlan,  //Removal of a plan
    gic_BuildRemoveHouse, //Removal of house
    gic_BuildHousePlan,   //Build HouseType

    //III.    House repair/delivery/orders (TKMHouse, Toggle(repair, delivery, orders))
    gic_HouseRepairToggle,
    gic_HouseDeliveryToggle,  //Including storehouse. (On/Off, ResourceType)
    gic_HouseOrderProduct,    //Place an order to manufacture warfare
    gic_HouseStoreAcceptFlag,
    gic_HouseTrain,           //Place an order to train citizen/warrior
    gic_HouseRemoveTrain,     //Remove unit being trained from School

    //IV.     Delivery ratios changes (and other game-global settings)
    gic_RatioChange,

    //V.      Cheatcodes affecting gameplay (props)

    //VI. Temporary and debug commands
    gic_TempAddScout,
    gic_TempKillUnit,
    gic_TempRevealMap, //Revealing the map can have an impact on the game. Events happen based on tiles being revealed
    gic_TempChangeMyPlayer //Make debugging easier

    { Optional input }
    //VI.     Viewport settings for replay (location, zoom)
    //VII.    Message queue handling in gameplay interface
    //IX.     Text messages for multiplayer (moved to Networking)
    );


  TGameInputCommand = record
    CommandType:TGameInputCommandType;
    Params:array[1..MAX_PARAMS]of integer;
    PlayerID: TPlayerID; //Player for which the command is to be issued.
                         //Needed for multiplayer. Also removes need for gic_TempChangeMyPlayer
  end;


  TGameInputProcess = class
  private
    fCount:integer;
    fReplayState:TGIPReplayState;
  protected
    fCursor:integer; //Used only in gipReplaying
    fQueue: array of packed record
      Tick:cardinal;
      Command:TGameInputCommand;
      Rand:cardinal; //acts as CRC check
    end;

    function MakeCommand(aGIC:TGameInputCommandType; const aParam:array of integer):TGameInputCommand;
    procedure TakeCommand(aCommand:TGameInputCommand); virtual; abstract;
    procedure ExecCommand(aCommand: TGameInputCommand);
    procedure StoreCommand(aCommand: TGameInputCommand);
  public
    constructor Create(aReplayState:TGIPReplayState);
    destructor Destroy; override;

    procedure CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior); overload;
    procedure CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior; aUnit:TKMUnit); overload;
    procedure CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior; aHouse:TKMHouse); overload;
    procedure CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior; aTurnAmount:shortint; aLineAmount:shortint); overload;
    procedure CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior; aLoc:TKMPoint; aDirection:TKMDirection=dir_NA); overload;

    procedure CmdBuild(aCommandType:TGameInputCommandType; aLoc:TKMPoint); overload;
    procedure CmdBuild(aCommandType:TGameInputCommandType; aLoc:TKMPoint; aHouseType:THouseType); overload;

    procedure CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse); overload;
    procedure CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse; aItem, aAmount:integer); overload;
    procedure CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse; aItem:TResourceType); overload;
    procedure CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse; aUnitType:TUnitType); overload;
    procedure CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse; aItem:integer); overload;

    procedure CmdRatio(aCommandType:TGameInputCommandType; aRes:TResourceType; aHouseType:THouseType; aValue:integer);

    procedure CmdTemp(aCommandType:TGameInputCommandType; aUnit:TKMUnit); overload;
    procedure CmdTemp(aCommandType:TGameInputCommandType; aLoc:TKMPoint); overload;
    procedure CmdTemp(aCommandType:TGameInputCommandType); overload;
    procedure CmdTemp(aCommandType:TGameInputCommandType; aNewPlayerID:TPlayerID); overload;

    function CommandsConfirmed(aTick:cardinal):boolean; virtual;
    procedure ReplayTimer(aTick:cardinal); virtual;
    procedure RunningTimer(aTick:cardinal); virtual;
    procedure UpdateState(aTick:cardinal); virtual;

    //Replay methods
    procedure SaveToFile(aFileName:string);
    procedure LoadFromFile(aFileName:string);
    property Count:integer read fCount;
    property ReplayState:TGIPReplayState read fReplayState;
    function GetLastTick:integer;
    function ReplayEnded:boolean;
  end;


implementation
uses KM_Game, KM_Terrain;


constructor TGameInputProcess.Create(aReplayState:TGIPReplayState);
begin
  Inherited Create;
  setlength(fQueue, 128);
  fCount := 0;
  fCursor := 1;
  fReplayState := aReplayState;
end;


destructor TGameInputProcess.Destroy;
begin
  Inherited;
end;


function TGameInputProcess.MakeCommand(aGIC:TGameInputCommandType; const aParam:array of integer):TGameInputCommand;
var i:integer;
begin
  Result.CommandType := aGIC;
  Result.PlayerID := MyPlayer.PlayerID;
  for i:=Low(aParam) to High(aParam) do
    Result.Params[i+1] := aParam[i];
  for i:=High(aParam)+1 to High(Result.Params)-1 do
    Result.Params[i+1] := maxint;
end;


procedure TGameInputProcess.ExecCommand(aCommand: TGameInputCommand);
var H:TKMHouse; P:TKMPlayer;
begin
  P := fPlayers.Player[byte(aCommand.PlayerID)];
  with aCommand do
  case CommandType of
    gic_ArmyFeed:         TKMUnitWarrior(P.Units.GetUnitByID(Params[1])).OrderFood;
    gic_ArmySplit:        TKMUnitWarrior(P.Units.GetUnitByID(Params[1])).OrderSplit;
    gic_ArmyStorm:        TKMUnitWarrior(P.Units.GetUnitByID(Params[1])).OrderStorm;
    gic_ArmyLink:         TKMUnitWarrior(P.Units.GetUnitByID(Params[1])).OrderLinkTo(TKMUnitWarrior(fPlayers.GetUnitByID(Params[2])));
    gic_ArmyAttackUnit:   TKMUnitWarrior(P.Units.GetUnitByID(Params[1])).GetCommander.OrderAttackUnit(fPlayers.GetUnitByID(Params[2]));
    gic_ArmyAttackHouse:  TKMUnitWarrior(P.Units.GetUnitByID(Params[1])).GetCommander.OrderAttackHouse(fPlayers.GetHouseByID(Params[2]));
    gic_ArmyHalt:         TKMUnitWarrior(P.Units.GetUnitByID(Params[1])).OrderHalt(Params[2],Params[3]);
    gic_ArmyWalk:         TKMUnitWarrior(P.Units.GetUnitByID(Params[1])).GetCommander.OrderWalk(KMPoint(Params[2],Params[3]), TKMDirection(Params[4]));

    gic_BuildRoadPlan:    P.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_RoadPlan,  false, P.PlayerID);
    gic_BuildFieldPlan:   P.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_FieldPlan,  false, P.PlayerID);
    gic_BuildWinePlan:    P.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_WinePlan,  false, P.PlayerID);
    gic_BuildWallPlan:    P.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_WallPlan,  false, P.PlayerID);
    gic_BuildRemovePlan:  P.RemPlan(KMPoint(Params[1],Params[2]));
    gic_BuildRemoveHouse: P.RemHouse(KMPoint(Params[1],Params[2]), false);
    gic_BuildHousePlan:   P.AddHousePlan(THouseType(Params[1]), KMPoint(Params[2],Params[3]));

    gic_HouseRepairToggle:      P.Houses.GetHouseByID(Params[1]).RepairToggle;
    gic_HouseDeliveryToggle:    with P.Houses.GetHouseByID(Params[1]) do WareDelivery := not WareDelivery;
    gic_HouseOrderProduct:      P.Houses.GetHouseByID(Params[1]).ResEditOrder(Params[2], Params[3]);
    gic_HouseStoreAcceptFlag:   TKMHouseStore(P.Houses.GetHouseByID(Params[1])).ToggleAcceptFlag(TResourceType(Params[2]));
    gic_HouseTrain:             begin
                                  H := P.Houses.GetHouseByID(Params[1]);
                                  case H.GetHouseType of
                                    ht_Barracks:  TKMHouseBarracks(H).Equip(TUnitType(Params[2]));
                                    ht_School:    TKMHouseSchool(H).AddUnitToQueue(TUnitType(Params[2]));
                                    else          Assert(false, 'Only Schools and Barracks supported yet');
                                  end;
                                end;
    gic_HouseRemoveTrain:       TKMHouseSchool(P.Houses.GetHouseByID(Params[1])).RemUnitFromQueue(Params[2]);

    gic_RatioChange:            begin
                                  P.Stats.SetRatio(TResourceType(Params[1]), THouseType(Params[2]), Params[3]);
                                  P.Houses.UpdateResRequest
                                end;

    gic_TempAddScout:           P.AddUnit(ut_HorseScout, KMPoint(Params[1],Params[2]));
    gic_TempKillUnit:           P.Units.GetUnitByID(Params[1]).KillUnit;
    gic_TempRevealMap:          P.FogOfWar.RevealEverything;
    gic_TempChangeMyPlayer:     MyPlayer := fPlayers.Player[Params[1]];
    else                        Assert(false);
  end;
end;


procedure TGameInputProcess.CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior);
begin
  Assert(aCommandType in [gic_ArmyFeed, gic_ArmySplit, gic_ArmyStorm]);
  TakeCommand( MakeCommand(aCommandType, aWarrior.ID) );
end;


procedure TGameInputProcess.CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior; aUnit:TKMUnit);
begin
  Assert(aCommandType in [gic_ArmyLink, gic_ArmyAttackUnit]);
  TakeCommand( MakeCommand(aCommandType, [aWarrior.ID, aUnit.ID]) );
end;


procedure TGameInputProcess.CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior; aHouse:TKMHouse);
begin
  Assert(aCommandType = gic_ArmyAttackHouse);
  TakeCommand( MakeCommand(aCommandType, [aWarrior.ID, aHouse.ID]) );
end;


procedure TGameInputProcess.CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior; aTurnAmount:shortint; aLineAmount:shortint);
begin
  Assert(aCommandType = gic_ArmyHalt);
  TakeCommand( MakeCommand(aCommandType, [aWarrior.ID, aTurnAmount, aLineAmount]) );
end;


procedure TGameInputProcess.CmdArmy(aCommandType:TGameInputCommandType; aWarrior:TKMUnitWarrior; aLoc:TKMPoint; aDirection:TKMDirection=dir_NA);
begin
  Assert(aCommandType = gic_ArmyWalk);
  TakeCommand( MakeCommand(aCommandType, [aWarrior.ID, aLoc.X, aLoc.Y, byte(aDirection)]) );
end;


procedure TGameInputProcess.CmdBuild(aCommandType:TGameInputCommandType; aLoc:TKMPoint);
begin
  Assert(aCommandType in [gic_BuildRoadPlan, gic_BuildFieldPlan, gic_BuildWinePlan, gic_BuildWallPlan, gic_BuildRemovePlan, gic_BuildRemoveHouse]);
  TakeCommand( MakeCommand(aCommandType, [aLoc.X, aLoc.Y]) );
end;


procedure TGameInputProcess.CmdBuild(aCommandType:TGameInputCommandType; aLoc:TKMPoint; aHouseType:THouseType);
begin
  Assert(aCommandType = gic_BuildHousePlan);
  TakeCommand( MakeCommand(aCommandType, [byte(aHouseType), aLoc.X, aLoc.Y]) );
end;


procedure TGameInputProcess.CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse);
begin
  Assert(aCommandType in [gic_HouseRepairToggle, gic_HouseDeliveryToggle]);
  TakeCommand( MakeCommand(aCommandType, aHouse.ID) );
end;


procedure TGameInputProcess.CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse; aItem, aAmount:integer);
begin
  Assert(aCommandType = gic_HouseOrderProduct);
  TakeCommand( MakeCommand(aCommandType, [aHouse.ID, aItem, aAmount]) );
end;


procedure TGameInputProcess.CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse; aItem:TResourceType);
begin
  Assert(aCommandType = gic_HouseStoreAcceptFlag);
  TakeCommand( MakeCommand(aCommandType, [aHouse.ID, byte(aItem)]) );
end;


procedure TGameInputProcess.CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse; aUnitType:TUnitType);
begin
  Assert(aCommandType = gic_HouseTrain);
  TakeCommand( MakeCommand(aCommandType, [aHouse.ID, byte(aUnitType)]) );
end;


procedure TGameInputProcess.CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse; aItem:integer);
begin
  Assert(aCommandType = gic_HouseRemoveTrain);
  Assert(aHouse is TKMHouseSchool);
  TakeCommand( MakeCommand(aCommandType, [aHouse.ID, aItem]) );
end;


procedure TGameInputProcess.CmdRatio(aCommandType:TGameInputCommandType; aRes:TResourceType; aHouseType:THouseType; aValue:integer);
begin
  Assert(aCommandType = gic_RatioChange);
  TakeCommand( MakeCommand(aCommandType, [byte(aRes), byte(aHouseType), aValue]) );
end;


procedure TGameInputProcess.CmdTemp(aCommandType:TGameInputCommandType; aUnit:TKMUnit);
begin
  Assert(aCommandType = gic_TempKillUnit);
  TakeCommand( MakeCommand(aCommandType, [aUnit.ID]) );
end;


procedure TGameInputProcess.CmdTemp(aCommandType:TGameInputCommandType; aLoc:TKMPoint);
begin
  Assert(aCommandType = gic_TempAddScout);
  TakeCommand( MakeCommand(aCommandType, [aLoc.X, aLoc.Y]) );
end;


procedure TGameInputProcess.CmdTemp(aCommandType:TGameInputCommandType);
begin
  Assert(aCommandType = gic_TempRevealMap);
  TakeCommand( MakeCommand(aCommandType, []) );
end;


procedure TGameInputProcess.CmdTemp(aCommandType:TGameInputCommandType; aNewPlayerID:TPlayerID);
begin
  Assert(aCommandType = gic_TempChangeMyPlayer);
  TakeCommand( MakeCommand(aCommandType, [Integer(aNewPlayerID)]) );
end;


procedure TGameInputProcess.SaveToFile(aFileName:string);
var i:integer; S:TKMemoryStream;
begin
  S := TKMemoryStream.Create;
  S.Write(GAME_VERSION); //
  S.Write(fCount);
  for i:=1 to fCount do
    S.Write(fQueue[i].Tick, SizeOf(fQueue[i]));

  S.SaveToFile(aFileName);
  S.Free;
end;


procedure TGameInputProcess.LoadFromFile(aFileName:string);
var FileVersion:string; i:integer; S:TKMemoryStream;
begin
  if not FileExists(aFileName) then exit;
  S := TKMemoryStream.Create;
  S.LoadFromFile(aFileName);
  S.Read(FileVersion);
  Assert(FileVersion=GAME_VERSION, 'Old or unexpected replay file. '+GAME_VERSION+' is required.');
  S.Read(fCount);
  setlength(fQueue, fCount+1);
  for i:=1 to fCount do
    S.Read(fQueue[i].Tick, SizeOf(fQueue[i]));
  S.Free;
end;


{ Return last recorded tick }
function TGameInputProcess.GetLastTick:integer;
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
  if (ReplayState = gipReplaying) and (aCommand.CommandType = gic_TempChangeMyPlayer) then
  begin
    MyPlayer := fPlayers.Player[aCommand.Params[1]];
    Exit;
  end;

  Assert(ReplayState = gipRecording);
  inc(fCount);
  if length(fQueue) <= fCount then SetLength(fQueue, fCount+128);

  fQueue[fCount].Tick    := fGame.GameTickCount;
  fQueue[fCount].Command := aCommand;
  fQueue[fCount].Rand    := Cardinal(Random(maxint)); //This will be our check to ensure everything is consistent
end;


function TGameInputProcess.CommandsConfirmed(aTick:cardinal):boolean;
begin
  Result := true;
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


unit KM_GameInputProcess;
{$I KaM_Remake.inc}
interface
uses SysUtils, Controls, KromUtils, KM_CommonTypes, KM_Defaults, KM_Utils,
    KM_Houses, KM_Units, KM_Units_Warrior;

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
  commands with StoreCommand, and in gipReplaying state commands are executed on Tick()
  }

const MAX_PARAMS = 4; //There are maximum of 4 integers passed along with a command

type TGameInputCommandType = (
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
  gic_TempChangeMyPlayer, //Make debugging easier

  { Optional input }
  //VI.     Viewport settings for replay (location, zoom)
  //VII.    Message queue handling in gameplay interface
  //IX.     Text messages for multiplayer (console)
  gic_TextMessage

  );

type TGameInputCommand = record
       CommandType:TGameInputCommandType;
       Params:array[1..MAX_PARAMS]of integer;
       //Text: string;
       //todo: PlayerID: TKMPlayerID; //Player for which the command is to be issued.
       //                               Needed for multiplayer. Also removes need for gic_TempChangeMyPlayer
     end;


type TGIPReplayState = (gipRecording, gipReplaying);


type
  TGameInputProcess = class
    private
      fCount:integer;
      fCursor:integer; //Used only in gipReplaying
      fQueue: array of packed record
        Tick:cardinal;
        Command:TGameInputCommand;
        Rand:cardinal; //acts as CRC check
      end;
      fReplayState:TGIPReplayState;
    protected
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
      procedure CmdTemp(aCommandType:TGameInputCommandType; aPlayerID:integer); overload;

      procedure CmdText(aCommandType:TGameInputCommandType; aPlayerID:integer; aTime:string; aText:string);

      procedure Tick(aTick:cardinal); virtual;

      //Replay methods
      procedure SaveToFile(aFileName:string);
      procedure LoadFromFile(aFileName:string);
      property Count:integer read fCount;
      property ReplayState:TGIPReplayState read fReplayState;
      function GetLastTick:integer;
      function ReplayEnded:boolean;
  end;


implementation
uses KM_Sound, KM_Game, KM_PlayersCollection, KM_Terrain;


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
  for i:=Low(aParam) to High(aParam) do
    Result.Params[i+1] := aParam[i];
  for i:=High(aParam)+1 to High(Result.Params)-1 do
    Result.Params[i+1] := maxint;
end;


procedure TGameInputProcess.ExecCommand(aCommand: TGameInputCommand);
var H:TKMHouse;
begin
  with aCommand do
  case CommandType of
    gic_ArmyFeed:         TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).OrderFood;
    gic_ArmySplit:        TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).OrderSplit;
    gic_ArmyStorm:        TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).OrderStorm;
    gic_ArmyLink:         TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).OrderLinkTo(TKMUnitWarrior(fPlayers.GetUnitByID(Params[2])));
    gic_ArmyAttackUnit:   TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).GetCommander.OrderAttackUnit(fPlayers.GetUnitByID(Params[2]));
    gic_ArmyAttackHouse:  TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).GetCommander.OrderAttackHouse(fPlayers.GetHouseByID(Params[2]));
    gic_ArmyHalt:         TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).OrderHalt(Params[2],Params[3]);
    gic_ArmyWalk:         TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).GetCommander.OrderWalk(KMPoint(Params[2],Params[3]), TKMDirection(Params[4]));

    gic_BuildRoadPlan:    MyPlayer.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_RoadPlan,  false, MyPlayer.PlayerID);
    gic_BuildFieldPlan:   MyPlayer.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_FieldPlan,  false, MyPlayer.PlayerID);
    gic_BuildWinePlan:    MyPlayer.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_WinePlan,  false, MyPlayer.PlayerID);
    gic_BuildWallPlan:    MyPlayer.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_WallPlan,  false, MyPlayer.PlayerID);
    gic_BuildRemovePlan:  MyPlayer.RemPlan(KMPoint(Params[1],Params[2]));
    gic_BuildRemoveHouse: MyPlayer.RemHouse(KMPoint(Params[1],Params[2]), false);
    gic_BuildHousePlan:   MyPlayer.AddHousePlan(THouseType(Params[1]), KMPoint(Params[2],Params[3]));

    gic_HouseRepairToggle:      MyPlayer.GetHouseByID(Params[1]).RepairToggle;
    gic_HouseDeliveryToggle:    with MyPlayer.GetHouseByID(Params[1]) do WareDelivery := not WareDelivery;
    gic_HouseOrderProduct:      MyPlayer.GetHouseByID(Params[1]).ResEditOrder(Params[2], Params[3]);
    gic_HouseStoreAcceptFlag:   TKMHouseStore(MyPlayer.GetHouseByID(Params[1])).ToggleAcceptFlag(TResourceType(Params[2]));
    gic_HouseTrain:             begin
                                  H := MyPlayer.GetHouseByID(Params[1]);
                                  case H.GetHouseType of
                                    ht_Barracks:  TKMHouseBarracks(H).Equip(TUnitType(Params[2]));
                                    ht_School:    TKMHouseSchool(H).AddUnitToQueue(TUnitType(Params[2]));
                                    else          Assert(false, 'Only Schools and Barracks supported yet');
                                  end;
                                end;
    gic_HouseRemoveTrain:       TKMHouseSchool(MyPlayer.GetHouseByID(Params[1])).RemUnitFromQueue(Params[2]);

    gic_RatioChange:            begin
                                  MyPlayer.fPlayerStats.SetRatio(TResourceType(Params[1]), THouseType(Params[2]), Params[3]);
                                  MyPlayer.GetHouses.UpdateResRequest
                                end;

    gic_TempAddScout:           MyPlayer.AddUnit(ut_HorseScout, KMPoint(Params[1],Params[2]));
    gic_TempKillUnit:           MyPlayer.GetUnitByID(Params[1]).KillUnit;
    gic_TempRevealMap:          fTerrain.RevealWholeMap(MyPlayer.PlayerID);
    gic_TempChangeMyPlayer:     MyPlayer := fPlayers.Player[Params[1]];
    //gic_TextMessage:            fGame.fChat.AddMessage(aPlayerID, aTime, aText);

    else Assert(false);
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
  TakeCommand( MakeCommand(aCommandType, [aWarrior.ID, aLoc.X, aLoc.Y, integer(aDirection)]) );
end;


procedure TGameInputProcess.CmdBuild(aCommandType:TGameInputCommandType; aLoc:TKMPoint);
begin
  Assert(aCommandType in [gic_BuildRoadPlan, gic_BuildFieldPlan, gic_BuildWinePlan, gic_BuildWallPlan, gic_BuildRemovePlan, gic_BuildRemoveHouse]);
  TakeCommand( MakeCommand(aCommandType, [aLoc.X, aLoc.Y]) );
end;


procedure TGameInputProcess.CmdBuild(aCommandType:TGameInputCommandType; aLoc:TKMPoint; aHouseType:THouseType);
begin
  Assert(aCommandType = gic_BuildHousePlan);
  TakeCommand( MakeCommand(aCommandType, [integer(aHouseType), aLoc.X, aLoc.Y]) );
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
  TakeCommand( MakeCommand(aCommandType, [aHouse.ID, integer(aItem)]) );
end;


procedure TGameInputProcess.CmdHouse(aCommandType:TGameInputCommandType; aHouse:TKMHouse; aUnitType:TUnitType);
begin
  Assert(aCommandType = gic_HouseTrain);
  TakeCommand( MakeCommand(aCommandType, [aHouse.ID, integer(aUnitType)]) );
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
  TakeCommand( MakeCommand(aCommandType, [integer(aRes), integer(aHouseType), aValue]) );
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


procedure TGameInputProcess.CmdTemp(aCommandType:TGameInputCommandType; aPlayerID:integer);
begin
  Assert(aCommandType = gic_TempChangeMyPlayer);
  TakeCommand( MakeCommand(aCommandType, [aPlayerID]) );
end;


procedure TGameInputProcess.CmdText(aCommandType:TGameInputCommandType; aPlayerID:integer; aTime:string; aText:string);
begin
  Assert(aCommandType = gic_TextMessage);
  //TakeCommand( MakeCommand(aCommandType, aPlayerID, aText) );
end;


procedure TGameInputProcess.SaveToFile(aFileName:string);
var i:integer; S:TKMemoryStream;
begin
  S := TKMemoryStream.Create;
  S.Write(integer(REPLAY_VERSION)); //
  S.Write(fCount);
  for i:=1 to fCount do
    S.Write(fQueue[i].Tick, SizeOf(fQueue[i]));

  S.SaveToFile(aFileName);
  S.Free;
end;


procedure TGameInputProcess.LoadFromFile(aFileName:string);
var FileVersion,i:integer; S:TKMemoryStream;
begin
  if not FileExists(aFileName) then exit;
  S := TKMemoryStream.Create;
  S.LoadFromFile(aFileName);
  S.Read(FileVersion);
  Assert(FileVersion=REPLAY_VERSION, 'Old or unexpected replay file. r'+inttostr(FileVersion)+' is required.');
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


procedure TGameInputProcess.StoreCommand(aCommand: TGameInputCommand);
begin
  //Store the command for the replay
  Assert(ReplayState=gipRecording);
  inc(fCount);
  if length(fQueue) <= fCount then setlength(fQueue, fCount+128);

  with fQueue[fCount] do begin
    Tick    := fGame.GetTickCount;
    Command := aCommand;
    Rand    := Random(maxint); //This will be our check to ensure everything is consistent
  end;
end;


procedure TGameInputProcess.Tick(aTick:cardinal);
begin
  if ReplayState = gipReplaying then
  begin
    while (aTick > fQueue[fCursor].Tick) and (fQueue[fCursor].Command.CommandType <> gic_None) do
      inc(fCursor);

    while (aTick = fQueue[fCursor].Tick) do
    begin
      ExecCommand(fQueue[fCursor].Command);
      //CRC check
      if (fQueue[fCursor].Rand <> Cardinal(Random(maxint))) then //This line should always be called to maintain randoms flow
        if CRASH_ON_REPLAY then begin
          fGame.GameError(KMPoint(10,10),'Replay mismatch');
          exit; //GameError calls GIP.Free, so exit immidiately
        end;
      inc(fCursor);
    end;
  end;
end;


end.


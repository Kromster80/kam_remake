unit KM_GameInputProcess;
{$I KaM_Remake.inc}
interface
uses SysUtils, Math, Controls, KromUtils,
    KM_CommonTypes, KM_Defaults, KM_Utils,
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
   - send input through LAN to make multiplayer games }

const MAX_PARAMS = 4;

type TGIPState = (gipRecording, gipReplaying);

type TGameInputCommand = (
  gic_None,
  //I.      Army commands, only warriors (TKMUnitWarrior, OrderInfo)
  gic_ArmyFeed,
  gic_ArmySplit,
  gic_ArmyLink,
  gic_ArmyAttackUnit,
  gic_ArmyAttackHouse,
  gic_ArmyHalt,         //Formation commands
  gic_ArmyWalk,         //Walking

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
  gic_TempKillUnit

  { Optional input }
  //VI.     Viewport settings for replay (location, zoom)
  //VII.    Message queue handling in gameplay interface
  );

type
TGameInputProcess = class
  private
    fCount:integer;
    fCursor:integer; //Used only in gipReplaying
    fQueue: array of packed record
      Tick:cardinal;
      Command:TGameInputCommand;
      Params:array[1..MAX_PARAMS]of integer;
      Rand:cardinal;
    end;
    fState:TGIPState;
    procedure SaveCommand(aGIC:TGameInputCommand; aParam1:integer=maxint; aParam2:integer=maxint; aParam3:integer=maxint; aParam4:integer=maxint);
    procedure ExecCommand(aIndex:integer);
  public
    constructor Create(aState:TGIPState);
    destructor Destroy; override;
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SaveToFile(aFileName:string);
    procedure LoadFromFile(aFileName:string);

    procedure CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand); overload;
    procedure CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aUnit:TKMUnit); overload;
    procedure CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aHouse:TKMHouse); overload;
    procedure CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aTurnAmount:shortint; aLineAmount:shortint); overload;
    procedure CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aLoc:TKMPoint; aDirection:TKMDirection=dir_NA); overload;

    procedure CmdBuild(aCommand:TGameInputCommand; aLoc:TKMPoint); overload;
    procedure CmdBuild(aCommand:TGameInputCommand; aLoc:TKMPoint; aHouse:THouseType); overload;

    procedure CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand); overload;
    procedure CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand; aItem, aAmount:integer); overload;
    procedure CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand; aItem:TResourceType); overload;
    procedure CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand; aUnitType:TUnitType); overload;
    procedure CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand; aItem:integer); overload;

    procedure CmdRatio(aCommand:TGameInputCommand; aRes:TResourceType; aHouse:THouseType; aValue:integer);

    procedure CmdTemp(aCommand:TGameInputCommand; aUnit:TKMUnit); overload;
    procedure CmdTemp(aCommand:TGameInputCommand; aLoc:TKMPoint); overload;

    procedure Tick(aTick:cardinal);

    property Count:integer read fCount;
    property State:TGIPState read fState;
    function GetLastTick():integer;
    function Ended():boolean;
end;



implementation
uses KM_Sound, KM_Game, KM_PlayersCollection;


constructor TGameInputProcess.Create(aState:TGIPState);
begin
  Inherited Create;
  setlength(fQueue, 128);
  fCount := 0;
  fCursor := 1;
  fState := aState;
end;


destructor TGameInputProcess.Destroy;
begin
  Inherited;
end;


procedure TGameInputProcess.Save(SaveStream:TKMemoryStream);
var i,k:integer;
begin
  SaveStream.Write('Game input process data');
  SaveStream.Write(fCount);
  for i:=1 to fCount do begin
    SaveStream.Write(fQueue[i].Tick);
    SaveStream.Write(fQueue[i].Command, SizeOf(fQueue[i].Command));
    for k:=1 to MAX_PARAMS do
      SaveStream.Write(fQueue[i].params[k]);
    SaveStream.Write(fQueue[i].Rand);
  end;
end;


procedure TGameInputProcess.Load(LoadStream:TKMemoryStream);
var i,k:integer; s:string;
begin
  LoadStream.Read(s);
  Assert(s='Game input process data');
  LoadStream.Read(fCount);
  setlength(fQueue, fCount+1);
  for i:=1 to fCount do begin
    LoadStream.Read(fQueue[i].Tick);
    LoadStream.Read(fQueue[i].Command, SizeOf(fQueue[i].Command));
    for k:=1 to MAX_PARAMS do
      LoadStream.Read(fQueue[i].params[k]);
    LoadStream.Read(fQueue[i].Rand);
  end;
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


procedure TGameInputProcess.SaveCommand(aGIC:TGameInputCommand; aParam1:integer=maxint; aParam2:integer=maxint; aParam3:integer=maxint; aParam4:integer=maxint);
begin
  Assert(fState=gipRecording);
  inc(fCount);
  if length(fQueue) <= fCount then setlength(fQueue, fCount+128);

  with fQueue[fCount] do begin
    Tick    := fGame.GetTickCount;
    Command := aGIC;
    Params[1] := aParam1;
    Params[2] := aParam2;
    Params[3] := aParam3;
    Params[4] := aParam4;
    Rand := Random(maxint); //This will be our check to ensure everything is consistent
  end;
end;


procedure TGameInputProcess.ExecCommand(aIndex:integer);
var H:TKMHouse;
begin
  Assert(fState=gipReplaying);
  with fQueue[aIndex] do
  case Command of
    gic_ArmyFeed:         TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).Split;
    gic_ArmySplit:        TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).OrderFood;
    gic_ArmyLink:         TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).LinkTo(TKMUnitWarrior(fPlayers.GetUnitByID(Params[2])));
    gic_ArmyAttackUnit:   TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).GetCommander.PlaceOrder(wo_Attack, fPlayers.GetUnitByID(Params[2]));
    gic_ArmyAttackHouse:  TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).GetCommander.PlaceOrder(wo_AttackHouse, fPlayers.GetHouseByID(Params[2]));
    gic_ArmyHalt:         TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).Halt(Params[2],Params[3]);
    gic_ArmyWalk:         TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).GetCommander.PlaceOrder(wo_Walk, KMPoint(Params[2],Params[3]), TKMDirection(Params[4]));

    gic_BuildRoadPlan:    MyPlayer.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_RoadPlan,  false, MyPlayer.PlayerID);
    gic_BuildFieldPlan:   MyPlayer.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_FieldPlan,  false, MyPlayer.PlayerID);
    gic_BuildWinePlan:    MyPlayer.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_WinePlan,  false, MyPlayer.PlayerID);
    gic_BuildWallPlan:    MyPlayer.AddRoadPlan(KMPoint(Params[1],Params[2]), mu_WallPlan,  false, MyPlayer.PlayerID);
    gic_BuildRemovePlan:  MyPlayer.RemPlan(KMPoint(Params[1],Params[2]));
    gic_BuildRemoveHouse: MyPlayer.RemHouse(KMPoint(Params[1],Params[2]), false);
    gic_BuildHousePlan:   MyPlayer.AddHousePlan(THouseType(Params[1]), KMPoint(Params[2],Params[3]), MyPlayer.PlayerID);

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

    gic_RatioChange:            MyPlayer.fMissionSettings.SetRatio(TResourceType(Params[1]), THouseType(Params[2]), Params[3]);

    gic_TempAddScout:           MyPlayer.AddUnit(ut_HorseScout, KMPoint(Params[1],Params[2]));
    gic_TempKillUnit:           MyPlayer.GetUnitByID(Params[1]).KillUnit;

    else Assert(false);
  end;

  if (fQueue[aIndex].Rand <> Cardinal(Random(maxint))) then //This line should always be called to maintain randoms flow
    if CRASH_ON_REPLAY then begin
      //fGame.GameError(KMPoint(10,10),'Replay mismatch');
      Assert(false);
    end;
end;


procedure TGameInputProcess.CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand);
begin
  Assert(aCommand in [gic_ArmyFeed, gic_ArmySplit]);
  case aCommand of
    gic_ArmyFeed:  aWarrior.OrderFood;
    gic_ArmySplit: aWarrior.Split;
  end;

  SaveCommand(aCommand, aWarrior.ID);
end;


procedure TGameInputProcess.CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aUnit:TKMUnit);
begin
  Assert(aCommand in [gic_ArmyLink, gic_ArmyAttackUnit]);
  case aCommand of
    gic_ArmyLink:       begin
                          aWarrior.LinkTo(TKMUnitWarrior(aUnit));
                          fSoundLib.PlayWarrior(aWarrior.UnitType, sp_Join);
                        end;
    gic_ArmyAttackUnit: begin
                          aWarrior.GetCommander.PlaceOrder(wo_Attack, aUnit);
                          fSoundLib.PlayWarrior(aWarrior.UnitType, sp_Attack);
                        end;
  end;

  SaveCommand(aCommand, aWarrior.ID, aUnit.ID);
end;


procedure TGameInputProcess.CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aHouse:TKMHouse);
begin
  Assert(aCommand = gic_ArmyAttackHouse);
  aWarrior.GetCommander.PlaceOrder(wo_AttackHouse, aHouse);
  fSoundLib.PlayWarrior(aWarrior.UnitType, sp_Attack);
  SaveCommand(aCommand, aWarrior.ID, aHouse.ID);
end;


procedure TGameInputProcess.CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aTurnAmount:shortint; aLineAmount:shortint);
begin
  Assert(aCommand = gic_ArmyHalt);
  aWarrior.Halt(aTurnAmount, aLineAmount);
  SaveCommand(aCommand, aWarrior.ID, aTurnAmount, aLineAmount);
end;


procedure TGameInputProcess.CmdArmy(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aLoc:TKMPoint; aDirection:TKMDirection=dir_NA);
begin
  Assert(aCommand = gic_ArmyWalk);
  aWarrior.GetCommander.PlaceOrder(wo_Walk, aLoc, aDirection);
  fSoundLib.PlayWarrior(aWarrior.UnitType, sp_Move);
  SaveCommand(aCommand, aWarrior.ID, aLoc.X, aLoc.Y, integer(aDirection));
end;


procedure TGameInputProcess.CmdBuild(aCommand:TGameInputCommand; aLoc:TKMPoint);
begin
  case aCommand of
    gic_BuildRoadPlan:    MyPlayer.AddRoadPlan(aLoc, mu_RoadPlan,  false, MyPlayer.PlayerID);
    gic_BuildFieldPlan:   MyPlayer.AddRoadPlan(aLoc, mu_FieldPlan, false, MyPlayer.PlayerID);
    gic_BuildWinePlan:    MyPlayer.AddRoadPlan(aLoc, mu_WinePlan,  false, MyPlayer.PlayerID);
    gic_BuildWallPlan:    MyPlayer.AddRoadPlan(aLoc, mu_WallPlan,  false, MyPlayer.PlayerID);
    gic_BuildRemovePlan:  MyPlayer.RemPlan(aLoc);
    gic_BuildRemoveHouse: MyPlayer.RemHouse(aLoc, false);
    else Assert(false, 'Unknown CmdBuild');
  end;

  SaveCommand(aCommand, aLoc.X, aLoc.Y);
end;


procedure TGameInputProcess.CmdBuild(aCommand:TGameInputCommand; aLoc:TKMPoint; aHouse:THouseType);
begin
  Assert(aCommand = gic_BuildHousePlan);
  MyPlayer.AddHousePlan(aHouse, aLoc, MyPlayer.PlayerID);
  SaveCommand(aCommand, integer(aHouse), aLoc.X, aLoc.Y);
end;


procedure TGameInputProcess.CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand);
begin
  case aCommand of
    gic_HouseRepairToggle:   aHouse.RepairToggle;
    gic_HouseDeliveryToggle: aHouse.WareDelivery := not aHouse.WareDelivery;
    else Assert(false, 'Unknown CmdHouse');
  end;
  SaveCommand(aCommand, aHouse.ID);
end;


procedure TGameInputProcess.CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand; aItem, aAmount:integer);
begin
  Assert(aCommand = gic_HouseOrderProduct);
  aHouse.ResEditOrder(aItem, aAmount);
  SaveCommand(aCommand, aHouse.ID, aItem, aAmount);
end;


procedure TGameInputProcess.CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand; aItem:TResourceType);
begin
  Assert(aCommand = gic_HouseStoreAcceptFlag);
  TKMHouseStore(aHouse).ToggleAcceptFlag(aItem);
  SaveCommand(aCommand, aHouse.ID, integer(aItem));
end;


procedure TGameInputProcess.CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand; aUnitType:TUnitType);
begin
  Assert(aCommand = gic_HouseTrain);
  //It might not be the best idea to rely on GetHouseType here,
  //but it will serve as additional data integrity check. Besides it looks better than pile of IF ELSEs
  case aHouse.GetHouseType of
    ht_Barracks:  TKMHouseBarracks(aHouse).Equip(aUnitType);
    ht_School:    TKMHouseSchool(aHouse).AddUnitToQueue(aUnitType);
    else          Assert(false, 'Only Schools and Barracks supported yet');
  end;
  SaveCommand(aCommand, aHouse.ID, integer(aUnitType));
end;


procedure TGameInputProcess.CmdHouse(aHouse:TKMHouse; aCommand:TGameInputCommand; aItem:integer);
begin
  Assert(aCommand = gic_HouseRemoveTrain);
  Assert(aHouse is TKMHouseSchool);
  TKMHouseSchool(aHouse).RemUnitFromQueue(aItem);
  SaveCommand(aCommand, aHouse.ID, aItem);
end;


procedure TGameInputProcess.CmdRatio(aCommand:TGameInputCommand; aRes:TResourceType; aHouse:THouseType; aValue:integer);
begin
  Assert(aCommand = gic_RatioChange);
  MyPlayer.fMissionSettings.SetRatio(aRes, aHouse, aValue);
  SaveCommand(aCommand, integer(aRes), integer(aHouse), aValue);
end;


procedure TGameInputProcess.CmdTemp(aCommand:TGameInputCommand; aUnit:TKMUnit);
begin
  Assert(aCommand = gic_TempKillUnit);
  aUnit.KillUnit;
  SaveCommand(aCommand, aUnit.ID);
end;


procedure TGameInputProcess.CmdTemp(aCommand:TGameInputCommand; aLoc:TKMPoint);
begin
  Assert(aCommand = gic_TempAddScout);
  MyPlayer.AddUnit(ut_HorseScout, aLoc);
  SaveCommand(aCommand, aLoc.X, aLoc.Y);
end;


procedure TGameInputProcess.Tick(aTick:cardinal);
begin
  while (aTick > fQueue[fCursor].Tick) and (fQueue[fCursor].Command <> gic_None) do
    inc(fCursor);

  while (aTick = fQueue[fCursor].Tick) do begin
    ExecCommand(fCursor);
    if fGame.GameState <> gsReplay then exit; //GameError during last command calls GIP.Free, exit immidiately
    inc(fCursor);
  end;
end;


{ Return last recorded tick }
function TGameInputProcess.GetLastTick:integer;
begin
  Result := fQueue[fCount].Tick;
end;


{ See if replay has ended (no more commands in queue) }
function TGameInputProcess.Ended():boolean;
begin
  Result := fCursor > fCount;
end;


end.


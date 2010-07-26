unit KM_GameInputProcess;
{$I KaM_Remake.inc}
interface
uses SysUtils, Math, Controls, KromUtils,
    KM_CommonTypes, KM_Defaults, KM_Utils,
    KM_Houses, KM_Units, KM_Units_Warrior
    ;

{ YET UNUSED, JUST AN IDEA}

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

  Player commands are:
 +I.   Unit commands, only warriors (TKMUnitWarrior, OrderInfo)
  II.  House production orders (TKMHouse, PlaceOrder(warfare, troops, citizens))
  III. House repair/delivery options (TKMHouse, Toggle(repair, delivery, storehouse))
 +IV.  Building/road plans (build what, Location)
  V.   Delivery ratios
  VI.  Cheatcodes affecting gameplay (goods, props)

  }

type TBuildOrder = (bo_RoadPlan, bo_FieldPlan, bo_WinePlan, bo_WallPlan, bo_RemovePlan, bo_RemoveHouse);

type TGameInputCommand = (
  gic_BuildRoadPlan,
  gic_BuildFieldPlan,
  gic_BuildWinePlan,
  gic_BuildWallPlan,
  gic_BuildRemovePlan,  //Removal of a plan
  gic_BuildRemoveHouse, //Removal of house
  gic_BuildHousePlan,   //Build HouseType

  gic_ArmyFeed,
  gic_ArmySplit,
  gic_ArmyLink,
  gic_ArmyAttackUnit,
  gic_ArmyAttackHouse,
  gic_ArmyHouse,        //House-dependant command
  gic_ArmyHalt,         //Formation commands
  gic_ArmyWalk          //Walking

  //gic_House

  //gic_Settings
  );

type
TGameInputProcess = class
  private
    fCount:integer;
    fCursor:integer;
    fQueue: array of packed record
      Tick:cardinal;
      Command:TGameInputCommand;
      params:array[1..8]of integer;
    end;

    procedure SaveCommand(aGIC:TGameInputCommand; aParam1:integer=maxint; aParam2:integer=maxint; aParam3:integer=maxint; aParam4:integer=maxint);
    procedure ExecCommand(aIndex:integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile();
    procedure LoadFromFile();
    procedure BuildCommand(aOrder:TBuildOrder; aLoc:TKMPoint); overload;
    procedure BuildCommand(aHouse:THouseType; aLoc:TKMPoint); overload;
    procedure WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand); overload;
    procedure WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aUnit:TKMUnit); overload;
    procedure WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aHouse:TKMHouse); overload;
    procedure WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aTurnAmount:shortint; aLineAmount:shortint); overload;
    procedure WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aLoc:TKMPoint; aDirection:TKMDirection=dir_NA); overload;

    procedure Tick(aTick:integer);
end;



implementation
uses KM_Terrain, KM_Unit1, KM_Sound, KM_Game, KM_PlayersCollection;


constructor TGameInputProcess.Create;
begin
  Inherited;
  setlength(fQueue, 1024);
  fCount := 0;
  fCursor := 1;
end;


destructor TGameInputProcess.Destroy;
begin
  Inherited;
end;


procedure TGameInputProcess.SaveToFile();
var f:file; i:integer;
begin
  AssignFile(f, ExeDir+'Log.sav');
  Rewrite(f, 1);
  BlockWrite(f, fCount, 4);
  for i:=1 to fCount do
    BlockWrite(f, fQueue[i].Tick, SizeOf(fQueue[i]));
  CloseFile(f);
end;


procedure TGameInputProcess.LoadFromFile();
var f:file; i,NumRead:integer;
begin
  if not FileExists(ExeDir+'Log.sav') then exit;
  AssignFile(f, ExeDir+'Log.sav');
  Reset(f, 1);
  BlockRead(f, fCount, 4, NumRead);
  if NumRead=0 then begin
    CloseFile(f);
    exit;
  end;
  for i:=1 to fCount do
    BlockRead(f, fQueue[i].Tick, SizeOf(fQueue[i]));
  CloseFile(f);
end;


procedure TGameInputProcess.SaveCommand(aGIC:TGameInputCommand; aParam1:integer=maxint; aParam2:integer=maxint; aParam3:integer=maxint; aParam4:integer=maxint);
begin
  inc(fCount);
  if length(fQueue) <= fCount then setlength(fQueue, fCount+128);

  with fQueue[fCount] do begin
    Tick    := fGame.GetTickCount;
    Command := aGIC;
    Params[1] := aParam1;
    Params[2] := aParam2;
    Params[3] := aParam3;
    Params[4] := aParam4;
  end;
end;


procedure TGameInputProcess.ExecCommand(aIndex:integer);
begin
  with fQueue[aIndex] do
  case Command of
    gic_ArmyHalt:   TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).Halt(Params[2],Params[3]);
    gic_ArmyWalk:   TKMUnitWarrior(MyPlayer.GetUnitByID(Params[1])).GetCommander.PlaceOrder(wo_Walk, KMPoint(Params[2],Params[3]), TKMDirection(Params[4]));
    else Assert(false);
  end;
end;


procedure TGameInputProcess.BuildCommand(aOrder:TBuildOrder; aLoc:TKMPoint);
begin
  case aOrder of
    bo_RoadPlan:    MyPlayer.AddRoadPlan(aLoc, mu_RoadPlan,  false, MyPlayer.PlayerID);
    bo_FieldPlan:   MyPlayer.AddRoadPlan(aLoc, mu_FieldPlan, false, MyPlayer.PlayerID);
    bo_WinePlan:    MyPlayer.AddRoadPlan(aLoc, mu_WinePlan,  false, MyPlayer.PlayerID);
    bo_WallPlan:    MyPlayer.AddRoadPlan(aLoc, mu_WallPlan,  false, MyPlayer.PlayerID);
    bo_RemovePlan:  MyPlayer.RemPlan(aLoc);
    bo_RemoveHouse: MyPlayer.RemHouse(aLoc, false);
    else Assert(false, 'Unknown BuildCommand');
  end;

  case aOrder of
    bo_RoadPlan:    SaveCommand(gic_BuildRoadPlan, aLoc.X, aLoc.Y);
    bo_FieldPlan:   SaveCommand(gic_BuildFieldPlan, aLoc.X, aLoc.Y);
    bo_WinePlan:    SaveCommand(gic_BuildWinePlan, aLoc.X, aLoc.Y);
    bo_WallPlan:    SaveCommand(gic_BuildWallPlan, aLoc.X, aLoc.Y);
    bo_RemovePlan:  SaveCommand(gic_BuildRemovePlan, aLoc.X, aLoc.Y);
    bo_RemoveHouse: SaveCommand(gic_BuildRemoveHouse, aLoc.X, aLoc.Y);
    else Assert(false, 'Unknown BuildCommand');
  end;
end;


procedure TGameInputProcess.BuildCommand(aHouse:THouseType; aLoc:TKMPoint);
begin
  MyPlayer.AddHousePlan(aHouse, aLoc, MyPlayer.PlayerID);
  SaveCommand(gic_BuildHousePlan, aLoc.X, aLoc.Y, integer(aHouse));
end;


procedure TGameInputProcess.WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand);
begin
  Assert(aCommand in [gic_ArmyFeed, gic_ArmySplit]);
  case aCommand of
    gic_ArmyFeed:  aWarrior.OrderFood;
    gic_ArmySplit: aWarrior.Split;
  end;

  SaveCommand(aCommand, aWarrior.ID);
end;


procedure TGameInputProcess.WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aUnit:TKMUnit);
begin
  Assert(aCommand in [gic_ArmyLink, gic_ArmyAttackUnit]);
  case aCommand of
    gic_ArmyLink:       aWarrior.LinkTo(TKMUnitWarrior(aUnit));
    gic_ArmyAttackUnit: aWarrior.GetCommander.PlaceOrder(wo_Attack, aUnit);
  end;

  SaveCommand(aCommand, aWarrior.ID, aUnit.ID);
end;


procedure TGameInputProcess.WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aHouse:TKMHouse);
begin
  Assert(aCommand = gic_ArmyAttackHouse);
  aWarrior.GetCommander.PlaceOrder(wo_Attack, aHouse);
  SaveCommand(aCommand, aWarrior.ID, aHouse.ID);
end;


procedure TGameInputProcess.WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aTurnAmount:shortint; aLineAmount:shortint);
begin
  Assert(aCommand = gic_ArmyHalt);
  aWarrior.Halt(aTurnAmount, aLineAmount);
  SaveCommand(aCommand, aWarrior.ID, aTurnAmount, aLineAmount);
end;


procedure TGameInputProcess.WarriorCommand(aWarrior:TKMUnitWarrior; aCommand:TGameInputCommand; aLoc:TKMPoint; aDirection:TKMDirection=dir_NA);
begin
  Assert(aCommand = gic_ArmyWalk);
  aWarrior.GetCommander.PlaceOrder(wo_Walk, aLoc, aDirection);
  SaveCommand(aCommand, aWarrior.ID, aLoc.X, aLoc.Y, integer(aDirection));
end;


procedure TGameInputProcess.Tick(aTick:integer);
begin
  while (aTick = fQueue[fCursor].Tick) do begin
    ExecCommand(fCursor);
    inc(fCursor);
  end;
end;



end.


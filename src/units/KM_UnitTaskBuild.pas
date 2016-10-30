unit KM_UnitTaskBuild;
{$I KaM_Remake.inc}
interface
uses
  SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Houses, KM_Terrain, KM_Units, KM_ResHouses;


//Do the building
type
  TTaskBuild = class(TUnitTask)
  public
    procedure CancelThePlan; virtual; abstract;
  end;

  TTaskBuildRoad = class(TTaskBuild)
  private
    fLoc: TKMPoint;
    BuildID: Integer;
    DemandSet: Boolean;
    TileLockSet: Boolean;
  public
    constructor Create(aWorker: TKMUnitWorker; aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TTaskBuildWine = class(TTaskBuild)
  private
    fLoc: TKMPoint;
    BuildID: Integer;
    DemandSet: Boolean;
    TileLockSet: Boolean;
  public
    constructor Create(aWorker: TKMUnitWorker; aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TTaskBuildField = class(TTaskBuild)
  private
    fLoc: TKMPoint;
    BuildID: Integer;
    TileLockSet: Boolean;
  public
    constructor Create(aWorker: TKMUnitWorker; aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TTaskBuildHouseArea = class(TTaskBuild)
  private
    fHouse: TKMHouse;
    fHouseType: THouseType;
    fHouseLoc: TKMPoint;
    BuildID: Integer;
    HouseNeedsWorker: Boolean;
    HouseReadyToBuild: Boolean;
    CellsToDig: array [0..15] of TKMPoint; //max house square is 4*4
    LastToDig: ShortInt;
    function GetHouseEntranceLoc: TKMPoint;
  public
    constructor Create(aWorker: TKMUnitWorker; aHouseType: THouseType; aLoc: TKMPoint; aID:integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Digging: Boolean;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TTaskBuildHouse = class(TUnitTask)
  private
    fHouse: TKMHouse;
    BuildID: Integer;
    BuildFrom: TKMPointDir; //Current WIP location
    Cells: TKMPointDirList; //List of surrounding cells and directions
  public
    constructor Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function WalkShouldAbandon:boolean; override;
    function Execute:TTaskResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

  TTaskBuildHouseRepair = class(TUnitTask)
  private
    fHouse: TKMHouse;
    fRepairID: Integer; //Remember the house we repair to report if we died and let others take our place
    BuildFrom: TKMPointDir; //Current WIP location
    Cells: TKMPointDirList; //List of surrounding cells and directions
  public
    constructor Create(aWorker: TKMUnitWorker; aHouse: TKMHouse; aRepairID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_HandLogistics, KM_HandsCollection, KM_Resource, KM_ResMapElements, KM_ResWares, KM_Game, KM_Hand;


{ TTaskBuildRoad }
constructor TTaskBuildRoad.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
  inherited Create(aWorker);
  fTaskName := utn_BuildRoad;
  fLoc      := aLoc;
  BuildID   := aID;
  DemandSet := False;
  TileLockSet := False;
end;


constructor TTaskBuildRoad.Load(LoadStream:TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fLoc);
  LoadStream.Read(BuildID);
  LoadStream.Read(DemandSet);
  LoadStream.Read(TileLockSet);
end;


destructor TTaskBuildRoad.Destroy;
begin
  if DemandSet   then gHands[fUnit.Owner].Deliveries.Queue.RemDemand(fUnit);
  if TileLockSet then gTerrain.UnlockTile(fLoc);

  //Yet unstarted
  if BuildID <> -1 then
  begin
    if gTerrain.CanAddField(fLoc.X, fLoc.Y, ft_Road) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].BuildList.FieldworksList.ReOpenField(BuildID)
    else
      //This plan is not valid anymore
      gHands[fUnit.Owner].BuildList.FieldworksList.CloseField(BuildID);
  end
  else
    //Autobuild AI should rebuild roads when worker dies (otherwise house is never built)
    if not gGame.IsExiting and gHands[fUnit.Owner].AI.Setup.AutoBuild and (fPhase < 9)
    and gHands[fUnit.Owner].CanAddFieldPlan(fLoc, ft_Road) then
      gHands[fUnit.Owner].BuildList.FieldworksList.AddField(fLoc, ft_Road);

  inherited;
end;


function TTaskBuildRoad.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (BuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ft_Road);
end;


procedure TTaskBuildRoad.CancelThePlan;
begin
  gHands[fUnit.Owner].BuildList.FieldworksList.CloseField(BuildID); //Close the job now because it can no longer be cancelled
  BuildID := -1;
end;


function TTaskBuildRoad.Execute: TTaskResult;
begin
  Result := TaskContinues;

  if WalkShouldAbandon then
  begin
    Result := TaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         SetActionWalkToSpot(fLoc);
         Thought := th_Build;
       end;
    1: begin
         Thought := th_None;
         gTerrain.SetTileLock(fLoc, tlRoadWork);
         TileLockSet := True;

         CancelThePlan;

         gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wt_Stone, 1, dtOnce, diHigh4);
         DemandSet := true;

         SetActionLockedStay(11,ua_Work1,false);
       end;
    2: begin
         gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house) after first dig
         gTerrain.IncDigState(fLoc);
         SetActionLockedStay(11,ua_Work1,false);
       end;
    3: begin
         gTerrain.IncDigState(fLoc);
         SetActionLockedStay(11,ua_Work1,false);
       end;
    4: begin //This step is repeated until Serf brings us some stone
         SetActionLockedStay(30,ua_Work1);
         Thought := th_Stone;
       end;
    5: begin
         SetActionLockedStay(11,ua_Work2,false);
         DemandSet := false;
         Thought := th_None;
       end;
    6: begin
         gTerrain.IncDigState(fLoc);
         SetActionLockedStay(11,ua_Work2,false);
       end;
    7: begin
         gTerrain.IncDigState(fLoc);
         gTerrain.FlattenTerrain(fLoc); //Flatten the terrain slightly on and around the road
         if MapElem[gTerrain.Land[fLoc.Y,fLoc.X].Obj].WineOrCorn then
           gTerrain.RemoveObject(fLoc); //Remove corn/wine/grass as they won't fit with road
         SetActionLockedStay(11,ua_Work2,false);
       end;
    8: begin
         gTerrain.SetField(fLoc, Owner, ft_Road);
         gTerrain.RemoveObjectsKilledByRoad(fLoc);
         SetActionStay(5, ua_Walk);
         gTerrain.UnlockTile(fLoc);
         TileLockSet := False;
       end;
    else Result := TaskDone;
  end;
  if fPhase<>4 then inc(fPhase); //Phase=4 is when worker waits for rt_Stone
end;


procedure TTaskBuildRoad.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fLoc);
  SaveStream.Write(BuildID);
  SaveStream.Write(DemandSet);
  SaveStream.Write(TileLockSet);
end;


{ TTaskBuildWine }
constructor TTaskBuildWine.Create(aWorker: TKMUnitWorker; aLoc: TKMPoint; aID: Integer);
begin
  inherited Create(aWorker);
  fTaskName := utn_BuildWine;
  fLoc      := aLoc;
  BuildID   := aID;
  DemandSet := False;
  TileLockSet := False;
end;


constructor TTaskBuildWine.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fLoc);
  LoadStream.Read(BuildID);
  LoadStream.Read(DemandSet);
  LoadStream.Read(TileLockSet);
end;


destructor TTaskBuildWine.Destroy;
begin
  //Yet unstarted
  if BuildID <> -1 then
    if gTerrain.CanAddField(fLoc.X, fLoc.Y, ft_Wine) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].BuildList.FieldworksList.ReOpenField(BuildID)
    else
      //This plan is not valid anymore
      gHands[fUnit.Owner].BuildList.FieldworksList.CloseField(BuildID);

  if DemandSet   then gHands[fUnit.Owner].Deliveries.Queue.RemDemand(fUnit);
  if TileLockSet then gTerrain.UnlockTile(fLoc);
  inherited;
end;


function TTaskBuildWine.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (BuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ft_Wine);
end;


procedure TTaskBuildWine.CancelThePlan;
begin
  gHands[fUnit.Owner].BuildList.FieldworksList.CloseField(BuildID); //Close the job now because it can no longer be cancelled
  BuildID := -1;
end;


function TTaskBuildWine.Execute: TTaskResult;
begin
  Result := TaskContinues;

  if WalkShouldAbandon then
  begin
    Result := TaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
   0: begin
        SetActionWalkToSpot(fLoc);
        Thought := th_Build;
      end;
   1: begin
        Thought := th_None;
        gTerrain.SetTileLock(fLoc, tlFieldWork);
        TileLockSet := True;

        CancelThePlan;

        gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)

        gHands[Owner].Deliveries.Queue.AddDemand(nil,fUnit,wt_Wood, 1, dtOnce, diHigh4);
        DemandSet := true;

        SetActionLockedStay(12*4,ua_Work1,false);
      end;
   2: begin
        gTerrain.IncDigState(fLoc);
        SetActionLockedStay(24,ua_Work1,false);
      end;
   3: begin
        gTerrain.IncDigState(fLoc);
        SetActionLockedStay(24,ua_Work1,false);
      end;
   4: begin
        gTerrain.ResetDigState(fLoc);
        gTerrain.SetField(fLoc, Owner, ft_InitWine); //Replace the terrain, but don't seed grapes yet
        SetActionLockedStay(30, ua_Work1);
        Thought := th_Wood;
      end;
   5: begin //This step is repeated until Serf brings us some wood
        SetActionLockedStay(30, ua_Work1);
        Thought := th_Wood;
      end;
   6: begin
        DemandSet := false;
        SetActionLockedStay(11*8, ua_Work2, False);
        Thought := th_None;
      end;
   7: begin
        gTerrain.SetField(fLoc, Owner, ft_Wine);
        SetActionStay(5, ua_Walk);
        gTerrain.UnlockTile(fLoc);
        TileLockSet := False;
      end;
   else Result := TaskDone;
  end;
  if fPhase<>5 then inc(fPhase); //Phase=5 is when worker waits for rt_Wood
end;


procedure TTaskBuildWine.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fLoc);
  SaveStream.Write(BuildID);
  SaveStream.Write(DemandSet);
  SaveStream.Write(TileLockSet);
end;


{ TTaskBuildField }
constructor TTaskBuildField.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
  inherited Create(aWorker);
  fTaskName := utn_BuildField;
  fLoc      := aLoc;
  BuildID   := aID;
  TileLockSet := False;
end;


constructor TTaskBuildField.Load(LoadStream:TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fLoc);
  LoadStream.Read(BuildID);
  LoadStream.Read(TileLockSet);
end;


destructor TTaskBuildField.Destroy;
begin
  //Yet unstarted
  if BuildID <> -1 then
    if gTerrain.CanAddField(fLoc.X, fLoc.Y, ft_Corn) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].BuildList.FieldworksList.ReOpenField(BuildID)
    else
      //This plan is not valid anymore
      gHands[fUnit.Owner].BuildList.FieldworksList.CloseField(BuildID);

  if TileLockSet then gTerrain.UnlockTile(fLoc);
  inherited;
end;


function TTaskBuildField.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (BuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ft_Corn);
end;


procedure TTaskBuildField.CancelThePlan;
begin
  gHands[fUnit.Owner].BuildList.FieldworksList.CloseField(BuildID); //Close the job now because it can no longer be cancelled
  BuildID := -1;
end;


function TTaskBuildField.Execute: TTaskResult;
begin
  Result := TaskContinues;

  if WalkShouldAbandon then
  begin
    Result := TaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         SetActionWalkToSpot(fLoc);
         Thought := th_Build;
       end;
    1: begin
        gTerrain.SetTileLock(fLoc, tlFieldWork);
        TileLockSet := True;
        CancelThePlan;
        SetActionLockedStay(0,ua_Walk);
       end;
    2: begin
        SetActionLockedStay(11,ua_Work1,false);
        inc(fPhase2);
        if fPhase2 = 2 then gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
        if (fPhase2 = 6) and MapElem[gTerrain.Land[fLoc.Y,fLoc.X].Obj].WineOrCorn then
          gTerrain.RemoveObject(fLoc); //Remove grass/corn/wine as they take up most of the tile
        if fPhase2 in [6,8] then gTerrain.IncDigState(fLoc);
       end;
    3: begin
        Thought := th_None; //Keep thinking build until it's done
        gTerrain.SetField(fLoc,Owner,ft_Corn);
        SetActionStay(5,ua_Walk);
        gTerrain.UnlockTile(fLoc);
        TileLockSet := False;
       end;
    else Result := TaskDone;
  end;
  if fPhase2 in [0,10] then inc(fPhase);
end;


procedure TTaskBuildField.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fLoc);
  SaveStream.Write(BuildID);
  SaveStream.Write(TileLockSet);
end;


{ TTaskBuildHouseArea }
constructor TTaskBuildHouseArea.Create(aWorker: TKMUnitWorker; aHouseType: THouseType; aLoc: TKMPoint; aID: Integer);
var
  I,K: Integer;
  HA: THouseArea;
begin
  inherited Create(aWorker);
  fTaskName  := utn_BuildHouseArea;
  fHouseType := aHouseType;
  fHouseLoc  := aLoc;
  BuildID    := aID;
  HouseNeedsWorker  := False; //House needs this worker to complete
  HouseReadyToBuild := False; //House is ready to be built

  HA := gRes.HouseDat[fHouseType].BuildArea;

  //Fill Cells left->right, top->bottom. Worker will start flattening from the end (reversed)
  LastToDig := -1;
  for I := 1 to 4 do for K := 1 to 4 do
  if HA[I,K] <> 0 then
  begin
    Inc(LastToDig);
    CellsToDig[LastToDig] := KMPoint(fHouseLoc.X + K - 3, fHouseLoc.Y + I - 4);
  end;
end;


constructor TTaskBuildHouseArea.Load(LoadStream:TKMemoryStream);
begin
  inherited;

  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fHouseType, SizeOf(fHouseType));
  LoadStream.Read(fHouseLoc);
  LoadStream.Read(BuildID);
  LoadStream.Read(HouseNeedsWorker);
  LoadStream.Read(HouseReadyToBuild);
  LoadStream.Read(LastToDig);
  LoadStream.Read(CellsToDig, SizeOf(CellsToDig));
end;


procedure TTaskBuildHouseArea.SyncLoad;
begin
  inherited;
  fHouse := gHands.GetHouseByUID(cardinal(fHouse));
end;


{ We need to revert all changes made }
destructor TTaskBuildHouseArea.Destroy;
begin
  //Don't demolish the house when the game is exiting (causes wrong stats and errors in script)
  if (gGame = nil) or gGame.IsExiting then
	  Exit;

  //Yet unstarted
  if (BuildID <> -1) then
    if gTerrain.CanPlaceHouse(GetHouseEntranceLoc,fHouseType) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].BuildList.HousePlanList.ReOpenPlan(BuildID)
    else
    begin
      //This plan is not valid anymore
      gHands[fUnit.Owner].BuildList.HousePlanList.ClosePlan(BuildID);
      gHands[fUnit.Owner].Stats.HousePlanRemoved(fHouseType);
    end;

  //Destroy the house if worker was killed (e.g. by archer or hunger)
  //as we don't have mechanics to resume the building process yet
  if HouseNeedsWorker and (fHouse <> nil) and not fHouse.IsDestroyed then
    fHouse.DemolishHouse(fUnit.Owner);

  //Complete the task in the end (Worker could have died while trying to exit building area)
  if HouseReadyToBuild and not HouseNeedsWorker and (fHouse <> nil) and not fHouse.IsDestroyed then
  begin
    fHouse.BuildingState := hbs_Wood;
    gHands[fUnit.Owner].BuildList.HouseList.AddHouse(fHouse); //Add the house to JobList, so then all workers could take it
    gHands[fUnit.Owner].Deliveries.Queue.AddDemand(fHouse, nil, wt_Wood, gRes.HouseDat[fHouse.HouseType].WoodCost, dtOnce, diHigh4);
    gHands[fUnit.Owner].Deliveries.Queue.AddDemand(fHouse, nil, wt_Stone, gRes.HouseDat[fHouse.HouseType].StoneCost, dtOnce, diHigh4);
  end;

  gHands.CleanUpHousePointer(fHouse);
  inherited;
end;


function TTaskBuildHouseArea.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (BuildID <> -1) and not gTerrain.CanPlaceHouse(GetHouseEntranceLoc, fHouseType);
end;


function TTaskBuildHouseArea.GetHouseEntranceLoc: TKMPoint;
begin
  Result.X := fHouseLoc.X + gRes.HouseDat[fHouseType].EntranceOffsetX;
  Result.Y := fHouseLoc.Y;
end;


//Tell if we are in Digging phase where we can walk on tlDigged tiles
//(incl. phase when we walk out)
function TTaskBuildHouseArea.Digging: Boolean;
begin
  Result := fPhase >= 2;
end;


procedure TTaskBuildHouseArea.CancelThePlan;
begin
  //House plan could be canceled during initial walk or while walking within the house area so
  //ignore it if it's already been canceled (occurs when trying to walk within range of an enemy tower during flattening)
  if BuildID = -1 then Exit;
  gHands[fUnit.Owner].BuildList.HousePlanList.ClosePlan(BuildID);
  gHands[fUnit.Owner].Stats.HousePlanRemoved(fHouseType);
  BuildID := -1;
end;


//Prepare building site - flatten terrain
function TTaskBuildHouseArea.Execute: TTaskResult;
var OutOfWay: TKMPoint;
begin
  Result := TaskContinues;

  if WalkShouldAbandon then
  begin
    Result := TaskDone;
    Exit;
  end;

  if (fHouse <> nil) and fHouse.IsDestroyed then
  begin
    Result := TaskDone;
    fUnit.Thought := th_None;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0:  begin
          SetActionWalkToSpot(GetHouseEntranceLoc);
          Thought := th_Build;
        end;
    1:  begin
          CancelThePlan;
          Assert(fHouse = nil);

          fHouse := gHands[Owner].AddHouseWIP(fHouseType, fHouseLoc);
          Assert(fHouse <> nil, 'Failed to add wip house');
          fHouse := fHouse.GetHousePointer; //We need to register a pointer to the house

          HouseNeedsWorker := True; //The house placed on the map, if something happens with Worker the house will be removed
          SetActionLockedStay(2, ua_Walk);
          Thought := th_None;
        end;
    2:  //The house can become too steep after we flatten one part of it
        if CanWalkTo(CellsToDig[LastToDig], 0) then
          SetActionWalkToSpot(CellsToDig[LastToDig])
        else
        begin
          Result := TaskDone;
          fUnit.Thought := th_None;
          Exit;
        end;
    3:  begin
          SetActionLockedStay(11,ua_Work1,false); //Don't flatten terrain here as we haven't started digging yet
        end;
    4:  begin
          SetActionLockedStay(11,ua_Work1,false);
          gTerrain.FlattenTerrain(CellsToDig[LastToDig]);
        end;
    5:  begin
          SetActionLockedStay(11,ua_Work1,false);
          gTerrain.FlattenTerrain(CellsToDig[LastToDig]);
        end;
    6:  begin
          SetActionLockedStay(11,ua_Work1,false);
          gTerrain.FlattenTerrain(CellsToDig[LastToDig]);
          gTerrain.FlattenTerrain(CellsToDig[LastToDig]); //Flatten the terrain twice now to ensure it really is flat
          gTerrain.SetTileLock(CellsToDig[LastToDig], tlDigged); //Block passability on tile
          if KMSamePoint(fHouse.GetEntrance, CellsToDig[LastToDig]) then
            gTerrain.SetField(fHouse.GetEntrance, Owner, ft_Road);
          gTerrain.RemoveObject(CellsToDig[LastToDig]); //All objects are removed
          Dec(LastToDig);
        end;
    7:  begin
          //Walk away from building site, before we get trapped when house becomes stoned
          OutOfWay := gTerrain.GetOutOfTheWay(fUnit, KMPoint(0,0), tpWalk);
          //GetOutOfTheWay can return the input position (GetPosition in this case) if no others are possible
          if KMSamePoint(OutOfWay, KMPoint(0,0)) or KMSamePoint(OutOfWay, GetPosition) then
            OutOfWay := KMPointBelow(fHouse.GetEntrance); //Don't get stuck in corners
          SetActionWalkToSpot(OutOfWay);
          HouseNeedsWorker := False; //House construction no longer needs the worker to continue
          HouseReadyToBuild := True; //If worker gets killed while walking house will be finished without him
        end;
    else
        Result := TaskDone;
  end;

  Inc(fPhase);

  if (fPhase = 7) and (LastToDig >= 0) then
    fPhase := 2; //Repeat with next cell
end;


procedure TTaskBuildHouseArea.Save(SaveStream:TKMemoryStream);
begin
  inherited;

  if fHouse <> nil then
    SaveStream.Write(fHouse.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fHouseType, SizeOf(fHouseType));
  SaveStream.Write(fHouseLoc);
  SaveStream.Write(BuildID);
  SaveStream.Write(HouseNeedsWorker);
  SaveStream.Write(HouseReadyToBuild);
  SaveStream.Write(LastToDig);
  SaveStream.Write(CellsToDig, SizeOf(CellsToDig));
end;


{ TTaskBuildHouse }
constructor TTaskBuildHouse.Create(aWorker: TKMUnitWorker; aHouse: TKMHouse; aID: Integer);
begin
  inherited Create(aWorker);
  fTaskName := utn_BuildHouse;
  fHouse    := aHouse.GetHousePointer;
  BuildID   := aID;

  Cells := TKMPointDirList.Create;
  fHouse.GetListOfCellsAround(Cells, aWorker.DesiredPassability);
end;


constructor TTaskBuildHouse.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(BuildID);
  LoadStream.Read(BuildFrom);
  Cells := TKMPointDirList.Create;
  Cells.LoadFromStream(LoadStream);
end;


procedure TTaskBuildHouse.SyncLoad;
begin
  inherited;
  fHouse := gHands.GetHouseByUID(Cardinal(fHouse));
end;


destructor TTaskBuildHouse.Destroy;
begin
  //We are no longer connected to the House (it's either done or we died)
  gHands[fUnit.Owner].BuildList.HouseList.RemWorker(BuildID);
  gHands.CleanUpHousePointer(fHouse);
  FreeAndNil(Cells);
  inherited;
end;


{ If we are walking to the house but the house is destroyed/canceled we should abandon immediately
  If house has not enough resource to be built, consider building task is done and look for a new
  task that has enough resouces. Once this house has building resources delivered it will be
  available from build queue again
  If house is already built by other workers}
function TTaskBuildHouse.WalkShouldAbandon: Boolean;
begin
  Result := fHouse.IsDestroyed or (not fHouse.CheckResToBuild) or fHouse.IsComplete;
end;


{Build the house}
function TTaskBuildHouse.Execute: TTaskResult;
begin
  Result := TaskContinues;

  if WalkShouldAbandon then
  begin
    fUnit.Thought := th_None;
    Result := TaskDone;
    Exit;
  end;

  with TKMUnitWorker(fUnit) do
  case fPhase of
    0:  if PickRandomSpot(Cells, BuildFrom) then
        begin
          Thought := th_Build;
          SetActionWalkToSpot(BuildFrom.Loc);
        end
        else
          Result := TaskDone;
    1:  begin
          //Face the building
          Direction := BuildFrom.Dir;
          SetActionLockedStay(0, ua_Walk);
        end;
    2:  begin
          //Start animation
          SetActionLockedStay(5, ua_Work, False);
          Direction := BuildFrom.Dir;
          //Remove house plan when we start the stone phase (it is still required for wood)
          //But don't do it every time we hit if it's already done!
          if fHouse.IsStone and (gTerrain.Land[fHouse.GetPosition.Y, fHouse.GetPosition.X].TileLock <> tlHouse) then
            gTerrain.SetHouse(fHouse.GetPosition, fHouse.HouseType, hsBuilt, Owner);
        end;
    3:  begin
          //Update house on hummer hit
          fHouse.IncBuildingProgress;
          SetActionLockedStay(6, ua_Work, False, 0, 5); //Do building and end animation
          Inc(fPhase2);
        end;
    4:  begin
          SetActionStay(1, ua_Walk);
          Thought := th_None;
        end;
    else Result := TaskDone;
  end;
  Inc(fPhase);

  {Worker does 5 hits from any spot around the house and then goes to new spot,
   but if the house is done worker should stop activity immediately}
  if (fPhase = 4) and (not fHouse.IsComplete) then //If animation cycle is done
    if fPhase2 mod 5 = 0 then //if worker did [5] hits from same spot
      fPhase := 0 //Then goto new spot
    else
      fPhase := 2; //else do more hits
end;


procedure TTaskBuildHouse.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if fHouse <> nil then
    SaveStream.Write(fHouse.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(BuildID);
  SaveStream.Write(BuildFrom);
  Cells.SaveToStream(SaveStream);
end;


{ TTaskBuildHouseRepair }
constructor TTaskBuildHouseRepair.Create(aWorker: TKMUnitWorker; aHouse: TKMHouse; aRepairID: Integer);
begin
  inherited Create(aWorker);
  fTaskName := utn_BuildHouseRepair;
  fHouse    := aHouse.GetHousePointer;
  fRepairID := aRepairID;

  Cells := TKMPointDirList.Create;
  fHouse.GetListOfCellsAround(Cells, aWorker.DesiredPassability);
end;


constructor TTaskBuildHouseRepair.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fRepairID);
  LoadStream.Read(BuildFrom);
  Cells := TKMPointDirList.Create;
  Cells.LoadFromStream(LoadStream);
end;


procedure TTaskBuildHouseRepair.SyncLoad;
begin
  inherited;
  fHouse := gHands.GetHouseByUID(Cardinal(fHouse));
end;


destructor TTaskBuildHouseRepair.Destroy;
begin
  gHands[fUnit.Owner].BuildList.RepairList.RemWorker(fRepairID);
  gHands.CleanUpHousePointer(fHouse);
  FreeAndNil(Cells);
  inherited;
end;


function TTaskBuildHouseRepair.WalkShouldAbandon: Boolean;
begin
  Result := fHouse.IsDestroyed
            or not fHouse.IsDamaged
            or not fHouse.BuildingRepair;
end;


{Repair the house}
function TTaskBuildHouseRepair.Execute: TTaskResult;
begin
  Result := TaskContinues;

  if WalkShouldAbandon then
  begin
    Result := TaskDone;
    Exit;
  end;

  with TKMUnitWorker(fUnit) do
    case fPhase of
      0:  if PickRandomSpot(Cells, BuildFrom) then
          begin
            Thought := th_Build;
            SetActionWalkToSpot(BuildFrom.Loc);
          end
          else
            Result := TaskDone;
      1:  begin
            Direction := BuildFrom.Dir;
            SetActionLockedStay(0, ua_Walk);
          end;
      2:  begin
            SetActionLockedStay(5, ua_Work, false, 0, 0); //Start animation
            Direction := BuildFrom.Dir;
          end;
      3:  begin
            fHouse.AddRepair;
            SetActionLockedStay(6, ua_Work,false, 0, 5); //Do building and end animation
            inc(fPhase2);
          end;
      4:  begin
            Thought := th_None;
            SetActionStay(1, ua_Walk);
          end;
      else
          Result := TaskDone;
    end;
  inc(fPhase);

  if fPhase = 4 then //If animation cycle is done
    if fPhase2 mod 5 = 0 then //if worker did [5] hits from same spot
      fPhase := 0 //Then goto new spot
    else
      fPhase := 2; //else do more hits
end;


procedure TTaskBuildHouseRepair.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if fHouse <> nil then
    SaveStream.Write(fHouse.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fRepairID);
  SaveStream.Write(BuildFrom);
  Cells.SaveToStream(SaveStream);
end;


end.

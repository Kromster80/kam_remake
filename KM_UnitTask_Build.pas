unit KM_UnitTask_Build;
interface
uses KromUtils, KM_CommonTypes, KM_Defaults, KM_Utils, KM_Houses, KM_Units;

{Perform building}
type
  TTaskBuildRoad = class(TUnitTask)
    public
      fLoc:TKMPoint;
      buildID:integer;
      constructor Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;

  TTaskBuildWine = class(TUnitTask)
    public
      fLoc:TKMPoint;
      buildID:integer;
      constructor Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;

  TTaskBuildField = class(TUnitTask)
    public
      fLoc:TKMPoint;
      buildID:integer;
      constructor Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;

  TTaskBuildWall = class(TUnitTask)
    public
      fLoc:TKMPoint;
      buildID:integer;
      constructor Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;

  TTaskBuildHouseArea = class(TUnitTask)
    private
      Step:byte;
      Cells:array[1..4*4]of TKMPoint;
    public
      fHouse:TKMHouse;
      buildID:integer;
      constructor Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad(); override;
      destructor Destroy; override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;

  TTaskBuildHouse = class(TUnitTask)
    private
      CurLoc:byte; //Current WIP location
      Cells:TKMPointDirList; //List of surrounding cells and directions
    public
      fHouse:TKMHouse;
      buildID:integer;
      constructor Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad(); override;
      destructor Destroy; override;
      procedure Abandon(); override;
      function WalkShouldAbandon:boolean; override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;

  TTaskBuildHouseRepair = class(TUnitTask)
    private
      CurLoc:byte; //Current WIP location
      Cells:TKMPointDirList; //List of surrounding cells and directions
    public
      fHouse:TKMHouse;
      buildID:integer;
      constructor Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad(); override;
      destructor Destroy; override;
      function WalkShouldAbandon:boolean; override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;


implementation
uses KM_DeliverQueue, KM_PlayersCollection, KM_Terrain;


{ TTaskBuildRoad }
constructor TTaskBuildRoad.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
  Inherited Create(aWorker);
  fTaskName := utn_BuildRoad;
  fLoc      := aLoc;
  buildID   := aID;
  fUnit.SetActionLockedStay(0,ua_Walk);
end;


constructor TTaskBuildRoad.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fLoc);
  LoadStream.Read(buildID);
end;


procedure TTaskBuildRoad.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fUnit do
case fPhase of
0: begin
     SetActionWalk(fUnit,fLoc);
     Thought := th_Build;
   end;
1: begin
     Thought := th_None;
     fTerrain.SetMarkup(fLoc,mu_UnderConstruction);
     fPlayers.Player[byte(GetOwner)].BuildList.CloseRoad(buildID); //Close the job now because it can no longer be cancelled
     SetActionStay(11,ua_Work1,false);
   end;
2: begin
     fTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house) after first dig
     fTerrain.IncDigState(fLoc);
     SetActionStay(11,ua_Work1,false);
   end;
3: begin
     fTerrain.IncDigState(fLoc);
     SetActionStay(11,ua_Work1,false);
     fPlayers.Player[byte(GetOwner)].DeliverList.AddNewDemand(nil, fUnit, rt_Stone, 1, dt_Once, di_High);
   end;

4: begin
     SetActionStay(30,ua_Work1);
     Thought:=th_Stone;
   end;

5: begin
     SetActionStay(11,ua_Work2,false);
     Thought:=th_None;
   end;
6: begin
     fTerrain.IncDigState(fLoc);
     SetActionStay(11,ua_Work2,false);
   end;
7: begin
     fTerrain.IncDigState(fLoc);
     fTerrain.FlattenTerrain(fLoc); //Flatten the terrain slightly on and around the road
     if MapElem[fTerrain.Land[fLoc.Y,fLoc.X].Obj+1].WineOrCorn then
       fTerrain.Land[fLoc.Y,fLoc.X].Obj:=255; //Remove fields and other quads as they won't fit with road
     SetActionStay(11,ua_Work2,false);
   end;
8: begin
     fTerrain.SetRoad(fLoc,GetOwner);
     SetActionStay(5,ua_Walk);
     fTerrain.RemMarkup(fLoc);
   end;
else TaskDone:=true;
end;
if fPhase<>4 then inc(fPhase); //Phase=4 is when worker waits for rt_Stone
end;


procedure TTaskBuildRoad.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fLoc);
  SaveStream.Write(buildID);
end;


{ TTaskBuildWine }
constructor TTaskBuildWine.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
  Inherited Create(aWorker);
  fTaskName := utn_BuildWine;
  fLoc      := aLoc;
  buildID   := aID;
  fUnit.SetActionLockedStay(0,ua_Walk);
end;


constructor TTaskBuildWine.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fLoc);
  LoadStream.Read(buildID);
end;


procedure TTaskBuildWine.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fUnit do
case fPhase of
 0: begin
      SetActionWalk(fUnit,fLoc);
      Thought := th_Build;
    end;
 1: begin
      Thought := th_None;
      fTerrain.SetMarkup(fLoc,mu_UnderConstruction);
      fTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
      fPlayers.Player[byte(GetOwner)].BuildList.CloseRoad(buildID); //Close the job now because it can no longer be cancelled
      SetActionStay(12*4,ua_Work1,false);
    end;
 2: begin
      fTerrain.IncDigState(fLoc);
      SetActionStay(24,ua_Work1,false);
    end;
 3: begin
      fTerrain.IncDigState(fLoc);
      SetActionStay(24,ua_Work1,false);
      fPlayers.Player[byte(GetOwner)].DeliverList.AddNewDemand(nil,fUnit,rt_Wood, 1, dt_Once, di_High);
    end;
 4: begin
      fTerrain.ResetDigState(fLoc);
      fTerrain.SetField(fLoc,GetOwner,ft_InitWine);
      SetActionStay(30,ua_Work1);
      Thought:=th_Wood;
    end;
 5: begin
      SetActionStay(11*8,ua_Work2,false);
      Thought:=th_None;
    end;
 6: begin
      fTerrain.SetField(fLoc,GetOwner,ft_Wine);
      SetActionStay(5,ua_Walk);
      fTerrain.RemMarkup(fLoc);
    end;
 else TaskDone:=true;
end;
if fPhase<>4 then inc(fPhase); //Phase=4 is when worker waits for rt_Wood
end;


procedure TTaskBuildWine.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fLoc);
  SaveStream.Write(buildID);
end;


{ TTaskBuildField }
constructor TTaskBuildField.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
  Inherited Create(aWorker);
  fTaskName := utn_BuildField;
  fLoc      := aLoc;
  buildID   := aID;
  fUnit.SetActionLockedStay(0,ua_Walk);
end;


constructor TTaskBuildField.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fLoc);
  LoadStream.Read(buildID);
end;


procedure TTaskBuildField.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fUnit do
case fPhase of
  0: begin
       SetActionWalk(fUnit,fLoc);
       Thought := th_Build;
     end;
  1: begin
      fTerrain.SetMarkup(fLoc,mu_UnderConstruction);
      fPlayers.Player[byte(GetOwner)].BuildList.CloseRoad(buildID); //Close the job now because it can no longer be cancelled
      SetActionLockedStay(0,ua_Walk);
     end;
  2: begin
      SetActionStay(11,ua_Work1,false);
      inc(fPhase2);
      if fPhase2 = 2 then fTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
      if fPhase2 in [6,8] then fTerrain.IncDigState(fLoc);
     end;
  3: begin
      Thought := th_None; //Keep thinking build until it's done
      fTerrain.SetField(fLoc,GetOwner,ft_Corn);
      SetActionStay(5,ua_Walk);
      fTerrain.RemMarkup(fLoc);
     end;
  else TaskDone:=true;
end;
if fPhase2 in [0,10] then inc(fPhase);
end;


procedure TTaskBuildField.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fLoc);
  SaveStream.Write(buildID);
end;


{ TTaskBuildWall }
constructor TTaskBuildWall.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
  Inherited Create(aWorker);
  fTaskName := utn_BuildWall;
  fLoc      := aLoc;
  buildID   := aID;
  fUnit.SetActionLockedStay(0,ua_Walk);
end;


constructor TTaskBuildWall.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fLoc);
  LoadStream.Read(buildID);
end;


procedure TTaskBuildWall.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fUnit do
case fPhase of
  0: begin
       SetActionWalk(fUnit,fLoc);
       Thought := th_Build;
     end;
  1: begin
      fTerrain.SetMarkup(fLoc,mu_UnderConstruction);
      fTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
      fPlayers.Player[byte(GetOwner)].BuildList.CloseRoad(buildID); //Close the job now because it can no longer be cancelled
      SetActionLockedStay(0,ua_Walk);
     end;
  2: begin
      fTerrain.IncDigState(fLoc);
      SetActionStay(22,ua_Work1,false);
    end;
  3: begin
      fTerrain.IncDigState(fLoc);
      SetActionStay(22,ua_Work1,false);
      fPlayers.Player[byte(GetOwner)].DeliverList.AddNewDemand(nil, fUnit, rt_Wood, 1, dt_Once, di_High);
    end;
  4: begin
      SetActionStay(30,ua_Work1);
      Thought:=th_Wood;
    end;
  5: begin
      Thought := th_None;
      SetActionStay(22,ua_Work2,false);
    end;
  6: begin
      fTerrain.ResetDigState(fLoc);
      fTerrain.IncDigState(fLoc);
      SetActionStay(22,ua_Work2,false);
    end;
    //Ask for 2 more wood now
    //@Lewin: It's yet incomplete
  7: begin
      //Walk away from tile and continue building from the side
      SetActionWalk(fUnit,fTerrain.GetOutOfTheWay(fUnit.GetPosition,KMPoint(0,0),GetDesiredPassability));
    end;
  8: begin
      //fTerrain.IncWallState(fLoc);
      SetActionStay(11,ua_Work,false);
    end;
  9: begin
      fTerrain.SetWall(fLoc,GetOwner);
      SetActionLockedStay(0,ua_Work);
      fTerrain.RemMarkup(fLoc);
     end;
  else TaskDone:=true;
end;
if (fPhase<>4)and(fPhase<>8) then inc(fPhase); //Phase=4 is when worker waits for rt_Wood
if fPhase=8 then inc(fPhase2);
if fPhase2=5 then inc(fPhase); //wait 5 cycles
end;


procedure TTaskBuildWall.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fLoc);
  SaveStream.Write(buildID);
end;


{ TTaskBuildHouseArea }
constructor TTaskBuildHouseArea.Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
var i,k:integer;
begin
  Inherited Create(aWorker);
  fTaskName := utn_BuildHouseArea;
  fHouse    := aHouse.GetSelf;
  buildID   := aID;
  Step      := 0;
  for i := 1 to 4 do for k := 1 to 4 do
  if HousePlanYX[byte(fHouse.GetHouseType),i,k] <> 0 then begin
    inc(Step);
    Cells[Step] := KMPoint(fHouse.GetPosition.X + k - 3,fHouse.GetPosition.Y + i - 4);
  end;
  fUnit.SetActionLockedStay(0, ua_Walk);
end;


constructor TTaskBuildHouseArea.Load(LoadStream:TKMemoryStream);
var i:integer;
begin
  Inherited;
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(buildID);
  LoadStream.Read(Step);
  for i:=1 to length(Cells) do
  LoadStream.Read(Cells[i]);
end;


procedure TTaskBuildHouseArea.SyncLoad();
begin
  Inherited;
  fHouse := fPlayers.GetHouseByID(integer(fHouse));
end;


destructor TTaskBuildHouseArea.Destroy;
begin
  if fHouse <> nil then fHouse.RemovePointer;
  Inherited Destroy;
end;


{Prepare building site - flatten terrain}
procedure TTaskBuildHouseArea.Execute(out TaskDone:boolean);
begin
TaskDone:=false;

if fHouse.IsDestroyed then
begin
  Abandon;
  TaskDone := true;
  exit;
end;

with fUnit do
case fPhase of
0:  begin
      SetActionWalk(fUnit,fHouse.GetEntrance);
      Thought := th_Build;
    end;
1:  if not fHouse.IsDestroyed then begin //House plan was cancelled before worker has arrived on site
      fPlayers.Player[byte(GetOwner)].BuildList.CloseHousePlan(buildID);
      fTerrain.SetHouse(fHouse.GetPosition, fHouse.GetHouseType, hs_Fence, GetOwner);
      fHouse.SetBuildingState(hbs_NoGlyph);
      SetActionStay(5,ua_Walk);
      Thought := th_None;
    end else begin
      TaskDone:=true;
      Thought := th_None;
    end;
2:  SetActionWalk(fUnit,Cells[Step]);
3:  begin
      SetActionStay(11,ua_Work1,false); //Don't flatten terrain here as we haven't started digging yet
    end;
4:  begin
      SetActionStay(11,ua_Work1,false);
      fTerrain.FlattenTerrain(Cells[Step]);
    end;
5:  begin
      SetActionStay(11,ua_Work1,false);
      fTerrain.FlattenTerrain(Cells[Step]);
    end;
6:  begin
      SetActionStay(11,ua_Work1,false);
      fTerrain.FlattenTerrain(Cells[Step]);
      fTerrain.FlattenTerrain(Cells[Step]); //Flatten the terrain twice now to ensure it really is flat
      if not fHouse.IsDestroyed then
      if KMSamePoint(fHouse.GetEntrance,Cells[Step]) then
        fTerrain.SetRoad(fHouse.GetEntrance, GetOwner);
      fTerrain.Land[Cells[Step].Y,Cells[Step].X].Obj:=255; //All objects are removed
      fTerrain.SetMarkup(Cells[Step],mu_HouseFenceNoWalk); //Block passability on tile
      fTerrain.RecalculatePassability(Cells[Step]);
      dec(Step);
    end;
7:  SetActionStay(0,ua_Walk);
8:  begin
      fHouse.SetBuildingState(hbs_Wood);
      fPlayers.Player[byte(GetOwner)].BuildList.AddNewHouse(fHouse); //Add the house to JobList, so then all workers could take it
      with HouseDAT[byte(fHouse.GetHouseType)] do begin
        fPlayers.Player[byte(GetOwner)].DeliverList.AddNewDemand(fHouse, nil, rt_Wood, WoodCost, dt_Once, di_High);
        fPlayers.Player[byte(GetOwner)].DeliverList.AddNewDemand(fHouse, nil, rt_Stone, StoneCost, dt_Once, di_High);
      end;
      SetActionStay(1,ua_Walk);
    end;
else TaskDone:=true;
end;
inc(fPhase);
if (fPhase=7)and(Step>0) then fPhase:=2; //Repeat with next cell
end;


procedure TTaskBuildHouseArea.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  Inherited;
  if fHouse <> nil then
    SaveStream.Write(fHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(buildID);
  SaveStream.Write(Step);
  for i:=1 to length(Cells) do
  SaveStream.Write(Cells[i]);
end;


{ TTaskBuildHouse }
constructor TTaskBuildHouse.Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
begin
  Inherited Create(aWorker);
  fTaskName := utn_BuildHouse;
  fHouse    := aHouse.GetSelf;
  buildID   := aID;

  Cells := TKMPointDirList.Create;
  fHouse.GetListOfCellsAround(Cells, aWorker.GetDesiredPassability);

  fUnit.SetActionLockedStay(0, ua_Walk);
end;


constructor TTaskBuildHouse.Load(LoadStream:TKMemoryStream);
var i:integer;
begin
  Inherited;
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(buildID);
  LoadStream.Read(CurLoc);
  Cells := TKMPointDirList.Create;
  Cells.Load(LoadStream);
end;


procedure TTaskBuildHouse.SyncLoad();
begin
  Inherited;
  fHouse := fPlayers.GetHouseByID(integer(fHouse));
end;


destructor TTaskBuildHouse.Destroy;
begin
  if fHouse <> nil then fHouse.RemovePointer;
  Inherited Destroy;
end;

procedure TTaskBuildHouse.Abandon();
begin
  fPlayers.Player[byte(fUnit.GetOwner)].BuildList.CloseHouse(buildID);
  Inherited;
end;

function TTaskBuildHouse.WalkShouldAbandon:boolean;
begin
  //If we are walking to the house but the house is destroyed or has run out of resources we should abandon
  Result := (fHouse.IsDestroyed or (not fHouse.CheckResToBuild));
end;

{Build the house}
procedure TTaskBuildHouse.Execute(out TaskDone:boolean);
  function PickRandomSpot(): byte;
  var i, MyCount: integer; Spots: array[1..16] of byte;
  begin
    MyCount := 0;
    for i:=1 to Cells.Count do
      if not KMSamePoint(Cells.List[i].Loc,fUnit.GetPosition) then
        if fTerrain.TileInMapCoords(Cells.List[i].Loc.X,Cells.List[i].Loc.Y) then
          if fTerrain.Route_CanBeMade(fUnit.GetPosition, Cells.List[i].Loc ,fUnit.GetDesiredPassability, true) then
          begin
            inc(MyCount);
            Spots[MyCount] := i;
          end;
    if MyCount > 0 then
      Result := Spots[Random(MyCount)+1]
    else Result := 0;
  end;
begin
  TaskDone:=false;
  //If the house has been destroyed during the building process then exit immediately
  if fHouse.IsDestroyed then
  begin
    Abandon;
    TaskDone:=true; //Drop the task
    exit;
  end;
  with fUnit do
    case fPhase of
      //Pick random location and go there
      0: begin
           //If house has not enough resource to be built, consider building task is done
           //and look for a new task that has enough resouces. Once this house has building resources
           //delivered it will be available from build queue again.
           if not fHouse.CheckResToBuild then begin
             TaskDone:=true; //Drop the task
             Thought := th_None;
             exit;
           end;
           Thought := th_Build;
           CurLoc := PickRandomSpot();
           SetActionWalk(fUnit,Cells.List[CurLoc].Loc);
         end;
      1: begin
           //Remember that we could be here because the walk abandoned, so this is definatly needed
           if not fHouse.CheckResToBuild then begin
             TaskDone:=true; //Drop the task
             Thought := th_None;
             exit;
           end;
           Direction:=TKMDirection(Cells.List[CurLoc].Dir);
           SetActionLockedStay(0,ua_Walk);
         end;
      2: begin
           SetActionLockedStay(5,ua_Work,false,0,0); //Start animation
           Direction:=TKMDirection(Cells.List[CurLoc].Dir);
           //Remove house plan when we start the stone phase (it is still required for wood) But don't do it every time we hit if it's already done!
           if fHouse.IsStone and (fTerrain.Land[fHouse.GetPosition.Y,fHouse.GetPosition.X].Markup <> mu_House) then
             fTerrain.SetHouse(fHouse.GetPosition, fHouse.GetHouseType, hs_Built, GetOwner);
         end;
      3: begin
           //Cancel building no matter progress if resource depleted or unit is hungry and is able to eat
           if ((GetCondition<UNIT_MIN_CONDITION)and(CanGoEat))or(not fHouse.CheckResToBuild) then begin
             TaskDone:=true; //Drop the task
             Thought := th_None;
             exit;
           end;
           fHouse.IncBuildingProgress;
           SetActionLockedStay(6,ua_Work,false,0,5); //Do building and end animation
           inc(fPhase2);
         end;
      4: begin
           fPlayers.Player[byte(GetOwner)].BuildList.CloseHouse(buildID);
           SetActionLockedStay(1,ua_Walk);
           Thought := th_None;
         end;
      else TaskDone:=true;
    end;
  inc(fPhase);
  if (fPhase=4) and (not fHouse.IsComplete) then //If animation cycle is done
    if fPhase2 mod 5 = 0 then //if worker did [5] hits from same spot
      fPhase:=0 //Then goto new spot
    else
      fPhase:=2; //else do more hits
end;


procedure TTaskBuildHouse.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  inherited;
  if fHouse <> nil then
    SaveStream.Write(fHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(buildID);
  SaveStream.Write(CurLoc);
  Cells.Save(SaveStream);
end;


{ TTaskBuildHouseRepair }
constructor TTaskBuildHouseRepair.Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
var i,k:integer;
begin
  Inherited Create(aWorker);
  fTaskName := utn_BuildHouseRepair;
  fHouse:=aHouse.GetSelf;
  buildID:=aID;
  CurLoc:=0;

  Cells := TKMPointDirList.Create;
  fHouse.GetListOfCellsAround(Cells, aWorker.GetDesiredPassability);

  fUnit.SetActionLockedStay(0,ua_Walk);
end;


constructor TTaskBuildHouseRepair.Load(LoadStream:TKMemoryStream);
var i:integer;
begin
  Inherited;
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(buildID);
  LoadStream.Read(CurLoc);
  Cells := TKMPointDirList.Create;
  Cells.Load(LoadStream);
end;


procedure TTaskBuildHouseRepair.SyncLoad();
begin
  Inherited;
  fHouse := fPlayers.GetHouseByID(integer(fHouse));
end;


destructor TTaskBuildHouseRepair.Destroy;
begin
  if fHouse <> nil then fHouse.RemovePointer;
  Inherited Destroy;
end;

function TTaskBuildHouseRepair.WalkShouldAbandon:boolean;
begin
  Result := (fHouse.IsDestroyed)or(not fHouse.IsDamaged)or(not fHouse.BuildingRepair);
end;

{Repair the house}
procedure TTaskBuildHouseRepair.Execute(out TaskDone:boolean);
begin
  TaskDone:=false;
  if (fHouse.IsDestroyed)or(not fHouse.IsDamaged)or(not fHouse.BuildingRepair) then begin
    Abandon;
    TaskDone:=true; //Drop the task
    exit;
  end;

  with fUnit do
    case fPhase of
      //Pick random location and go there
      0: begin
           Thought := th_Build;
           CurLoc := Random(Cells.Count)+1;
           SetActionWalk(fUnit,Cells.List[CurLoc].Loc);
         end;
      1: begin
           Direction:=TKMDirection(Cells.List[CurLoc].Dir);
           SetActionLockedStay(0,ua_Walk);
         end;
      2: begin
           SetActionStay(5,ua_Work,false,0,0); //Start animation
           Direction:=TKMDirection(Cells.List[CurLoc].Dir);
         end;
      3: begin
           if (GetCondition<UNIT_MIN_CONDITION) and CanGoEat then begin
             TaskDone:=true; //Drop the task
             exit;
           end;
           fHouse.AddRepair;
           SetActionStay(6,ua_Work,false,0,5); //Do building and end animation
           inc(fPhase2);
         end;
      4: begin
           Thought := th_None;
           fPlayers.Player[byte(GetOwner)].BuildList.CloseHouse(buildID);
           SetActionStay(1,ua_Walk);
         end;
      else TaskDone:=true;
    end;
  inc(fPhase);
  if (fPhase=4) and (fHouse.IsDamaged) then //If animation cycle is done
    if fPhase2 mod 5 = 0 then //if worker did [5] hits from same spot
      fPhase:=0 //Then goto new spot
    else
      fPhase:=2; //else do more hits
end;


procedure TTaskBuildHouseRepair.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  inherited;
  if fHouse <> nil then
    SaveStream.Write(fHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(buildID);
  SaveStream.Write(CurLoc);
  Cells.Save(SaveStream);
end;

end.

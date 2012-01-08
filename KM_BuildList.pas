unit KM_BuildList;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, Math,
    KM_CommonClasses, KM_Defaults, KM_Houses, KM_Units, KM_Points;


type
  TJobStatus = (
        js_Empty,   //Empty - empty spot for a new job
        js_Open,    //Open - job is free to take by anyone
        js_Taken);  //Taken - job is taken by some worker

  TKMHouseList = class
  private
    fHousesCount: Integer;
    fHouses: array of record
      House: TKMHouse;
      Assigned: Integer; //How many workers are on this house
    end;
    procedure RemoveExtraHouses;
  public
    destructor Destroy; override;

    procedure AddHouse(aHouse: TKMHouse); //New house to build
    procedure RemWorker(aIndex: Integer);
    procedure GiveTask(aIndex: Integer; aWorker: TKMUnitWorker);
    function BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;

    procedure UpdateState;
  end;


  TKMHousePlanList = class //Workers, Houseplans
  private
    fPlansCount: Integer;
    fPlans: array of record
      House: TKMHouse;
      JobStatus: TJobStatus;
      Worker: TKMUnit;
    end;
  public
    //Player orders
    procedure AddPlan(aHouse: TKMHouse);
    function HasPlan(aLoc: TKMPoint): Boolean;
    procedure RemPlan(aLoc: TKMPoint);

    //Game events
    function BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer; //Calculate best bid for a given worker
    procedure GiveTask(aIndex: Integer; aWorker: TKMUnitWorker); //Assign worker to a field
    procedure ReOpenPlan(aIndex: Integer); //Worker has died while walking to the Field, allow other worker to take the task
    procedure ClosePlan(aIndex: Integer); //Worker has finished the task

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


  TKMFieldworksList = class //Workers, Fields
  private
    fFieldsCount: Integer;
    fFields: array of record
      Loc: TKMPoint;
      FieldType: TFieldType;
      JobStatus: TJobStatus;
      Worker: TKMUnit;
    end;
  public
    //Player orders
    procedure AddField(aLoc: TKMPoint; aFieldType: TFieldType);
    function HasField(aLoc: TKMPoint): Boolean;
    procedure RemFieldPlan(aLoc: TKMPoint);

    //Game events
    function BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer; //Calculate best bid for a given worker
    procedure GiveTask(aIndex: Integer; aWorker: TKMUnitWorker); //Assign worker to a field
    procedure ReOpenField(aIndex: Integer); //Worker has died while walking to the Field, allow other worker to take the task
    procedure CloseField(aIndex: Integer); //Worker has finished the task

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


  //Use simple approach since repairs are quite rare events
  //Houses are only added to the list. List checks itself when House should be removed from it
  TKMRepairList = class
  private
    fHousesCount: Integer;
    fHouses: array of record
      House: TKMHouse; //Pointer to house
      Assigned: Byte; //How many workers are assigned to it
    end;

    function HouseAlreadyInList(aHouse: TKMHouse): Boolean;
    procedure RemoveExtraHouses;
  public
    destructor Destroy; override;

    procedure AddHouse(aHouse: TKMHouse);
    function BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer; //Calculate best bid for a given worker
    procedure GiveTask(aIndex: Integer; aWorker: TKMUnitWorker);
    procedure RemWorker(aIndex: Integer);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;


  TKMBuildList = class
  private
    fFieldworksList: TKMFieldworksList;
    fHouseList: TKMHouseList;
    fHousePlanList: TKMHousePlanList;
    fRepairList: TKMRepairList;

    fWorkersCount: Integer;
    fWorkers: array of record
      Worker: TKMUnitWorker; //Pointer to Worker
    end;
    procedure RemWorker(aIndex: Integer);
    procedure RemoveExtraWorkers;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddWorker(aWorker: TKMUnitWorker);

    property FieldworksList: TKMFieldworksList read fFieldworksList;
    property HouseList: TKMHouseList read fHouseList;
    property HousePlanList: TKMHousePlanList read fHousePlanList;
    property RepairList: TKMRepairList read fRepairList;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;


implementation
uses KM_Game, KM_Utils, KM_Terrain, KM_PlayersCollection, KM_UnitTaskBuild, KM_UnitActionStay;


const
  LENGTH_INC = 32; //Increment array lengths by this value
  BID_MODIF = 30; //Modificator for every next assigned worker


{TKMHouseList}
destructor TKMHouseList.Destroy;
var
  I: Integer;
begin
  for I := 0 to fHousesCount - 1 do
  if fHouses[I].House <> nil then
    fPlayers.CleanUpHousePointer(fHouses[I].House);

  inherited;
end;


//Add new job to the list
procedure TKMHouseList.AddHouse(aHouse: TKMHouse);
var I: Integer;
begin
  I := 0;
  while (I < fHousesCount) and (fHouses[I].House <> nil) do
    Inc(I);

  if I >= fHousesCount then
    Inc(fHousesCount);

  if I >= Length(fHouses) then
    SetLength(fHouses, Length(fHouses) + LENGTH_INC);

  fHouses[I].House := aHouse.GetHousePointer;
  fHouses[I].Assigned := 0;
end;


function TKMHouseList.BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer;
var
  I: Integer;
  NewBid: Single;
begin
  //We can weight the repairs by distance, severity, etc..
  //For now, each worker will go for the house closest to him

  Result := -1;
  aBid := 999;
  for I := fHousesCount - 1 downto 0 do
  if (fHouses[i].House <> nil) and fHouses[i].House.CheckResToBuild
  and fTerrain.Route_CanBeMade(aWorker.GetPosition, KMPointBelow(fHouses[i].House.GetEntrance), aWorker.GetDesiredPassability, 0, false)
  then
  begin
    NewBid := GetLength(aWorker.GetPosition, fHouses[I].House.GetPosition);
    NewBid := NewBid + fHouses[I].Assigned * BID_MODIF;

    if (Result = -1) or (NewBid < aBid) then
    begin
      aBid := NewBid;
      Result := I;
    end;
  end;
end;


procedure TKMHouseList.GiveTask(aIndex: Integer; aWorker: TKMUnitWorker);
begin
  aWorker.SetUnitTask := TTaskBuildHouse.Create(aWorker, fHouses[aIndex].House, aIndex);
  Inc(fHouses[aIndex].Assigned);
end;


//Whenever worker dies we need to remove him from assigned to the house
procedure TKMHouseList.RemWorker(aIndex: Integer);
begin
  Dec(fHouses[aIndex].Assigned);
  //If the house is complete or destroyed it will be removed in next UpdateState
end;


//We can remove house only when there are no workers left to it (e.g. stuck on their way)
procedure TKMHouseList.RemoveExtraHouses;
var I: Integer;
begin
  for I := 0 to fHousesCount - 1 do
  if (fHouses[i].House <> nil) and (fHouses[I].House.IsDestroyed or fHouses[I].House.IsComplete) and (fHouses[I].Assigned = 0) then
      fPlayers.CleanUpHousePointer(fHouses[I].House);
end;


procedure TKMHouseList.UpdateState;
begin
  RemoveExtraHouses;
end;


procedure TKMHouseList.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.Write('HouseList');

  SaveStream.Write(fHousesCount);
  for I := 0 to fHousesCount - 1 do
  begin
    if fHouses[i].House <> nil then
      SaveStream.Write(fHouses[i].House.ID)
    else
      SaveStream.Write(Integer(0));
    SaveStream.Write(fHouses[i].Assigned);
  end;
end;


procedure TKMHouseList.Load(LoadStream: TKMemoryStream);
var I: Integer; s:string;
begin
  LoadStream.Read(s);
  Assert(s = 'HouseList');

  LoadStream.Read(fHousesCount);
  SetLength(fHouses, fHousesCount);
  for I := 0 to fHousesCount - 1 do
  begin
    LoadStream.Read(fHouses[i].House, 4);
    LoadStream.Read(fHouses[i].Assigned);
  end;
end;


procedure TKMHouseList.SyncLoad;
var I: Integer;
begin
  for I := 0 to fHousesCount - 1 do
    fHouses[i].House := fPlayers.GetHouseByID(cardinal(fHouses[i].House));
end;


{ TKMFieldworksList }
function TKMFieldworksList.BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer;
var
  I: Integer;
  NewBid: Single;
begin
  Result := -1;
  aBid := MaxSingle;

  for I := 0 to fFieldsCount - 1 do
  if (fFields[I].JobStatus = js_Open)
  and fTerrain.Route_CanBeMade(aWorker.GetPosition, fFields[I].Loc, aWorker.GetDesiredPassability, 0, False) then
  begin
    NewBid := GetLength(aWorker.GetPosition, fFields[I].Loc);
    if (Result = -1) or (NewBid < aBid) then
    begin
      Result := I;
      aBid := NewBid;
    end;
  end;
end;


procedure TKMFieldworksList.CloseField(aIndex: Integer);
begin
  fFields[aIndex].Loc := KMPoint(0,0);
  fFields[aIndex].FieldType := ft_None;
  fFields[aIndex].JobStatus := js_Empty;
  fPlayers.CleanUpUnitPointer(fFields[aIndex].Worker); //Will nil the worker as well
end;


procedure TKMFieldworksList.GiveTask(aIndex: Integer; aWorker: TKMUnitWorker);
begin
  case fFields[aIndex].FieldType of
    ft_Road: aWorker.SetUnitTask := TTaskBuildRoad.Create(aWorker, fFields[aIndex].Loc, aIndex);
    ft_Corn: aWorker.SetUnitTask := TTaskBuildField.Create(aWorker, fFields[aIndex].Loc, aIndex);
    ft_Wine: aWorker.SetUnitTask := TTaskBuildWine.Create(aWorker, fFields[aIndex].Loc, aIndex);
    ft_Wall: aWorker.SetUnitTask := TTaskBuildWall.Create(aWorker, fFields[aIndex].Loc, aIndex);
    else     begin Assert(false, 'Unexpected Field Type'); aWorker.SetUnitTask := nil; Exit; end;
  end;
  fFields[aIndex].JobStatus := js_Taken;
  fFields[aIndex].Worker := aWorker.GetUnitPointer;
end;


//Keep list items in place, since Workers use indexes to address them
procedure TKMFieldworksList.AddField(aLoc: TKMPoint; aFieldType: TFieldType);
var
  I: Integer;
begin
  I := 0;
  while (I < fFieldsCount) and (fFields[I].JobStatus <> js_Empty) do
    Inc(I);

  if I >= fFieldsCount then
    Inc(fFieldsCount);

  if I >= Length(fFields) then
    SetLength(fFields, Length(fFields) + LENGTH_INC);

  fFields[I].Loc := aLoc;
  fFields[I].FieldType := aFieldType;
  fFields[I].JobStatus := js_Open;
  fFields[I].Worker := nil;
end;


procedure TKMFieldworksList.RemFieldPlan(aLoc: TKMPoint);
var I: Integer;
begin
  for I := 0 to fFieldsCount - 1 do
  if KMSamePoint(fFields[I].Loc, aLoc) then
  begin
    if fFields[I].Worker <> nil then
      fFields[I].Worker.CancelUnitTask;
    CloseField(I);
    Exit;
  end;
end;


function TKMFieldworksList.HasField(aLoc: TKMPoint): Boolean;
var I: Integer;
begin
  for I := 0 to fFieldsCount - 1 do
  if KMSamePoint(fFields[I].Loc, aLoc) then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
end;


//When a worker dies while walking to the task aIndex, we should allow other workers to take this task
procedure TKMFieldworksList.ReOpenField(aIndex: Integer);
begin
  fFields[aIndex].JobStatus := js_Open;
  fPlayers.CleanUpUnitPointer(fFields[aIndex].Worker); //Will nil the worker as well
end;


procedure TKMFieldworksList.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.Write('FieldworksList');

  SaveStream.Write(fFieldsCount);
  for I := 0 to fFieldsCount - 1 do
  begin
    SaveStream.Write(fFields[I].Loc);
    SaveStream.Write(fFields[I].FieldType, SizeOf(fFields[I].FieldType));
    SaveStream.Write(fFields[I].JobStatus, SizeOf(fFields[I].JobStatus));
    if fFields[I].Worker <> nil then
      SaveStream.Write(fFields[I].Worker.ID)
    else
      SaveStream.Write(Integer(0));
  end;
end;


procedure TKMFieldworksList.Load(LoadStream: TKMemoryStream);
var I: Integer; s: string;
begin
  LoadStream.Read(s);
  Assert(s = 'FieldworksList');

  LoadStream.Read(fFieldsCount);
  SetLength(fFields, fFieldsCount);
  for I := 0 to fFieldsCount - 1 do
  begin
    LoadStream.Read(fFields[I].Loc);
    LoadStream.Read(fFields[I].FieldType, SizeOf(fFields[I].FieldType));
    LoadStream.Read(fFields[I].JobStatus, SizeOf(fFields[I].JobStatus));
    LoadStream.Read(fFields[I].Worker, 4);
  end;
end;


procedure TKMFieldworksList.SyncLoad;
var I: Integer;
begin
  for I := 0 to fFieldsCount - 1 do
    fFields[I].Worker := fPlayers.GetUnitByID(Cardinal(fFields[I].Worker));
end;


{ TKMHousePlanList }
procedure TKMHousePlanList.AddPlan(aHouse: TKMHouse);
var I: Integer;
begin
  Assert(aHouse <> nil);

  I := 0;
  while (I < fPlansCount) and (fPlans[I].JobStatus <> js_Empty) do
    Inc(I);

  if I >= fPlansCount then
    Inc(fPlansCount);

  if I >= Length(fPlans) then
    SetLength(fPlans, Length(fPlans) + LENGTH_INC);

  fPlans[I].House := aHouse.GetHousePointer;
  fPlans[I].JobStatus := js_Open;
  fPlans[I].Worker := nil;
end;


function TKMHousePlanList.BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer;
var
  I: Integer;
  NewBid: Single;
begin
  Result := -1;
  aBid := MaxSingle;

  for I := 0 to fPlansCount - 1 do
    if (fPlans[I].JobStatus = js_Open)
    and fTerrain.Route_CanBeMade(aWorker.GetPosition, fPlans[I].House.GetPosition, aWorker.GetDesiredPassability, 0, false)
    then
    begin
      NewBid := GetLength(aWorker.GetPosition, fPlans[I].House.GetPosition);
      if (Result = -1) or (NewBid < aBid) then
      begin
        Result := I;
        aBid := NewBid;
      end;
    end;
end;


procedure TKMHousePlanList.ClosePlan(aIndex: Integer);
begin
  fPlayers.CleanUpHousePointer(fPlans[aIndex].House);
  fPlans[aIndex].JobStatus := js_Empty;
  fPlayers.CleanUpUnitPointer(fPlans[aIndex].Worker);
end;


procedure TKMHousePlanList.GiveTask(aIndex: Integer; aWorker: TKMUnitWorker);
begin
  aWorker.SetUnitTask := TTaskBuildHouseArea.Create(aWorker, fPlans[aIndex].House, aIndex);

  fPlans[aIndex].JobStatus := js_Taken;
  fPlans[aIndex].Worker := aWorker.GetUnitPointer;
end;


function TKMHousePlanList.HasPlan(aLoc: TKMPoint): Boolean;
var I: Integer;
begin
  for I := 0 to fPlansCount - 1 do
  if (fPlans[I].House <> nil) and (fPlans[I].House.HitTest(aLoc.X, aLoc.Y)) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
end;


procedure TKMHousePlanList.RemPlan(aLoc: TKMPoint);
var I: Integer;
begin
  for I := 0 to fPlansCount - 1 do
  if (fPlans[I].House <> nil) and (fPlans[I].House.HitTest(aLoc.X, aLoc.Y)) then
  begin
    if fPlans[I].Worker <> nil then
      fPlans[I].Worker.CancelUnitTask;
    ClosePlan(I);
    Exit;
  end;
end;


//When a worker dies while walking to the task aIndex, we should allow other workers to take this task
procedure TKMHousePlanList.ReOpenPlan(aIndex: Integer);
begin
  fPlayers.CleanUpUnitPointer(fPlans[aIndex].Worker);
  fPlans[aIndex].JobStatus := js_Open;
end;


procedure TKMHousePlanList.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.Write('HousePlanList');

  SaveStream.Write(fPlansCount);
  for I := 0 to fPlansCount - 1 do
  with fPlans[I] do
  begin
    if House <> nil then
      SaveStream.Write(House.ID)
    else
      SaveStream.Write(Integer(0));
    SaveStream.Write(JobStatus, SizeOf(JobStatus));
    if Worker <> nil then
      SaveStream.Write(Worker.ID)
    else
      SaveStream.Write(Integer(0));
  end;
end;


procedure TKMHousePlanList.Load(LoadStream: TKMemoryStream);
var I: Integer; s: string;
begin
  LoadStream.Read(s);
  Assert(s = 'HousePlanList');

  LoadStream.Read(fPlansCount);
  SetLength(fPlans, fPlansCount);
  for I := 0 to fPlansCount - 1 do
  with fPlans[I] do
  begin
    LoadStream.Read(House, 4);
    LoadStream.Read(JobStatus, SizeOf(JobStatus));
    LoadStream.Read(Worker, 4);
  end;
end;


procedure TKMHousePlanList.SyncLoad;
var
  I: Integer;
begin
  for I := 0 to fPlansCount - 1 do
  begin
    fPlans[I].House := fPlayers.GetHouseByID(Cardinal(fPlans[I].House));
    fPlans[I].Worker := fPlayers.GetUnitByID(Cardinal(fPlans[I].Worker));
  end;
end;


{ TKMRepairList }
destructor TKMRepairList.Destroy;
var
  I: Integer;
begin
  for I := 0 to fHousesCount - 1 do
  if fHouses[I].House <> nil then
    fPlayers.CleanUpHousePointer(fHouses[I].House);

  inherited;
end;


function TKMRepairList.HouseAlreadyInList(aHouse: TKMHouse): Boolean;
var I: Integer;
begin
  for I := 0 to fHousesCount - 1 do
    if fHouses[I].House = aHouse then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;


//Include the House into the List
procedure TKMRepairList.AddHouse(aHouse: TKMHouse);
var I: Integer;
begin
  if HouseAlreadyInList(aHouse) then Exit;

  I := 0;
  while (I < fHousesCount) and (fHouses[I].House <> nil) do
    Inc(I);

  if I >= fHousesCount then
    Inc(fHousesCount);

  if I >= Length(fHouses) then
    SetLength(fHouses, Length(fHouses) + LENGTH_INC);

  fHouses[fHousesCount].House := aHouse.GetHousePointer;
  fHouses[fHousesCount].Assigned := 0;
end;


function TKMRepairList.BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer;
var
  I: Integer;
  NewBid: Single;
begin
  //We can weight the repairs by distance, severity, etc..
  //For now, each worker will go for the house closest to him

  Result := -1;
  aBid := 999;
  for I := 0 to fHousesCount - 1 do
  if fHouses[I].House <> nil then
  begin
    NewBid := GetLength(aWorker.GetPosition, fHouses[I].House.GetPosition);
    NewBid := NewBid + fHouses[I].Assigned * BID_MODIF;

    if (Result = -1) or (NewBid < aBid) then
    begin
      aBid := NewBid;
      Result := I;
    end;
  end;
end;


procedure TKMRepairList.GiveTask(aIndex: Integer; aWorker: TKMUnitWorker);
begin
  Inc(fHouses[aIndex].Assigned);
  aWorker.SetUnitTask := TTaskBuildHouseRepair.Create(aWorker, fHouses[aIndex].House, aIndex);
end;


procedure TKMRepairList.RemWorker(aIndex: Integer);
begin
  Dec(fHouses[aIndex].Assigned);
end;


//Remove houses that should not be repaired any more
procedure TKMRepairList.RemoveExtraHouses;
var
  I: Integer;
begin
  for I := 0 to fHousesCount - 1 do
    if fHouses[I].House <> nil then
      if (not fHouses[I].House.IsDamaged or not fHouses[I].House.BuildingRepair or fHouses[I].House.IsDestroyed)
      and (fHouses[I].Assigned = 0) then
        fPlayers.CleanUpHousePointer(fHouses[I].House);
end;


procedure TKMRepairList.UpdateState;
begin
  RemoveExtraHouses;
end;


procedure TKMRepairList.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.Write('RepairList');

  SaveStream.Write(fHousesCount);
  for I := 0 to fHousesCount - 1 do
  begin
    if fHouses[I].House <> nil then
      SaveStream.Write(fHouses[I].House.ID)
    else
      SaveStream.Write(Integer(0));
    SaveStream.Write(fHouses[I].Assigned);
  end;
end;


procedure TKMRepairList.Load(LoadStream: TKMemoryStream);
var I: Integer; s: string;
begin
  LoadStream.Read(s);
  Assert(s = 'RepairList');

  LoadStream.Read(fHousesCount);
  SetLength(fHouses, fHousesCount);
  for I := 0 to fHousesCount - 1 do
  begin
    LoadStream.Read(fHouses[I].House, 4);
    LoadStream.Read(fHouses[I].Assigned);
  end;
end;


procedure TKMRepairList.SyncLoad;
var I: Integer;
begin
  for I := 0 to fHousesCount - 1 do
    fHouses[I].House := fPlayers.GetHouseByID(Cardinal(fHouses[I].House));
end;


{ TKMWorkersList }
constructor TKMBuildList.Create;
begin
  inherited;

  fFieldworksList := TKMFieldworksList.Create;
  fHouseList := TKMHouseList.Create;
  fHousePlanList := TKMHousePlanList.Create;
  fRepairList := TKMRepairList.Create;
end;


destructor TKMBuildList.Destroy;
var
  I: Integer;
begin
  fFieldworksList.Free;
  fHouseList.Free;
  fHousePlanList.Free;
  fRepairList.Free;

  for I := fWorkersCount - 1 downto 0 do
    fPlayers.CleanUpUnitPointer(TKMUnit(fWorkers[I].Worker));

  inherited;
end;


//Add the Worker to the List
procedure TKMBuildList.AddWorker(aWorker: TKMUnitWorker);
begin
  if fWorkersCount >= Length(fWorkers) then
    SetLength(fWorkers, fWorkersCount + LENGTH_INC);

  fWorkers[fWorkersCount].Worker := TKMUnitWorker(aWorker.GetUnitPointer);
  Inc(fWorkersCount);
end;

//Remove died Worker from the List
procedure TKMBuildList.RemWorker(aIndex: Integer);
begin
  fPlayers.CleanUpUnitPointer(TKMUnit(fWorkers[aIndex].Worker));

  if aIndex <> fWorkersCount - 1 then
    Move(fWorkers[aIndex+1], fWorkers[aIndex], SizeOf(fWorkers[aIndex]) * (fWorkersCount - 1 - aIndex));

  Dec(fWorkersCount);
end;


//Remove dead workers
procedure TKMBuildList.RemoveExtraWorkers;
var
  I: Integer;
begin
  for I := fWorkersCount - 1 downto 0 do
    if fWorkers[I].Worker.IsDeadOrDying then
      RemWorker(I);
end;


procedure TKMBuildList.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.Write('WorkerList');

  SaveStream.Write(fWorkersCount);
  for I := 0 to fWorkersCount - 1 do
  begin
    if fWorkers[I].Worker <> nil then
      SaveStream.Write(fWorkers[I].Worker.ID)
    else
      SaveStream.Write(Integer(0));
  end;

  fFieldworksList.Save(SaveStream);
  fHouseList.Save(SaveStream);
  fHousePlanList.Save(SaveStream);
  fRepairList.Save(SaveStream);
end;


procedure TKMBuildList.Load(LoadStream: TKMemoryStream);
var I: Integer; s: string;
begin
  LoadStream.Read(s);
  Assert(s = 'WorkerList');

  LoadStream.Read(fWorkersCount);
  SetLength(fWorkers, fWorkersCount);
  for I := 0 to fWorkersCount - 1 do
    LoadStream.Read(fWorkers[I].Worker, 4);

  fFieldworksList.Load(LoadStream);
  fHouseList.Load(LoadStream);
  fHousePlanList.Load(LoadStream);
  fRepairList.Load(LoadStream);
end;


procedure TKMBuildList.SyncLoad;
var I: Integer; U: TKMUnit;
begin
  for I := 0 to fWorkersCount - 1 do
  begin
    U := fPlayers.GetUnitByID(Cardinal(fWorkers[I].Worker));
    Assert(U is TKMUnitWorker, 'Non-worker in Repairs list');
    fWorkers[I].Worker := TKMUnitWorker(U);
  end;

  fFieldworksList.SyncLoad;
  fHouseList.SyncLoad;
  fHousePlanList.SyncLoad;
  fRepairList.SyncLoad;
end;


procedure TKMBuildList.UpdateState;
var I: Integer;
  Idx: array [0..3] of Integer;
  Bid: array [0..3] of Single;
begin
  HouseList.UpdateState;
  fRepairList.UpdateState;

  RemoveExtraWorkers;

  //We can weight the repairs by distance, severity, etc..
  //For now, each worker will go for the house closest to him
  for I := 0 to fWorkersCount - 1 do
    if (fWorkers[I].Worker.GetUnitAction is TUnitActionStay)
    and not TUnitActionStay(fWorkers[I].Worker.GetUnitAction).Locked then
    begin

      Idx[0] := fFieldworksList.BestBid(fWorkers[I].Worker, Bid[0]);
      Idx[1] := fHouseList.BestBid(fWorkers[I].Worker, Bid[1]);
      Idx[2] := fHousePlanList.BestBid(fWorkers[I].Worker, Bid[2]);
      Idx[3] := fRepairList.BestBid(fWorkers[I].Worker, Bid[3]);

      if Idx[3] <> -1 then
        fRepairList.GiveTask(Idx[3], fWorkers[I].Worker)
      else
      if Idx[2] <> -1 then
        fHousePlanList.GiveTask(Idx[2], fWorkers[I].Worker)
      else
      if Idx[1] <> -1 then
        fHouseList.GiveTask(Idx[1], fWorkers[I].Worker)
      else
      if Idx[0] <> -1 then
        fFieldworksList.GiveTask(Idx[0], fWorkers[I].Worker);
    end;
end;


end.

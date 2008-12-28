unit KM_DeliverQueue;
interface
uses windows, math, classes, KromUtils, OpenGL, dglOpenGL, KromOGLUtils, KM_Defaults, KM_Houses, KM_Units;

  type TJobStatus = (js_Open, js_Taken, js_Done);

type
  TKMDeliverQueue = class
  public
  fOffer:array[1..1024]of
  record
    Loc_House:TKMHouse;
    Resource:TResourceType;
    Status:TJobStatus;
  end;
  fDemand:array[1..1024]of
  record
    Loc_House:TKMHouse;
    Loc_Unit:TKMUnit;
    DemandType:TDemandType;
    Resource:TResourceType;
    Importance:byte;
    Status:TJobStatus;
  end;
  fQueue:array[1..1024]of
  record
    Loc1:TKMPoint;
    Loc2_House:TKMHouse;
    Loc2_Unit:TKMUnit;
    Resource:TResourceType;
    Importance:byte;
    JobStatus:TJobStatus;
  end;
  constructor Create();
  procedure AddNewOffer(aHouse:TKMHouse; aResource:TResourceType);
  procedure AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aDemandType:TDemandType);
  procedure AddNewDelivery(aHouse:TKMHouse; bLoc_House:TKMHouse; bLoc_Unit:TKMUnit; aResource:TResourceType);
  function WriteToText():string;
  function  AskForDelivery(KMSerf:TKMSerf):TTaskDeliver;
  procedure CloseDelivery(aID:integer);
  end;

  TKMBuildingQueue = class
  public
  fFieldsQueue:array[1..1024]of
  record
    Loc:TKMPoint;
    FieldType:TFieldType;
    Importance:byte;
    JobStatus:TJobStatus;
  end;
  fHousesQueue:array[1..1024]of
  record
    Loc:TKMPoint;
    HouseType:THouseType;
    Importance:byte;
    JobStatus:TJobStatus;
  end;
  constructor Create();
  procedure AddNewRoadToBuild(aLoc:TKMPoint; aFieldType:TFieldType);
  function RemRoadToBuild(aLoc:TKMPoint):boolean;
  procedure AddNewHouseToBuild(aLoc:TKMPoint; aHouseType: THouseType);
  procedure CloseHouseToBuild(aID:integer);
  function  AskForHouseToBuild(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
  function  AskForRoadToBuild(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
  procedure CloseRoadToBuild(aID:integer);
  end;

implementation
uses KM_Unit1, KM_Terrain, KM_Global_Data;

{ TKMDeliverQueue }

constructor TKMDeliverQueue.Create();
var i:integer;
begin
for i:=1 to length(fQueue) do
  CloseDelivery(i);
end;

//Adds new Offer to the list. List is stored without sorting,
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewOffer(aHouse:TKMHouse; aResource:TResourceType);
var i:integer;
begin
//Check if new offer meets any of existing demands
for i:=1 to 1024 do
  if (fDemand[i].Resource=aResource) and (fDemand[i].Status=js_Open) then begin
    if fDemand[i].Loc_House<>nil then
    begin
      fDemand[i].Status:=js_Taken;
      AddNewDelivery(aHouse,fDemand[i].Loc_House,nil,aResource);
    end else

    if fDemand[i].Loc_Unit<>nil then
    begin
      fDemand[i].Status:=js_Taken;
      AddNewDelivery(aHouse,nil,fDemand[i].Loc_Unit,aResource);
    end;
    exit;
  end;

for i:=1 to 1024 do
  if fDemand[i].Resource=rt_All then
    if fDemand[i].Loc_House<>nil then
    begin
      AddNewDelivery(aHouse,fDemand[i].Loc_House,nil,aResource);
      exit;
    end else Assert(false);

i:=1;
while (i<1024)and(fOffer[i].Loc_House<>nil) do inc(i);
with fOffer[i] do
  begin
    Loc_House:=aHouse;
    Resource:=aResource;
  end;
end;

//Adds new Demand to the list. List is stored without sorting,
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aDemandType:TDemandType);
var i:integer;
begin
for i:=1 to 1024 do
  if fOffer[i].Resource=aResource then
    begin
      if aHouse<>nil then AddNewDelivery(fOffer[i].Loc_House,aHouse,nil,aResource)
      else
      if aUnit<>nil then AddNewDelivery(fOffer[i].Loc_House,nil,aUnit,aResource)
      else Assert(false);
      exit;
    end;

i:=1;
while (i<1024)and(fDemand[i].Resource<>rt_None) do inc(i);
with fDemand[i] do
  begin
    Loc_House:=aHouse;
    Loc_Unit:=aUnit;
    DemandType:=aDemandType;
    Resource:=aResource;
    Importance:=1;
    Status:=js_Open;
  end;
end;

//Find an empty place
procedure TKMDeliverQueue.AddNewDelivery(aHouse:TKMHouse; bLoc_House:TKMHouse; bLoc_Unit:TKMUnit; aResource:TResourceType);
var i:integer;
begin
i:=1;
while (i<1024)and(fQueue[i].JobStatus<>js_Done) do inc(i);

with fQueue[i] do
  begin
    Loc1:=aHouse.GetPosition;
    //Loc2 should be either house or unit
    if bLoc_House<>nil then begin
      Loc2_House:=bLoc_House;
      Loc2_Unit:=nil;
    end else
    if bLoc_Unit<>nil then begin
      Loc2_House:=nil;
      Loc2_Unit:=bLoc_Unit;
    end else
      exit; //Do not take this task for it has no destination unit nor house
    Resource:=aResource;
    Importance:=1;
    JobStatus:=js_Open;
  end;

end;

function TKMDeliverQueue.WriteToText():string;
var i:integer;
begin
Result:='Offer:'+eol+'---------------------------------';
for i:=1 to 1024 do begin
  if fDemand[i].Loc_House<>nil then Result:=Result+'H-'+TypeToString(fDemand[i].Loc_House.GetHouseType)+#9;
  if fDemand[i].Loc_Unit<>nil then Result:=Result+'U-'+TypeToString(fDemand[i].Loc_Unit.GetUnitType)+#9;
//  Result:=Result+'U-'+TypeToString(fDemand[i].Loc_Unit.GetUnitType)+#9;
  Result:=Result+eol;
end;
end;


//Should issue a job based on requesters location and job importance
function TKMDeliverQueue.AskForDelivery(KMSerf:TKMSerf):TTaskDeliver;
var i:integer;
begin
i:=1;
while (i<1024)and(fQueue[i].JobStatus<>js_Open) do inc(i);
if i=1024 then
begin
  Result:=nil;
  exit;
end;
Result:=TTaskDeliver.Create(KMSerf, fQueue[i].Loc1, fQueue[i].Loc2_House, fQueue[i].Loc2_Unit, fQueue[i].Resource, i);
fQueue[i].JobStatus:=js_Taken;
end;

//Job successfully done and we ommit it.
procedure TKMDeliverQueue.CloseDelivery(aID:integer);
begin
with fQueue[aID] do
  begin
    Loc1:=KMPoint(0,0);
    Loc2_House:=nil;
    Loc2_Unit:=nil;
    Resource:=rt_None;
    Importance:=0;
    JobStatus:=js_Done;
  end;
end;

constructor TKMBuildingQueue.Create();
var i:integer;
begin
for i:=1 to length(fFieldsQueue) do CloseRoadToBuild(i);
for i:=1 to length(fHousesQueue) do CloseHouseToBuild(i);
end;

procedure TKMBuildingQueue.AddNewRoadToBuild(aLoc:TKMPoint; aFieldType:TFieldType);
var i:integer;
begin
i:=1;
while fFieldsQueue[i].Loc.X<>0 do inc(i);

fFieldsQueue[i].Loc:=aLoc;
fFieldsQueue[i].FieldType:=aFieldType;
fFieldsQueue[i].Importance:=1;
fFieldsQueue[i].JobStatus:=js_Open;
end;

function TKMBuildingQueue.RemRoadToBuild(aLoc:TKMPoint):boolean;
var i:integer;
begin
Result:=false;
for i:=1 to length(fFieldsQueue) do
  with fFieldsQueue[i] do
  if (JobStatus<>js_Taken)and(Loc.X=aLoc.X)and(Loc.Y=aLoc.Y) then begin
    CloseRoadToBuild(i);
    Result:=true;
    break;
  end;
end;

procedure TKMBuildingQueue.AddNewHouseToBuild(aLoc:TKMPoint; aHouseType: THouseType);
var i:integer;
begin
i:=1;
while fHousesQueue[i].Loc.X<>0 do inc(i);

fHousesQueue[i].Loc:=aLoc;
fHousesQueue[i].HouseType:=aHouseType;
fHousesQueue[i].Importance:=1;
fHousesQueue[i].JobStatus:=js_Open;
end;

procedure TKMBuildingQueue.CloseHouseToBuild(aID:integer);
begin
fHousesQueue[aID].Loc:=KMPoint(0,0);
fHousesQueue[aID].HouseType:=ht_None;
fHousesQueue[aID].Importance:=0;
fHousesQueue[aID].JobStatus:=js_Done;
end;

function  TKMBuildingQueue.AskForHouseToBuild(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
var i:integer;
begin
Result:=nil;
i:=1;
while (i<1024)and(fHousesQueue[i].JobStatus<>js_Open) do inc(i);
if i=1024 then
begin
  Result:=nil;
  exit;
end;
if fHousesQueue[i].JobStatus=js_Open then
  Result:=TTaskBuildHouseArea.Create(KMWorker, fHousesQueue[i].Loc, fHousesQueue[i].HouseType, i);
fHousesQueue[i].JobStatus:=js_Taken;
end;


function  TKMBuildingQueue.AskForRoadToBuild(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
var i:integer;
begin
Result:=nil;
i:=1;
while (i<1024)and(fFieldsQueue[i].JobStatus<>js_Open) do inc(i);
if i=1024 then
begin
  Result:=nil;
  exit;
end;
if fFieldsQueue[i].FieldType=fdt_Road then  Result:=TTaskBuildRoad.Create(KMWorker, fFieldsQueue[i].Loc, i);
if fFieldsQueue[i].FieldType=fdt_Field then Result:=TTaskBuildField.Create(KMWorker, fFieldsQueue[i].Loc, i);
if fFieldsQueue[i].FieldType=fdt_Wine then  Result:=TTaskBuildWine.Create(KMWorker, fFieldsQueue[i].Loc, i);
fFieldsQueue[i].JobStatus:=js_Taken;
end;

procedure TKMBuildingQueue.CloseRoadToBuild(aID:integer);
begin
fFieldsQueue[aID].Loc:=KMPoint(0,0);
fFieldsQueue[aID].FieldType:=fdt_None;
fFieldsQueue[aID].Importance:=0;
fFieldsQueue[aID].JobStatus:=js_Done;
end;


end.

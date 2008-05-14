unit KM_DeliverQueue;

interface

uses
  windows, math, classes, KromUtils, OpenGL, dglOpenGL, KromOGLUtils, KM_Defaults, KM_Houses, KM_Units;

  type TJobStatus = (js_Open, js_Taken, js_Done);

type
  TKMDeliverQueue = class
  public
  fOffer,fDemand:array[1..1024]of
  record
    House:TKMHouse;
    Loc:TKMPoint;
    Resource:TResourceType;
    Importance:byte;
    Status:TJobStatus;
  end;
  fQueue:array[1..1024]of
  record
    Loc1,Loc2:TKMPoint;
    Resource:TResourceType;
    Importance:byte;
    JobStatus:TJobStatus;
  end;
  LastQueueID:integer;
  constructor Create();
  procedure AddNewOffer(aHouse:TKMHouse; aResource:TResourceType);
  procedure AddNewDemand(aLoc:TKMPoint; aResource:TResourceType);
  procedure AddNewDelivery(aHouse:TKMHouse; bLoc:TKMPoint; aResource:TResourceType);
  function  AskForDelivery(KMSerf:TKMSerf):TTaskDeliver;
  procedure CloseDelivery(aID:integer);
  end;

  TKMBuildingQueue = class
  public
  fRoadsQueue:array[1..1024]of
  record
    Loc:TKMPoint;
    Importance:byte;
    JobStatus:TJobStatus;
  end;
  constructor Create();
  procedure AddNewRoadToBuild(aLoc:TKMPoint);
  function  AskForRoadToBuild(KMWorker:TKMWorker; aLoc:TKMPoint):TTaskBuildRoad;
  procedure CloseRoadToBuild(aID:integer);
  end;

implementation
uses KM_Unit1, KM_Terrain, KM_Global_Data, KM_Classes;

{ TKMDeliverQueue }

constructor TKMDeliverQueue.Create();
var i:integer;
begin
for i:=1 to length(fQueue) do
  CloseDelivery(i);
LastQueueID:=1;
end;

//Adds new Offer to the list. List is stored without sorting,
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewOffer(aHouse:TKMHouse; aResource:TResourceType);
var i:integer;
begin
for i:=1 to 1024 do
  if (fDemand[i].Resource=aResource) and (fDemand[i].Status=js_Open) and ((fDemand[i].House=nil) or (fDemand[i].House.CheckResIn(aResource)<MaxResInHouse)) then
    begin
      fDemand[i].Status:=js_Taken;
      AddNewDelivery(aHouse,fDemand[i].Loc,aResource);
      exit;
    end;

for i:=1 to 1024 do
  if fDemand[i].Resource=rt_All then
    begin
      AddNewDelivery(aHouse,fDemand[i].Loc,aResource);
      exit;
    end;

i:=1;
while (i<1024)and(fOffer[i].House<>nil) do inc(i);
with fOffer[i] do
  begin
    House:=aHouse;
    Loc:=House.GetPosition;
    Resource:=aResource;
    Importance:=1;
  end;
end;

//Adds new Demand to the list. List is stored without sorting,
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewDemand(aLoc:TKMPoint; aResource:TResourceType);
var i:integer;
begin
for i:=1 to 1024 do
  if fOffer[i].Resource=aResource then
    begin
      AddNewDelivery(fOffer[i].House,aLoc,aResource);
      exit;
    end;

i:=1;
while (i<1024)and(fDemand[i].Resource<>rt_None) do inc(i);
with fDemand[i] do
  begin
    House:=ControlList.HousesHitTest(aLoc.X,aLoc.Y);
    Loc:=aLoc;
    Resource:=aResource;
    Importance:=1;
    Status:=js_Open;
  end;
end;

//Find an empty place
procedure TKMDeliverQueue.AddNewDelivery(aHouse:TKMHouse; bLoc:TKMPoint; aResource:TResourceType);
//var i:integer;
begin
//i:=1;
//while (i<1024)and(fQueue[i].JobStatus<>js_Done) do inc(i);

with fQueue[LastQueueID] do
  begin
    Loc1:=aHouse.GetPosition;
    Loc2:=bLoc;
    Resource:=aResource;
    Importance:=1;
    JobStatus:=js_Open;
  end;

inc(LastQueueID);
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
Result:=TTaskDeliver.Create(KMSerf, fQueue[i].Loc1, fQueue[i].Loc2, fQueue[i].Resource, i);
fQueue[i].JobStatus:=js_Taken;
end;

//Job successfully done and we ommit it.
procedure TKMDeliverQueue.CloseDelivery(aID:integer);
begin
with fQueue[aID] do
  begin
    Loc1:=KMPoint(0,0);
    Loc2:=KMPoint(0,0);
    Resource:=rt_None;
    Importance:=0;
    JobStatus:=js_Done;
  end;
end;

constructor TKMBuildingQueue.Create();
var i:integer;
begin
for i:=1 to length(fRoadsQueue) do fRoadsQueue[i].JobStatus:=js_Done;
end;

procedure TKMBuildingQueue.AddNewRoadToBuild(aLoc:TKMPoint);
var i:integer;
begin
i:=1;
while fRoadsQueue[i].Loc.X<>0 do inc(i);

fRoadsQueue[i].Loc:=aLoc;
fRoadsQueue[i].Importance:=1;
fRoadsQueue[i].JobStatus:=js_Open;
end;

function  TKMBuildingQueue.AskForRoadToBuild(KMWorker:TKMWorker; aLoc:TKMPoint):TTaskBuildRoad;
var i:integer;
begin
i:=1;
while (i<1024)and(fRoadsQueue[i].JobStatus<>js_Open) do inc(i);
if i=1024 then
begin
  Result:=nil;
  exit;
end;
Result:=TTaskBuildRoad.Create(KMWorker, fRoadsQueue[i].Loc, i);
fRoadsQueue[i].JobStatus:=js_Taken;
end;

procedure TKMBuildingQueue.CloseRoadToBuild(aID:integer);
begin
fRoadsQueue[aID].Loc:=KMPoint(0,0);
fRoadsQueue[aID].Importance:=0;
fRoadsQueue[aID].JobStatus:=js_Done;
end;


end.

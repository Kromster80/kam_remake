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
    Resource:TResourceType;
    Importance:byte;
  end;
  fQueue:array[1..1024]of
  record
    Loc1,Loc2:TKMPoint;
    Resource:TResourceType;
    Importance:byte;
    JobStatus:TJobStatus;
  end;
  constructor Create();
  procedure AddNewOffer(aHouse:TKMHouse; aResource:TResourceType);
  procedure AddNewDemand(aHouse:TKMHouse; aResource:TResourceType);
  procedure AddJob(aLoc1,aLoc2:TKMPoint; aResource:TResourceType);
  function AskForJob(KMSerf:TKMSerf):TDeliverJob;
  procedure CloseJob(aID:integer);
  end;

implementation
uses KM_Unit1, KM_Terrain, KM_Global_Data, KM_Classes;

{ TKMDeliverQueue }

constructor TKMDeliverQueue.Create();
var i:integer;
begin
for i:=1 to length(fQueue) do
  CloseJob(i);{
  with fQueue[i] do
  begin
    Loc1:=Point(0,0);
    Loc2:=Point(0,0);
    fResource:=rt_None;
    Importance:=0;
  end;      }
end;

//Adds new Offer to the list. List is stored without sorting,
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewOffer(aHouse:TKMHouse; aResource:TResourceType);
var i:integer;
begin
i:=1;
while (i<1024)and(fOffer[i].House<>nil) do inc(i);
with fOffer[i] do
  begin
    House:=aHouse;
    Resource:=aResource;
    Importance:=1;
  end;
end;

//Adds new Demand to the list. List is stored without sorting,
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewDemand(aHouse:TKMHouse; aResource:TResourceType);
var i:integer;
begin
i:=1;
while (i<1024)and(fDemand[i].House<>nil) do inc(i);
with fDemand[i] do
  begin
    House:=aHouse;
    Resource:=aResource;
    Importance:=1;
  end;
end;

//Adds new task to the list. List is stored without sorting,
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddJob(aLoc1,aLoc2:TKMPoint; aResource:TResourceType);
var i:integer;
begin
i:=1;
while (i<1024)and(fQueue[i].Loc1.X<>0) do inc(i);
with fQueue[i] do
  begin
    Loc1:=aLoc1;
    Loc2:=aLoc2;
    Resource:=aResource;
    Importance:=1;
    JobStatus:=js_Open;
  end;
end;

//Should issue a job based on requesters location and job importance,
//aswell increase importance of jobs located far away from requester,
//otherwise they likely to never get done
//Returns JobID, so asker could close it when it's done
//Returns
function TKMDeliverQueue.AskForJob(KMSerf:TKMSerf):TDeliverJob;
var i:integer;
TDJ:TDeliverJob;
begin
i:=1;
while (i<1024)and(fQueue[i].JobStatus<>js_Open) do inc(i);
if i=1024 then
begin
  Result:=nil;
  exit;
end;
TDJ:=TDeliverJob.Create(KMSerf, fQueue[i].Loc1, fQueue[i].Loc2, fQueue[i].Resource, i);
Result:=TDJ;
//Result.fFrom:=fQueue[i].Loc1;
//Result.fTo:=fQueue[i].Loc2;
//Result.fResource:=fQueue[i].Resource;
//Result.Phase:=0;
//Result.ID:=i;
fQueue[i].JobStatus:=js_Taken;
end;

//Job successfully done and we ommit it.
procedure TKMDeliverQueue.CloseJob(aID:integer);
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



end.

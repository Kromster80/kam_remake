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
  LastQueueID:integer;
  constructor Create();
  procedure AddNewOffer(aHouse:TKMHouse; aResource:TResourceType);
  procedure AddNewDemand(aHouse:TKMHouse; aResource:TResourceType);
  procedure AddNewJob(aHouse:TKMHouse; bHouse:TKMHouse; aResource:TResourceType);
  function AskForJob(KMSerf:TKMSerf):TTaskDeliver;
  procedure CloseJob(aID:integer);
  end;

implementation
uses KM_Unit1, KM_Terrain, KM_Global_Data, KM_Classes;

{ TKMDeliverQueue }

constructor TKMDeliverQueue.Create();
var i:integer;
begin
for i:=1 to length(fQueue) do
  CloseJob(i);
LastQueueID:=1;
end;

//Adds new Offer to the list. List is stored without sorting,
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewOffer(aHouse:TKMHouse; aResource:TResourceType);
var i:integer;
begin
for i:=1 to 1024 do
  if (fDemand[i].Resource=aResource) and (fDemand[i].House.CheckResIn(aResource)<MaxResInHouse) then
    begin
      AddNewJob(aHouse,fDemand[i].House,aResource);
      exit;
    end;

for i:=1 to 1024 do
  if fDemand[i].Resource=rt_All then
    begin
      AddNewJob(aHouse,fDemand[i].House,aResource);
      exit;
    end;

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
for i:=1 to 1024 do
  if fOffer[i].Resource=aResource then
    begin
      AddNewJob(aHouse,fOffer[i].House,aResource);
      exit;
    end;

i:=1;
while (i<1024)and(fDemand[i].House<>nil) do inc(i);
with fDemand[i] do
  begin
    House:=aHouse;
    Resource:=aResource;
    Importance:=1;
  end;
end;

//Find an empty place
procedure TKMDeliverQueue.AddNewJob(aHouse:TKMHouse; bHouse:TKMHouse; aResource:TResourceType);
//var i:integer;
begin
//i:=1;
//while (i<1024)and(fQueue[i].JobStatus<>js_Done) do inc(i);

with fQueue[LastQueueID] do
  begin
    Loc1:=aHouse.GetPosition;
    Loc2:=bHouse.GetPosition;
    Resource:=aResource;
    Importance:=1;
    JobStatus:=js_Open;
  end;

inc(LastQueueID);
end;

//Should issue a job based on requesters location and job importance
function TKMDeliverQueue.AskForJob(KMSerf:TKMSerf):TTaskDeliver;
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

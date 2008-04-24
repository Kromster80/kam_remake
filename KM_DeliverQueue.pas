unit KM_DeliverQueue;

interface

uses
  windows, math, classes, OpenGL, dglOpenGL, KromOGLUtils, KM_Defaults;

  type TJobStatus = (js_Open, js_Taken, js_Done);

type
  TKMDeliverQueue = class
  public
  fOffer,fDemand:array[1..1024]of
  record
    Loc:TPoint;
    Resource:TResourceType;
    Importance:byte;
    JobStatus:TJobStatus;
  end;
  fQueue:array[1..1024]of
  record
    Loc1,Loc2:TPoint;
    Resource:TResourceType;
    Importance:byte;
    JobStatus:TJobStatus;
  end;
  constructor Create();
  procedure AddJob(aLoc1,aLoc2:TPoint; aResource:TResourceType);
  function TakeJob(aLoc:TPoint):integer;
  procedure CloseJob(aID:integer);
  end;

implementation
uses KM_Unit1, KM_Terrain, KM_Global_Data, KM_Classes, KM_Houses;

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

//Adds new task to the list. List is stored without sorting,
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddJob(aLoc1,aLoc2:TPoint; aResource:TResourceType);
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
function TKMDeliverQueue.TakeJob(aLoc:TPoint):integer;
var i:integer;
begin
i:=1;
while (i<1024)and(fQueue[i].JobStatus<>js_Open) do inc(i);
Result:=i;
end;

//Job successfully done and we ommit it.
procedure TKMDeliverQueue.CloseJob(aID:integer);
begin
with fQueue[aID] do
  begin
    Loc1:=Point(0,0);
    Loc2:=Point(0,0);
    Resource:=rt_None;
    Importance:=0;
    JobStatus:=js_Done;
  end;
end;



end.

unit KM_DeliverQueue;
interface
uses windows, math, classes, SysUtils, KromUtils, OpenGL, dglOpenGL, KromOGLUtils, KM_Defaults, KM_Houses, KM_Units;

  type TJobStatus = (js_Open, js_Taken, js_Done);
  const MaxEntries=1024;

type
  TKMDeliverQueue = class
  private
    fOffer:array[1..MaxEntries]of
    record
      Loc_House:TKMHouse;
      Resource:TResourceType;
      Count:integer;
    end;
    fDemand:array[1..MaxEntries]of
    record
      Loc_House:TKMHouse;
      Loc_Unit:TKMUnit;
      DemandType:TDemandType; //Once for everything, Always for Store and Barracks
      Resource:TResourceType;
    end;
    fQueue:array[1..MaxEntries]of
    record
      OfferID,DemandID:integer;
      //Bid:integer; //Contains bid of current runner, e.g. 12sec, if new asker can make it quicker, then reassign to him,
      //of course if Resource is still not taken from Offer
      JobStatus:TJobStatus; //Open slot, resource Taken, job Done
    end;
  public
    constructor Create();
    procedure AddNewOffer(aHouse:TKMHouse; aResource:TResourceType; aCount:integer);
    procedure AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aDemandType:TDemandType);
    function  AskForDelivery(KMSerf:TKMSerf):TTaskDeliver;
    procedure CloseDelivery(aID:integer);
    function WriteToText():string;
  end;

  TKMBuildingQueue = class
  private
    fFieldsQueue:array[1..MaxEntries]of
    record
      Loc:TKMPoint;
      FieldType:TFieldType;
      Importance:byte;
      JobStatus:TJobStatus;
    end;
    fHousePlansQueue:array[1..MaxEntries]of
    record
      Loc:TKMPoint;
      HouseType:THouseType;
      Importance:byte;
      JobStatus:TJobStatus;
    end;
    fHousesQueue:array[1..MaxEntries]of
    record
      Loc:TKMPoint;
      House:TKMHouse;
      Importance:byte;
      JobStatus:TJobStatus;
    end;
  public
    constructor Create();
    procedure AddNewRoad(aLoc:TKMPoint; aFieldType:TFieldType);
    function RemRoad(aLoc:TKMPoint):boolean;
    function  AskForRoad(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
    procedure CloseRoad(aID:integer);

    procedure AddNewHousePlan(aLoc:TKMPoint; aHouseType: THouseType);
    function  AskForHousePlan(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
    procedure CloseHousePlan(aID:integer);

    procedure AddNewHouse(aLoc:TKMPoint; aHouse: TKMHouse);
    function  AskForHouse(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
    procedure CloseHouse(aID:integer);
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

//Adds new Offer to the list. List is stored without sorting
//(it matters only for Demand to keep evything in waiting its order in line),
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewOffer(aHouse:TKMHouse; aResource:TResourceType; aCount:integer);
var i:integer;
begin
  for i:=1 to length(fOffer) do //Add Count of resource to old offer
    if (fOffer[i].Loc_House=aHouse)and(fOffer[i].Resource=aResource) then begin
      inc(fOffer[i].Count,aCount);
      exit; //Done
    end;

  i:=1; while (i<MaxEntries)and(fOffer[i].Loc_House<>nil) do inc(i); //Find an empty spot
  with fOffer[i] do begin //Put offer
    Loc_House:=aHouse;
    Resource:=aResource;
    Count:=aCount;
  end;
end;

//Adds new Demand to the list. List is stored sorted, but the sorting is done upon Deliver completion,
//so we just find an empty place (which is last one) and write there.
procedure TKMDeliverQueue.AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aDemandType:TDemandType);
var i:integer;
begin
i:=1; while (i<MaxEntries)and(fDemand[i].Resource<>rt_None) do inc(i);
with fDemand[i] do begin
    Loc_House:=aHouse;
    Loc_Unit:=aUnit;
    DemandType:=aDemandType; //Once or Always
    Resource:=aResource;
  end;
end;


//Should issue a job based on requesters location and job importance
function TKMDeliverQueue.AskForDelivery(KMSerf:TKMSerf):TTaskDeliver;
var h,i,k:integer; Bid,BestBid:single;
begin
Result:=nil;

i:=1; while (i<MaxEntries)and(fQueue[i].JobStatus<>js_Open) do inc(i);
if i=MaxEntries then exit;

//Find Offer matching Demand
//TravelRoute Asker>Offer>Demand should be shortest
BestBid:=0;
for h:=1 to length(fDemand) do
  if fDemand[h].Resource <> rt_None then
  for k:=1 to length(fOffer) do
    if fOffer[k].Resource <> rt_None then
    if (fDemand[h].Resource = fOffer[k].Resource)or(fDemand[h].Resource = rt_All) then
    if (fDemand[h].Loc_House=nil)or((fDemand[h].Loc_House<>nil)and(fOffer[k].Loc_House.GetHouseType<>fDemand[h].Loc_House.GetHouseType))
    then begin

      if fDemand[h].Loc_House<>nil then
        Bid := GetLength(fDemand[h].Loc_House.GetPosition,fOffer[k].Loc_House.GetPosition)
      else
        Bid := GetLength(fDemand[h].Loc_Unit.GetPosition,fOffer[k].Loc_House.GetPosition);

      if fDemand[h].Resource=rt_All then
        Bid:=Bid*5; //Prefer deliveries House-House

      if fQueue[i].JobStatus=js_Open then BestBid:=Bid;

      if BestBid>=Bid then begin
        fQueue[i].DemandID:=h;
        fQueue[i].OfferID:=k;
        fQueue[i].JobStatus:=js_Taken; //The job is found, at least something
        BestBid:=Bid;
      end;
    end;

  if BestBid=0 then exit;

  h:=fQueue[i].DemandID;
  k:=fQueue[i].OfferID;
  //Now we have best job and can perform it
  Result:=TTaskDeliver.Create(KMSerf, fOffer[k].Loc_House.GetPosition, fDemand[h].Loc_House, fDemand[h].Loc_Unit, fOffer[k].Resource, i);

  dec(fOffer[k].Count); //Remove resource from Offer
  if fOffer[k].Count=0 then begin
    fOffer[k].Loc_House:=nil;
    fOffer[k].Resource:=rt_None;
  end;

  if fDemand[h].DemandType=dt_Once then
  for i:=h to length(fDemand)-1 do //Remove resource from Demand
    fDemand[i]:=fDemand[i+1];

end;

//Job successfully done and we ommit it.
procedure TKMDeliverQueue.CloseDelivery(aID:integer);
var i:integer;
begin
with fQueue[aID] do
  begin
    OfferID:=0;
    DemandID:=0;
    JobStatus:=js_Open; //Open slot
  end;
end;


function TKMDeliverQueue.WriteToText():string;
var i:integer;
begin
Result:='Demand:'+eol+'---------------------------------'+eol;
for i:=1 to length(fDemand) do if fDemand[i].Resource<>rt_None then begin
  if fDemand[i].Loc_House<>nil then Result:=Result+TypeToString(fDemand[i].Loc_House.GetHouseType)+#9+#9;
  if fDemand[i].Loc_Unit<>nil then Result:=Result+TypeToString(fDemand[i].Loc_Unit.GetUnitType)+#9+#9;
  Result:=Result+TypeToString(fDemand[i].Resource);
  Result:=Result+eol;
end;
Result:=Result+eol+'Offer:'+eol+'---------------------------------'+eol;
for i:=1 to length(fOffer) do if fOffer[i].Resource<>rt_None then begin
  if fOffer[i].Loc_House<>nil then Result:=Result+TypeToString(fOffer[i].Loc_House.GetHouseType)+#9+#9;
  Result:=Result+TypeToString(fOffer[i].Resource)+#9;
  Result:=Result+IntToStr(fOffer[i].Count);
  Result:=Result+eol;
end;
Result:=Result+eol+'Deliveries:'+eol+'---------------------------------'+eol;
{for i:=1 to length(fQueue) do if fQueue[i].Resource<>rt_None then begin
  //Result:=Result+TypeToString(fOffer[i].Resource);
  //if fQueue[i].Loc_House<>nil then Result:=Result+TypeToString(fOffer[i].Loc_House.GetHouseType)+#9;
  Result:=Result+TypeToString(fQueue[i].Resource);
  Result:=Result+eol;
end;}
end;


{==================================================================================================}
{TKMBuildingQueue}
{==================================================================================================}
constructor TKMBuildingQueue.Create();
var i:integer;
begin
for i:=1 to length(fFieldsQueue) do CloseRoad(i);
for i:=1 to length(fHousesQueue) do CloseHouse(i);
for i:=1 to length(fHousePlansQueue) do CloseHousePlan(i);
end;


procedure TKMBuildingQueue.AddNewRoad(aLoc:TKMPoint; aFieldType:TFieldType);
var i:integer;
begin
i:=1;
while fFieldsQueue[i].Loc.X<>0 do inc(i);

fFieldsQueue[i].Loc:=aLoc;
fFieldsQueue[i].FieldType:=aFieldType;
fFieldsQueue[i].Importance:=1;
fFieldsQueue[i].JobStatus:=js_Open;
end;

{Remove task if Player has cancelled it}
function TKMBuildingQueue.RemRoad(aLoc:TKMPoint):boolean;
var i:integer;
begin
Result:=false;
for i:=1 to length(fFieldsQueue) do
  with fFieldsQueue[i] do
  if (JobStatus<>js_Taken)and(Loc.X=aLoc.X)and(Loc.Y=aLoc.Y) then begin
    CloseRoad(i);
    Result:=true;
    break;
  end;
end;


function  TKMBuildingQueue.AskForRoad(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
var i:integer;
begin
Result:=nil;
i:=1;
while (i<MaxEntries)and(fFieldsQueue[i].JobStatus<>js_Open) do inc(i);
if i=MaxEntries then
begin
  Result:=nil;
  exit;
end;
if fFieldsQueue[i].FieldType=fdt_Road then  Result:=TTaskBuildRoad.Create(KMWorker, fFieldsQueue[i].Loc, i);
if fFieldsQueue[i].FieldType=fdt_Field then Result:=TTaskBuildField.Create(KMWorker, fFieldsQueue[i].Loc, i);
if fFieldsQueue[i].FieldType=fdt_Wine then  Result:=TTaskBuildWine.Create(KMWorker, fFieldsQueue[i].Loc, i);
fFieldsQueue[i].JobStatus:=js_Taken;
end;


procedure TKMBuildingQueue.CloseRoad(aID:integer);
begin
fFieldsQueue[aID].Loc:=KMPoint(0,0);
fFieldsQueue[aID].FieldType:=fdt_None;
fFieldsQueue[aID].Importance:=0;
fFieldsQueue[aID].JobStatus:=js_Done;
end;


procedure TKMBuildingQueue.AddNewHousePlan(aLoc:TKMPoint; aHouseType: THouseType);
var i:integer;
begin
i:=1; while fHousePlansQueue[i].Loc.X<>0 do inc(i);
fHousePlansQueue[i].Loc:=aLoc;
fHousePlansQueue[i].HouseType:=aHouseType;
fHousePlansQueue[i].Importance:=1;
fHousePlansQueue[i].JobStatus:=js_Open;
end;


function  TKMBuildingQueue.AskForHousePlan(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
var i:integer;
begin
Result:=nil;
i:=1;
while (i<MaxEntries)and(fHousePlansQueue[i].JobStatus<>js_Open) do inc(i);
if i=MaxEntries then
begin
  Result:=nil;
  exit;
end;
if fHousePlansQueue[i].JobStatus=js_Open then
  Result:=TTaskBuildHouseArea.Create(KMWorker, fHousePlansQueue[i].Loc, fHousePlansQueue[i].HouseType, i);
fHousePlansQueue[i].JobStatus:=js_Taken;
end;


procedure TKMBuildingQueue.CloseHousePlan(aID:integer);
begin
fHousePlansQueue[aID].Loc:=KMPoint(0,0);
fHousePlansQueue[aID].HouseType:=ht_None;
fHousePlansQueue[aID].Importance:=0;
fHousePlansQueue[aID].JobStatus:=js_Done;
end;


{Add new job to the list}
procedure TKMBuildingQueue.AddNewHouse(aLoc:TKMPoint; aHouse: TKMHouse);
var i:integer;
begin
  i:=1;
  while fHousesQueue[i].Loc.X<>0 do inc(i); //Find an empty spot
  fHousesQueue[i].Loc:=aLoc;
  fHousesQueue[i].House:=aHouse;
  fHousesQueue[i].Importance:=1;
  fHousesQueue[i].JobStatus:=js_Open;
end;

{Find a job for worker}
function  TKMBuildingQueue.AskForHouse(KMWorker:TKMWorker; aLoc:TKMPoint):TUnitTask;
var i:integer;
begin
Result:=nil;
i:=1;
while (i<length(fHousesQueue))and(fHousesQueue[i].JobStatus<>js_Open) do inc(i);

if fHousesQueue[i].JobStatus=js_Open then
  Result:=TTaskBuildHouse.Create(KMWorker, fHousesQueue[i].Loc, fHousesQueue[i].House, i);
//fHousesQueue[i].JobStatus:=js_Taken; //Not required since many workers can build one same house
end;

{Clear up}
procedure TKMBuildingQueue.CloseHouse(aID:integer);
begin
  fHousesQueue[aID].Loc:=KMPoint(0,0);
  fHousesQueue[aID].House:=nil;
  fHousesQueue[aID].Importance:=0;
  fHousesQueue[aID].JobStatus:=js_Done;
end;





end.

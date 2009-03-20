unit KM_DeliverQueue;
interface
uses Windows, Math, Classes, SysUtils, KromUtils, OpenGL, dglOpenGL, KromOGLUtils, KM_Defaults, KM_Houses, KM_Units;

  type TJobStatus = (js_Open, js_Taken, js_Done);
  type TDemandImportance = (di_Norm, di_High);
  const MaxEntries=1024;

type
  TKMDeliverQueue = class
  private
    fOffer:array[1..MaxEntries]of
    record
      Resource:TResourceType;
      Count:integer;
      Loc_House:TKMHouse;
    end;
    fDemand:array[1..MaxEntries]of
    record
      Resource:TResourceType;
      DemandType:TDemandType; //Once for everything, Always for Store and Barracks
      Importance:TDemandImportance; //How important demand is, e.g. Workers and building sites should be di_High
      Loc_House:TKMHouse;
      Loc_Unit:TKMUnit;
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
    procedure AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aDemandType:TDemandType; aImp:TDemandImportance);
    function  AskForDelivery(KMSerf:TKMUnitSerf):TTaskDeliver;
    procedure CloseDelivery(aID:integer);
    procedure AbandonDelivery(aID:integer); //Occurs when unit is killed or something alike happens
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
      House:TKMHouse;
      Importance:byte;
      JobStatus:TJobStatus;
    end;
    fHousesQueue:array[1..MaxEntries]of
    record
      House:TKMHouse;
      Importance:byte;
      JobStatus:TJobStatus;
    end;
  public
    constructor Create();
    procedure AddNewRoad(aLoc:TKMPoint; aFieldType:TFieldType);
    function RemRoad(aLoc:TKMPoint):boolean;
    function  AskForRoad(KMWorker:TKMUnitWorker; aLoc:TKMPoint):TUnitTask;
    procedure CloseRoad(aID:integer);

    procedure AddNewHousePlan(aHouse: TKMHouse);
    function  AskForHousePlan(KMWorker:TKMUnitWorker; aLoc:TKMPoint):TUnitTask;
    procedure CloseHousePlan(aID:integer);

    procedure AddNewHouse(aHouse: TKMHouse);
    function  AskForHouse(KMWorker:TKMUnitWorker; aLoc:TKMPoint):TUnitTask;
    procedure CloseHouse(aID:integer);

    {procedure AddHouseRepair(aHouse: TKMHouse);
    function  AskForHouseRepair(KMWorker:TKMUnitWorker; aLoc:TKMPoint):TUnitTask;
    procedure CloseHouseRepair(aID:integer);}
  end;

implementation
uses KM_Unit1, KM_Terrain;

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
procedure TKMDeliverQueue.AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aDemandType:TDemandType; aImp:TDemandImportance);
var i:integer;
begin
i:=1; while (i<MaxEntries)and(fDemand[i].Resource<>rt_None) do inc(i);

with fDemand[i] do begin
    Loc_House:=aHouse;
    Loc_Unit:=aUnit;
    DemandType:=aDemandType; //Once or Always
    Resource:=aResource;
    Importance:=aImp;
    if GOLD_TO_SCHOOLS_IMPORTANT then
      if (Resource=rt_gold)and(Loc_House<>nil)and(Loc_House.GetHouseType=ht_School) then Importance:=di_High;
    if FOOD_TO_INN_IMPORTANT then
      if (Resource in [rt_bread,rt_Sausages,rt_Wine,rt_Fish])and
      (Loc_House<>nil)and(Loc_House.GetHouseType=ht_Inn) then Importance:=di_High;
  end;
end;


//Should issue a job based on requesters location and job importance
function TKMDeliverQueue.AskForDelivery(KMSerf:TKMUnitSerf):TTaskDeliver;
var h,i,k:integer; Bid,BestBid:single; NCount:word; Nodes:array[1..1024] of TKMPoint;
begin
Result:=nil;

i:=1; while (i<MaxEntries)and(fQueue[i].JobStatus<>js_Open) do inc(i);
if i=MaxEntries then exit;

//Find Offer matching Demand
//TravelRoute Asker>Offer>Demand should be shortest
BestBid:=0;
for h:=1 to length(fDemand) do
  if BestBid<>1 then //Quit loop when best bid is found
  if fDemand[h].Resource <> rt_None then
  for k:=1 to length(fOffer) do
    if BestBid<>1 then //Quit loop when best bid is found
    if fOffer[k].Resource <> rt_None then

    if (fDemand[h].Resource = fOffer[k].Resource)or //If Offer Resource matches Demand
       (fDemand[h].Resource = rt_All)or
       ((fDemand[h].Resource = rt_Warfare)and(fOffer[k].Resource in [rt_Shield..rt_Horse])) then

    if (fDemand[h].Loc_House=nil)or //If Demand house has WareDelivery toggled ON
       ((fDemand[h].Loc_House<>nil)and(fDemand[h].Loc_House.WareDelivery)) then

    if (fDemand[h].Loc_House=nil)or //If Demand is a Storehouse and it has WareDelivery toggled ON
       ((fDemand[h].Loc_House<>nil)and((fDemand[h].Loc_House.GetHouseType<>ht_Store)or(
       (fDemand[h].Loc_House.GetHouseType=ht_Store)and(TKMHouseStore(fDemand[h].Loc_House).NotAcceptFlag[byte(fOffer[k].Resource)]=false)))) then

    if (fDemand[h].Loc_House=nil)or //If Demand is a Barracks and it has resource count below MAX_WARFARE_IN_BARRACKS
       ((fDemand[h].Loc_House<>nil)and((fDemand[h].Loc_House.GetHouseType<>ht_Barracks)or( //How do we know how many resource are on-route already??
       (fDemand[h].Loc_House.GetHouseType=ht_Barracks)and(TKMHouseBarracks(fDemand[h].Loc_House).CheckResIn(fOffer[k].Resource)<=MAX_WARFARE_IN_BARRACKS)))) then

    if (fDemand[h].Loc_House=nil)or //If Demand and Offer are different HouseTypes, means forbid Store>Store deliveries
       ((fDemand[h].Loc_House<>nil)and(fOffer[k].Loc_House.GetHouseType<>fDemand[h].Loc_House.GetHouseType)) then

    begin

      //Basic Bid is length of route
      if fDemand[h].Loc_House<>nil then
        Bid := KMLength(fOffer[k].Loc_House.GetEntrance,fDemand[h].Loc_House.GetEntrance)
      else
        Bid := KMLength(fOffer[k].Loc_House.GetEntrance,fDemand[h].Loc_Unit.GetPosition);

      //Modifications for bidding system
      if fDemand[h].Resource=rt_All then //Prefer deliveries House>House instead of House>Store
        Bid:=Bid*3+5;

      if fDemand[h].Loc_House<>nil then //Prefer delivering to houses with fewer supply
      if (fDemand[h].Resource <> rt_All)and(fDemand[h].Resource <> rt_Warfare) then //Except Barracks and Store, where supply doesn't matter or matter less
        Bid:=Bid * (1+fDemand[h].Loc_House.CheckResIn(fDemand[h].Resource)/3);

      if fDemand[h].Importance=di_High then //If Demand importance is high - make it done ASAP
        Bid:=1;

      {if fDemand[h].Loc_House<>nil then begin//House>House delivery
        fTerrain.MakeRoute(KMPointY1(fOffer[k].Loc_House.GetEntrance),KMPointY1(fDemand[h].Loc_House.GetEntrance),canWalkRoad,NCount,Nodes);
        if NCount=0 then Bid:=0;
      end else
        Bid:=10;}

      //Take first one incase there's nothing better to be found
      //Do not take deliveries with Bid=0 (no route found)
      if (Bid<>0)and((fQueue[i].JobStatus=js_Open)or(Bid<BestBid)) then begin
        fQueue[i].DemandID:=h;
        fQueue[i].OfferID:=k;
        fQueue[i].JobStatus:=js_Taken; //The job is found, at least something
        BestBid:=Bid;
      end;

    end;

  if BestBid=0 then exit; //No suitable delivery has been found

  h:=fQueue[i].DemandID;
  k:=fQueue[i].OfferID;
  //Now we have best job and can perform it
  Result:=TTaskDeliver.Create(KMSerf, fOffer[k].Loc_House, fDemand[h].Loc_House, fDemand[h].Loc_Unit, fOffer[k].Resource, i);

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
begin
with fQueue[aID] do
  begin
    OfferID:=0;
    DemandID:=0;
    JobStatus:=js_Open; //Open slot
  end;
end;

//Job was abandoned before it was completed
procedure TKMDeliverQueue.AbandonDelivery(aID:integer);
begin
//
end;


function TKMDeliverQueue.WriteToText():string;
var i:integer;
begin
Result:='Demand:'+eol+'---------------------------------'+eol;
for i:=1 to length(fDemand) do if fDemand[i].Resource<>rt_None then begin
  Result:=Result+#9;
  if fDemand[i].Loc_House<>nil then Result:=Result+TypeToString(fDemand[i].Loc_House.GetHouseType)+#9+#9;
  if fDemand[i].Loc_Unit<>nil then Result:=Result+TypeToString(fDemand[i].Loc_Unit.GetUnitType)+#9+#9;
  Result:=Result+TypeToString(fDemand[i].Resource);
  if fDemand[i].Importance=di_High then Result:=Result+'*****';
  Result:=Result+eol;
end;
Result:=Result+eol+'Offer:'+eol+'---------------------------------'+eol;
for i:=1 to length(fOffer) do if fOffer[i].Resource<>rt_None then begin
  Result:=Result+#9;
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


function  TKMBuildingQueue.AskForRoad(KMWorker:TKMUnitWorker; aLoc:TKMPoint):TUnitTask;
var i:integer;
begin
Result:=nil;

i:=1; while (i<MaxEntries)and(fFieldsQueue[i].JobStatus<>js_Open) do inc(i);
if i=MaxEntries then exit;

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


procedure TKMBuildingQueue.AddNewHousePlan(aHouse: TKMHouse);
var i:integer;
begin
i:=1; while fHousePlansQueue[i].House<>nil do inc(i);
fHousePlansQueue[i].House:=aHouse;
fHousePlansQueue[i].Importance:=1;
fHousePlansQueue[i].JobStatus:=js_Open;
end;


function  TKMBuildingQueue.AskForHousePlan(KMWorker:TKMUnitWorker; aLoc:TKMPoint):TUnitTask;
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
  Result:=TTaskBuildHouseArea.Create(KMWorker, fHousePlansQueue[i].House, i);
fHousePlansQueue[i].JobStatus:=js_Taken;
end;


procedure TKMBuildingQueue.CloseHousePlan(aID:integer);
begin
fHousePlansQueue[aID].House:=nil;
fHousePlansQueue[aID].Importance:=0;
fHousePlansQueue[aID].JobStatus:=js_Done;
end;


{Add new job to the list}
procedure TKMBuildingQueue.AddNewHouse(aHouse: TKMHouse);
var i:integer;
begin
  i:=1;
  while fHousesQueue[i].House<>nil do inc(i); //Find an empty spot
  fHousesQueue[i].House:=aHouse;
  fHousesQueue[i].Importance:=1;
  fHousesQueue[i].JobStatus:=js_Open;
end;

{Find a job for worker}
function  TKMBuildingQueue.AskForHouse(KMWorker:TKMUnitWorker; aLoc:TKMPoint):TUnitTask;
var i:integer;
begin
for i:=1 to MaxEntries do begin
  if (fHousesQueue[i].JobStatus=js_Open)and
     (fHousesQueue[i].House<>nil)and
     (fHousesQueue[i].House.CheckResToBuild) then break;
end;

if i>=MaxEntries then
begin
  Result:=nil;
  exit;
end;

  Result:=TTaskBuildHouse.Create(KMWorker, fHousesQueue[i].House, i);
//fHousesQueue[i].JobStatus:=js_Taken; //Not required since many workers can build one same house
end;

{Clear up}
procedure TKMBuildingQueue.CloseHouse(aID:integer);
begin
  fHousesQueue[aID].House:=nil;
  fHousesQueue[aID].Importance:=0;
  fHousesQueue[aID].JobStatus:=js_Done;
end;





end.

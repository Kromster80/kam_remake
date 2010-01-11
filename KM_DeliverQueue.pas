unit KM_DeliverQueue;
interface
uses Windows, Math, Classes, SysUtils, KromUtils, KM_CommonTypes, KM_Defaults, KM_Houses, KM_Units, KM_Utils, KM_UnitTaskDelivery;

  type TJobStatus = (js_Empty, js_Open, js_Taken);
  //Empty - empty spot for a new job
  //Open - job is free to take by anyone
  //Taken - job is taken by some worker
  type TDemandImportance = (di_Norm, di_High);
  const MaxEntries=1024; //todo 1: replace with dynamic length list

type
  TKMDeliverQueue = class
  private
    fOffer:array[1..MaxEntries]of
    record
      Resource:TResourceType;
      Count:integer;
      Loc_House:TKMHouse;
      BeingPerformed:integer; //How many items are being delivered atm from total Count offered
    end;
    fDemand:array[1..MaxEntries]of
    record
      Resource:TResourceType;
      DemandType:TDemandType; //Once for everything, Always for Store and Barracks
      Importance:TDemandImportance; //How important demand is, e.g. Workers and building sites should be di_High
      Loc_House:TKMHouse;
      Loc_Unit:TKMUnit;
      BeingPerformed:boolean;
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
    procedure RemoveOffer(aHouse:TKMHouse);
    procedure RemoveDemand(aHouse:TKMHouse);
    procedure AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aDemandCount:byte; aDemandType:TDemandType; aImp:TDemandImportance);
    function PermitDelivery(iO,iD:integer):boolean;
    function AskForDelivery(KMSerf:TKMUnitSerf; KMHouse:TKMHouse=nil):TTaskDeliver;
    procedure TakenOffer(aID:integer);
    procedure GaveDemand(aID:integer);
    procedure CloseDelivery(aID:integer);
    procedure AbandonDelivery(aID:integer); //Occurs when unit is killed or something alike happens
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
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
      Worker:TKMUnit;
    end;
    fHousesQueue:array[1..MaxEntries]of
    record
      House:TKMHouse;
      Importance:byte;
      //No need to have JobStatus since many workers can build same house
    end;
    fHousePlansQueue:array[1..MaxEntries]of
    record
      House:TKMHouse;
      Importance:byte;
      JobStatus:TJobStatus;
      Worker:TKMUnit;
    end;
    fHousesRepairQueue:array[1..MaxEntries]of
    record
      House:TKMHouse;
      Importance:byte;
      //No need to have JobStatus since many workers can repair same house
    end;
  public
    constructor Create();
    procedure CloseRoad(aID:integer);
    procedure CloseHouse(aID:integer);
    procedure CloseHousePlan(aID:integer);

    procedure ReOpenRoad(aID:integer);
    procedure ReOpenHousePlan(aID:integer);

    procedure AddNewRoad(aLoc:TKMPoint; aFieldType:TFieldType);
    procedure AddNewHouse(aHouse: TKMHouse);
    procedure AddNewHousePlan(aHouse: TKMHouse);
    function AddHouseRepair(aHouse: TKMHouse):integer;

    function CancelRoad(aLoc:TKMPoint; Simulated:boolean=false):boolean;
    function CancelHousePlan(aLoc:TKMPoint; Simulated:boolean=false):boolean;

    function AskForRoad(aWorker:TKMUnitWorker):TUnitTask;
    function AskForHousePlan(aWorker:TKMUnitWorker):TUnitTask;
    function AskForHouse(aWorker:TKMUnitWorker):TUnitTask;

    function  AskForHouseRepair(aWorker:TKMUnitWorker):TUnitTask;
    procedure CloseHouseRepair(aID:integer);
    procedure RemoveHouseRepair(aHouse: TKMHouse);
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;

implementation
uses KM_Unit1, KM_Terrain, KM_PlayersCollection;


{ TKMDeliverQueue }
constructor TKMDeliverQueue.Create();
var i:integer;
begin
for i:=1 to length(fQueue) do
  CloseDelivery(i);
end;


//Adds new Offer to the list. List is stored without sorting
//(it matters only for Demand to keep everything in waiting its order in line),
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewOffer(aHouse:TKMHouse; aResource:TResourceType; aCount:integer);
var i:integer;
begin
  //Add Count of resource to old offer
  for i:=1 to length(fOffer) do
    if (fOffer[i].Loc_House=aHouse)and(fOffer[i].Resource=aResource) then begin
      inc(fOffer[i].Count,aCount);
      exit; //Done
    end;

  //Find an empty spot for new unique offer
  i:=1; while (i<MaxEntries)and(fOffer[i].Resource<>rt_None) do inc(i);
  with fOffer[i] do begin //Put offer
    if aHouse <> nil then Loc_House:=aHouse.GetSelf;
    Resource:=aResource;
    Count:=aCount;
    BeingPerformed:=0; //New unique offer is available to be performed apriori
  end;
end;


//Remove Offer from the list. List is stored without sorting
//so we parse it to find that entry..
procedure TKMDeliverQueue.RemoveOffer(aHouse:TKMHouse);
var i:integer;
begin
  //We need to parse whole list, never knowing how many offers the house had
  for i:=1 to MaxEntries do
  if fOffer[i].Loc_House=aHouse then
  begin
    fOffer[i].Loc_House.RemovePointer;
    FillChar(fOffer[i],SizeOf(fOffer[i]),#0); //Remove offer
  end;
end;


//Remove Demand from the list. List is stored without sorting
//so we parse it to find all entries..
procedure TKMDeliverQueue.RemoveDemand(aHouse:TKMHouse);
var i:integer;
begin
  //We need to parse whole list, never knowing how many demands the house had
  for i:=1 to MaxEntries do
  if fDemand[i].Loc_House=aHouse then
  begin
    fDemand[i].Loc_House.RemovePointer;
    if fDemand[i].Loc_Unit <> nil then fDemand[i].Loc_Unit.RemovePointer;
    FillChar(fDemand[i],SizeOf(fDemand[i]),#0); //Clear up demand
  end;
end;


//Adds new Demand to the list. List is stored sorted, but the sorting is done upon Deliver completion,
//so we just find an empty place (which is last one) and write there.
procedure TKMDeliverQueue.AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aDemandCount:byte; aDemandType:TDemandType; aImp:TDemandImportance);
var i,k:integer;
begin
  fLog.AssertToLog(aResource<>rt_None, 'Demanding rt_None');
  for k:=1 to aDemandCount do begin
    i:=1; while (i<MaxEntries)and(fDemand[i].Resource<>rt_None) do inc(i);

    with fDemand[i] do begin
      if aHouse <> nil then Loc_House:=aHouse.GetSelf;
      if aUnit <> nil then Loc_Unit:=aUnit.GetSelf;
      DemandType:=aDemandType; //Once or Always
      Resource:=aResource;
      Importance:=aImp;
      BeingPerformed:=false;
      if GOLD_TO_SCHOOLS_IMPORTANT then
        if (Resource=rt_gold)and(Loc_House<>nil)and(Loc_House.GetHouseType=ht_School) then Importance:=di_High;
      if FOOD_TO_INN_IMPORTANT then
        if (Resource in [rt_bread,rt_Sausages,rt_Wine,rt_Fish])and
        (Loc_House<>nil)and(Loc_House.GetHouseType=ht_Inn) then Importance:=di_High;
    end;
  end;
end;


function TKMDeliverQueue.PermitDelivery(iO,iD:integer):boolean;
begin
  //If Offer Resource matches Demand
  Result := (fDemand[iD].Resource = fOffer[iO].Resource)or
            (fDemand[iD].Resource = rt_All)or
            ((fDemand[iD].Resource = rt_Warfare)and(fOffer[iO].Resource in [rt_Shield..rt_Horse]));

  //If Demand and Offer aren't reserved already
  Result := Result and ((not fDemand[iD].BeingPerformed) and (fOffer[iO].BeingPerformed < fOffer[iO].Count));

  //If Demand house has WareDelivery toggled ON
  Result := Result and ((fDemand[iD].Loc_House=nil) or (fDemand[iD].Loc_House.WareDelivery));

  //If Demand is a Storehouse and it has WareDelivery toggled ON
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fDemand[iD].Loc_House.GetHouseType<>ht_Store)or
                        (TKMHouseStore(fDemand[iD].Loc_House).NotAcceptFlag[byte(fOffer[iO].Resource)]=false));

  //If Demand is a Barracks and it has resource count below MAX_WARFARE_IN_BARRACKS
  //How do we know how many resource are on-route already??
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fDemand[iD].Loc_House.GetHouseType<>ht_Barracks)or
                       (TKMHouseBarracks(fDemand[iD].Loc_House).CheckResIn(fOffer[iO].Resource)<=MAX_WARFARE_IN_BARRACKS));

  {@Krom: BUG REPORT:
  Weapons are delivered from houses to store instead of barracks. If there is a barracks then
  weapons should never go to a store.
  After going to the store, they are soon taken to the barracks though.
  Tried to fix but was unable to. ;) I had trouble fully understanding the bidding system.
  @Lewin: There should be new condition perhaps - if Player.hasBarracks then do not bid delivery to store
  @Krom: Fixed? I can't seem to reproduce this bug. Did you fix it?}

  //if (fDemand[iD].Loc_House=nil)or //If Demand is a Barracks and it has resource count below MAX_WARFARE_IN_BARRACKS
  //   ((fDemand[iD].Loc_House<>nil)and((fDemand[iD].Loc_House.GetHouseType<>ht_Store)or(
  //   (fDemand[iD].Loc_House.GetHouseType=ht_Store)and(fPlayers.Player[byte(KMSerf.GetOwner)].fMissionSettings.GetHouseQty(ht_Barracks)=0)))) then
  //Works wrong, besides we need to check ALL barracks player owns

  //If Demand and Offer are different HouseTypes, means forbid Store<->Store deliveries except the case where 2nd store is being built and requires building materials
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fOffer[iO].Loc_House.GetHouseType<>fDemand[iD].Loc_House.GetHouseType)or(fOffer[iO].Loc_House.IsComplete<>fDemand[iD].Loc_House.IsComplete));


  Result := Result and (
            ( //House-House delivery should be performed only if there's a connecting road
            (fDemand[iD].Loc_House<>nil)and
            (fTerrain.Route_CanBeMade(KMPointY1(fOffer[iO].Loc_House.GetEntrance),KMPointY1(fDemand[iD].Loc_House.GetEntrance),canWalkRoad,true))
            )
            or
            ( //House-Unit delivery can be performed without connecting road
            (fDemand[iD].Loc_Unit<>nil)and
            (fTerrain.Route_CanBeMade(KMPointY1(fOffer[iO].Loc_House.GetEntrance),fDemand[iD].Loc_Unit.GetPosition,canWalk,false))
            )
            or
            ( //Or maybe serfs can walk anywhere?
            not DO_SERFS_WALK_ROADS
            )
            );
end;


//Should issue a job based on requesters location and job importance
function TKMDeliverQueue.AskForDelivery(KMSerf:TKMUnitSerf; KMHouse:TKMHouse=nil):TTaskDeliver;
var i,iD,iO:integer; Bid,BestBid:single;
begin
Result:=nil;

//Find place for new Delivery to be written to after Offer-Demand pair is found
i:=1; while (i<MaxEntries)and(fQueue[i].JobStatus<>js_Open) do inc(i);
if i=MaxEntries then exit;

//Cleanup for destroyed houses
{for iD:=1 to length(fDemand) do
  if (fDemand[iD].Loc_House<>nil)and(fDemand[iD].Loc_House.IsDestroyed) then fDemand[iD].Resource:=rt_None
  else
  if (fDemand[iD].Loc_Unit<>nil)and(fDemand[iD].Loc_Unit.IsDestroyed) then fDemand[iD].Resource:=rt_None;

for iO:=1 to length(fOffer) do
  if (fOffer[iO].Loc_House<>nil)and(fOffer[iO].Loc_House.IsDestroyed) then fOffer[iO].Resource:=rt_None;
}

//Find Offer matching Demand
//TravelRoute Asker>Offer>Demand should be shortest
BestBid:=0;
for iD:=1 to length(fDemand) do
  if BestBid=1 then break else //Quit loop when best bid is found
  if fDemand[iD].Resource <> rt_None then
  for iO:=1 to length(fOffer) do
   if BestBid=1 then break else //Quit loop when best bid is found
    if (KMHouse = nil) or (fOffer[iO].Loc_House = KMHouse) then //Make sure from house is the one requested
    if fOffer[iO].Resource <> rt_None then

    if PermitDelivery(iO,iD) then
    begin

      //Basic Bid is length of route
      if fDemand[iD].Loc_House<>nil then
        Bid := GetLength(fOffer[iO].Loc_House.GetEntrance,fDemand[iD].Loc_House.GetEntrance)
      else
        Bid := GetLength(fOffer[iO].Loc_House.GetEntrance,fDemand[iD].Loc_Unit.GetPosition);

      //Modifications for bidding system
      if fDemand[iD].Resource=rt_All then //Prefer deliveries House>House instead of House>Store
        Bid:=Bid*3+5;

      if fDemand[iD].Loc_House<>nil then //Prefer delivering to houses with fewer supply
      if (fDemand[iD].Resource <> rt_All)and(fDemand[iD].Resource <> rt_Warfare) then //Except Barracks and Store, where supply doesn't matter or matter less
        Bid:=Bid + Bid * fDemand[iD].Loc_House.CheckResIn(fDemand[iD].Resource) / 3;

      if fDemand[iD].Importance=di_High then //If Demand importance is high - make it done ASAP
        Bid:=1;

      //Take first one incase there's nothing better to be found
      //Do not take deliveries with Bid=0 (no route found)
      if (Bid<>0)and((fQueue[i].JobStatus=js_Open)or(Bid<BestBid)) then begin
        fQueue[i].DemandID:=iD;
        fQueue[i].OfferID:=iO;
        fQueue[i].JobStatus:=js_Taken; //The job is found, at least something
        BestBid:=Bid;
      end;

    end;

  if BestBid=0 then exit; //No suitable delivery has been found

  iD:=fQueue[i].DemandID;
  iO:=fQueue[i].OfferID;
  inc(fOffer[iO].BeingPerformed); //Places a virtual "Reserved" sign on an Offer
  fDemand[iD].BeingPerformed:=true; //Places a virtual "Reserved" sign on Demand

  //Store never has enough demand performed
  if (fDemand[iD].Loc_House<>nil)and(fDemand[iD].DemandType = dt_Always) then fDemand[iD].BeingPerformed:=false;

  //Now we have best job and can perform it
  Result:=TTaskDeliver.Create(KMSerf, fOffer[iO].Loc_House, fDemand[iD].Loc_House, fDemand[iD].Loc_Unit, fOffer[iO].Resource, i);
end;


//Resource has been taken from Offer
procedure TKMDeliverQueue.TakenOffer(aID:integer);
var iO:integer;
begin
  iO:=fQueue[aID].OfferID;
  fQueue[aID].OfferID:=0; //We don't need it any more

  dec(fOffer[iO].BeingPerformed); //Remove reservation
  dec(fOffer[iO].Count); //Remove resource from Offer list

  if fOffer[iO].Count=0 then
  begin
    fOffer[iO].Resource:=rt_None;
    if fOffer[iO].Loc_House <> nil then fOffer[iO].Loc_House.RemovePointer;
    fOffer[iO].Loc_House:=nil;
  end;
end;


//Resource has been delivered to Demand
procedure TKMDeliverQueue.GaveDemand(aID:integer);
var iD:integer;
begin
  iD:=fQueue[aID].DemandID;
  fQueue[aID].DemandID:=0; //We don't need it any more

  fDemand[iD].BeingPerformed:=false; //Remove reservation

  if fDemand[iD].DemandType=dt_Once then begin//Remove resource from Demand list
    fDemand[iD].Resource:=rt_None;
    if fDemand[iD].Loc_House <> nil then fDemand[iD].Loc_House.RemovePointer;
    fDemand[iD].Loc_House:=nil;
    if fDemand[iD].Loc_Unit <> nil then fDemand[iD].Loc_Unit.RemovePointer;
    fDemand[iD].Loc_Unit:=nil;
  end;
end;


//AbandonDelivery
procedure TKMDeliverQueue.AbandonDelivery(aID:integer);
begin
  //Remove reservations without removing items from lists
  if fQueue[aID].OfferID <> 0 then dec(fOffer[fQueue[aID].OfferID].BeingPerformed);
  if fQueue[aID].DemandID <> 0 then fDemand[fQueue[aID].DemandID].BeingPerformed:=false;
  CloseDelivery(aID);
end;


//Job successfully done and we ommit it.
procedure TKMDeliverQueue.CloseDelivery(aID:integer);
begin
  fQueue[aID].OfferID:=0;
  fQueue[aID].DemandID:=0;
  fQueue[aID].JobStatus:=js_Open; //Open slot
end;


procedure TKMDeliverQueue.Save(SaveStream:TKMemoryStream);
var i,Count:integer;
begin
  SaveStream.Write('Deliveries');
  Count := length(fOffer);
  SaveStream.Write(Count);
  for i:=1 to Count do
  begin
    SaveStream.Write(fOffer[i].Resource, SizeOf(fOffer[i].Resource));
    SaveStream.Write(fOffer[i].Count);
    if fOffer[i].Loc_House <> nil then SaveStream.Write(fOffer[i].Loc_House.ID) else SaveStream.Write(Zero);
    SaveStream.Write(fOffer[i].BeingPerformed);
  end;

  Count := length(fDemand);
  SaveStream.Write(Count);
  for i:=1 to Count do
  with fDemand[i] do
  begin
    SaveStream.Write(Resource, SizeOf(Resource));
    SaveStream.Write(DemandType, SizeOf(DemandType));
    SaveStream.Write(Importance, SizeOf(Importance));
    if Loc_House <> nil then SaveStream.Write(Loc_House.ID) else SaveStream.Write(Zero);
    if Loc_Unit  <> nil then SaveStream.Write(Loc_Unit.ID ) else SaveStream.Write(Zero);
    SaveStream.Write(BeingPerformed);
  end;

  Count := length(fQueue);
  SaveStream.Write(Count);
  for i:=1 to Count do
  begin
    SaveStream.Write(fQueue[i].OfferID);
    SaveStream.Write(fQueue[i].DemandID);
    SaveStream.Write(fQueue[i].JobStatus, SizeOf(fQueue[i].JobStatus));
  end;
end;


procedure TKMDeliverQueue.Load(LoadStream:TKMemoryStream);
var i,Count:integer; s:string;
begin
  LoadStream.Read(s); if s <> 'Deliveries' then exit;
  LoadStream.Read(Count);
  for i:=1 to Count do
  begin
    LoadStream.Read(fOffer[i].Resource, SizeOf(fOffer[i].Resource));
    LoadStream.Read(fOffer[i].Count);
    LoadStream.Read(fOffer[i].Loc_House, 4);
    fOffer[i].Loc_House := fPlayers.GetHouseByID(integer(fOffer[i].Loc_House)); //todo: split into SyncLoad for cases when we need to access yet unloaded players assets
    LoadStream.Read(fOffer[i].BeingPerformed);
  end;

  LoadStream.Read(Count);
  for i:=1 to Count do
  with fDemand[i] do
  begin
    LoadStream.Read(Resource, SizeOf(Resource));
    LoadStream.Read(DemandType, SizeOf(DemandType));
    LoadStream.Read(Importance, SizeOf(Importance));
    LoadStream.Read(Loc_House, 4);
    Loc_House := fPlayers.GetHouseByID(integer(Loc_House));
    LoadStream.Read(Loc_Unit, 4);
    Loc_Unit := fPlayers.GetUnitByID(integer(Loc_Unit));
    LoadStream.Read(BeingPerformed);
  end;

  LoadStream.Read(Count);
  for i:=1 to Count do
  begin
    LoadStream.Read(fQueue[i].OfferID);
    LoadStream.Read(fQueue[i].DemandID);
    LoadStream.Read(fQueue[i].JobStatus, SizeOf(fQueue[i].JobStatus));
  end;
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
  if fDemand[i].Importance=di_High then Result:=Result+'^';
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
Result:=Result+eol+'Running deliveries:'+eol+'---------------------------------'+eol;
for i:=1 to length(fQueue) do if fQueue[i].OfferID<>0 then begin
  Result:=Result+TypeToString(fOffer[fQueue[i].OfferID].Resource)+#9;
  Result:=Result+TypeToString(fOffer[fQueue[i].OfferID].Loc_House.GetHouseType)+' >>> ';
  Result:=Result+TypeToString(fOffer[fQueue[i].DemandID].Loc_House.GetHouseType)+#9;
  Result:=Result+eol;
end;
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


procedure TKMBuildingQueue.CloseRoad(aID:integer);
begin
  fFieldsQueue[aID].Loc:=KMPoint(0,0);
  fFieldsQueue[aID].FieldType:=ft_None;
  fFieldsQueue[aID].Importance:=0;
  fFieldsQueue[aID].JobStatus:=js_Empty;
  if fFieldsQueue[aID].Worker <> nil then fFieldsQueue[aID].Worker.RemovePointer;
  fFieldsQueue[aID].Worker:=nil;
end;


{Clear up}
procedure TKMBuildingQueue.CloseHouse(aID:integer);
begin
  if fHousesQueue[aID].House <> nil then fHousesQueue[aID].House.RemovePointer;
  fHousesQueue[aID].House:=nil;
  fHousesQueue[aID].Importance:=0;
end;


procedure TKMBuildingQueue.CloseHousePlan(aID:integer);
begin
  if fHousePlansQueue[aID].House <> nil then fHousePlansQueue[aID].House.RemovePointer;
  fHousePlansQueue[aID].House:=nil;
  fHousePlansQueue[aID].Importance:=0;
  fHousePlansQueue[aID].JobStatus:=js_Empty;
  if fHousePlansQueue[aID].Worker <> nil then fHousePlansQueue[aID].Worker.RemovePointer;
  fHousePlansQueue[aID].Worker:=nil;
end;


procedure TKMBuildingQueue.CloseHouseRepair(aID:integer);
begin
  if fHousesRepairQueue[aID].House <> nil then fHousesRepairQueue[aID].House.RemovePointer;
  fHousesRepairQueue[aID].House:=nil;
  fHousesRepairQueue[aID].Importance:=0;
end;


//This procedure is called when a worker dies while walking to the task aID. We should allow other workers to take this task.
procedure TKMBuildingQueue.ReOpenRoad(aID:integer);
begin
  fFieldsQueue[aID].JobStatus:=js_Open;
  if fFieldsQueue[aID].Worker <> nil then fFieldsQueue[aID].Worker.RemovePointer;
  fFieldsQueue[aID].Worker:=nil;
end;


//This procedure is called when a worker dies while walking to the task aID. We should allow other workers to take this task.
procedure TKMBuildingQueue.ReOpenHousePlan(aID:integer);
begin
  fHousePlansQueue[aID].JobStatus:=js_Open;
  if fHousePlansQueue[aID].Worker <> nil then fHousePlansQueue[aID].Worker.RemovePointer;
  fHousePlansQueue[aID].Worker:=nil;
end;


procedure TKMBuildingQueue.RemoveHouseRepair(aHouse: TKMHouse);
var i:integer;
begin
  for i:=1 to MaxEntries do
  if fHousesRepairQueue[i].House=aHouse then
  begin
    fHousesRepairQueue[i].House.RemovePointer;
    FillChar(fHousesRepairQueue[i],SizeOf(fHousesRepairQueue[i]),#0); //Remove offer
  end;
end;


procedure TKMBuildingQueue.AddNewRoad(aLoc:TKMPoint; aFieldType:TFieldType);
var i:integer;
begin
  i:=1; while (i<MaxEntries)and(fFieldsQueue[i].JobStatus<>js_Empty) do inc(i);
  fFieldsQueue[i].Loc:=aLoc;
  fFieldsQueue[i].FieldType:=aFieldType;
  fFieldsQueue[i].Importance:=1;
  fFieldsQueue[i].JobStatus:=js_Open;
end;


{Add new job to the list}
procedure TKMBuildingQueue.AddNewHouse(aHouse: TKMHouse);
var i:integer;
begin
  i:=1; while (i<MaxEntries)and(fHousesQueue[i].House<>nil) do inc(i);
  if aHouse <> nil then fHousesQueue[i].House := aHouse.GetSelf;
  fHousesQueue[i].Importance:=1;
end;


procedure TKMBuildingQueue.AddNewHousePlan(aHouse: TKMHouse);
var i:integer;
begin
  i:=1; while (i<MaxEntries)and(fHousePlansQueue[i].JobStatus<>js_Empty) do inc(i);
  if aHouse <> nil then fHousePlansQueue[i].House:=aHouse.GetSelf;
  fHousePlansQueue[i].Importance:=1;
  fHousePlansQueue[i].JobStatus:=js_Open;
end;


function TKMBuildingQueue.AddHouseRepair(aHouse: TKMHouse):integer;
var i:integer;
begin
  i:=1; while (i<MaxEntries)and(fHousesRepairQueue[i].House<>nil) do inc(i);
  if aHouse <> nil then fHousesRepairQueue[i].House:=aHouse.GetSelf;
  fHousesRepairQueue[i].Importance:=1;
  Result:=i;
end;


{Remove task if Player has cancelled it}
{Simulated just means that we simply want to check if player ever issued that task}
{The erase cursor changes when you move over a piece of deletable road/field}
function TKMBuildingQueue.CancelRoad(aLoc:TKMPoint; Simulated:boolean=false):boolean;
var i:integer;
begin
  Result:=false;
  for i:=1 to length(fFieldsQueue) do
  with fFieldsQueue[i] do
  if (JobStatus<>js_Empty)and(KMSamePoint(aLoc,Loc)) then
  begin

    if not Simulated then
    begin
      if Worker<>nil then
        Worker.CancelUnitTask;
      CloseRoad(i);
    end;

    Result:=true;
    break;
  end;
end;


{Remove task if Player has cancelled it}
{Simulated just means that we simply want to check if player ever issued that task}
{The erase cursor changes when you move over a piece of deletable road/field}
function TKMBuildingQueue.CancelHousePlan(aLoc:TKMPoint; Simulated:boolean=false):boolean;
var i:integer;
begin
  Result:=false;
  for i:=1 to length(fHousePlansQueue) do
  with fHousePlansQueue[i] do
  if (JobStatus<>js_Empty)and(House<>nil)and(House.HitTest(aLoc.X,aLoc.Y)) then
  begin

    if not Simulated then
    begin
      if Worker<>nil then
        Worker.CancelUnitTask;
      CloseHousePlan(i);
    end;

    Result:=true;
    break;
  end;
end;


function  TKMBuildingQueue.AskForRoad(aWorker:TKMUnitWorker):TUnitTask;
var i:integer;
begin
  Result:=nil;

  i:=1;
  while
    (i<=MaxEntries)and
    ((fFieldsQueue[i].JobStatus<>js_Open)or
    (not fTerrain.Route_CanBeMade(aWorker.GetPosition, fFieldsQueue[i].Loc, canWalk, true))) do
      inc(i);

  if i>MaxEntries then exit;

  case fFieldsQueue[i].FieldType of
    ft_Road: Result:=TTaskBuildRoad.Create(aWorker, fFieldsQueue[i].Loc, i);
    ft_Corn: Result:=TTaskBuildField.Create(aWorker, fFieldsQueue[i].Loc, i);
    ft_Wine: Result:=TTaskBuildWine.Create(aWorker, fFieldsQueue[i].Loc, i);
    ft_Wall: Result:=TTaskBuildWall.Create(aWorker, fFieldsQueue[i].Loc, i);
    else Result:=nil;
  end;
  fFieldsQueue[i].JobStatus:=js_Taken;
  if aWorker <> nil then fFieldsQueue[i].Worker:=aWorker.GetSelf;
end;


{Find a job for worker}
function  TKMBuildingQueue.AskForHouse(aWorker:TKMUnitWorker):TUnitTask;
var i:integer;
begin
  Result:=nil; i:=1;
  while (i<MaxEntries)and((fHousesQueue[i].House=nil)or(not fHousesQueue[i].House.CheckResToBuild)) do inc(i);
  if i=MaxEntries then exit;

  Result:=TTaskBuildHouse.Create(aWorker, fHousesQueue[i].House, i);
end;


function  TKMBuildingQueue.AskForHousePlan(aWorker:TKMUnitWorker):TUnitTask;
var i:integer;
begin
  Result:=nil; i:=1;
  while (i<MaxEntries)and(fHousePlansQueue[i].JobStatus<>js_Open) do inc(i);
  if i=MaxEntries then exit;

  Result:=TTaskBuildHouseArea.Create(aWorker, fHousePlansQueue[i].House, i);
  fHousePlansQueue[i].JobStatus:=js_Taken;
  if aWorker <> nil then fHousePlansQueue[i].Worker:=aWorker.GetSelf;
end;


function  TKMBuildingQueue.AskForHouseRepair(aWorker:TKMUnitWorker):TUnitTask;
var i:integer;
begin
  Result := nil;
  for i:=1 to MaxEntries-1 do
    if fHousesRepairQueue[i].House<>nil then
      if fHousesRepairQueue[i].House.IsDamaged then
        if fHousesRepairQueue[i].House.BuildingRepair then
        begin
          Result := TTaskBuildHouseRepair.Create(aWorker, fHousesRepairQueue[i].House, i);
          exit;
        end
end;


procedure TKMBuildingQueue.Save(SaveStream:TKMemoryStream);
var i,Count:integer;
begin
  Count := length(fFieldsQueue);
  SaveStream.Write(Count);
  for i:=1 to Count do
  with fFieldsQueue[i] do
  begin
    SaveStream.Write(Loc);
    SaveStream.Write(FieldType, SizeOf(FieldType));
    SaveStream.Write(Importance);
    SaveStream.Write(JobStatus, SizeOf(JobStatus));
    if Worker <> nil then SaveStream.Write(Worker.ID) else SaveStream.Write(Zero);
  end;

  Count := length(fHousesQueue);
  SaveStream.Write(Count);
  for i:=1 to Count do
  begin
    if fHousesQueue[i].House <> nil then SaveStream.Write(fHousesQueue[i].House.ID) else SaveStream.Write(Zero);
    SaveStream.Write(fHousesQueue[i].Importance);
  end;

  Count := length(fHousePlansQueue);
  SaveStream.Write(Count);
  for i:=1 to Count do
  with fHousePlansQueue[i] do
  begin
    if House <> nil then SaveStream.Write(House.ID) else SaveStream.Write(Zero);
    SaveStream.Write(Importance);
    SaveStream.Write(JobStatus, SizeOf(JobStatus));
    if Worker <> nil then SaveStream.Write(Worker.ID) else SaveStream.Write(Zero);
  end;

  Count := length(fHousesRepairQueue);
  SaveStream.Write(Count);
  for i:=1 to Count do
  with fHousesRepairQueue[i] do
  begin
    if House <> nil then SaveStream.Write(House.ID) else SaveStream.Write(Zero);
    SaveStream.Write(Importance);
  end;
end;


procedure TKMBuildingQueue.Load(LoadStream:TKMemoryStream);
var i,Count:integer;
begin
  LoadStream.Read(Count);
  for i:=1 to Count do
  with fFieldsQueue[i] do
  begin
    LoadStream.Read(Loc);
    LoadStream.Read(FieldType, SizeOf(FieldType));
    LoadStream.Read(Importance);
    LoadStream.Read(JobStatus, SizeOf(JobStatus));
    LoadStream.Read(Worker, 4);
    Worker := fPlayers.GetUnitByID(integer(Worker));
  end;

  LoadStream.Read(Count);
  for i:=1 to Count do
  begin
    LoadStream.Read(fHousesQueue[i].House, 4);
    fHousesQueue[i].House := fPlayers.GetHouseByID(integer(fHousesQueue[i].House));
    LoadStream.Read(fHousesQueue[i].Importance);
  end;

  LoadStream.Read(Count);
  for i:=1 to Count do
  with fHousePlansQueue[i] do
  begin
    LoadStream.Read(House, 4);
    House := fPlayers.GetHouseByID(integer(House));
    LoadStream.Read(Importance);
    LoadStream.Read(JobStatus, SizeOf(JobStatus));
    LoadStream.Read(Worker, 4);
    Worker := fPlayers.GetUnitByID(integer(Worker));
  end;

  LoadStream.Read(Count);
  for i:=1 to Count do
  with fHousesRepairQueue[i] do
  begin
    LoadStream.Read(House, 4);
    House := fPlayers.GetHouseByID(integer(House));
    LoadStream.Read(Importance);
  end;
end;



end.

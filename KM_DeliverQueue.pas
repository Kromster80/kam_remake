unit KM_DeliverQueue;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, Math,
    KM_CommonClasses, KM_Defaults, KM_Houses, KM_Units, KM_UnitTaskDelivery, KM_Points;


type
  TJobStatus = (
        js_Empty,   //Empty - empty spot for a new job
        js_Open,    //Open - job is free to take by anyone
        js_Taken);  //Taken - job is taken by some worker

  TDemandImportance = (di_Norm, di_High);

type
  TKMDeliverQueue = class
  private
    OfferCount:integer;
    fOffer:array of
    record
      Resource:TResourceType;
      Count:cardinal;
      Loc_House:TKMHouse;
      BeingPerformed:cardinal; //How many items are being delivered atm from total Count offered
      IsDeleted:boolean; //So we don't get pointer issues
    end;
    DemandCount:integer;
    fDemand:array of
    record
      Resource:TResourceType;
      DemandType:TDemandType; //Once for everything, Always for Store and Barracks
      Importance:TDemandImportance; //How important demand is, e.g. Workers and building sites should be di_High
      Loc_House:TKMHouse;
      Loc_Unit:TKMUnit;
      BeingPerformed:boolean;
      IsDeleted:boolean; //So we don't get pointer issues
    end;
    QueueCount:integer;
    fQueue:array of
    record
      OfferID,DemandID:integer;
      JobStatus:TJobStatus; //Empty slot, resource Taken, job Done
    end;
    procedure CloseDelivery(aID:integer);
    procedure CloseDemand(aID:integer);
    procedure CloseOffer(aID:integer);
    function PermitDelivery(iO,iD:integer; KMSerf:TKMUnitSerf):boolean;
  public
    procedure AddNewOffer(aHouse:TKMHouse; aResource:TResourceType; aCount:integer);
    procedure RemoveOffer(aHouse:TKMHouse);

    procedure AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aCount:byte; aType:TDemandType; aImp:TDemandImportance);
    function TryRemoveDemand(aHouse:TKMHouse; aResource:TResourceType; aCount:word):word;
    procedure RemoveDemand(aHouse:TKMHouse); overload;
    procedure RemoveDemand(aUnit:TKMUnit); overload;

    function AskForDelivery(KMSerf:TKMUnitSerf; KMHouse:TKMHouse=nil):TTaskDeliver;
    procedure TakenOffer(aID:integer);
    procedure GaveDemand(aID:integer);
    procedure AbandonDelivery(aID:integer); //Occurs when unit is killed or something alike happens

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;

    procedure ExportToFile(aFileName:string);
  end;

  TKMBuildingQueue = class
  private
    fHousesCount:integer;
    fHousesQueue:array of
    record
      House:TKMHouse;
      WorkerCount:integer; //So we don't get pointer issues
      IsDeleted:boolean;
      //No need to have JobStatus since many workers can build same house
    end;
    procedure CloseHouse(aID:integer);
  public
    procedure RemoveHouse(aID: integer); overload;
    procedure RemoveHouse(aHouse: TKMHouse); overload;
    procedure RemoveHousePointer(aID:integer);
    procedure AddNewHouse(aHouse: TKMHouse);
    function AskForHouse(aWorker:TKMUnitWorker):TUnitTask;
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
  end;

  //Most complicated class
  //We need to combine 2 approaches for wares > serfs and wares < serfs
  //Houses signal when they have new wares/needs
  //Serfs signal when they are free to perform actions
  //List should be able to override Idling Serfs action
  //List should not override serfs deliveries even if the other serf can do it quicker,
  //because it will look bad to player, if first serfs stops for no reason
  //List does the comparison between houses and serfs and picks best pairs
  //(logic can be quite complicated and try to predict serfs/wares ETA)
  //Comparison function could be executed more rare or frequent depending on signals from houses/serfs
  //e.g. with no houses signals it can sleep till first on. At any case - not more frequent than 1/tick
  //TKMDeliveryList = class; //Serfs, Houses/Warriors/Workers

  //TKMBuildingList = class; //Workers, WIP Houses
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
    procedure RemField(aLoc: TKMPoint);

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
  //Houses and Workers are only added to the list. List checks itself when Houses/Workers should be removed from it
  TKMRepairList = class
  private
    fHousesCount: Integer;
    fHouses: array of record
      House: TKMHouse; //Pointer to house
      Assigned: Byte; //How many workers are assigned to it
    end;

    function HouseAlreadyInList(aHouse: TKMHouse): Boolean;
    procedure RemHouse(aIndex: Integer);
    procedure RemoveExtraHouses;
  public
    destructor Destroy; override;

    procedure AddHouse(aHouse: TKMHouse);
    function BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer; //Calculate best bid for a given worker
    procedure GiveTask(aIndex: Integer; aWorker: TKMUnitWorker);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;


  TKMWorkerList = class
  private
    fFieldworksList: TKMFieldworksList;
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
    property HousePlanList: TKMHousePlanList read fHousePlanList;
    property RepairList: TKMRepairList read fRepairList;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;


implementation
uses KM_Game, KM_Utils, KM_Terrain, KM_PlayersCollection, KM_UnitTaskBuild, KM_ResourceGFX, KM_Log, KM_UnitActionStay;


const
  LENGTH_INC = 32; //Increment array lengths by this value


{ TKMDeliverQueue }
//Adds new Offer to the list. List is stored without sorting
//(it matters only for Demand to keep everything in waiting its order in line),
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewOffer(aHouse:TKMHouse; aResource:TResourceType; aCount:integer);
var i,k:integer;
begin
  //Add Count of resource to old offer
  for i:=1 to OfferCount do
    if (fOffer[i].Loc_House=aHouse)and(fOffer[i].Resource=aResource)and not fOffer[i].IsDeleted then
    begin
      inc(fOffer[i].Count, aCount);
      exit; //we should exit now
    end;

  i:=1; while (i<=OfferCount)and(fOffer[i].Resource<>rt_None) do inc(i);
  if i>OfferCount then begin
    inc(OfferCount, LENGTH_INC);
    SetLength(fOffer, OfferCount+1);
    for k:=i to OfferCount do FillChar(fOffer[k],SizeOf(fOffer[k]),#0); //Initialise the new queue space
  end;

  with fOffer[i] do begin //Put offer
    if aHouse <> nil then Loc_House:=aHouse.GetHousePointer;
    Resource:=aResource;
    Count:=aCount;
    assert((BeingPerformed=0) and not IsDeleted); //Make sure this item has been closed properly, if not there is a flaw
  end;
end;


//Remove Offer from the list. E.G on house demolish
//List is stored without sorting so we have to parse it to find that entry..
procedure TKMDeliverQueue.RemoveOffer(aHouse:TKMHouse);
var i:integer;
begin
  //We need to parse whole list, never knowing how many offers the house had
  for i:=1 to OfferCount do
  if fOffer[i].Loc_House=aHouse then
    if fOffer[i].BeingPerformed > 0 then
    begin
      //Keep it until all associated deliveries are abandoned
      fOffer[i].IsDeleted := true; //Don't reset it until serfs performing this offer are done with it
      fOffer[i].Count := 0; //Make the count 0 so no one else tries to take this offer
    end
    else
      CloseOffer(i);
end;


//Remove Demand from the list. List is stored without sorting
//so we parse it to find all entries..
procedure TKMDeliverQueue.RemoveDemand(aHouse:TKMHouse);
var i:integer;
begin
  assert(aHouse <> nil);
  for i:=1 to DemandCount do
  if fDemand[i].Loc_House=aHouse then
  begin
    if fDemand[i].BeingPerformed then
      //Can't free it yet, some serf is using it
      fDemand[i].IsDeleted := true
    else
     CloseDemand(i); //Clear up demand
     //Keep on scanning cos House can have multiple demands entries
  end;
end;


//Remove Demand from the list. List is stored without sorting
//so we parse it to find all entries..
procedure TKMDeliverQueue.RemoveDemand(aUnit:TKMUnit);
var i:integer;
begin
  assert(aUnit <> nil);
  for i:=1 to DemandCount do
  if fDemand[i].Loc_Unit=aUnit then
  begin
    if fDemand[i].BeingPerformed then
      //Can't free it yet, some serf is using it
      fDemand[i].IsDeleted := true
    else
      CloseDemand(i); //Clear up demand
      //Keep on scanning cos Unit can have multiple demands entries (foreseeing Walls building)
  end;
end;


//Attempt to remove aCount demands from this house and report the number (only ones that are not yet being performed)
function TKMDeliverQueue.TryRemoveDemand(aHouse:TKMHouse; aResource:TResourceType; aCount:word):word;
var i:integer;
begin
  Result := 0;
  if aCount = 0 then exit;
  assert(aHouse <> nil);
  for i:=1 to DemandCount do
    if (fDemand[i].Loc_House = aHouse) and (fDemand[i].Resource = aResource) then
      if not fDemand[i].BeingPerformed then
      begin
        CloseDemand(i); //Clear up demand
        inc(Result);
        if Result = aCount then exit; //We have removed enough demands
      end;
end;


//Adds new Demand to the list. List is stored sorted, but the sorting is done upon Deliver completion,
//so we just find an empty place (which is last one) and write there.
procedure TKMDeliverQueue.AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aCount:byte; aType:TDemandType; aImp:TDemandImportance);
var i,k,j:integer;
begin
  if aResource = rt_None then
    fGame.GameError(KMPoint(0,0), 'Demanding rt_None');

  for k:=1 to aCount do begin
    i:=1; while (i<=DemandCount)and(fDemand[i].Resource<>rt_None) do inc(i);
    if i>DemandCount then begin
      inc(DemandCount, LENGTH_INC);
      SetLength(fDemand, DemandCount+1);
      for j:=i to DemandCount do FillChar(fDemand[j],SizeOf(fDemand[j]),#0); //Initialise the new queue space
    end;

    with fDemand[i] do begin
      if aHouse <> nil then Loc_House:=aHouse.GetHousePointer;
      if aUnit <> nil then Loc_Unit:=aUnit.GetUnitPointer;
      DemandType:=aType; //Once or Always
      Resource:=aResource;
      Importance:=aImp;
      assert((not IsDeleted) and (not BeingPerformed)); //Make sure this item has been closed properly, if not there is a flaw
      if GOLD_TO_SCHOOLS_IMPORTANT then
        if (Resource=rt_Gold)and(Loc_House<>nil)and(Loc_House.HouseType=ht_School) then Importance:=di_High;
      if FOOD_TO_INN_IMPORTANT then
        if (Resource in [rt_Bread,rt_Sausages,rt_Wine,rt_Fish])and
        (Loc_House<>nil)and(Loc_House.HouseType=ht_Inn) then Importance:=di_High;
    end;
  end;
end;


function TKMDeliverQueue.PermitDelivery(iO,iD:integer; KMSerf:TKMUnitSerf):boolean;
begin
  //If Offer Resource matches Demand
  Result := (fDemand[iD].Resource = fOffer[iO].Resource)or
            (fDemand[iD].Resource = rt_All)or
            ((fDemand[iD].Resource = rt_Warfare)and(fOffer[iO].Resource in [WARFARE_MIN..WARFARE_MAX]))or
            ((fDemand[iD].Resource = rt_Food)and(fOffer[iO].Resource in [rt_Bread,rt_Sausages,rt_Wine,rt_Fish]));

  //If Demand and Offer aren't reserved already
  Result := Result and ((not fDemand[iD].BeingPerformed) and (fOffer[iO].BeingPerformed < fOffer[iO].Count));

  //If Demand and Offer aren't deleted
  Result := Result and (not fDemand[iD].IsDeleted) and (not fOffer[iO].IsDeleted);

  //If Demand house has WareDelivery toggled ON
  Result := Result and ((fDemand[iD].Loc_House=nil) or (fDemand[iD].Loc_House.WareDelivery));

  //If Demand is a Storehouse and it has WareDelivery toggled ON
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fDemand[iD].Loc_House.HouseType<>ht_Store)or
                        (not TKMHouseStore(fDemand[iD].Loc_House).NotAcceptFlag[fOffer[iO].Resource]));

  //NEVER deliver weapons to the storehouse when player has a barracks
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fDemand[iD].Loc_House.HouseType<>ht_Store)or
                       (not (fOffer[iO].Resource in [WARFARE_MIN..WARFARE_MAX]))or(fPlayers.Player[fDemand[iD].Loc_House.GetOwner].Stats.GetHouseQty(ht_Barracks)=0));

  //If Demand and Offer are different HouseTypes, means forbid Store<->Store deliveries except the case where 2nd store is being built and requires building materials
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fOffer[iO].Loc_House.HouseType<>fDemand[iD].Loc_House.HouseType)or(fOffer[iO].Loc_House.IsComplete<>fDemand[iD].Loc_House.IsComplete));


  Result := Result and (
            ( //House-House delivery should be performed only if there's a connecting road
            (fDemand[iD].Loc_House<>nil)and
            (fTerrain.Route_CanBeMade(KMPointBelow(fOffer[iO].Loc_House.GetEntrance),KMPointBelow(fDemand[iD].Loc_House.GetEntrance),CanWalkRoad,0, false))
            )
            or
            ( //House-Unit delivery can be performed without connecting road
            (fDemand[iD].Loc_Unit<>nil)and
            (fTerrain.Route_CanBeMade(KMPointBelow(fOffer[iO].Loc_House.GetEntrance),fDemand[iD].Loc_Unit.GetPosition,CanWalk,1, false))
            )
            );

  Result := Result and //Delivery is only permitted if the serf can access the from house. If the serf is inside (invisible) test from point below.
           ((    KMSerf.Visible and fTerrain.Route_CanBeMade(KMPointBelow(fOffer[iO].Loc_House.GetEntrance),KMSerf.GetPosition,CanWalk,0,false)) or
            (not KMSerf.Visible and fTerrain.Route_CanBeMade(KMPointBelow(fOffer[iO].Loc_House.GetEntrance),KMPointBelow(KMSerf.GetPosition),CanWalk,0,false)));
end;


//Should issue a job based on requesters location and job importance
//Serf may ask for a job from within a house after completing previous delivery
function TKMDeliverQueue.AskForDelivery(KMSerf:TKMUnitSerf; KMHouse:TKMHouse=nil):TTaskDeliver;
var i,iD,iO:integer; Bid,BestBid:single; BidIsPriority: boolean;
begin
  Result:=nil;

  //Find a place where Delivery will be written to after Offer-Demand pair is found
  i:=1; while (i<=QueueCount)and(fQueue[i].JobStatus<>js_Empty) do inc(i);
  if i>QueueCount then begin
    inc(QueueCount, LENGTH_INC);
    SetLength(fQueue, QueueCount+1);
  end;


  //Find Offer matching Demand
  //TravelRoute Asker>Offer>Demand should be shortest
  BestBid:=0;
  BidIsPriority := false;
  for iD:=1 to DemandCount do
  if BestBid=1 then break else //Quit loop when best bid is found
  if fDemand[iD].Resource <> rt_None then
  for iO:=1 to OfferCount do
   if BestBid=1 then break else //Quit loop when best bid is found
    if (KMHouse = nil) or (fOffer[iO].Loc_House = KMHouse) then //Make sure from house is the one requested
    if fOffer[iO].Resource <> rt_None then

    if PermitDelivery(iO,iD,KMSerf) then
    begin

      //Basic Bid is length of route
      if fDemand[iD].Loc_House<>nil then
        Bid := GetLength(fOffer[iO].Loc_House.GetEntrance,fDemand[iD].Loc_House.GetEntrance)
      else
        Bid := GetLength(fOffer[iO].Loc_House.GetEntrance,fDemand[iD].Loc_Unit.GetPosition);

      //Add some random element so in the case of identical bids the same resource will not always be chosen (e.g. weapons storehouse->barracks should take random weapon types not sequentially)
      Bid:=Bid + KaMRandom(5);

      //Modifications for bidding system
      if (fDemand[iD].Resource=rt_All) //Always prefer deliveries House>House instead of House>Store
      or (fOffer[iO].Loc_House.HouseType = ht_Store) then //Prefer taking wares from House rather than Store
        Bid:=Bid + 1000;

      if fDemand[iD].Loc_House<>nil then //Prefer delivering to houses with fewer supply
      if (fDemand[iD].Resource <> rt_All)and(fDemand[iD].Resource <> rt_Warfare) then //Except Barracks and Store, where supply doesn't matter or matter less
        Bid:=Bid + 20 * fDemand[iD].Loc_House.CheckResIn(fDemand[iD].Resource);

      //Delivering weapons from store to barracks, make it lowest priority when there are >50 of that weapon in the barracks.
      //In some missions the storehouse has vast amounts of weapons, and we don't want the serfs to spend the whole game moving these.
      //In KaM, if the barracks has >200 weapons the serfs will stop delivering from the storehouse. I think our solution is better.
      if fDemand[iD].Loc_House<>nil then
      if (fDemand[iD].Loc_House.HouseType = ht_Barracks)and(fOffer[iO].Loc_House.HouseType = ht_Store)and
         (fDemand[iD].Loc_House.CheckResIn(fOffer[iO].Resource) > 50) then
         Bid := Bid + 10000;

      //When delivering food to warriors, add a random amount to bid to ensure that a variety of food is taken. Also prefer food which is more abundant.
      if (fDemand[iD].Loc_Unit<>nil) and (fDemand[iD].Resource = rt_Food) then
        Bid:=Bid + KaMRandom(5+(100 div fOffer[iO].Count)); //The more resource there is, the smaller Random can be. >100 we no longer care, it's just random 5.

      if fDemand[iD].Importance=di_High then //If Demand importance is high - make it done ASAP
      begin
        if not BidIsPriority then BestBid := 9999999; //Override previously chosen low priority delivery
        BidIsPriority := true;
      end
      else
        if BidIsPriority then continue; //Do not take any low priority bids once a high one is found

      //Take first one incase there's nothing better to be found
      //Do not take deliveries with Bid=0 (no route found)
      if (Bid<>0)and((fQueue[i].JobStatus=js_Empty)or(Bid<BestBid)) then
      begin
        fQueue[i].DemandID:=iD;
        fQueue[i].OfferID:=iO;
        fQueue[i].JobStatus:=js_Taken; //The job is found, at least something
        BestBid:=Bid;
      end;

    end;

  if BestBid=0 then exit; //No suitable delivery has been found at all

  iD:=fQueue[i].DemandID;
  iO:=fQueue[i].OfferID;
  inc(fOffer[iO].BeingPerformed); //Places a virtual "Reserved" sign on Offer
  fDemand[iD].BeingPerformed:=true; //Places a virtual "Reserved" sign on Demand

  //Store never has enough demand performed
  if (fDemand[iD].Loc_House<>nil)and(fDemand[iD].DemandType = dt_Always) then fDemand[iD].BeingPerformed:=false;

  if WRITE_DELIVERY_LOG then fLog.AppendLog('Creating delivery ID', i);

  //Now we have best job and can perform it
  if fDemand[iD].Loc_House <> nil then
    Result := TTaskDeliver.Create(KMSerf, fOffer[iO].Loc_House, fDemand[iD].Loc_House, fOffer[iO].Resource, i)
  else
    Result := TTaskDeliver.Create(KMSerf, fOffer[iO].Loc_House, fDemand[iD].Loc_Unit, fOffer[iO].Resource, i)
end;


//Resource has been taken from Offer
procedure TKMDeliverQueue.TakenOffer(aID:integer);
var iO:integer;
begin
  if WRITE_DELIVERY_LOG then fLog.AppendLog('Taken offer from delivery ID', aID);

  iO:=fQueue[aID].OfferID;
  fQueue[aID].OfferID:=0; //We don't need it any more

  dec(fOffer[iO].BeingPerformed); //Remove reservation
  dec(fOffer[iO].Count); //Remove resource from Offer list

  if fOffer[iO].Count=0 then
    CloseOffer(iO);
end;


//Resource has been delivered to Demand
procedure TKMDeliverQueue.GaveDemand(aID:integer);
var iD:integer;
begin
  if WRITE_DELIVERY_LOG then fLog.AppendLog('Gave demand from delivery ID', aID);
  iD:=fQueue[aID].DemandID;
  fQueue[aID].DemandID:=0; //We don't need it any more

  fDemand[iD].BeingPerformed:=false; //Remove reservation

  if fDemand[iD].DemandType=dt_Once then
    CloseDemand(iD); //Remove resource from Demand list
end;


//AbandonDelivery
procedure TKMDeliverQueue.AbandonDelivery(aID:integer);
begin
  if WRITE_DELIVERY_LOG then fLog.AppendLog('Abandoned delivery ID', aID);

  //Remove reservations without removing items from lists
  if fQueue[aID].OfferID <> 0 then
  begin
    dec(fOffer[fQueue[aID].OfferID].BeingPerformed);
    //Now see if we need to delete the Offer as we are the last remaining pointer
    if fOffer[fQueue[aID].OfferID].IsDeleted and (fOffer[fQueue[aID].OfferID].BeingPerformed = 0) then
      CloseOffer(fQueue[aID].OfferID);
  end;

  if fQueue[aID].DemandID <> 0 then
  begin
    fDemand[fQueue[aID].DemandID].BeingPerformed:=false;
    if fDemand[fQueue[aID].DemandID].IsDeleted then
      CloseDemand(fQueue[aID].DemandID);
  end;

  CloseDelivery(aID);
end;


//Job successfully done and we ommit it
procedure TKMDeliverQueue.CloseDelivery(aID:integer);
begin
  if WRITE_DELIVERY_LOG then fLog.AppendLog('Closed delivery ID', aID);

  fQueue[aID].OfferID:=0;
  fQueue[aID].DemandID:=0;
  fQueue[aID].JobStatus:=js_Empty; //Open slot
end;


procedure TKMDeliverQueue.CloseDemand(aID:integer);
begin
  assert(not fDemand[aID].BeingPerformed);
  fDemand[aID].Resource := rt_None;
  fDemand[aID].DemandType := dt_Once;
  fDemand[aID].Importance := di_Norm;
  fPlayers.CleanUpHousePointer(fDemand[aID].Loc_House);
  fPlayers.CleanUpUnitPointer(fDemand[aID].Loc_Unit);
  fDemand[aID].IsDeleted := false;
end;


procedure TKMDeliverQueue.CloseOffer(aID:integer);
begin
  assert(fOffer[aID].BeingPerformed = 0);
  fOffer[aID].IsDeleted := false;
  fOffer[aID].Resource := rt_None;
  fOffer[aID].Count := 0;
  fPlayers.CleanUpHousePointer(fOffer[aID].Loc_House);
end;


procedure TKMDeliverQueue.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write('Deliveries');
  SaveStream.Write(OfferCount);
  for i:=1 to OfferCount do
  begin
    SaveStream.Write(fOffer[i].Resource, SizeOf(fOffer[i].Resource));
    SaveStream.Write(fOffer[i].Count);
    if fOffer[i].Loc_House <> nil then
      SaveStream.Write(fOffer[i].Loc_House.ID)
    else
      SaveStream.Write(Integer(0));
    SaveStream.Write(fOffer[i].BeingPerformed);
    SaveStream.Write(fOffer[i].IsDeleted);
  end;

  SaveStream.Write(DemandCount);
  for i:=1 to DemandCount do
  with fDemand[i] do
  begin
    SaveStream.Write(Resource, SizeOf(Resource));
    SaveStream.Write(DemandType, SizeOf(DemandType));
    SaveStream.Write(Importance, SizeOf(Importance));
    if Loc_House <> nil then SaveStream.Write(Loc_House.ID) else SaveStream.Write(Integer(0));
    if Loc_Unit  <> nil then SaveStream.Write(Loc_Unit.ID ) else SaveStream.Write(Integer(0));
    SaveStream.Write(BeingPerformed);
    SaveStream.Write(IsDeleted);
  end;

  SaveStream.Write(QueueCount);
  for i:=1 to QueueCount do
  begin
    SaveStream.Write(fQueue[i].OfferID);
    SaveStream.Write(fQueue[i].DemandID);
    SaveStream.Write(fQueue[i].JobStatus, SizeOf(fQueue[i].JobStatus));
  end;
end;


procedure TKMDeliverQueue.Load(LoadStream:TKMemoryStream);
var i:integer; s:string;
begin
  LoadStream.Read(s);
  Assert(s = 'Deliveries');
  LoadStream.Read(OfferCount);
  SetLength(fOffer, OfferCount+1);
  for i:=1 to OfferCount do
  begin
    LoadStream.Read(fOffer[i].Resource, SizeOf(fOffer[i].Resource));
    LoadStream.Read(fOffer[i].Count);
    LoadStream.Read(fOffer[i].Loc_House, 4);
    LoadStream.Read(fOffer[i].BeingPerformed);
    LoadStream.Read(fOffer[i].IsDeleted);
  end;

  LoadStream.Read(DemandCount);
  SetLength(fDemand, DemandCount+1);
  for i:=1 to DemandCount do
  with fDemand[i] do
  begin
    LoadStream.Read(Resource, SizeOf(Resource));
    LoadStream.Read(DemandType, SizeOf(DemandType));
    LoadStream.Read(Importance, SizeOf(Importance));
    LoadStream.Read(Loc_House, 4);
    LoadStream.Read(Loc_Unit, 4);
    LoadStream.Read(BeingPerformed);
    LoadStream.Read(IsDeleted);
  end;

  LoadStream.Read(QueueCount);
  SetLength(fQueue, QueueCount+1);
  for i:=1 to QueueCount do
  begin
    LoadStream.Read(fQueue[i].OfferID);
    LoadStream.Read(fQueue[i].DemandID);
    LoadStream.Read(fQueue[i].JobStatus, SizeOf(fQueue[i].JobStatus));
  end;
end;


procedure TKMDeliverQueue.SyncLoad;
var i:integer;
begin
  for i:=1 to OfferCount do
    fOffer[i].Loc_House := fPlayers.GetHouseByID(cardinal(fOffer[i].Loc_House));

  for i:=1 to DemandCount do
  with fDemand[i] do
  begin
    Loc_House := fPlayers.GetHouseByID(cardinal(Loc_House));
    Loc_Unit := fPlayers.GetUnitByID(cardinal(Loc_Unit));
  end;
end;


procedure TKMDeliverQueue.ExportToFile(aFileName:string);
var i:integer; f:textfile; s:string;
begin
  assignfile(f,aFileName); Rewrite(f);

  s:='Demand:'+eol+'---------------------------------'+eol;
  for i:=1 to DemandCount do if fDemand[i].Resource<>rt_None then begin
    s:=s+#9;
    if fDemand[i].Loc_House<>nil then s:=s+fResource.HouseDat[fDemand[i].Loc_House.HouseType].HouseName+#9+#9;
    if fDemand[i].Loc_Unit<>nil then s:=s+fResource.UnitDat[fDemand[i].Loc_Unit.UnitType].UnitName+#9+#9;
    s:=s+fResource.Resources[fDemand[i].Resource].Name;
    if fDemand[i].Importance=di_High then s:=s+'^';
    s:=s+eol;
  end;
  s:=s+eol+'Offer:'+eol+'---------------------------------'+eol;
  for i:=1 to OfferCount do if fOffer[i].Resource<>rt_None then begin
    s:=s+#9;
    if fOffer[i].Loc_House<>nil then s:=s+fResource.HouseDat[fOffer[i].Loc_House.HouseType].HouseName+#9+#9;
    s:=s+fResource.Resources[fOffer[i].Resource].Name+#9;
    s:=s+IntToStr(fOffer[i].Count);
    s:=s+eol;
  end;

  s:=s+eol+'Running deliveries:'+eol+'---------------------------------'+eol;
  for i:=1 to QueueCount do if fQueue[i].OfferID<>0 then begin

    s:=s+'id '+inttostr(i)+'.'+#9;
    s:=s+fResource.Resources[fOffer[fQueue[i].OfferID].Resource].Name+#9;

    if fOffer[fQueue[i].OfferID].Loc_House = nil then
      s:=s+'Destroyed'+' >>> '
    else
      s:=s+fResource.HouseDat[fOffer[fQueue[i].OfferID].Loc_House.HouseType].HouseName+' >>> ';

    if fDemand[fQueue[i].DemandID].Loc_House = nil then
      s:=s+'Destroyed'
    else
      s:=s+fResource.HouseDat[fDemand[fQueue[i].DemandID].Loc_House.HouseType].HouseName;
    s:=s+eol;
  end;

  write(f,s);
  closefile(f);
end;


{TKMBuildingQueue}
procedure TKMBuildingQueue.CloseHouse(aID:integer);
begin
  assert(fHousesQueue[aID].WorkerCount=0);
  fPlayers.CleanUpHousePointer(fHousesQueue[aID].House);
  fHousesQueue[aID].IsDeleted := false;
end;


procedure TKMBuildingQueue.RemoveHouse(aID: integer);
begin
  if fHousesQueue[aID].WorkerCount = 0 then
    CloseHouse(aID)
  else
    fHousesQueue[aID].IsDeleted := true; //Can't delete it until all workers have finished with i
end;


procedure TKMBuildingQueue.RemoveHouse(aHouse: TKMHouse);
var i:integer;
begin
  for i:=1 to fHousesCount do
    if fHousesQueue[i].House=aHouse then
      RemoveHouse(i);
end;


procedure TKMBuildingQueue.RemoveHousePointer(aID:integer);
begin
  dec(fHousesQueue[aID].WorkerCount);
  if (fHousesQueue[aID].IsDeleted) and (fHousesQueue[aID].WorkerCount = 0) then
    CloseHouse(aID);
end;


{Add new job to the list}
procedure TKMBuildingQueue.AddNewHouse(aHouse: TKMHouse);
var i,k:integer;
begin
  i:=1; while (i<=fHousesCount)and(fHousesQueue[i].House<>nil) do inc(i);
  if i>fHousesCount then begin
    inc(fHousesCount, LENGTH_INC);
    SetLength(fHousesQueue, fHousesCount+1);
    for k:=i to fHousesCount do FillChar(fHousesQueue[k],SizeOf(fHousesQueue[k]),#0);
  end;

  assert((fHousesQueue[i].WorkerCount=0) and not fHousesQueue[i].IsDeleted);
  if aHouse <> nil then fHousesQueue[i].House := aHouse.GetHousePointer;
end;


{Find a job for worker}
function TKMBuildingQueue.AskForHouse(aWorker:TKMUnitWorker):TUnitTask;
var i, Best: integer; BestDist: single;
begin
  Result := nil;
  Best := -1;
  BestDist := MaxSingle;

  for i:=1 to fHousesCount do
    if (fHousesQueue[i].House<>nil) and (not fHousesQueue[i].IsDeleted) and fHousesQueue[i].House.CheckResToBuild
    and fTerrain.Route_CanBeMade(aWorker.GetPosition, KMPointBelow(fHousesQueue[i].House.GetEntrance), aWorker.GetDesiredPassability, 0, false)
    and((Best = -1)or(GetLength(aWorker.GetPosition, fHousesQueue[i].House.GetPosition) < BestDist))then
    begin
      Best := i;
      BestDist := GetLength(aWorker.GetPosition, fHousesQueue[i].House.GetPosition);
    end;

  if Best <> -1 then
  begin
    Result := TTaskBuildHouse.Create(aWorker, fHousesQueue[Best].House, Best);
    inc(fHousesQueue[Best].WorkerCount);
  end;
end;


procedure TKMBuildingQueue.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write('BuildQueue');

  SaveStream.Write(fHousesCount);
  for i:=1 to fHousesCount do
  begin
    if fHousesQueue[i].House <> nil then SaveStream.Write(fHousesQueue[i].House.ID) else SaveStream.Write(Integer(0));
    SaveStream.Write(fHousesQueue[i].WorkerCount);
    SaveStream.Write(fHousesQueue[i].IsDeleted);
  end;
end;


procedure TKMBuildingQueue.Load(LoadStream:TKMemoryStream);
var i:integer; s:string;
begin
  LoadStream.Read(s);
  Assert(s = 'BuildQueue');

  LoadStream.Read(fHousesCount);
  SetLength(fHousesQueue, fHousesCount+1);
  for i:=1 to fHousesCount do
  begin
    LoadStream.Read(fHousesQueue[i].House, 4);
    LoadStream.Read(fHousesQueue[i].WorkerCount);
    LoadStream.Read(fHousesQueue[i].IsDeleted);
  end;
end;


procedure TKMBuildingQueue.SyncLoad;
var i:integer;
begin
  for i:=1 to fHousesCount do
    fHousesQueue[i].House := fPlayers.GetHouseByID(cardinal(fHousesQueue[i].House));
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
    SetLength(fFields, fFieldsCount + LENGTH_INC);

  fFields[I].Loc := aLoc;
  fFields[I].FieldType := aFieldType;
  fFields[I].JobStatus := js_Open;
  fFields[I].Worker := nil;

  Inc(fFieldsCount);
end;


procedure TKMFieldworksList.RemField(aLoc: TKMPoint);
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

  SaveStream.Write(Length(fFields));
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
    SetLength(fPlans, fPlansCount + LENGTH_INC);

  fPlans[I].House := aHouse.GetHousePointer;
  fPlans[I].JobStatus := js_Open;
  fPlans[I].Worker := nil;

  Inc(fPlansCount);
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
  for I := fHousesCount - 1 downto 0 do
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
begin
  if HouseAlreadyInList(aHouse) then Exit;

  if fHousesCount >= Length(fHouses) then
    SetLength (fHouses, fHousesCount + 16);

  fHouses[fHousesCount].House := aHouse.GetHousePointer;
  fHouses[fHousesCount].Assigned := 0;
  Inc(fHousesCount);
end;


//Remove repaired or destroyed House from the List
procedure TKMRepairList.RemHouse(aIndex: Integer);
begin
  fPlayers.CleanUpHousePointer(fHouses[aIndex].House);

  if aIndex <> fHousesCount - 1 then
    Move(fHouses[aIndex+1], fHouses[aIndex], SizeOf(fHouses[aIndex]) * (fHousesCount - 1 - aIndex));

  Dec(fHousesCount);
end;


//Remove houses that should not be repaired any more
procedure TKMRepairList.RemoveExtraHouses;
var
  I: Integer;
begin
  for I := fHousesCount - 1 downto 0 do
    if not fHouses[I].House.IsComplete
    or not fHouses[I].House.IsDamaged
    or not fHouses[I].House.BuildingRepair
    or fHouses[I].House.IsDestroyed then
      RemHouse(I);
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


function TKMRepairList.BestBid(aWorker: TKMUnitWorker; out aBid: Single): Integer;
var
  I: Integer;
  NewBid: Single;
begin
  //We can weight the repairs by distance, severity, etc..
  //For now, each worker will go for the house closest to him

  Result := -1;
  aBid := 999;
  for I := fHousesCount - 1 downto 0 do
  begin
    NewBid := GetLength(aWorker.GetPosition, fHouses[I].House.GetPosition);
    NewBid := NewBid * fHouses[I].Assigned;

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
  aWorker.SetUnitTask := TTaskBuildHouseRepair.Create(aWorker, fHouses[aIndex].House);
end;


procedure TKMRepairList.UpdateState;
begin
  RemoveExtraHouses;
end;


{ TKMWorkersList }
constructor TKMWorkerList.Create;
begin
  inherited;

  fFieldworksList := TKMFieldworksList.Create;
  fHousePlanList := TKMHousePlanList.Create;
  fRepairList := TKMRepairList.Create;
end;


destructor TKMWorkerList.Destroy;
var
  I: Integer;
begin
  fFieldworksList.Free;
  fHousePlanList.Free;
  fRepairList.Free;

  for I := fWorkersCount - 1 downto 0 do
    fPlayers.CleanUpUnitPointer(TKMUnit(fWorkers[I].Worker));

  inherited;
end;


//Add the Worker to the List
procedure TKMWorkerList.AddWorker(aWorker: TKMUnitWorker);
begin
  if fWorkersCount >= Length(fWorkers) then
    SetLength (fWorkers, fWorkersCount + 16);

  fWorkers[fWorkersCount].Worker := TKMUnitWorker(aWorker.GetUnitPointer);
  Inc(fWorkersCount);
end;

//Remove died Worker from the List
procedure TKMWorkerList.RemWorker(aIndex: Integer);
begin
  fPlayers.CleanUpUnitPointer(TKMUnit(fWorkers[aIndex].Worker));

  if aIndex <> fWorkersCount - 1 then
    Move(fWorkers[aIndex+1], fWorkers[aIndex], SizeOf(fWorkers[aIndex]) * (fWorkersCount - 1 - aIndex));

  Dec(fWorkersCount);
end;


//Remove dead workers
procedure TKMWorkerList.RemoveExtraWorkers;
var
  I: Integer;
begin
  for I := fWorkersCount - 1 downto 0 do
    if fWorkers[I].Worker.IsDeadOrDying then
      RemWorker(I);
end;


procedure TKMWorkerList.Save(SaveStream: TKMemoryStream);
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
  fHousePlanList.Save(SaveStream);
  fRepairList.Save(SaveStream);
end;


procedure TKMWorkerList.Load(LoadStream: TKMemoryStream);
var I: Integer; s: string;
begin
  LoadStream.Read(s);
  Assert(s = 'WorkerList');

  LoadStream.Read(fWorkersCount);
  SetLength(fWorkers, fWorkersCount);
  for I := 0 to fWorkersCount - 1 do
    LoadStream.Read(fWorkers[I].Worker, 4);

  fFieldworksList.Load(LoadStream);
  fHousePlanList.Load(LoadStream);
  fRepairList.Load(LoadStream);
end;


procedure TKMWorkerList.SyncLoad;
var I: Integer; U: TKMUnit;
begin
  for I := 0 to fWorkersCount - 1 do
  begin
    U := fPlayers.GetUnitByID(Cardinal(fWorkers[I].Worker));
    Assert(U is TKMUnitWorker, 'Non-worker in Repairs list');
    fWorkers[I].Worker := TKMUnitWorker(U);
  end;

  fFieldworksList.SyncLoad;
  fHousePlanList.SyncLoad;
  fRepairList.SyncLoad;
end;


procedure TKMWorkerList.UpdateState;
var I: Integer;
  IndexRepair, IndexPlan, IndexField: Integer;
  BidRepair, BidPlan, BidField: Single;
begin
  fRepairList.UpdateState;

  RemoveExtraWorkers;

  //We can weight the repairs by distance, severity, etc..
  //For now, each worker will go for the house closest to him
  for I := 0 to fWorkersCount - 1 do
    if (fWorkers[I].Worker.GetUnitAction is TUnitActionStay)
    and not TUnitActionStay(fWorkers[I].Worker.GetUnitAction).Locked then
    begin

      IndexField := fFieldworksList.BestBid(fWorkers[I].Worker, BidField);
      IndexPlan := fHousePlanList.BestBid(fWorkers[I].Worker, BidPlan);
      IndexRepair := fRepairList.BestBid(fWorkers[I].Worker, BidRepair);

      if IndexRepair <> -1 then
        fRepairList.GiveTask(IndexRepair, fWorkers[I].Worker)
      else
      if IndexPlan <> -1 then
        fHousePlanList.GiveTask(IndexPlan, fWorkers[I].Worker)
      else
      if IndexField <> -1 then
        fFieldworksList.GiveTask(IndexField, fWorkers[I].Worker);
    end;
end;


end.

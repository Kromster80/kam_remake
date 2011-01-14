unit KM_DeliverQueue;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, KM_CommonTypes, KM_Defaults, KM_Houses, KM_Units, KM_UnitTaskDelivery;

  type TJobStatus = (js_Empty, js_Open, js_Taken);
  //Empty - empty spot for a new job
  //Open - job is free to take by anyone
  //Taken - job is taken by some worker
  type TDemandImportance = (di_Norm, di_High);
  const LENGTH_INC = 32; //Increment array lengths by this value

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
    end;
    QueueCount:integer;
    fQueue:array of
    record
      OfferID,DemandID:integer;
      //Bid:integer; //Contains bid of current runner, e.g. 12sec, if new asker can make it quicker, then reassign to him,
      //of course if Resource is still not taken from Offer
      JobStatus:TJobStatus; //Empty slot, resource Taken, job Done
    end;
    procedure CloseDelivery(aID:integer);
  public
    constructor Create;
    procedure AddNewOffer(aHouse:TKMHouse; aResource:TResourceType; aCount:integer);
    procedure RemoveOffer(aHouse:TKMHouse);
    procedure RemoveDemand(aHouse:TKMHouse); overload;
    procedure RemoveDemand(aUnit:TKMUnit); overload;
    procedure AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aCount:byte; aType:TDemandType; aImp:TDemandImportance);
    function PermitDelivery(iO,iD:integer):boolean;
    function AskForDelivery(KMSerf:TKMUnitSerf; KMHouse:TKMHouse=nil):TTaskDeliver;
    procedure TakenOffer(aID:integer);
    procedure GaveDemand(aID:integer);
    procedure AbandonDelivery(aID:integer); //Occurs when unit is killed or something alike happens
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad();
    procedure SaveToFile(aFileName:string);
  end;

  TKMBuildingQueue = class
  private
    FieldsCount:integer;
    fFieldsQueue:array of
    record
      Loc:TKMPoint;
      FieldType:TFieldType;
      Importance:byte;
      JobStatus:TJobStatus;
      Worker:TKMUnit;
    end;
    HousesCount:integer;
    fHousesQueue:array of
    record
      House:TKMHouse;
      Importance:byte;
      //No need to have JobStatus since many workers can build same house
    end;
    HousePlansCount:integer;
    fHousePlansQueue:array of
    record
      House:TKMHouse;
      Importance:byte;
      JobStatus:TJobStatus;
      Worker:TKMUnit;
    end;
    HouseRepairsCount:integer;
    fHouseRepairsQueue:array of
    record
      House:TKMHouse;
      Importance:byte;
      //No need to have JobStatus since many workers can repair same house
    end;
  public
    constructor Create;
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

    function GetHouseWipCount(aHouseType: THouseType):integer;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad();
  end;

implementation
uses KM_Game, KM_Utils, KM_Units_Warrior, KM_Terrain, KM_PlayersCollection, KM_UnitTaskBuild;


{ TKMDeliverQueue }
constructor TKMDeliverQueue.Create;
begin
  Inherited;
end;


//Adds new Offer to the list. List is stored without sorting
//(it matters only for Demand to keep everything in waiting its order in line),
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddNewOffer(aHouse:TKMHouse; aResource:TResourceType; aCount:integer);
var i:integer;
begin
  //Add Count of resource to old offer
  for i:=1 to OfferCount do
    if (fOffer[i].Loc_House=aHouse)and(fOffer[i].Resource=aResource) then
    begin
      inc(fOffer[i].Count, aCount);
      exit; //we should exit now
    end;

  i:=1; while (i<=OfferCount)and(fOffer[i].Resource<>rt_None) do inc(i);
  if i>OfferCount then begin
    inc(OfferCount, LENGTH_INC);
    setlength(fOffer, OfferCount+1);
  end;

  with fOffer[i] do begin //Put offer
    if aHouse <> nil then Loc_House:=aHouse.GetHousePointer;
    Resource:=aResource;
    Count:=aCount;
    BeingPerformed:=0; //New unique offer is available to be performed apriori
    IsDeleted := false;
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
    begin
      fOffer[i].IsDeleted := false;
      fOffer[i].Resource := rt_None;
      fOffer[i].Count := 0;
      fPlayers.CleanUpHousePointer(fOffer[i].Loc_House);
    end;
end;


//Remove Demand from the list. List is stored without sorting
//so we parse it to find all entries..
procedure TKMDeliverQueue.RemoveDemand(aHouse:TKMHouse);
var i:integer;
begin
  for i:=1 to DemandCount do
  if fDemand[i].Loc_House=aHouse then
  begin
    fPlayers.CleanUpHousePointer(fDemand[i].Loc_House);
    FillChar(fDemand[i],SizeOf(fDemand[i]),#0); //Clear up demand
    //Keep on scanning cos House can have multiple demands entries
  end;
end;


//Remove Demand from the list. List is stored without sorting
//so we parse it to find all entries..
procedure TKMDeliverQueue.RemoveDemand(aUnit:TKMUnit);
var i:integer;
begin
  for i:=1 to DemandCount do
  if fDemand[i].Loc_Unit=aUnit then
  begin
    fPlayers.CleanUpUnitPointer(fDemand[i].Loc_Unit);
    FillChar(fDemand[i],SizeOf(fDemand[i]),#0); //Clear up demand
    //Keep on scanning cos Unit can have multiple demands entries (foreseeing Walls building)
  end;
end;


//Adds new Demand to the list. List is stored sorted, but the sorting is done upon Deliver completion,
//so we just find an empty place (which is last one) and write there.
procedure TKMDeliverQueue.AddNewDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TResourceType; aCount:byte; aType:TDemandType; aImp:TDemandImportance);
var i,k:integer;
begin
  if aResource = rt_None then
    fGame.GameError(KMPoint(0,0), 'Demanding rt_None');

  for k:=1 to aCount do begin
    i:=1; while (i<=DemandCount)and(fDemand[i].Resource<>rt_None) do inc(i);
    if i>DemandCount then begin
      inc(DemandCount, LENGTH_INC);
      setlength(fDemand, DemandCount+1);
    end;

    with fDemand[i] do begin
      if aHouse <> nil then Loc_House:=aHouse.GetHousePointer;
      if aUnit <> nil then Loc_Unit:=aUnit.GetUnitPointer;
      DemandType:=aType; //Once or Always
      Resource:=aResource;
      Importance:=aImp;
      BeingPerformed:=false;
      if GOLD_TO_SCHOOLS_IMPORTANT then
        if (Resource=rt_Gold)and(Loc_House<>nil)and(Loc_House.GetHouseType=ht_School) then Importance:=di_High;
      if FOOD_TO_INN_IMPORTANT then
        if (Resource in [rt_Bread,rt_Sausages,rt_Wine,rt_Fish])and
        (Loc_House<>nil)and(Loc_House.GetHouseType=ht_Inn) then Importance:=di_High;
    end;
  end;
end;


function TKMDeliverQueue.PermitDelivery(iO,iD:integer):boolean;
begin
  //If Offer Resource matches Demand
  Result := (fDemand[iD].Resource = fOffer[iO].Resource)or
            (fDemand[iD].Resource = rt_All)or
            ((fDemand[iD].Resource = rt_Warfare)and(fOffer[iO].Resource in [rt_Shield..rt_Horse]))or
            ((fDemand[iD].Resource = rt_Food)and(fOffer[iO].Resource in [rt_Bread,rt_Sausages,rt_Wine,rt_Fish]));

  //If Demand and Offer aren't reserved already
  Result := Result and ((not fDemand[iD].BeingPerformed) and (fOffer[iO].BeingPerformed < fOffer[iO].Count));

  //If Demand house has WareDelivery toggled ON
  Result := Result and ((fDemand[iD].Loc_House=nil) or (fDemand[iD].Loc_House.WareDelivery));

  //If Demand is a Storehouse and it has WareDelivery toggled ON
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fDemand[iD].Loc_House.GetHouseType<>ht_Store)or
                        (not TKMHouseStore(fDemand[iD].Loc_House).NotAcceptFlag[byte(fOffer[iO].Resource)]));

  //If Demand is a Barracks and it has resource count below MAX_WARFARE_IN_BARRACKS
  //How do we know how many resource are on-route already?? We don't, but it's not that important.
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fDemand[iD].Loc_House.GetHouseType<>ht_Barracks)or
                       (TKMHouseBarracks(fDemand[iD].Loc_House).CheckResIn(fOffer[iO].Resource)<MAX_WARFARE_IN_BARRACKS));

  //If Demand is a Barracks, Offer is a store and barracks has resource count below MAX_WARFARE_IN_BARRACKS_FROM_STORE
  //Use < rather than <= as this delivery will be the last
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fDemand[iD].Loc_House.GetHouseType<>ht_Barracks)or
                      (fOffer[iO].Loc_House=nil)or(fOffer[iO].Loc_House.GetHouseType<>ht_Store)or
                      (TKMHouseBarracks(fDemand[iD].Loc_House).CheckResIn(fOffer[iO].Resource)<MAX_WARFARE_IN_BARRACKS_FROM_STORE));

  //NEVER deliver weapons to the storehouse when player has a barracks
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fDemand[iD].Loc_House.GetHouseType<>ht_Store)or
                       (not (fOffer[iO].Resource in [rt_Shield..rt_Horse]))or(fPlayers.Player[byte(fDemand[iD].Loc_House.GetOwner)].fPlayerStats.GetHouseQty(ht_Barracks)=0));

  //if (fDemand[iD].Loc_House=nil)or //If Demand is a Barracks and it has resource count below MAX_WARFARE_IN_BARRACKS
  //   ((fDemand[iD].Loc_House<>nil)and((fDemand[iD].Loc_House.GetHouseType<>ht_Store)or(
  //   (fDemand[iD].Loc_House.GetHouseType=ht_Store)and(fPlayers.Player[byte(KMSerf.GetOwner)].fPlayerStats.GetHouseQty(ht_Barracks)=0)))) then
  //Works wrong, besides we need to check ALL barracks player owns

  //If Demand and Offer are different HouseTypes, means forbid Store<->Store deliveries except the case where 2nd store is being built and requires building materials
  Result := Result and ((fDemand[iD].Loc_House=nil)or(fOffer[iO].Loc_House.GetHouseType<>fDemand[iD].Loc_House.GetHouseType)or(fOffer[iO].Loc_House.IsComplete<>fDemand[iD].Loc_House.IsComplete));


  Result := Result and (
            ( //House-House delivery should be performed only if there's a connecting road
            (fDemand[iD].Loc_House<>nil)and
            (fTerrain.Route_CanBeMade(KMPointY1(fOffer[iO].Loc_House.GetEntrance),KMPointY1(fDemand[iD].Loc_House.GetEntrance),CanWalkRoad,0, false))
            )
            or
            ( //House-Unit delivery can be performed without connecting road
            (fDemand[iD].Loc_Unit<>nil)and
            (fTerrain.Route_CanBeMade(KMPointY1(fOffer[iO].Loc_House.GetEntrance),fDemand[iD].Loc_Unit.GetPosition,CanWalk,1, false))
            )
            or
            ( //Or maybe serfs can walk anywhere?
            not DO_SERFS_WALK_ROADS
            )
            );
end;


//Should issue a job based on requesters location and job importance
function TKMDeliverQueue.AskForDelivery(KMSerf:TKMUnitSerf; KMHouse:TKMHouse=nil):TTaskDeliver;
var i,iD,iO:integer; Bid,BestBid:single; BidIsPriority: boolean;
begin
  Result:=nil;

  //Find a place where Delivery will be written to after Offer-Demand pair is found
  i:=1; while (i<=QueueCount)and(fQueue[i].JobStatus<>js_Empty) do inc(i);
  if i>QueueCount then begin
    inc(QueueCount, LENGTH_INC);
    setlength(fQueue, QueueCount+1);
  end;


  //if WRITE_DETAILED_LOG then fLog.AppendLog('Reserved delivery ID', i);

//Cleanup for destroyed houses
{for iD:=1 to DemandCount do
  if (fDemand[iD].Loc_House<>nil)and(fDemand[iD].Loc_House.IsDestroyed) then fDemand[iD].Resource:=rt_None
  else
  if (fDemand[iD].Loc_Unit<>nil)and(fDemand[iD].Loc_Unit.IsDestroyed) then fDemand[iD].Resource:=rt_None;

for iO:=1 to OfferCount do
  if (fOffer[iO].Loc_House<>nil)and(fOffer[iO].Loc_House.IsDestroyed) then fOffer[iO].Resource:=rt_None;
}

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

    if PermitDelivery(iO,iD) then
    begin

      //Basic Bid is length of route
      if fDemand[iD].Loc_House<>nil then
        Bid := GetLength(fOffer[iO].Loc_House.GetEntrance,fDemand[iD].Loc_House.GetEntrance)
      else
        Bid := GetLength(fOffer[iO].Loc_House.GetEntrance,fDemand[iD].Loc_Unit.GetPosition);

      //Modifications for bidding system
      if (fDemand[iD].Resource=rt_All) //Always prefer deliveries House>House instead of House>Store
      or (fOffer[iO].Loc_House.GetHouseType = ht_Store) then //Prefer taking wares from House rather than Store
        Bid:=Bid + 1000;

      if fDemand[iD].Loc_House<>nil then //Prefer delivering to houses with fewer supply
      if (fDemand[iD].Resource <> rt_All)and(fDemand[iD].Resource <> rt_Warfare) then //Except Barracks and Store, where supply doesn't matter or matter less
        Bid:=Bid + 20 * fDemand[iD].Loc_House.CheckResIn(fDemand[iD].Resource);

      //When delivering food to warriors, add a random amount to bid to ensure that a variety of food is taken. Also prefer food which is more abundant.
      if (fDemand[iD].Loc_Unit<>nil) and (fDemand[iD].Loc_Unit is TKMUnitWarrior) then
      if (fDemand[iD].Resource = rt_Food) then
        Bid:=Bid + Random(5+(100 div fOffer[iO].Count)); //The more resource there is, the smaller Random can be. >100 we no longer care, it's just random 5.

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
  Result:=TTaskDeliver.Create(KMSerf, fOffer[iO].Loc_House, fDemand[iD].Loc_House, fDemand[iD].Loc_Unit, fOffer[iO].Resource, i);
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
  begin
    fOffer[iO].Resource:=rt_None;
    fPlayers.CleanUpHousePointer(fOffer[iO].Loc_House);
  end;
end;


//Resource has been delivered to Demand
procedure TKMDeliverQueue.GaveDemand(aID:integer);
var iD:integer;
begin
  if WRITE_DELIVERY_LOG then fLog.AppendLog('Gave demand from delivery ID', aID);
  iD:=fQueue[aID].DemandID;
  fQueue[aID].DemandID:=0; //We don't need it any more

  fDemand[iD].BeingPerformed:=false; //Remove reservation

  if fDemand[iD].DemandType=dt_Once then begin//Remove resource from Demand list
    fDemand[iD].Resource:=rt_None;
    fPlayers.CleanUpHousePointer(fDemand[iD].Loc_House);
    fPlayers.CleanUpUnitPointer(fDemand[iD].Loc_Unit);
  end;
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
    begin
      fOffer[fQueue[aID].OfferID].IsDeleted := false;
      fOffer[fQueue[aID].OfferID].Resource := rt_None;
      fOffer[fQueue[aID].OfferID].Count := 0;
      fPlayers.CleanUpHousePointer(fOffer[fQueue[aID].OfferID].Loc_House);
    end;
  end;
  if fQueue[aID].DemandID <> 0 then fDemand[fQueue[aID].DemandID].BeingPerformed:=false;
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


procedure TKMDeliverQueue.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write('Deliveries');
  SaveStream.Write(OfferCount);
  for i:=1 to OfferCount do
  begin
    SaveStream.Write(fOffer[i].Resource, SizeOf(fOffer[i].Resource));
    SaveStream.Write(fOffer[i].Count);
    if fOffer[i].Loc_House <> nil then SaveStream.Write(fOffer[i].Loc_House.ID) else SaveStream.Write(Zero);
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
    if Loc_House <> nil then SaveStream.Write(Loc_House.ID) else SaveStream.Write(Zero);
    if Loc_Unit  <> nil then SaveStream.Write(Loc_Unit.ID ) else SaveStream.Write(Zero);
    SaveStream.Write(BeingPerformed);
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
  setlength(fOffer, OfferCount+1);
  for i:=1 to OfferCount do
  begin
    LoadStream.Read(fOffer[i].Resource, SizeOf(fOffer[i].Resource));
    LoadStream.Read(fOffer[i].Count);
    LoadStream.Read(fOffer[i].Loc_House, 4);
    LoadStream.Read(fOffer[i].BeingPerformed);
    LoadStream.Read(fOffer[i].IsDeleted);
  end;

  LoadStream.Read(DemandCount);
  setlength(fDemand, DemandCount+1);
  for i:=1 to DemandCount do
  with fDemand[i] do
  begin
    LoadStream.Read(Resource, SizeOf(Resource));
    LoadStream.Read(DemandType, SizeOf(DemandType));
    LoadStream.Read(Importance, SizeOf(Importance));
    LoadStream.Read(Loc_House, 4);
    LoadStream.Read(Loc_Unit, 4);
    LoadStream.Read(BeingPerformed);
  end;

  LoadStream.Read(QueueCount);
  setlength(fQueue, QueueCount+1);
  for i:=1 to QueueCount do
  begin
    LoadStream.Read(fQueue[i].OfferID);
    LoadStream.Read(fQueue[i].DemandID);
    LoadStream.Read(fQueue[i].JobStatus, SizeOf(fQueue[i].JobStatus));
  end;
end;


procedure TKMDeliverQueue.SyncLoad();
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


procedure TKMDeliverQueue.SaveToFile(aFileName:string);
var i:integer; f:textfile; s:string;
begin
  assignfile(f,aFileName); Rewrite(f);

  s:='Demand:'+eol+'---------------------------------'+eol;
  for i:=1 to DemandCount do if fDemand[i].Resource<>rt_None then begin
    s:=s+#9;
    if fDemand[i].Loc_House<>nil then s:=s+TypeToString(fDemand[i].Loc_House.GetHouseType)+#9+#9;
    if fDemand[i].Loc_Unit<>nil then s:=s+TypeToString(fDemand[i].Loc_Unit.UnitType)+#9+#9;
    s:=s+TypeToString(fDemand[i].Resource);
    if fDemand[i].Importance=di_High then s:=s+'^';
    s:=s+eol;
  end;
  s:=s+eol+'Offer:'+eol+'---------------------------------'+eol;
  for i:=1 to OfferCount do if fOffer[i].Resource<>rt_None then begin
    s:=s+#9;
    if fOffer[i].Loc_House<>nil then s:=s+TypeToString(fOffer[i].Loc_House.GetHouseType)+#9+#9;
    s:=s+TypeToString(fOffer[i].Resource)+#9;
    s:=s+IntToStr(fOffer[i].Count);
    s:=s+eol;
  end;

  s:=s+eol+'Running deliveries:'+eol+'---------------------------------'+eol;
  for i:=1 to QueueCount do if fQueue[i].OfferID<>0 then begin

    s:=s+'id '+inttostr(i)+'.'+#9;
    s:=s+TypeToString(fOffer[fQueue[i].OfferID].Resource)+#9;

    if fOffer[fQueue[i].OfferID].Loc_House = nil then
      s:=s+'Destroyed'+' >>> '
    else
      s:=s+TypeToString(fOffer[fQueue[i].OfferID].Loc_House.GetHouseType)+' >>> ';

    if fDemand[fQueue[i].DemandID].Loc_House = nil then
      s:=s+'Destroyed'
    else
      s:=s+TypeToString(fDemand[fQueue[i].DemandID].Loc_House.GetHouseType);
    s:=s+eol;
  end;

  write(f,s);
  closefile(f);
end;


{==================================================================================================}
{TKMBuildingQueue}
{==================================================================================================}
constructor TKMBuildingQueue.Create;
begin
  Inherited;
end;


procedure TKMBuildingQueue.CloseRoad(aID:integer);
begin
  fFieldsQueue[aID].Loc:=KMPoint(0,0);
  fFieldsQueue[aID].FieldType:=ft_None;
  fFieldsQueue[aID].Importance:=0;
  fFieldsQueue[aID].JobStatus:=js_Empty;
  fPlayers.CleanUpUnitPointer(fFieldsQueue[aID].Worker);
end;


{Clear up}
procedure TKMBuildingQueue.CloseHouse(aID:integer);
begin
  fPlayers.CleanUpHousePointer(fHousesQueue[aID].House);
  fHousesQueue[aID].Importance:=0;
end;


procedure TKMBuildingQueue.CloseHousePlan(aID:integer);
begin
  fPlayers.CleanUpHousePointer(fHousePlansQueue[aID].House);
  fHousePlansQueue[aID].Importance:=0;
  fHousePlansQueue[aID].JobStatus:=js_Empty;
  fPlayers.CleanUpUnitPointer(fHousePlansQueue[aID].Worker);
end;


procedure TKMBuildingQueue.CloseHouseRepair(aID:integer);
begin
  fPlayers.CleanUpHousePointer(fHouseRepairsQueue[aID].House);
  fHouseRepairsQueue[aID].Importance:=0;
end;


//This procedure is called when a worker dies while walking to the task aID. We should allow other workers to take this task.
procedure TKMBuildingQueue.ReOpenRoad(aID:integer);
begin
  fFieldsQueue[aID].JobStatus := js_Open;
  fPlayers.CleanUpUnitPointer(fFieldsQueue[aID].Worker);
end;


//This procedure is called when a worker dies while walking to the task aID. We should allow other workers to take this task.
procedure TKMBuildingQueue.ReOpenHousePlan(aID:integer);
begin
  fHousePlansQueue[aID].JobStatus := js_Open;
  fPlayers.CleanUpUnitPointer(fHousePlansQueue[aID].Worker);
end;


procedure TKMBuildingQueue.RemoveHouseRepair(aHouse: TKMHouse);
var i:integer;
begin
  for i:=1 to HouseRepairsCount do
  if fHouseRepairsQueue[i].House=aHouse then
  begin
    fPlayers.CleanUpHousePointer(fHouseRepairsQueue[i].House);
    FillChar(fHouseRepairsQueue[i],SizeOf(fHouseRepairsQueue[i]),#0); //Remove offer
  end;
end;


function TKMBuildingQueue.GetHouseWipCount(aHouseType: THouseType):integer;
var i:integer;
begin
  Result := 0;
  Assert(aHouseType <> ht_None, 'Querrying wrong house type');
  for i:=1 to HousesCount do
    if (fHousesQueue[i].House<>nil) and (fHousesQueue[i].House.GetHouseType = aHouseType) then
      inc(Result);

  for i:=1 to HousePlansCount do
    if (fHousePlansQueue[i].House<>nil) and (fHousePlansQueue[i].House.GetHouseType = aHouseType) then
      inc(Result);
end;


procedure TKMBuildingQueue.AddNewRoad(aLoc:TKMPoint; aFieldType:TFieldType);
var i:integer;
begin
  i:=1; while (i<=FieldsCount)and(fFieldsQueue[i].JobStatus<>js_Empty) do inc(i);
  if i>FieldsCount then begin
    inc(FieldsCount, LENGTH_INC);
    setlength(fFieldsQueue, FieldsCount+1);
  end;

  fFieldsQueue[i].Loc:=aLoc;
  fFieldsQueue[i].FieldType:=aFieldType;
  fFieldsQueue[i].Importance:=1;
  fFieldsQueue[i].JobStatus:=js_Open;
end;


{Add new job to the list}
procedure TKMBuildingQueue.AddNewHouse(aHouse: TKMHouse);
var i:integer;
begin
  i:=1; while (i<=HousesCount)and(fHousesQueue[i].House<>nil) do inc(i);
  if i>HousesCount then begin
    inc(HousesCount, LENGTH_INC);
    setlength(fHousesQueue, HousesCount+1);
  end;

  if aHouse <> nil then fHousesQueue[i].House := aHouse.GetHousePointer;
  fHousesQueue[i].Importance:=1;
end;


procedure TKMBuildingQueue.AddNewHousePlan(aHouse: TKMHouse);
var i:integer;
begin
  i:=1; while (i<=HousePlansCount)and(fHousePlansQueue[i].JobStatus<>js_Empty) do inc(i);
  if i>HousePlansCount then begin
    inc(HousePlansCount, LENGTH_INC);
    setlength(fHousePlansQueue, HousePlansCount+1);
  end;

  if aHouse <> nil then fHousePlansQueue[i].House:=aHouse.GetHousePointer;
  fHousePlansQueue[i].Importance:=1;
  fHousePlansQueue[i].JobStatus:=js_Open;
end;


function TKMBuildingQueue.AddHouseRepair(aHouse: TKMHouse):integer;
var i:integer;
begin
  i:=1; while (i<=HouseRepairsCount)and(fHouseRepairsQueue[i].House<>nil) do inc(i);
  if i>HouseRepairsCount then begin
    inc(HouseRepairsCount, LENGTH_INC);
    setlength(fHouseRepairsQueue, HouseRepairsCount+1);
  end;

  if aHouse <> nil then fHouseRepairsQueue[i].House:=aHouse.GetHousePointer;
  fHouseRepairsQueue[i].Importance:=1;
  Result:=i;
end;


{Remove task if Player has cancelled it}
{Simulated just means that we simply want to check if player ever issued that task}
{The erase cursor changes when you move over a piece of deletable road/field}
function TKMBuildingQueue.CancelRoad(aLoc:TKMPoint; Simulated:boolean=false):boolean;
var i:integer;
begin
  Result:=false;
  for i:=1 to FieldsCount do
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
  for i:=1 to HousePlansCount do
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
var i, Best: integer; BestDist: single;
begin
  Result := nil;
  Best := -1;
  for i:=1 to FieldsCount do
    if (fFieldsQueue[i].JobStatus = js_Open) and
      fTerrain.Route_CanBeMade(aWorker.GetPosition, fFieldsQueue[i].Loc, aWorker.GetDesiredPassability, 0, false)
    and((Best = -1)or(GetLength(aWorker.GetPosition, fFieldsQueue[i].Loc) < BestDist))then
    begin
      Best := i;
      BestDist := GetLength(aWorker.GetPosition, fFieldsQueue[i].Loc);
    end;
  if Best <> -1 then
  begin
    case fFieldsQueue[Best].FieldType of
      ft_Road: Result := TTaskBuildRoad.Create(aWorker, fFieldsQueue[Best].Loc, Best);
      ft_Corn: Result := TTaskBuildField.Create(aWorker, fFieldsQueue[Best].Loc, Best);
      ft_Wine: Result := TTaskBuildWine.Create(aWorker, fFieldsQueue[Best].Loc, Best);
      ft_Wall: Result := TTaskBuildWall.Create(aWorker, fFieldsQueue[Best].Loc, Best);
      else     Result := nil;
    end;
    fFieldsQueue[Best].JobStatus := js_Taken;
    fFieldsQueue[Best].Worker := aWorker.GetUnitPointer;
  end;
end;


{Find a job for worker}
function  TKMBuildingQueue.AskForHouse(aWorker:TKMUnitWorker):TUnitTask;
var i, Best: integer; BestDist: single;
begin
  Result := nil;
  Best := -1;
  for i:=1 to HousesCount do
    if (fHousesQueue[i].House<>nil) and fHousesQueue[i].House.CheckResToBuild
    and((Best = -1)or(GetLength(aWorker.GetPosition, fHousesQueue[i].House.GetPosition) < BestDist))then
    begin
      Best := i;
      BestDist := GetLength(aWorker.GetPosition, fHousesQueue[i].House.GetPosition);
    end;
  if Best <> -1 then
    Result := TTaskBuildHouse.Create(aWorker, fHousesQueue[Best].House, Best);
end;


function  TKMBuildingQueue.AskForHousePlan(aWorker:TKMUnitWorker):TUnitTask;
var i, Best: integer; BestDist: single;
begin
  Result := nil;
  Best := -1;
  for i:=1 to HousePlansCount do
    if (fHousePlansQueue[i].JobStatus = js_Open)
    and((Best = -1)or(GetLength(aWorker.GetPosition, fHousePlansQueue[i].House.GetPosition) < BestDist))then
    begin
      Best := i;
      BestDist := GetLength(aWorker.GetPosition, fHousePlansQueue[i].House.GetPosition);
    end;
  if Best <> -1 then
  begin
    Result := TTaskBuildHouseArea.Create(aWorker, fHousePlansQueue[Best].House, Best);
    fHousePlansQueue[Best].JobStatus := js_Taken;
    fHousePlansQueue[Best].Worker := aWorker.GetUnitPointer;
  end;
end;


function  TKMBuildingQueue.AskForHouseRepair(aWorker:TKMUnitWorker):TUnitTask;
var i, Best: integer; BestDist: single;
begin
  Result := nil;
  Best := -1;
  for i:=1 to HouseRepairsCount do
    if (fHouseRepairsQueue[i].House<>nil) and
      fHouseRepairsQueue[i].House.IsDamaged and
      fHouseRepairsQueue[i].House.BuildingRepair
    and((Best = -1)or(GetLength(aWorker.GetPosition, fHouseRepairsQueue[i].House.GetPosition) < BestDist))then
    begin
      Best := i;
      BestDist := GetLength(aWorker.GetPosition, fHouseRepairsQueue[i].House.GetPosition);
    end;
  if Best <> -1 then
    Result := TTaskBuildHouseRepair.Create(aWorker, fHouseRepairsQueue[Best].House, Best);
end;


procedure TKMBuildingQueue.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write(FieldsCount);
  for i:=1 to FieldsCount do
  with fFieldsQueue[i] do
  begin
    SaveStream.Write(Loc);
    SaveStream.Write(FieldType, SizeOf(FieldType));
    SaveStream.Write(Importance);
    SaveStream.Write(JobStatus, SizeOf(JobStatus));
    if Worker <> nil then SaveStream.Write(Worker.ID) else SaveStream.Write(Zero);
  end;

  SaveStream.Write(HousesCount);
  for i:=1 to HousesCount do
  begin
    if fHousesQueue[i].House <> nil then SaveStream.Write(fHousesQueue[i].House.ID) else SaveStream.Write(Zero);
    SaveStream.Write(fHousesQueue[i].Importance);
  end;

  SaveStream.Write(HousePlansCount);
  for i:=1 to HousePlansCount do
  with fHousePlansQueue[i] do
  begin
    if House <> nil then SaveStream.Write(House.ID) else SaveStream.Write(Zero);
    SaveStream.Write(Importance);
    SaveStream.Write(JobStatus, SizeOf(JobStatus));
    if Worker <> nil then SaveStream.Write(Worker.ID) else SaveStream.Write(Zero);
  end;

  SaveStream.Write(HouseRepairsCount);
  for i:=1 to HouseRepairsCount do
  with fHouseRepairsQueue[i] do
  begin
    if House <> nil then SaveStream.Write(House.ID) else SaveStream.Write(Zero);
    SaveStream.Write(Importance);
  end;
end;


procedure TKMBuildingQueue.Load(LoadStream:TKMemoryStream);
var i:integer;
begin
  LoadStream.Read(FieldsCount);
  setlength(fFieldsQueue, FieldsCount+1);
  for i:=1 to FieldsCount do
  with fFieldsQueue[i] do
  begin
    LoadStream.Read(Loc);
    LoadStream.Read(FieldType, SizeOf(FieldType));
    LoadStream.Read(Importance);
    LoadStream.Read(JobStatus, SizeOf(JobStatus));
    LoadStream.Read(Worker, 4);
  end;

  LoadStream.Read(HousesCount);
  setlength(fHousesQueue, HousesCount+1);
  for i:=1 to HousesCount do
  begin
    LoadStream.Read(fHousesQueue[i].House, 4);
    LoadStream.Read(fHousesQueue[i].Importance);
  end;

  LoadStream.Read(HousePlansCount);
  setlength(fHousePlansQueue, HousePlansCount+1);
  for i:=1 to HousePlansCount do
  with fHousePlansQueue[i] do
  begin
    LoadStream.Read(House, 4);
    LoadStream.Read(Importance);
    LoadStream.Read(JobStatus, SizeOf(JobStatus));
    LoadStream.Read(Worker, 4);
  end;

  LoadStream.Read(HouseRepairsCount);
  setlength(fHouseRepairsQueue, HouseRepairsCount+1);
  for i:=1 to HouseRepairsCount do
  with fHouseRepairsQueue[i] do
  begin
    LoadStream.Read(House, 4);
    LoadStream.Read(Importance);
  end;
end;


procedure TKMBuildingQueue.SyncLoad();
var i:integer;
begin
  for i:=1 to FieldsCount do
    fFieldsQueue[i].Worker := fPlayers.GetUnitByID(cardinal(fFieldsQueue[i].Worker));

  for i:=1 to HousesCount do
    fHousesQueue[i].House := fPlayers.GetHouseByID(cardinal(fHousesQueue[i].House));

  for i:=1 to HousePlansCount do
  begin
    fHousePlansQueue[i].House := fPlayers.GetHouseByID(cardinal(fHousePlansQueue[i].House));
    fHousePlansQueue[i].Worker := fPlayers.GetUnitByID(cardinal(fHousePlansQueue[i].Worker));
  end;

  for i:=1 to HouseRepairsCount do
    fHouseRepairsQueue[i].House := fPlayers.GetHouseByID(cardinal(fHouseRepairsQueue[i].House));
end;


end.

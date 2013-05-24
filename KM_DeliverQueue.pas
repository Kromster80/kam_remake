unit KM_DeliverQueue;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, Math,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Houses, KM_Units, KM_ResourceWares;


type
  TJobStatus = (
        js_Empty,   //Empty - empty spot for a new job
        js_Open,    //Open - job is free to take by anyone
        js_Taken);  //Taken - job is taken by some worker

  TKMDeliveryOffer = record
    Ware: TWareType;
    Count: Cardinal; //How many items are offered
    Loc_House: TKMHouse;
    BeingPerformed: Cardinal; //How many items are being delivered atm from total Count offered
    //Keep offer until serfs that do it abandons it
    IsDeleted: Boolean;
  end;

  TKMDeliveryDemand =  record
    Ware: TWareType;
    DemandType: TDemandType; //Once for everything, Always for Store and Barracks
    Importance: TDemandImportance; //How important demand is, e.g. Workers and building sites should be di_High
    Loc_House: TKMHouse;
    Loc_Unit: TKMUnit;
    BeingPerformed: Boolean;
    IsDeleted: Boolean; //So we don't get pointer issues
  end;

type
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

  TKMDeliverQueue = class
  private
    fOfferCount: Integer;
    fOffer: array of TKMDeliveryOffer;
    fDemandCount: Integer;
    fDemand: array of TKMDeliveryDemand;
    fQueueCount: Integer;
    fQueue: array of
    record
      OfferID, DemandID: Integer;
      JobStatus: TJobStatus; //Empty slot, resource Taken, job Done
    end;

    procedure CloseDelivery(aID: Integer);
    procedure CloseDemand(aID: Integer);
    procedure CloseOffer(aID: Integer);
    function ValidDelivery(iO, iD: Integer): Boolean;
    function SerfCanDoDelivery(iO, iD: Integer; aSerf: TKMUnitSerf): Boolean;
    function PermitDelivery(iO, iD: Integer; aSerf: TKMUnitSerf): Boolean;
    function CalculateBid(iO, iD: Integer; aSerf: TKMUnitSerf): Single;
  public
    procedure AddOffer(aHouse: TKMHouse; aWare: TWareType; aCount: Integer);
    procedure RemAllOffers(aHouse: TKMHouse);
    procedure RemOffer(aHouse: TKMHouse; aWare: TWareType; aCount: Integer);

    procedure AddDemand(aHouse: TKMHouse; aUnit: TKMUnit; aResource: TWareType; aCount: Byte; aType: TDemandType; aImp: TDemandImportance);
    function TryRemoveDemand(aHouse: TKMHouse; aResource: TWareType; aCount: Word): word;
    procedure RemDemand(aHouse: TKMHouse); overload;
    procedure RemDemand(aUnit: TKMUnit); overload;

    function GetAvailableDeliveriesCount: Integer;
    procedure AssignDelivery(iO, iD: Integer; aSerf: TKMUnitSerf);
    procedure AskForDelivery(aSerf: TKMUnitSerf; aHouse: TKMHouse = nil);
    procedure TakenOffer(aID: Integer);
    procedure GaveDemand(aID: Integer);
    procedure AbandonDelivery(aID: Integer); //Occurs when unit is killed or something alike happens

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;

    procedure ExportToFile(aFileName: string);
  end;

  TKMDeliveries = class
  private
    fQueue: TKMDeliverQueue;

    fSerfCount: Integer;
    fSerfs: array of record //Not sure what else props we planned to add here
      Serf: TKMUnitSerf;
    end;

    procedure RemSerf(aIndex: Integer);
    procedure RemoveExtraSerfs;
    function GetIdleSerfCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSerf(aSerf: TKMUnitSerf);
    property Queue: TKMDeliverQueue read fQueue;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;


implementation
uses KM_Utils, KM_PlayersCollection, KM_Resource, KM_Log, KM_Terrain, KM_HouseBarracks;


const
  LENGTH_INC = 32; //Increment array lengths by this value


{ TKMDeliveries }
constructor TKMDeliveries.Create;
begin
  inherited;
  fQueue := TKMDeliverQueue.Create;
end;


destructor TKMDeliveries.Destroy;
begin
  fQueue.Free;
  inherited;
end;


procedure TKMDeliveries.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.Write('SerfList');

  SaveStream.Write(fSerfCount);
  for I := 0 to fSerfCount - 1 do
  begin
    if fSerfs[I].Serf <> nil then
      SaveStream.Write(fSerfs[I].Serf.ID)
    else
      SaveStream.Write(Integer(0));
  end;

  fQueue.Save(SaveStream);
end;


procedure TKMDeliveries.Load(LoadStream: TKMemoryStream);
var I: Integer;
begin
  LoadStream.ReadAssert('SerfList');

  LoadStream.Read(fSerfCount);
  SetLength(fSerfs, fSerfCount);
  for I := 0 to fSerfCount - 1 do
    LoadStream.Read(fSerfs[I].Serf, 4);

  fQueue.Load(LoadStream);
end;


procedure TKMDeliveries.SyncLoad;
var I: Integer; U: TKMUnit;
begin
  for I := 0 to fSerfCount - 1 do
  begin
    U := fPlayers.GetUnitByID(Cardinal(fSerfs[I].Serf));
    Assert(U is TKMUnitSerf, 'Non-serf in delivery list');
    fSerfs[I].Serf := TKMUnitSerf(U);
  end;
  fQueue.SyncLoad;
end;


//Add the Serf to the List
procedure TKMDeliveries.AddSerf(aSerf: TKMUnitSerf);
begin
  if fSerfCount >= Length(fSerfs) then
    SetLength(fSerfs, fSerfCount + LENGTH_INC);

  fSerfs[fSerfCount].Serf := TKMUnitSerf(aSerf.GetUnitPointer);
  Inc(fSerfCount);
end;


//Remove died Serf from the List
procedure TKMDeliveries.RemSerf(aIndex: Integer);
begin
  fPlayers.CleanUpUnitPointer(TKMUnit(fSerfs[aIndex].Serf));

  //Serf order is not important, so we just move last one into freed spot
  if aIndex <> fSerfCount - 1 then
    fSerfs[aIndex] := fSerfs[fSerfCount - 1];

  Dec(fSerfCount);
end;


function TKMDeliveries.GetIdleSerfCount: Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to fSerfCount - 1 do
    if fSerfs[I].Serf.IsIdle then
      Inc(Result);
end;


//Remove dead serfs
procedure TKMDeliveries.RemoveExtraSerfs;
var
  I: Integer;
begin
  for I := fSerfCount - 1 downto 0 do
    if fSerfs[I].Serf.IsDeadOrDying then
      RemSerf(I);
end;


procedure TKMDeliveries.UpdateState;

  function AnySerfCanDoDelivery(iO,iD: Integer): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I := 0 to fSerfCount - 1 do
      if fSerfs[I].Serf.IsIdle and fQueue.SerfCanDoDelivery(iO, iD, fSerfs[I].Serf) then
      begin
        Result := True;
        Exit;
      end;
  end;

var
  I, K, iD, iO, FoundO, FoundD: Integer;
  Bid, BestBid: Single;
  AvailableDeliveries, AvailableSerfs: Integer;
  Serf: TKMUnitSerf;
begin
  RemoveExtraSerfs;

  AvailableDeliveries := fQueue.GetAvailableDeliveriesCount;
  AvailableSerfs := GetIdleSerfCount;
  if AvailableSerfs * AvailableDeliveries = 0 then Exit;

  if AvailableDeliveries > AvailableSerfs then
  begin
    for I := 0 to fSerfCount - 1 do
      if fSerfs[I].Serf.IsIdle then
        fQueue.AskForDelivery(fSerfs[I].Serf);
  end
  else
    //I is not used anywhere, but we must loop through once for each delivery available so each one is taken
    for I := 1 to AvailableDeliveries do
    begin
      //First we decide on the best delivery to be done based on current Offers and Demands
      //We need to choose the best delivery out of all of them, otherwise we could get
      //a further away storehouse when there are multiple possibilities.
      //Note: All deliveries will be taken, because we have enough serfs to fill them all.
      //The important concept here is to always get the shortest delivery when a delivery can be taken to multiple places.
      BestBid := -1;
      FoundO := -1;
      FoundD := -1;
      for iD := 1 to fQueue.fDemandCount do
        if fQueue.fDemand[iD].Ware <> wt_None then
          for iO := 1 to fQueue.fOfferCount do
            if (fQueue.fOffer[iO].Ware <> wt_None)
            and fQueue.ValidDelivery(iO,iD)
            and AnySerfCanDoDelivery(iO,iD) then //Only choose this delivery if at least one of the serfs can do it
            begin
              Bid := fQueue.CalculateBid(iO,iD,nil);
              if (BestBid = -1) or (Bid < BestBid) then
              begin
                BestBid := Bid;
                FoundO := iO;
                FoundD := iD;
              end;
            end;

      //FoundO and FoundD give us the best delivery to do at this moment. Now find the best serf for the job.
      if BestBid <> -1 then
      begin
        Serf := nil;
        BestBid := -1;
        for K := 0 to fSerfCount - 1 do
          if fSerfs[K].Serf.IsIdle then
            if fQueue.SerfCanDoDelivery(FoundO,FoundD,fSerfs[K].Serf) then
            begin
              Bid := KMLength(fSerfs[K].Serf.GetPosition, fQueue.fOffer[FoundO].Loc_House.GetEntrance);
              if (BestBid = -1) or (Bid < BestBid) then
              begin
                BestBid := Bid;
                Serf := fSerfs[K].Serf;
              end;
            end;
        if Serf <> nil then
          fQueue.AssignDelivery(FoundO,FoundD,Serf);
      end;
    end;
end;


{ TKMDeliverQueue }
//Adds new Offer to the list. List is stored without sorting
//(it matters only for Demand to keep everything in waiting its order in line),
//so we just find an empty place and write there.
procedure TKMDeliverQueue.AddOffer(aHouse: TKMHouse; aWare: TWareType; aCount: Integer);
var
  I, K: Integer;
begin
  //Add Count of resource to old offer
  for I := 1 to fOfferCount do
    if (fOffer[I].Loc_House = aHouse)
    and (fOffer[I].Ware = aWare) then
    begin
      if fOffer[I].IsDeleted then
      begin
        //Revive old offer because some serfs are still walking to perform it
        Assert(fOffer[I].BeingPerformed > 0);
        fOffer[I].Count :=  aCount;
        fOffer[I].IsDeleted := False;
        Exit; //Count added, thats all
      end
      else
      begin
        Inc(fOffer[I].Count, aCount);
        Exit; //Count added, thats all
      end;
    end;

  //Find empty place or allocate new one
  I := 1;
  while (I <= fOfferCount) and (fOffer[I].Ware <> wt_None) do
    Inc(I);
  if I > fOfferCount then
  begin
    Inc(fOfferCount, LENGTH_INC);
    SetLength(fOffer, fOfferCount + 1);
    for K := I to fOfferCount do
      FillChar(fOffer[K], SizeOf(fOffer[K]), #0); //Initialise the new queue space
  end;

  //Add offer
  with fOffer[I] do
  begin
    if aHouse <> nil then
      Loc_House := aHouse.GetHousePointer;
    Ware := aWare;
    Count := aCount;
    Assert((BeingPerformed = 0) and not IsDeleted); //Make sure this item has been closed properly, if not there is a flaw
  end;
end;


//Remove Offer from the list. E.G on house demolish
//List is stored without sorting so we have to parse it to find that entry..
procedure TKMDeliverQueue.RemAllOffers(aHouse: TKMHouse);
var i:integer;
begin
  //We need to parse whole list, never knowing how many offers the house had
  for i:=1 to fOfferCount do
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


procedure TKMDeliverQueue.RemOffer(aHouse: TKMHouse; aWare: TWareType; aCount: Integer);
var
  I: Integer;
begin
  //Add Count of resource to old offer
  for I := 1 to fOfferCount do
    if (fOffer[I].Loc_House = aHouse)
    and (fOffer[I].Ware = aWare)
    and not fOffer[I].IsDeleted then
    begin
      Assert(fOffer[I].Count >= aCount, 'Removing too many offers');
      Dec(fOffer[I].Count, aCount);
      if fOffer[I].Count = 0 then
      begin
        if fOffer[i].BeingPerformed > 0 then
          fOffer[i].IsDeleted := True
        else
          CloseOffer(i);
      end;
      Exit; //Count decreased, that's all
    end;
  Assert(False, 'Failed to remove offer');
end;


//Remove Demand from the list. List is stored without sorting
//so we parse it to find all entries..
procedure TKMDeliverQueue.RemDemand(aHouse: TKMHouse);
var i:integer;
begin
  assert(aHouse <> nil);
  for i:=1 to fDemandCount do
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
procedure TKMDeliverQueue.RemDemand(aUnit:TKMUnit);
var i:integer;
begin
  assert(aUnit <> nil);
  for i:=1 to fDemandCount do
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
function TKMDeliverQueue.TryRemoveDemand(aHouse:TKMHouse; aResource:TWareType; aCount:word):word;
var i:integer;
begin
  Result := 0;
  if aCount = 0 then exit;
  assert(aHouse <> nil);
  for i:=1 to fDemandCount do
    if (fDemand[i].Loc_House = aHouse) and (fDemand[i].Ware = aResource) then
      if not fDemand[i].BeingPerformed then
      begin
        CloseDemand(i); //Clear up demand
        inc(Result);
        if Result = aCount then exit; //We have removed enough demands
      end;
end;


//Adds new Demand to the list. List is stored sorted, but the sorting is done upon Deliver completion,
//so we just find an empty place (which is last one) and write there.
procedure TKMDeliverQueue.AddDemand(aHouse:TKMHouse; aUnit:TKMUnit; aResource:TWareType; aCount:byte; aType:TDemandType; aImp:TDemandImportance);
var i,k,j:integer;
begin
  Assert(aResource <> wt_None, 'Demanding rt_None');

  for k:=1 to aCount do
  begin
    i:=1; while (i<=fDemandCount)and(fDemand[i].Ware<>wt_None) do inc(i);
    if i>fDemandCount then
    begin
      inc(fDemandCount, LENGTH_INC);
      SetLength(fDemand, fDemandCount+1);
      for j:=i to fDemandCount do FillChar(fDemand[j],SizeOf(fDemand[j]),#0); //Initialise the new queue space
    end;

    with fDemand[i] do
    begin
      if aHouse <> nil then Loc_House:=aHouse.GetHousePointer;
      if aUnit <> nil then Loc_Unit:=aUnit.GetUnitPointer;
      DemandType:=aType; //Once or Always
      Ware:=aResource;
      Importance:=aImp;
      assert((not IsDeleted) and (not BeingPerformed)); //Make sure this item has been closed properly, if not there is a flaw

      //Gold to Schools
      if (Ware = wt_Gold)
      and (Loc_House <> nil) and (Loc_House.HouseType = ht_School) then
        Importance := diHigh1;

      //Food to Inn
      if (Ware in [wt_Bread, wt_Sausages, wt_Wine, wt_Fish])
      and (Loc_House <> nil) and (Loc_House.HouseType = ht_Inn) then
        Importance := diHigh3;
    end;
  end;
end;


function TKMDeliverQueue.ValidDelivery(iO,iD: Integer): Boolean;
var
  I: Integer;
  B: TKMHouseBarracks;
begin
  //If Offer Resource matches Demand
  Result := (fDemand[iD].Ware = fOffer[iO].Ware) or
            (fDemand[iD].Ware = wt_All) or
            ((fDemand[iD].Ware = wt_Warfare) and (fOffer[iO].Ware in [WARFARE_MIN..WARFARE_MAX])) or
            ((fDemand[iD].Ware = wt_Food) and (fOffer[iO].Ware in [wt_Bread, wt_Sausages, wt_Wine, wt_Fish]));

  //If Demand and Offer aren't reserved already
  Result := Result and ((not fDemand[iD].BeingPerformed) and (fOffer[iO].BeingPerformed < fOffer[iO].Count));

  //If Demand and Offer aren't deleted
  Result := Result and (not fDemand[iD].IsDeleted) and (not fOffer[iO].IsDeleted);

  //If Demand house has WareDelivery toggled ON
  Result := Result and ((fDemand[iD].Loc_House = nil) or (fDemand[iD].Loc_House.WareDelivery));

  //If Demand is a Storehouse and it has WareDelivery toggled ON
  Result := Result and ((fDemand[iD].Loc_House = nil) or
                        (fDemand[iD].Loc_House.HouseType <> ht_Store) or
                        (not TKMHouseStore(fDemand[iD].Loc_House).NotAcceptFlag[fOffer[iO].Ware]));

  //Warfare has a preference to be deivered to Barracks
  if Result
  and (fOffer[iO].Ware in [WARFARE_MIN..WARFARE_MAX])
  and (fDemand[iD].Loc_House <> nil) then
  begin
    //If Demand is a Barracks and it has WareDelivery toggled OFF
    if (fDemand[iD].Loc_House.HouseType = ht_Barracks)
    and TKMHouseBarracks(fDemand[iD].Loc_House).NotAcceptFlag[fOffer[iO].Ware] then
      Result := False;

    //Permit delivery of warfares to Store only if player has no Barracks or they all have blocked ware
    if (fDemand[iD].Loc_House <> nil)
    and (fDemand[iD].Loc_House.HouseType = ht_Store) then
    begin
      //Scan through players Barracks, if none accepts - allow deliver to Store
      I := 1;
      repeat
        B := TKMHouseBarracks(fPlayers[fDemand[iD].Loc_House.Owner].FindHouse(ht_Barracks, I));
        if (B <> nil) and (not B.NotAcceptFlag[fOffer[iO].Ware]) then
        begin
          Result := False;
          Break;
        end;
        Inc(I);
      until (B = nil);
    end;
  end;

  //If Demand and Offer are different HouseTypes, means forbid Store<->Store deliveries except the case where 2nd store is being built and requires building materials
  Result := Result and ((fDemand[iD].Loc_House = nil) or
                        (fOffer[iO].Loc_House.HouseType <> fDemand[iD].Loc_House.HouseType) or
                        (fOffer[iO].Loc_House.IsComplete <> fDemand[iD].Loc_House.IsComplete));

  //Do not permit Barracks -> Store deliveries
  Result := Result and ((fDemand[iD].Loc_House = nil) or
                        (fDemand[iD].Loc_House.HouseType <> ht_Store) or
                        (fOffer[iO].Loc_House.HouseType <> ht_Barracks));

  Result := Result and (
            ( //House-House delivery should be performed only if there's a connecting road
            (fDemand[iD].Loc_House <> nil) and
            (gTerrain.Route_CanBeMade(KMPointBelow(fOffer[iO].Loc_House.GetEntrance), KMPointBelow(fDemand[iD].Loc_House.GetEntrance), CanWalkRoad, 0))
            )
            or
            ( //House-Unit delivery can be performed without connecting road
            (fDemand[iD].Loc_Unit <> nil) and
            (gTerrain.Route_CanBeMade(KMPointBelow(fOffer[iO].Loc_House.GetEntrance), fDemand[iD].Loc_Unit.GetPosition, CanWalk, 1))
            ));
end;


//Delivery is only permitted if the serf can access the from house.
function TKMDeliverQueue.SerfCanDoDelivery(iO,iD: Integer; aSerf: TKMUnitSerf): Boolean;
var
  LocA, LocB: TKMPoint;
begin
  LocA := aSerf.GetPosition;
  LocB := KMPointBelow(fOffer[iO].Loc_House.GetEntrance);

  //If the serf is inside the house (invisible) test from point below
  if not aSerf.Visible then
    LocA := KMPointBelow(LocA);

  Result := aSerf.CanWalkTo(LocA, LocB, CanWalk, 0);
end;


function TKMDeliverQueue.PermitDelivery(iO,iD: Integer; aSerf: TKMUnitSerf): Boolean;
begin
  Result := ValidDelivery(iO, iD) and SerfCanDoDelivery(iO, iD, aSerf);
end;


//Get the total number of possible deliveries with current Offers and Demands
function TKMDeliverQueue.GetAvailableDeliveriesCount: Integer;
var
  iD,iO:integer;
  OffersTaken:Cardinal;
  DemandTaken:array of Boolean; //Each demand can only be taken once in our measurements
begin
  SetLength(DemandTaken,fDemandCount+1);
  FillChar(DemandTaken[0], SizeOf(Boolean)*(fDemandCount+1), #0);

  Result := 0;
  for iO:=1 to fOfferCount do
    if (fOffer[iO].Ware <> wt_None) then
    begin
      OffersTaken := 0;
      for iD:=1 to fDemandCount do
        if (fDemand[iD].Ware <> wt_None) and not DemandTaken[iD] and ValidDelivery(iO,iD) then
        begin
          if fDemand[iD].DemandType = dt_Once then
          begin
            DemandTaken[iD] := True;
            inc(Result);
            inc(OffersTaken);
            if fOffer[iO].Count-OffersTaken = 0 then
              Break; //Finished with this offer
          end
          else
          begin
            //This demand will take all the offers, so increase result by that many
            inc(Result, fOffer[iO].Count-OffersTaken);
            Break; //This offer is finished (because this demand took it all)
          end;
        end;
    end;
end;


function TKMDeliverQueue.CalculateBid(iO,iD:Integer; aSerf: TKMUnitSerf):Single;
begin
  //Basic Bid is length of route
  if fDemand[iD].Loc_House <> nil then
  begin
    Result := KMLength(fOffer[iO].Loc_House.GetEntrance, fDemand[iD].Loc_House.GetEntrance)
    //Resource ratios are also considered
    + fPlayers[fOffer[iO].Loc_House.Owner].Stats.Ratio[fDemand[iD].Ware, fDemand[iD].Loc_House.HouseType];
  end
  else
    Result := KMLength(fOffer[iO].Loc_House.GetEntrance, fDemand[iD].Loc_Unit.GetPosition);

  //For weapons production in cases with little resources available, they should be distributed
  //evenly between places rather than caring about route length.
  //This means weapon and armour smiths should get same amount of iron, even if one is closer to the smelter.
  if (fDemand[iD].Loc_House<>nil) and fResource.HouseDat[fDemand[iD].Loc_House.HouseType].DoesOrders
  and (fOffer[iO].Count < 3) //Little resources to share around
  and (fDemand[iD].Loc_House.CheckResIn(fDemand[iD].Ware) < 2) then //Few resources already delivered
    Result := 10 + KaMRandom(20);

  //Also prefer deliveries near to the serf
  if aSerf <> nil then
    Result := Result + KMLength(aSerf.GetPosition,fOffer[iO].Loc_House.GetEntrance);

  //Add some random element so in the case of identical bids the same resource will not always be chosen (e.g. weapons storehouse->barracks should take random weapon types not sequentially)
  Result := Result + KaMRandom(5);

  //Modifications for bidding system
  if (fDemand[iD].Ware = wt_All) //Always prefer deliveries House>House instead of House>Store
  or ((fOffer[iO].Loc_House.HouseType = ht_Store) //Prefer taking wares from House rather than Store...
  and (fDemand[iD].Ware <> wt_Warfare)) then    //...except weapons Store>Barracks, that is also prefered
    Result := Result + 1000;

  if (fDemand[iD].Loc_House <> nil) //Prefer delivering to houses with fewer supply
  and (fDemand[iD].Ware <> wt_All)
  and (fDemand[iD].Ware <> wt_Warfare) then //Except Barracks and Store, where supply doesn't matter or matter less
    Result := Result + 20 * fDemand[iD].Loc_House.CheckResIn(fDemand[iD].Ware);

  //Delivering weapons from store to barracks, make it lowest priority when there are >50 of that weapon in the barracks.
  //In some missions the storehouse has vast amounts of weapons, and we don't want the serfs to spend the whole game moving these.
  //In KaM, if the barracks has >200 weapons the serfs will stop delivering from the storehouse. I think our solution is better.
  if (fDemand[iD].Loc_House <> nil)
  and (fDemand[iD].Loc_House.HouseType = ht_Barracks)
  and (fOffer[iO].Loc_House.HouseType = ht_Store)
  and (fDemand[iD].Loc_House.CheckResIn(fOffer[iO].Ware) > 50) then
    Result := Result + 10000;

  //When delivering food to warriors, add a random amount to bid to ensure that a variety of food is taken. Also prefer food which is more abundant.
  if (fDemand[iD].Loc_Unit <> nil)
  and (fDemand[iD].Ware = wt_Food) then
    Result := Result + KaMRandom(5+(100 div fOffer[iO].Count)); //The more resource there is, the smaller Random can be. >100 we no longer care, it's just random 5.
end;


//Should issue a job based on requesters location and job importance
//Serf may ask for a job from within a house after completing previous delivery
procedure TKMDeliverQueue.AskForDelivery(aSerf: TKMUnitSerf; aHouse: TKMHouse = nil);
var
  iD, iO, BestD, BestO: Integer;
  Bid, BestBid: Single;
  BestImportance: TDemandImportance;
begin
  //Find Offer matching Demand
  //TravelRoute Asker>Offer>Demand should be shortest
  BestBid := MaxSingle;
  BestO := -1;
  BestD := -1;
  BestImportance := diNorm;

  for iD := 1 to fDemandCount do
    if (fDemand[iD].Ware <> wt_None)
    and (fDemand[iD].Importance >= BestImportance) then //Skip any less important than the best we found
      for iO := 1 to fOfferCount do
        if ((aHouse = nil) or (fOffer[iO].Loc_House = aHouse))  //Make sure from house is the one requested
        and (fOffer[iO].Ware <> wt_None)
        and PermitDelivery(iO, iD, aSerf) then
        begin
          Bid := CalculateBid(iO, iD, aSerf);
          if (Bid < BestBid) or (fDemand[iD].Importance > BestImportance) then
          begin
            BestO := iO;
            BestD := iD;
            BestBid := Bid;
            BestImportance := fDemand[iD].Importance;
          end;
        end;

  if (BestO <> -1) and (BestD <> -1) then
    AssignDelivery(BestO, BestD, aSerf);
end;


procedure TKMDeliverQueue.AssignDelivery(iO,iD:Integer; aSerf:TKMUnitSerf);
var i:Integer;
begin
  //Find a place where Delivery will be written to after Offer-Demand pair is found
  i:=1; while (i<=fQueueCount)and(fQueue[i].JobStatus<>js_Empty) do inc(i);
  if i > fQueueCount then
  begin
    inc(fQueueCount, LENGTH_INC);
    SetLength(fQueue, fQueueCount+1);
  end;

  fQueue[i].DemandID:=iD;
  fQueue[i].OfferID:=iO;
  fQueue[i].JobStatus:=js_Taken;

  inc(fOffer[iO].BeingPerformed); //Places a virtual "Reserved" sign on Offer
  fDemand[iD].BeingPerformed:=true; //Places a virtual "Reserved" sign on Demand

  //Store never has enough demand performed
  if (fDemand[iD].Loc_House<>nil)and(fDemand[iD].DemandType = dt_Always) then fDemand[iD].BeingPerformed:=false;

  if WRITE_DELIVERY_LOG then gLog.AddTime('Creating delivery ID', i);

  //Now we have best job and can perform it
  if fDemand[iD].Loc_House <> nil then
    aSerf.Deliver(fOffer[iO].Loc_House, fDemand[iD].Loc_House, fOffer[iO].Ware, i)
  else
    aSerf.Deliver(fOffer[iO].Loc_House, fDemand[iD].Loc_Unit, fOffer[iO].Ware, i)
end;


//Resource has been taken from Offer
procedure TKMDeliverQueue.TakenOffer(aID: Integer);
var iO: Integer;
begin
  if WRITE_DELIVERY_LOG then gLog.AddTime('Taken offer from delivery ID', aID);

  iO := fQueue[aID].OfferID;
  fQueue[aID].OfferID := 0; //We don't need it any more

  Dec(fOffer[iO].BeingPerformed); //Remove reservation
  Dec(fOffer[iO].Count); //Remove resource from Offer list

  if fOffer[iO].Count = 0 then
    if fOffer[iO].BeingPerformed > 0 then
      fOffer[iO].IsDeleted := True
    else
      CloseOffer(iO);
end;


//Resource has been delivered to Demand
procedure TKMDeliverQueue.GaveDemand(aID:integer);
var iD:integer;
begin
  if WRITE_DELIVERY_LOG then gLog.AddTime('Gave demand from delivery ID', aID);
  iD:=fQueue[aID].DemandID;
  fQueue[aID].DemandID:=0; //We don't need it any more

  fDemand[iD].BeingPerformed:=false; //Remove reservation

  if fDemand[iD].DemandType=dt_Once then
    CloseDemand(iD); //Remove resource from Demand list
end;


//AbandonDelivery
procedure TKMDeliverQueue.AbandonDelivery(aID:integer);
begin
  if WRITE_DELIVERY_LOG then gLog.AddTime('Abandoned delivery ID', aID);

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
    fDemand[fQueue[aID].DemandID].BeingPerformed := False;
    if fDemand[fQueue[aID].DemandID].IsDeleted then
      CloseDemand(fQueue[aID].DemandID);
  end;

  CloseDelivery(aID);
end;


//Job successfully done and we ommit it
procedure TKMDeliverQueue.CloseDelivery(aID:integer);
begin
  if WRITE_DELIVERY_LOG then gLog.AddTime('Closed delivery ID', aID);

  fQueue[aID].OfferID:=0;
  fQueue[aID].DemandID:=0;
  fQueue[aID].JobStatus:=js_Empty; //Open slot
end;


procedure TKMDeliverQueue.CloseDemand(aID:integer);
begin
  assert(not fDemand[aID].BeingPerformed);
  fDemand[aID].Ware := wt_None;
  fDemand[aID].DemandType := dt_Once;
  fDemand[aID].Importance := diNorm;
  fPlayers.CleanUpHousePointer(fDemand[aID].Loc_House);
  fPlayers.CleanUpUnitPointer(fDemand[aID].Loc_Unit);
  fDemand[aID].IsDeleted := false;
end;


procedure TKMDeliverQueue.CloseOffer(aID:integer);
begin
  assert(fOffer[aID].BeingPerformed = 0);
  fOffer[aID].IsDeleted := false;
  fOffer[aID].Ware := wt_None;
  fOffer[aID].Count := 0;
  fPlayers.CleanUpHousePointer(fOffer[aID].Loc_House);
end;


procedure TKMDeliverQueue.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write('Deliveries');
  SaveStream.Write(fOfferCount);
  for i:=1 to fOfferCount do
  begin
    SaveStream.Write(fOffer[i].Ware, SizeOf(fOffer[i].Ware));
    SaveStream.Write(fOffer[i].Count);
    if fOffer[i].Loc_House <> nil then
      SaveStream.Write(fOffer[i].Loc_House.ID)
    else
      SaveStream.Write(Integer(0));
    SaveStream.Write(fOffer[i].BeingPerformed);
    SaveStream.Write(fOffer[i].IsDeleted);
  end;

  SaveStream.Write(fDemandCount);
  for i:=1 to fDemandCount do
  with fDemand[i] do
  begin
    SaveStream.Write(Ware, SizeOf(Ware));
    SaveStream.Write(DemandType, SizeOf(DemandType));
    SaveStream.Write(Importance, SizeOf(Importance));
    if Loc_House <> nil then SaveStream.Write(Loc_House.ID) else SaveStream.Write(Integer(0));
    if Loc_Unit  <> nil then SaveStream.Write(Loc_Unit.ID ) else SaveStream.Write(Integer(0));
    SaveStream.Write(BeingPerformed);
    SaveStream.Write(IsDeleted);
  end;

  SaveStream.Write(fQueueCount);
  for i:=1 to fQueueCount do
  begin
    SaveStream.Write(fQueue[i].OfferID);
    SaveStream.Write(fQueue[i].DemandID);
    SaveStream.Write(fQueue[i].JobStatus, SizeOf(fQueue[i].JobStatus));
  end;
end;


procedure TKMDeliverQueue.Load(LoadStream:TKMemoryStream);
var i:integer;
begin
  LoadStream.ReadAssert('Deliveries');
  LoadStream.Read(fOfferCount);
  SetLength(fOffer, fOfferCount+1);
  for i:=1 to fOfferCount do
  begin
    LoadStream.Read(fOffer[i].Ware, SizeOf(fOffer[i].Ware));
    LoadStream.Read(fOffer[i].Count);
    LoadStream.Read(fOffer[i].Loc_House, 4);
    LoadStream.Read(fOffer[i].BeingPerformed);
    LoadStream.Read(fOffer[i].IsDeleted);
  end;

  LoadStream.Read(fDemandCount);
  SetLength(fDemand, fDemandCount+1);
  for i:=1 to fDemandCount do
  with fDemand[i] do
  begin
    LoadStream.Read(Ware, SizeOf(Ware));
    LoadStream.Read(DemandType, SizeOf(DemandType));
    LoadStream.Read(Importance, SizeOf(Importance));
    LoadStream.Read(Loc_House, 4);
    LoadStream.Read(Loc_Unit, 4);
    LoadStream.Read(BeingPerformed);
    LoadStream.Read(IsDeleted);
  end;

  LoadStream.Read(fQueueCount);
  SetLength(fQueue, fQueueCount+1);
  for i:=1 to fQueueCount do
  begin
    LoadStream.Read(fQueue[i].OfferID);
    LoadStream.Read(fQueue[i].DemandID);
    LoadStream.Read(fQueue[i].JobStatus, SizeOf(fQueue[i].JobStatus));
  end;
end;


procedure TKMDeliverQueue.SyncLoad;
var i:integer;
begin
  for i:=1 to fOfferCount do
    fOffer[i].Loc_House := fPlayers.GetHouseByID(cardinal(fOffer[i].Loc_House));

  for i:=1 to fDemandCount do
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
  for i:=1 to fDemandCount do if fDemand[i].Ware<>wt_None then
  begin
    s:=s+#9;
    if fDemand[i].Loc_House<>nil then s:=s+fResource.HouseDat[fDemand[i].Loc_House.HouseType].HouseName+#9+#9;
    if fDemand[i].Loc_Unit<>nil then s:=s+fResource.UnitDat[fDemand[i].Loc_Unit.UnitType].UnitName+#9+#9;
    s:=s+fResource.Wares[fDemand[i].Ware].Title;
    if fDemand[i].Importance <> diNorm then s:=s+'^';
    s:=s+eol;
  end;
  s:=s+eol+'Offer:'+eol+'---------------------------------'+eol;
  for i:=1 to fOfferCount do if fOffer[i].Ware<>wt_None then
  begin
    s:=s+#9;
    if fOffer[i].Loc_House<>nil then s:=s+fResource.HouseDat[fOffer[i].Loc_House.HouseType].HouseName+#9+#9;
    s:=s+fResource.Wares[fOffer[i].Ware].Title+#9;
    s:=s+IntToStr(fOffer[i].Count);
    s:=s+eol;
  end;

  s:=s+eol+'Running deliveries:'+eol+'---------------------------------'+eol;
  for i:=1 to fQueueCount do if fQueue[i].OfferID<>0 then
  begin

    s:=s+'id '+inttostr(i)+'.'+#9;
    s:=s+fResource.Wares[fOffer[fQueue[i].OfferID].Ware].Title+#9;

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


end.

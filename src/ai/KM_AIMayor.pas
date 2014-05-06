unit KM_AIMayor;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Utils, KM_Points,
  KM_CityPlanner, KM_PathfindingRoad, KM_AISetup, KM_AIMayorBalance,
  KM_ResHouses;

type
  //Mayor is the one who manages the town
  TKMayor = class
  private
    fOwner: THandIndex;
    fSetup: TKMHandAISetup;
    fBalance: TKMayorBalance;
    fCityPlanner: TKMCityPlanner;
    fPathFindingRoad: TPathFindingRoad;
    fPathFindingRoadShortcuts: TPathFindingRoadShortcuts;

    fAutoRepair: Boolean;

    fRoadBelowStore: Boolean;
    fArmyType: TArmyType;

    ShieldNeed: Single;
    ArmorNeed: Single;
    AxeNeed: Single;
    PikeNeed: Single;
    BowNeed: Single;
    HorseNeed: Single;

    procedure SetArmyDemand(aFootmen, aPikemen, aHorsemen, aArchers: Single);
    procedure SetAutoRepair(const Value: Boolean);

    function TryBuildHouse(aHouse: THouseType): Boolean;
    function TryConnectToRoad(aLoc: TKMPoint): Boolean;

    procedure CheckStrategy;

    procedure CheckUnitCount;
    procedure CheckWareFlow;
    procedure CheckHouseCount;
    procedure CheckHousePlans;
    procedure CheckRoadsCount;
    procedure CheckExhaustedMines;
    procedure CheckWeaponOrderCount;
    procedure CheckArmyDemand;
  public
    constructor Create(aPlayer: THandIndex; aSetup: TKMHandAISetup);
    destructor Destroy; override;

    property CityPlanner: TKMCityPlanner read fCityPlanner;

    procedure AfterMissionInit;
    property AutoRepair: Boolean read fAutoRepair write SetAutoRepair;
    procedure OwnerUpdate(aPlayer: THandIndex);
    function BalanceText: UnicodeString;

    procedure UpdateState(aTick: Cardinal);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Game, KM_Houses, KM_HouseCollection, KM_HouseSchool, KM_HandsCollection, KM_Hand, KM_Terrain, KM_Resource,
  KM_ResWares, KM_AIFields, KM_Units, KM_UnitTaskDelivery, KM_UnitActionWalkTo, KM_UnitTaskGoEat;


const //Sample list made by AntonP
  WarriorHouses: array [0..44] of THouseType = (
  ht_School, ht_Inn, ht_Quary, ht_Quary, ht_Quary,
  ht_Woodcutters, ht_Woodcutters, ht_Woodcutters, ht_Woodcutters, ht_Woodcutters,
  ht_Sawmill, ht_Sawmill, ht_Woodcutters, ht_GoldMine, ht_CoalMine,
  ht_GoldMine, ht_CoalMine, ht_Metallurgists, ht_CoalMine, ht_CoalMine,
  ht_IronMine, ht_IronMine, ht_CoalMine, ht_IronMine, ht_WeaponSmithy,
  ht_WeaponSmithy, ht_Wineyard, ht_Wineyard, ht_Wineyard, ht_Store,
  ht_Barracks, ht_Farm, ht_Farm, ht_Farm, ht_Mill,
  ht_Mill, ht_Bakery, ht_Bakery, ht_School, ht_IronSmithy,
  ht_IronSmithy, ht_Farm, ht_Swine, ht_WeaponSmithy, ht_ArmorSmithy
  );

  //Vital (Store, School, Inn)
  //Mining_Core (Quary x3, Woodcutters x3, Sawmill)
  //Mining_Gold (CoalMine x2, GoldMine, Metallurgists)
  //Food_Basic (Farm, Mill, Bakery, Wineyard)

  //Food

  //Warfare_Leather (Woodcutters x2, Sawmill, Swine x4, Tannery x2, Armor x2, Weapon x3)
  //Warfare_Iron (Coal x4, Iron x2, IronSmithy x2, Armor, Weapon x2)

  //Hiring_Army (Barracks)
  //Hiring_Army2 (School, CoalMine x2, GoldMine)

  WOOD_BLOCK_RAD = 5.8;


{ TKMayor }
constructor TKMayor.Create(aPlayer: THandIndex; aSetup: TKMHandAISetup);
begin
  inherited Create;

  fOwner := aPlayer;
  fSetup := aSetup;

  fBalance := TKMayorBalance.Create(fOwner);
  fCityPlanner := TKMCityPlanner.Create(fOwner);
  fPathFindingRoad := TPathFindingRoad.Create(fOwner);
  fPathFindingRoadShortcuts := TPathFindingRoadShortcuts.Create(fOwner);

  fAutoRepair := False; //In KaM it is Off by default
end;


destructor TKMayor.Destroy;
begin
  fBalance.Free;
  fCityPlanner.Free;
  fPathFindingRoad.Free;
  fPathFindingRoadShortcuts.Free;
  inherited;
end;


procedure TKMayor.AfterMissionInit;
begin
  fCityPlanner.AfterMissionInit;
  CheckStrategy;
  CheckArmyDemand;
end;


{ Check existing unit count vs house count and train missing citizens }
procedure TKMayor.CheckUnitCount;
var
  P: TKMHand;
  HS: TKMHouseSchool;
  UnitReq: array [CITIZEN_MIN..CITIZEN_MAX] of Integer;

  function HasEnoughGold: Boolean;
  begin
    //Producing gold or (Gold > 10)
    Result := (P.Stats.GetWaresProduced(wt_Gold) > 1)
              or (P.Stats.GetWareBalance(wt_Gold) > 10);
  end;

  function TryAddToQueue(aUnitType: TUnitType; aReq: Integer): Boolean;
  begin
    //We summ up requirements for e.g. Recruits required at Towers and Barracks
    if P.Stats.GetUnitQty(aUnitType) < (aReq + UnitReq[aUnitType]) then
    begin
      Dec(UnitReq[aUnitType]); //So other schools don't order same unit
      HS.AddUnitToQueue(aUnitType, 1);
      Result := True;
    end
    else
      Result := False;
  end;

var
  I,K: Integer;
  H: THouseType;
  UT: TUnitType;
  Schools: array of TKMHouseSchool;
begin
  //todo: When training new units make sure we have enough gold left to train
  //stonemason-woodcutter-carpenter-2miners-metallurgist. In other words -
  //dont waste gold if it's not producing yet

  P := gHands[fOwner];

  //Citizens
  //Count overall unit requirement (excluding Barracks and ownerless houses)
  FillChar(UnitReq, SizeOf(UnitReq), #0); //Clear up
  for H := HOUSE_MIN to HOUSE_MAX do
    if (gResource.HouseDat[H].OwnerType <> ut_None) and (H <> ht_Barracks) then
      Inc(UnitReq[gResource.HouseDat[H].OwnerType], P.Stats.GetHouseQty(H));

  //Schools
  //Count overall schools count and exclude already training units from UnitReq
  SetLength(Schools, P.Stats.GetHouseQty(ht_School));
  K := 1;
  HS := TKMHouseSchool(P.FindHouse(ht_School, K));
  while HS <> nil do
  begin
    Schools[K-1] := HS;
    for I := 0 to HS.QueueLength - 1 do //Decrease requirement for each unit in training
      if HS.Queue[I] <> ut_None then
        Dec(UnitReq[HS.Queue[I]]); //Can be negative and compensated by e.g. ReqRecruits
    Inc(K);
    HS := TKMHouseSchool(P.FindHouse(ht_School, K));
  end;

  //Order the training. Keep up to 2 units in the queue so the school doesn't have to wait
  for K := 0 to High(Schools) do
  begin
    HS := Schools[K];
    if (HS <> nil) and (HS.QueueCount < 2) then
    begin
      //Order citizen training
      for UT := Low(UnitReq) to High(UnitReq) do
        if (UnitReq[UT] > 0) //Skip units that dont needed by houses (Serf/Worker)
        and (UnitReq[UT] > P.Stats.GetUnitQty(UT)) then
        begin
          Dec(UnitReq[UT]); //So other schools don't order same unit
          HS.AddUnitToQueue(UT, 1);
          Break; //Don't need more UnitTypes yet
        end;

      //If we are here then a citizen to train wasn't found, so try other unit types (citizens get top priority)
      //Serf factor is like this: Serfs = (10/FACTOR)*Total_Building_Count) (from: http://atfreeforum.com/knights/viewtopic.php?t=465)
      while (HS.QueueCount < 2) //Still haven't found a match...
      and HasEnoughGold do  //If we are low on Gold don't hire more ppl
        if not TryAddToQueue(ut_Serf, Round(fSetup.SerfsPerHouse * (P.Stats.GetHouseQty(ht_Any) + P.Stats.GetUnitQty(ut_Worker)/2))) then
          if not TryAddToQueue(ut_Worker, fSetup.WorkerCount) then
            if not gGame.CheckTime(fSetup.RecruitDelay) then //Recruits can only be trained after this time
              Break
            else
              if not TryAddToQueue(ut_Recruit, fSetup.RecruitCount * P.Stats.GetHouseQty(ht_Barracks)) then
                Break; //There's no unit demand at all
    end;
  end;
end;


procedure TKMayor.CheckArmyDemand;
var Footmen, Pikemen, Horsemen, Archers: Integer;
begin
  gHands[fOwner].AI.General.DefencePositions.GetArmyDemand(Footmen, Pikemen, Horsemen, Archers);
  SetArmyDemand(Footmen, Pikemen, Horsemen, Archers);
end;


//Check that we have weapons ordered for production
procedure TKMayor.CheckWeaponOrderCount;
const
  //Order weapons in portions to avoid overproduction of one ware over another
  //(e.g. Shields in armory until Leather is available)
  PORTIONS = 8;
var
  I,K: Integer;
  H: TKMHouse;
  ResOrder: Integer;
begin
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
  begin
    H := gHands[fOwner].Houses[I];

    ResOrder := H.ResOrder[1] + H.ResOrder[2] + H.ResOrder[3] + H.ResOrder[4];

    if not H.IsDestroyed and (ResOrder = 0) then
    case H.HouseType of
      ht_ArmorSmithy:     for K := 1 to 4 do
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_MetalShield then
                              H.ResOrder[K] := Round(ShieldNeed * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_MetalArmor then
                              H.ResOrder[K] := Round(ArmorNeed * PORTIONS);
      ht_ArmorWorkshop:   for K := 1 to 4 do
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Shield then
                              H.ResOrder[K] := Round(ShieldNeed * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Armor then
                              H.ResOrder[K] := Round(ArmorNeed * PORTIONS);
      ht_WeaponSmithy:    for K := 1 to 4 do
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Sword then
                              H.ResOrder[K] := Round(AxeNeed * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Hallebard then
                              H.ResOrder[K] := Round(PikeNeed * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Arbalet then
                              H.ResOrder[K] := Round(BowNeed * PORTIONS);
      ht_WeaponWorkshop:  for K := 1 to 4 do
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Axe then
                              H.ResOrder[K] := Round(AxeNeed * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Pike then
                              H.ResOrder[K] := Round(PikeNeed * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Bow then
                              H.ResOrder[K] := Round(BowNeed * PORTIONS);
    end;
  end;
end;


//We want to connect to nearest road piece (not necessarily built yet)
function TKMayor.TryConnectToRoad(aLoc: TKMPoint): Boolean;
var
  I: Integer;
  P: TKMHand;
  H: TKMHouse;
  LocTo: TKMPoint;
  NodeList: TKMPointList;
  RoadExists: Boolean;
begin
  Result := False;
  P := gHands[fOwner];

  //Find nearest wip or ready house
  H := P.Houses.FindHouse(ht_Any, aLoc.X, aLoc.Y, 1, False);
  if H = nil then Exit; //We are screwed, no houses left
  LocTo := KMPointBelow(H.GetEntrance);

  NodeList := TKMPointList.Create;
  try
    RoadExists := fPathFindingRoad.Route_ReturnToWalkable(aLoc, LocTo, NodeList);

    if not RoadExists then
      Exit;

    for I := 0 to NodeList.Count - 1 do
      //We must check if we can add the plan ontop of plans placed earlier in this turn
      if P.CanAddFieldPlan(NodeList[I], ft_Road) then
        P.BuildList.FieldworksList.AddField(NodeList[I], ft_Road);
    Result := True;
  finally
    NodeList.Free;
  end;
end;


//Try to place a building plan for requested house
//Report back if failed to do so (that will allow requester to choose different action)
function TKMayor.TryBuildHouse(aHouse: THouseType): Boolean;
var
  I, K: Integer;
  Loc: TKMPoint;
  P: TKMHand;
  NodeTagList: TKMPointTagList;
begin
  Result := False;
  P := gHands[fOwner];

  //Skip disabled houses
  if not P.Stats.GetCanBuild(aHouse) then Exit;

  //Number of simultaneous WIP houses is limited
  if (P.Stats.GetHouseWip(ht_Any) > MAX_AI_PLANS) then Exit;

  //Maybe we get more lucky next tick
  //todo: That only works if FindPlaceForHouse is quick, right now it takes ~11ms for iron/gold/coal mines (to decide that they can't be placed).
  //      If there's no place for the house we try again and again and again every update, so it's very inefficient
  //      I think the best solution would be to make FindPlaceForHouse only take a long time if we succeed in finding a place for the house, if we
  //      fail it should be quick. Doing a flood fill with radius=40 should really be avoided anyway, 11ms is a long time for placing 1 house.
  //      We could also make it not try to place houses again each update if it failed the first time, if we can't make FindPlaceForHouse quick when it fails.
  if not fCityPlanner.FindPlaceForHouse(aHouse, Loc) then Exit;

  //Place house before road, so that road is made around it
  P.AddHousePlan(aHouse, Loc);

  //Try to connect newly planned house to road network
  //if it is not possible - scrap the plan
  if not TryConnectToRoad(KMPointBelow(Loc)) then
  begin
    P.RemHousePlan(Loc);
    Exit;
  end;

  //Build fields for Farm
  if aHouse = ht_Farm then
  begin
    NodeTagList := TKMPointTagList.Create;
    try
      for I := Min(Loc.Y + 2, gTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, gTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, gTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Corn) then
          NodeTagList.Add(KMPoint(K, I), Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y));

      NodeTagList.SortByTag;
      for I := 0 to Min(NodeTagList.Count, 16) - 1 do
        P.BuildList.FieldworksList.AddField(NodeTagList[I], ft_Corn);
    finally
      NodeTagList.Free;
    end;
  end;

  //Build fields for Wineyard
  if aHouse = ht_Wineyard then
  begin
    NodeTagList := TKMPointTagList.Create;
    try
      for I := Min(Loc.Y + 1, gTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, gTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, gTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Wine) then
          NodeTagList.Add(KMPoint(K, I), Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y));

      NodeTagList.SortByTag;
      for I := 0 to Min(NodeTagList.Count, 10) - 1 do
        P.BuildList.FieldworksList.AddField(NodeTagList[I], ft_Wine);
    finally
      NodeTagList.Free;
    end;
  end;

  //Block any buildings nearby
  if aHouse = ht_Woodcutters then
    fAIFields.Influences.AddAvoidBuilding(Loc.X-1, Loc.Y, WOOD_BLOCK_RAD); //X-1 because entrance is on right

  //Build more roads around 2nd Store
  if aHouse = ht_Store then
    for I := Max(Loc.Y - 3, 1) to Min(Loc.Y + 2, gTerrain.MapY - 1) do
    for K := Max(Loc.X - 2, 1) to Min(Loc.X + 2, gTerrain.MapY - 1) do
    if P.CanAddFieldPlan(KMPoint(K, I), ft_Road) then
      P.BuildList.FieldworksList.AddField(KMPoint(K, I), ft_Road);

  Result := True;
end;


//todo: Check if planned houses are being connected with roads
//(worker could die while digging a road piece or elevation changed to impassable)
procedure TKMayor.CheckHousePlans;
begin
  //
end;


//Manage ware distribution
procedure TKMayor.CheckWareFlow;
var
  I: Integer;
  S: TKMHouseStore;
  Houses: TKMHousesCollection;
begin
  Houses := gHands[fOwner].Houses;

  //Iterate through all Stores and block certain wares to reduce serf usage
  for I := 0 to Houses.Count - 1 do
    if (Houses[I].HouseType = ht_Store)
    and Houses[I].IsComplete
    and not Houses[I].IsDestroyed then
    begin
      S := TKMHouseStore(Houses[I]);

      //We like to always keep a supply of these
      S.NotAcceptFlag[wt_Wood] := S.CheckResIn(wt_Wood) > 50;
      S.NotAcceptFlag[wt_Stone] := S.CheckResIn(wt_Stone) > 50;
      S.NotAcceptFlag[wt_Gold] := S.CheckResIn(wt_Gold) > 50;

      //Storing these causes lots of congestion with very little gain
      //Auto build AI aims for perfectly balanced village where these goods don't need storing
      //Keep them only until we have the house which consumes them.
      S.NotAcceptFlag[wt_Trunk] := gHands[fOwner].Stats.GetHouseQty(ht_Sawmill) > 0;
      S.NotAcceptFlag[wt_GoldOre] := gHands[fOwner].Stats.GetHouseQty(ht_Metallurgists) > 0;
      S.NotAcceptFlag[wt_IronOre] := gHands[fOwner].Stats.GetHouseQty(ht_IronSmithy) > 0;
      S.NotAcceptFlag[wt_Coal] := gHands[fOwner].Stats.GetHouseQty(ht_Metallurgists) +
                                  gHands[fOwner].Stats.GetHouseQty(ht_IronSmithy) > 0;
      S.NotAcceptFlag[wt_Steel] := gHands[fOwner].Stats.GetHouseQty(ht_WeaponSmithy) +
                                   gHands[fOwner].Stats.GetHouseQty(ht_ArmorSmithy) > 0;
      S.NotAcceptFlag[wt_Corn] := gHands[fOwner].Stats.GetHouseQty(ht_Mill) +
                                  gHands[fOwner].Stats.GetHouseQty(ht_Swine) +
                                  gHands[fOwner].Stats.GetHouseQty(ht_Stables) > 0;
      S.NotAcceptFlag[wt_Leather] := gHands[fOwner].Stats.GetHouseQty(ht_ArmorWorkshop) > 0;
      S.NotAcceptFlag[wt_Flour] := gHands[fOwner].Stats.GetHouseQty(ht_Bakery) > 0;
      //Pigs and skin cannot be blocked since if swinefarm is full of one it stops working (blocks other)
      //S.NotAcceptFlag[wt_Skin] := gHands[fOwner].Stats.GetHouseQty(ht_Tannery) > 0;
      //S.NotAcceptFlag[wt_Pig] := gHands[fOwner].Stats.GetHouseQty(ht_Butchers) > 0;
    end;
end;


//Demolish any exhausted mines, they will be rebuilt if needed
procedure TKMayor.CheckExhaustedMines;
var
  I: Integer;
  Houses: TKMHousesCollection;
begin
  Houses := gHands[fOwner].Houses;

  //Wait until resource is depleted and output is empty
  for I := 0 to Houses.Count - 1 do
  if not Houses[I].IsDestroyed
  and Houses[I].ResourceDepletedMsgIssued
  and (Houses[I].CheckResOut(wt_All) = 0) then
    Houses[I].DemolishHouse(fOwner);
end;


procedure TKMayor.CheckHouseCount;
var
  P: TKMHand;
  H: THouseType;
begin
  P := gHands[fOwner];

  //Try to express needs in terms of Balance = Production - Demand
  fBalance.Refresh;

  //Peek - see if we can build this house
  //Take - take this house into building
  //Reject - we can't build this house (that could affect other houses in queue)

  while (P.Stats.GetHouseWip(ht_Any) < MAX_AI_PLANS) do
  begin
    H := fBalance.Peek;

    //There are no more suggestions
    if H = ht_None then
      Break;

    //See if we can build that
    if TryBuildHouse(H) then
      fBalance.Take
    else
      fBalance.Reject;
  end;

  //Check if we need to demolish depleted mining houses
  CheckExhaustedMines;

  //Verify all plans are being connected with roads
  CheckHousePlans;
end;


procedure TKMayor.CheckRoadsCount;
var
  P: TKMHand;
  Store: TKMHouse;
  StoreLoc: TKMPoint;
  I, K: Integer;
  FromLoc, ToLoc: TKMPoint;
  NodeList: TKMPointList;
  RoadExists: Boolean;
begin
  P := gHands[fOwner];

  //This is one time task to build roads around Store
  //When town becomes larger add road around Store to make traffic smoother
  if not fRoadBelowStore and (P.Stats.GetHouseQty(ht_Any) > 14) then
  begin
    fRoadBelowStore := True;

    Store := P.Houses.FindHouse(ht_Store, 0, 0, 1);
    if Store = nil then Exit;
    StoreLoc := Store.GetEntrance;

    for I := Max(StoreLoc.Y - 3, 1) to Min(StoreLoc.Y + 2, gTerrain.MapY - 1) do
    for K := StoreLoc.X - 2 to StoreLoc.X + 2 do
    if P.CanAddFieldPlan(KMPoint(K, I), ft_Road) then
      P.BuildList.FieldworksList.AddField(KMPoint(K, I), ft_Road);
  end;

  //Check if we need to connect separate branches of road network
  //Town has no plan and usually roadnetwork looks like a tree,
  //where we can improve it by connecting near branches with shortcuts.
  NodeList := TKMPointList.Create;
  try
    //See where our citizens are walking and build shortcuts where possible
    for I := 0 to gHands[fOwner].Units.Count - 1 do
      if not gHands[fOwner].Units[I].IsDeadOrDying
      and (gHands[fOwner].Units[I].GetUnitAction is TUnitActionWalkTo) then
        if ((gHands[fOwner].Units[I] is TKMUnitSerf) and (gHands[fOwner].Units[I].UnitTask is TTaskDeliver))
        or ((gHands[fOwner].Units[I] is TKMUnitCitizen) and (gHands[fOwner].Units[I].UnitTask is TTaskGoEat)) then
        begin
          FromLoc := TUnitActionWalkTo(gHands[fOwner].Units[I].GetUnitAction).WalkFrom;
          ToLoc := TUnitActionWalkTo(gHands[fOwner].Units[I].GetUnitAction).WalkTo;
          //Unit's route must be using road network, not f.e. delivering to soldiers
          if gTerrain.Route_CanBeMade(FromLoc, ToLoc, CanWalkRoad, 0) then
          begin
            //Check for shortcuts we could build
            NodeList.Clear;
            RoadExists := fPathFindingRoadShortcuts.Route_Make(FromLoc, ToLoc, NodeList);

            if not RoadExists then
              Break;

            for K := 0 to NodeList.Count - 1 do
              //We must check if we can add the plan ontop of plans placed earlier in this turn
              if P.CanAddFieldPlan(NodeList[K], ft_Road) then
                P.BuildList.FieldworksList.AddField(NodeList[K], ft_Road);
          end;
        end;
  finally
    NodeList.Free;
  end;
end;


procedure TKMayor.CheckStrategy;
var
  Store: TKMHouse;
  StoreLoc, T: TKMPoint;
begin
  Store := gHands[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1);
  if Store = nil then Exit;
  StoreLoc := Store.GetEntrance;

  //AI will be Wooden if there is no iron/coal nearby
  if (gHands[fOwner].PlayerType = hndComputer) and fSetup.AutoBuild then
  begin
    //todo: Add preferred army type selector to MapEd
    if (fCityPlanner.FindNearest(KMPointBelow(StoreLoc), 60, fnIron, T) and fCityPlanner.FindNearest(KMPointBelow(StoreLoc), 60, fnCoal, T)) then
      if KaMRandom < 0 then
        //Sometimes AI will be pure Iron
        fArmyType := atIron
      else
        //Usually AI will do both
        fArmyType := atLeatherIron
    else
      //If there's no iron ore or coal
      fArmyType := atLeather;
  end;

  fBalance.ArmyType := fArmyType;
end;


procedure TKMayor.OwnerUpdate(aPlayer: THandIndex);
begin
  fOwner := aPlayer;
  fBalance.OwnerUpdate(aPlayer);
  fCityPlanner.OwnerUpdate(aPlayer);
  fPathFindingRoad.OwnerUpdate(aPlayer);
  fPathFindingRoadShortcuts.OwnerUpdate(aPlayer);
end;


//Tell Mayor what proportions of army is needed
//Input values are normalized
procedure TKMayor.SetArmyDemand(aFootmen, aPikemen, aHorsemen, aArchers: Single);

  function IsIronProduced: Boolean;
  var I: Integer;
  begin
    Result := False;
    for I := 0 to gHands[fOwner].Houses.Count-1 do
      if (gHands[fOwner].Houses[I].HouseType = ht_IronMine)
      and not gHands[fOwner].Houses[I].ResourceDepletedMsgIssued then
      begin
        Result := True;
        Exit;
      end;
  end;

var
  Summ: Single;
  Footmen, Pikemen, Horsemen, Archers: Single;
  WarPerMin: Single;
begin
  Summ := aFootmen + aPikemen + aHorsemen + aArchers;
  if Summ = 0 then
  begin
    Footmen := 0;
    Pikemen := 0;
    Horsemen := 0;
    Archers := 0;
  end
  else
  begin
    Footmen := aFootmen / Summ;
    Pikemen := aPikemen / Summ;
    Horsemen := aHorsemen / Summ;
    Archers := aArchers / Summ;
  end;

  //Store localy in Mayor to place weapon orders
  ShieldNeed := Footmen + Horsemen;
  ArmorNeed := Footmen + Pikemen + Horsemen + Archers;
  AxeNeed := Footmen + Horsemen;
  PikeNeed := Pikemen;
  BowNeed := Archers;
  HorseNeed := Horsemen;

  //How many warriors we would need to equip per-minute
  WarPerMin := fSetup.WarriorsPerMinute(fArmyType);
  if IsIronProduced then
    WarPerMin := WarPerMin / 2; //Half leather half iron

  //Update warfare needs accordingly
  fBalance.SetArmyDemand(ShieldNeed * WarPerMin, ArmorNeed * WarPerMin, AxeNeed * WarPerMin, PikeNeed * WarPerMin, BowNeed * WarPerMin, HorseNeed * WarPerMin);
end;


procedure TKMayor.SetAutoRepair(const Value: Boolean);
var
  I: Integer;
begin
  fAutoRepair := Value;

  //Apply to those houses placed by a script before AutoRepair command
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
    gHands[fOwner].Houses[I].BuildingRepair := fAutoRepair;
end;


function TKMayor.BalanceText: UnicodeString;
begin
  Result := fBalance.BalanceText;
end;


procedure TKMayor.UpdateState(aTick: Cardinal);
begin
  if (aTick + Byte(fOwner)) mod (MAX_HANDS * 10) <> 0 then Exit;

  //Train new units (citizens, serfs, workers and recruits) if needed
  CheckUnitCount;

  CheckArmyDemand;
  CheckWeaponOrderCount;

  if fSetup.AutoBuild then
  begin
    CheckHouseCount;

    //Manage wares ratios and block stone to Store
    CheckWareFlow;

    //Build more roads if necessary
    CheckRoadsCount;
  end;
end;


procedure TKMayor.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
  SaveStream.Write(fAutoRepair);
  SaveStream.Write(fRoadBelowStore);
  SaveStream.Write(fArmyType, SizeOf(TArmyType));

  SaveStream.Write(ShieldNeed);
  SaveStream.Write(ArmorNeed);
  SaveStream.Write(AxeNeed);
  SaveStream.Write(PikeNeed);
  SaveStream.Write(BowNeed);
  SaveStream.Write(HorseNeed);

  fBalance.Save(SaveStream);
  fCityPlanner.Save(SaveStream);
  fPathFindingRoad.Save(SaveStream);
  fPathFindingRoadShortcuts.Save(SaveStream);
end;


procedure TKMayor.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
  LoadStream.Read(fAutoRepair);
  LoadStream.Read(fRoadBelowStore);
  LoadStream.Read(fArmyType, SizeOf(TArmyType));

  LoadStream.Read(ShieldNeed);
  LoadStream.Read(ArmorNeed);
  LoadStream.Read(AxeNeed);
  LoadStream.Read(PikeNeed);
  LoadStream.Read(BowNeed);
  LoadStream.Read(HorseNeed);

  fBalance.Load(LoadStream);
  fCityPlanner.Load(LoadStream);
  fPathFindingRoad.Load(LoadStream);
  fPathFindingRoadShortcuts.Load(LoadStream);
end;


end.

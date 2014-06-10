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
    fDefenceTowersPlanned: Boolean;
    fDefenceTowers: TKMPointTagList;

    WarfareRatios: TWarfareDemands;

    procedure SetArmyDemand(aFootmen, aPikemen, aHorsemen, aArchers: Single);
    procedure SetAutoRepair(const Value: Boolean);

    function TryBuildHouse(aHouse: THouseType): Boolean;
    function TryConnectToRoad(aLoc: TKMPoint): Boolean;
    function GetMaxPlans: Byte;

    procedure CheckUnitCount;
    procedure CheckWareFlow;
    procedure CheckHouseCount;
    procedure CheckHousePlans;
    procedure CheckRoadsCount;
    procedure CheckExhaustedMines;
    procedure CheckWeaponOrderCount;
    procedure CheckArmyDemand;
    procedure PlanDefenceTowers;
    procedure TryBuildDefenceTower;
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
  KM_ResWares, KM_AIFields, KM_Units, KM_UnitTaskDelivery, KM_UnitActionWalkTo, KM_UnitTaskGoEat, KM_UnitsCollection,
  KM_NavMesh;


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
  fDefenceTowers := TKMPointTagList.Create;

  fAutoRepair := False; //In KaM it is Off by default
end;


destructor TKMayor.Destroy;
begin
  fBalance.Free;
  fCityPlanner.Free;
  fPathFindingRoad.Free;
  fPathFindingRoadShortcuts.Free;
  fDefenceTowers.Free;
  inherited;
end;


procedure TKMayor.AfterMissionInit;
begin
  fCityPlanner.AfterMissionInit;
  CheckArmyDemand;
  fBalance.StoneNeed := GetMaxPlans * 2.5;
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
              or (P.Stats.GetWareBalance(wt_Gold) > 20);
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

  function RecruitsNeeded: Integer;
  var AxesLeft: Integer;
  begin
    if P.Stats.GetHouseQty(ht_Barracks) = 0 then
      Result := 0
    else
      if gGame.IsPeaceTime then
      begin
        //Keep enough recruits to equip using all weapons once PT ends
        //Iron soldiers
        Result := Min(P.Stats.GetWareBalance(wt_MetalArmor),
                      P.Stats.GetWareBalance(wt_Arbalet) + P.Stats.GetWareBalance(wt_Hallebard)
                      + Min(P.Stats.GetWareBalance(wt_Sword), P.Stats.GetWareBalance(wt_MetalShield)));
        //Leather soldiers we can make
        Inc(Result, Min(P.Stats.GetWareBalance(wt_Armor),
                        P.Stats.GetWareBalance(wt_Bow) + P.Stats.GetWareBalance(wt_Pike)
                        + Min(P.Stats.GetWareBalance(wt_Axe), P.Stats.GetWareBalance(wt_Shield))));
        //Militia with leftover axes
        AxesLeft := P.Stats.GetWareBalance(wt_Axe) - Min(P.Stats.GetWareBalance(wt_Armor), P.Stats.GetWareBalance(wt_Shield));
        if AxesLeft > 0 then
          Inc(Result, AxesLeft);
      end
      else
        Result := fSetup.RecruitCount * P.Stats.GetHouseQty(ht_Barracks);
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
              if not TryAddToQueue(ut_Recruit, RecruitsNeeded) then
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
                              H.ResOrder[K] := Round(WarfareRatios[wt_MetalShield] * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_MetalArmor then
                              H.ResOrder[K] := Round(WarfareRatios[wt_MetalArmor] * PORTIONS);
      ht_ArmorWorkshop:   for K := 1 to 4 do
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Shield then
                              H.ResOrder[K] := Round(WarfareRatios[wt_Shield] * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Armor then
                              H.ResOrder[K] := Round(WarfareRatios[wt_Armor] * PORTIONS);
      ht_WeaponSmithy:    for K := 1 to 4 do
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Sword then
                              H.ResOrder[K] := Round(WarfareRatios[wt_Sword] * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Hallebard then
                              H.ResOrder[K] := Round(WarfareRatios[wt_Hallebard] * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Arbalet then
                              H.ResOrder[K] := Round(WarfareRatios[wt_Arbalet] * PORTIONS);
      ht_WeaponWorkshop:  for K := 1 to 4 do
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Axe then
                              H.ResOrder[K] := Round(WarfareRatios[wt_Axe] * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Pike then
                              H.ResOrder[K] := Round(WarfareRatios[wt_Pike] * PORTIONS)
                            else
                            if gResource.HouseDat[H.HouseType].ResOutput[K] = wt_Bow then
                              H.ResOrder[K] := Round(WarfareRatios[wt_Bow] * PORTIONS);
    end;
  end;
end;


procedure TKMayor.PlanDefenceTowers;
const
  DISTANCE_BETWEEN_TOWERS = 10;
var
  P: TKMHand;
  Outline1, Outline2: TKMWeightSegments;
  I, K, DefCount: Integer;
  Loc: TKMPoint;
  SegLength, Ratio: Single;
begin
  if fDefenceTowersPlanned then Exit;
  fDefenceTowersPlanned := True;
  P := gHands[fOwner];
  if not P.Stats.GetCanBuild(ht_WatchTower) then Exit;

  //Get defence Outline with weights representing how important each segment is
  fAIFields.NavMesh.GetDefenceOutline(fOwner, Outline1, Outline2);
  //Make list of defence positions
  for I := 0 to High(Outline2) do
  begin
    //Longer segments will get several towers
    SegLength := KMLength(Outline2[I].A, Outline2[I].B);
    DefCount := Max(Trunc(SegLength / DISTANCE_BETWEEN_TOWERS), 1);
    for K := 0 to DefCount - 1 do
    begin
      Ratio := (K + 1) / (DefCount + 1);
      Loc := KMPointRound(KMLerp(Outline2[I].A, Outline2[I].B, Ratio));
      fDefenceTowers.Add(Loc, Trunc(1000*Outline2[I].Weight));
    end;
  end;
  fDefenceTowers.SortByTag;
  fDefenceTowers.Inverse; //So highest weight is first
end;


procedure TKMayor.TryBuildDefenceTower;
const
  SEARCH_RAD = 6;
  MAX_ROAD_DISTANCE = 25;
var
  P: TKMHand;
  IY, IX: Integer;
  Loc: TKMPoint;
  DistSqr, BestDistSqr: Integer;
  BestLoc: TKMPoint;

  NodeList: TKMPointList;
  H: TKMHouse;
  LocTo: TKMPoint;
begin
  P := gHands[fOwner];
  //Take the first tower from the list
  Loc := fDefenceTowers[0];
  fDefenceTowers.Delete(0);
  //Look for a place for the tower
  BestDistSqr := High(BestDistSqr);
  BestLoc := KMPoint(0, 0);
  for IY := Max(1, Loc.Y-SEARCH_RAD) to Min(gTerrain.MapY, Loc.Y+SEARCH_RAD) do
    for IX := Max(1, Loc.X-SEARCH_RAD) to Min(gTerrain.MapX, Loc.X+SEARCH_RAD) do
    begin
      DistSqr := KMLengthSqr(Loc, KMPoint(IX, IY));
      if (DistSqr < BestDistSqr) and P.CanAddHousePlanAI(IX, IY, ht_WatchTower, False) then
      begin
        BestLoc := KMPoint(IX, IY);
        BestDistSqr := DistSqr;
      end;
    end;
  if (BestLoc.X > 0) then
  begin
    //See if the road required is too long (tower might be across unwalkable terrain)
    H := P.Houses.FindHouse(ht_Any, BestLoc.X, BestLoc.Y, 1, False);
    if H = nil then Exit; //We are screwed, no houses left
    LocTo := KMPointBelow(H.GetEntrance);
    NodeList := TKMPointList.Create;
    fPathFindingRoad.Route_ReturnToWalkable(BestLoc, LocTo, NodeList);
    //If length of road is short enough, build the tower
    if NodeList.Count <= MAX_ROAD_DISTANCE then
    begin
      gHands[fOwner].AddHousePlan(ht_WatchTower, BestLoc);
      TryConnectToRoad(KMPointBelow(BestLoc));
    end;
    NodeList.Free;
  end;
end;


function TKMayor.GetMaxPlans: Byte;
begin
  Result := Ceil(fSetup.WorkerCount / 4);
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
  Weight: Cardinal;
begin
  Result := False;
  P := gHands[fOwner];

  //Skip disabled houses
  if not P.Stats.GetCanBuild(aHouse) then Exit;

  //Number of simultaneous WIP houses is limited
  if (P.Stats.GetHouseWip(ht_Any) > GetMaxPlans) then Exit;

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
      for I := Min(Loc.Y - 2, gTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, gTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, gTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Corn) then
        begin
          //Base weight is distance from door (weight X higher so nice rectangle is formed)
          Weight := Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y);
          //Prefer fields below the farm
          if (I < Loc.Y + 2) then
            Inc(Weight, 100);
          //Avoid building on row with roads (so we can expand from this house)
          if I = Loc.Y + 1 then
            Inc(Weight, 1000);
          NodeTagList.Add(KMPoint(K, I), Weight);
        end;

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
      for I := Min(Loc.Y - 2, gTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, gTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, gTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Wine) then
        begin
          //Base weight is distance from door (weight X higher so nice rectangle is formed)
          Weight := Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y);
          //Prefer fields below the farm
          if (I < Loc.Y + 2) then
            Inc(Weight, 100);
          //Avoid building on row with roads (so we can expand from this house)
          if I = Loc.Y + 1 then
            Inc(Weight, 1000);
          NodeTagList.Add(KMPoint(K, I), Weight);
        end;

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
  Loc: TKMPoint;
begin
  Houses := gHands[fOwner].Houses;

  //Wait until resource is depleted and output is empty
  for I := 0 to Houses.Count - 1 do
  if not Houses[I].IsDestroyed
  and Houses[I].ResourceDepletedMsgIssued
  and (Houses[I].CheckResOut(wt_All) = 0) then
  begin
    //Set it so we can build over coal that was removed
    if Houses[I].HouseType = ht_CoalMine then
    begin
      Loc := Houses[I].GetEntrance;
      fAIFields.Influences.RemAvoidBuilding(KMRect(Loc.X-2, Loc.Y-2, Loc.X+3, Loc.Y+1));
    end;
    Houses[I].DemolishHouse(fOwner);
  end;
end;


procedure TKMayor.CheckHouseCount;
var
  P: TKMHand;

  function MaxPlansForTowers: Integer;
  begin
    Result := GetMaxPlans;
    //Once there are 2 towers wip then allow balance to build something
    if (fBalance.Peek <> ht_None) and (P.Stats.GetHouseWip(ht_WatchTower) >= 2) then
      Result := Result - 1;
    Result := Max(1, Result);
  end;

var
  H: THouseType;
begin
  P := gHands[fOwner];

  //Try to express needs in terms of Balance = Production - Demand
  fBalance.Refresh;

  //Peek - see if we can build this house
  //Take - take this house into building
  //Reject - we can't build this house (that could affect other houses in queue)

  //Build towers if village is done, or peacetime is nearly over
  if P.Stats.GetCanBuild(ht_WatchTower) then
    if ((fBalance.Peek = ht_None) and (P.Stats.GetHouseWip(ht_Any) = 0)) //Finished building
    or ((gGame.GameOptions.Peacetime <> 0) and gGame.CheckTime(600 * Max(0, gGame.GameOptions.Peacetime - 15))) then
      PlanDefenceTowers;

  if fDefenceTowersPlanned then
    while (fDefenceTowers.Count > 0) and (P.Stats.GetHouseWip(ht_Any) < MaxPlansForTowers) do
      TryBuildDefenceTower;

  while (P.Stats.GetHouseWip(ht_Any) < GetMaxPlans) do
  begin
    H := fBalance.Peek;

    //There are no more suggestions
    if H = ht_None then
      Break;

    //See if we can build that
    if TryBuildHouse(H) then
    begin
      fBalance.Take;
      fBalance.Refresh; //Balance will be changed by the construction of this house
    end
    else
      fBalance.Reject;
  end;

  //Check if we need to demolish depleted mining houses
  CheckExhaustedMines;

  //Verify all plans are being connected with roads
  CheckHousePlans;
end;


procedure TKMayor.CheckRoadsCount;
const SHORTCUT_CHECKS_PER_UPDATE = 10;
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
    begin
      //Checking for shortcuts is slow, so skip some units randomly each update
      if KaMRandom(gHands[fOwner].Stats.GetUnitQty(ut_Serf)) >= SHORTCUT_CHECKS_PER_UPDATE then
        Continue;
      if not gHands[fOwner].Units[I].IsDeadOrDying
      and (gHands[fOwner].Units[I].GetUnitAction is TUnitActionWalkTo) then
        if ((gHands[fOwner].Units[I] is TKMUnitSerf) and (gHands[fOwner].Units[I].UnitTask is TTaskDeliver)
                                                     and (TTaskDeliver(gHands[fOwner].Units[I].UnitTask).DeliverKind <> dk_ToUnit))
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
    end;
  finally
    NodeList.Free;
  end;
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
  begin
    Result := (  gHands[fOwner].Stats.GetHouseQty(ht_IronMine)
               + gHands[fOwner].Stats.GetHouseWip(ht_IronMine)
               + gHands[fOwner].Stats.GetHousePlans(ht_IronMine)) > 0;
  end;

  function GroupBlocked(aGT: TGroupType; aIron: Boolean): Boolean;
  begin
    if aIron then
      case aGT of
        gt_Melee:     Result := gHands[fOwner].Stats.UnitBlocked[ut_Swordsman];
        gt_AntiHorse: Result := gHands[fOwner].Stats.UnitBlocked[ut_Hallebardman];
        gt_Ranged:    Result := gHands[fOwner].Stats.UnitBlocked[ut_Arbaletman];
        gt_Mounted:   Result := gHands[fOwner].Stats.UnitBlocked[ut_Cavalry];
        else          Result := True;
      end
    else
      case aGT of
        gt_Melee:     Result := gHands[fOwner].Stats.UnitBlocked[ut_Militia] and
                                gHands[fOwner].Stats.UnitBlocked[ut_AxeFighter];
        gt_AntiHorse: Result := gHands[fOwner].Stats.UnitBlocked[ut_Pikeman];
        gt_Ranged:    Result := gHands[fOwner].Stats.UnitBlocked[ut_Bowman];
        gt_Mounted:   Result := gHands[fOwner].Stats.UnitBlocked[ut_HorseScout];
        else          Result := True;
      end;
  end;

  function GetUnitRatio(aUT: TUnitType): Byte;
  begin
    if gHands[fOwner].Stats.UnitBlocked[aUT] then
      Result := 0 //This warrior is blocked
    else
      if (fSetup.ArmyType = atIronAndLeather)
      and GroupBlocked(UnitGroups[aUT], not (aUT in WARRIORS_IRON)) then
        Result := 2 //In mixed army type, if our compliment is blocked we need to make double
      else
        Result := 1;
  end;

var
  Summ: Single;
  Footmen, Pikemen, Horsemen, Archers: Single;
  IronPerMin, LeatherPerMin: Single;
  WT: TWareType;
  WarfarePerMinute: TWarfareDemands;
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

  //Store ratios localy in Mayor to place weapon orders
  //Leather
  WarfareRatios[wt_Armor] :=      Footmen  * GetUnitRatio(ut_AxeFighter)
                                 + Horsemen * GetUnitRatio(ut_HorseScout)
                                 + Pikemen  * GetUnitRatio(ut_Pikeman)
                                 + Archers  * GetUnitRatio(ut_Bowman);
  WarfareRatios[wt_Shield] :=     Footmen  * GetUnitRatio(ut_AxeFighter)
                                 + Horsemen * GetUnitRatio(ut_HorseScout);
  WarfareRatios[wt_Axe] :=        Footmen  * Max(GetUnitRatio(ut_AxeFighter), GetUnitRatio(ut_Militia))
                                 + Horsemen * GetUnitRatio(ut_HorseScout);
  WarfareRatios[wt_Pike] :=       Pikemen  * GetUnitRatio(ut_Pikeman);
  WarfareRatios[wt_Bow] :=        Archers  * GetUnitRatio(ut_Bowman);
  //Iron
  WarfareRatios[wt_MetalArmor] := Footmen  * GetUnitRatio(ut_Swordsman)
                                 + Horsemen * GetUnitRatio(ut_Cavalry)
                                 + Pikemen  * GetUnitRatio(ut_Hallebardman)
                                 + Archers  * GetUnitRatio(ut_Arbaletman);
  WarfareRatios[wt_MetalShield] :=Footmen  * GetUnitRatio(ut_Swordsman)
                                 + Horsemen * GetUnitRatio(ut_Cavalry);
  WarfareRatios[wt_Sword] :=      Footmen  * GetUnitRatio(ut_Swordsman)
                                 + Horsemen * GetUnitRatio(ut_Cavalry);
  WarfareRatios[wt_Hallebard] :=  Pikemen  * GetUnitRatio(ut_Hallebardman);
  WarfareRatios[wt_Arbalet] :=    Archers  * GetUnitRatio(ut_Arbaletman);

  WarfareRatios[wt_Horse] := Horsemen * (GetUnitRatio(ut_Cavalry) + GetUnitRatio(ut_HorseScout));

  //How many warriors we would need to equip per-minute
  IronPerMin := fSetup.WarriorsPerMinute(atIron);
  LeatherPerMin := fSetup.WarriorsPerMinute(atLeather);

  //If the AI is meant to make both but runs out, we must make it up with leather
  if (fSetup.ArmyType = atIronAndLeather) and not IsIronProduced then
    LeatherPerMin := LeatherPerMin + IronPerMin; //Once iron runs out start making leather to replace it

  //Make only iron first then if it runs out make leather
  if (fSetup.ArmyType = atIronThenLeather) and IsIronProduced then
    LeatherPerMin := 0; //Don't make leather until the iron runs out

  for WT := WEAPON_MIN to WEAPON_MAX do
    if WT in WARFARE_IRON then
      WarfarePerMinute[WT] := WarfareRatios[WT] * IronPerMin
    else
      WarfarePerMinute[WT] := WarfareRatios[WT] * LeatherPerMin;

  //Horses require separate calculation
  WarfarePerMinute[wt_Horse] := Horsemen * (  GetUnitRatio(ut_Cavalry) * IronPerMin
                                            + GetUnitRatio(ut_HorseScout) * LeatherPerMin);

  //Update warfare needs accordingly
  fBalance.SetArmyDemand(WarfarePerMinute);
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
  //Checking mod result against MAX_HANDS causes first update to happen ASAP
  if (aTick + Byte(fOwner)) mod (MAX_HANDS * 10) <> MAX_HANDS then Exit;

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
  SaveStream.Write(fDefenceTowersPlanned);
  fDefenceTowers.SaveToStream(SaveStream);

  SaveStream.Write(WarfareRatios, SizeOf(WarfareRatios));

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
  LoadStream.Read(fDefenceTowersPlanned);
  fDefenceTowers.LoadFromStream(LoadStream);

  LoadStream.Read(WarfareRatios, SizeOf(WarfareRatios));

  fBalance.Load(LoadStream);
  fCityPlanner.Load(LoadStream);
  fPathFindingRoad.Load(LoadStream);
  fPathFindingRoadShortcuts.Load(LoadStream);
end;


end.

unit KM_AIMayor;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points,
  KM_CityPlanner, KM_PathfindingRoad, KM_AISetup, KM_AIMayorBalance;

type
  //Mayor is the one who manages the town
  TKMayor = class
  private
    fOwner: TPlayerIndex;
    fSetup: TKMPlayerAISetup;
    fBalance: TKMayorBalance;
    fCityPlanner: TKMCityPlanner;
    fPathFindingRoad: TPathFindingRoad;

    fAutoRepair: Boolean;

    fRoadBelowStore: Boolean;
    fArmyType: TArmyType;

    ShieldNeed: Single;
    ArmorNeed: Single;
    AxeNeed: Single;
    PikeNeed: Single;
    BowNeed: Single;
    HorseNeed: Single;

    procedure SetAutoRepair(const Value: Boolean);

    function TryBuildHouse(aHouse: THouseType): Boolean;
    function TryConnectToRoad(aLoc: TKMPoint): Boolean;

    procedure CheckStrategy;

    procedure CheckUnitCount;
    procedure CheckDeliveriesBalance;
    procedure CheckHouseCount;
    procedure CheckHousePlans;
    procedure CheckRoadsCount;
    procedure CheckExhaustedMines;
    procedure CheckWeaponOrderCount;
  public
    constructor Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
    destructor Destroy; override;

    property CityPlanner: TKMCityPlanner read fCityPlanner;

    procedure AfterMissionInit;
    property AutoRepair: Boolean read fAutoRepair write SetAutoRepair;
    procedure OwnerUpdate(aPlayer: TPlayerIndex);
    procedure SetArmyDemand(FootmenDemand, PikemenDemand, HorsemenDemand, ArchersDemand: Single);
    function BalanceText: string;

    procedure UpdateState(aTick: Cardinal);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Game, KM_Houses, KM_PlayersCollection, KM_Player, KM_Terrain, KM_Resource, KM_AIFields;


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

  //How much building materials do we need for city development
  TOWN_STONE_NEED = 10;
  TOWN_WOOD_NEED = 4.5;
  TOWN_GOLD_NEED = 1;


{ TKMayor }
constructor TKMayor.Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
begin
  inherited Create;

  fOwner := aPlayer;
  fSetup := aSetup;

  fBalance := TKMayorBalance.Create(fOwner);
  fCityPlanner := TKMCityPlanner.Create(fOwner);
  fPathFindingRoad := TPathFindingRoad.Create(fOwner);

  fAutoRepair := False; //In KaM it is Off by default
end;


destructor TKMayor.Destroy;
begin
  fBalance.Free;
  fCityPlanner.Free;
  fPathFindingRoad.Free;
  inherited;
end;


procedure TKMayor.AfterMissionInit;
begin
  fCityPlanner.AfterMissionInit;
  CheckStrategy;
end;


{ Check existing unit count vs house count and train missing citizens }
procedure TKMayor.CheckUnitCount;
var
  P: TKMPlayer;
  HS: TKMHouseSchool;
  UnitReq: array [CITIZEN_MIN..CITIZEN_MAX] of Integer;

  function HasEnoughGold: Boolean;
  begin
    //Producing gold or (Gold > 10)
    Result := (P.Stats.GetWaresProduced(rt_Gold) > 1)
              or (P.Stats.GetResourceQty(rt_Gold) > 10);
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

  P := fPlayers[fOwner];

  //Citizens
  //Count overall unit requirement (excluding Barracks and ownerless houses)
  FillChar(UnitReq, SizeOf(UnitReq), #0); //Clear up
  for H := HOUSE_MIN to HOUSE_MAX do
    if (fResource.HouseDat[H].OwnerType <> ut_None) and (H <> ht_Barracks) then
      Inc(UnitReq[fResource.HouseDat[H].OwnerType], P.Stats.GetHouseQty(H));

  //Schools
  //Count overall schools count and exclude already training units from UnitReq
  SetLength(Schools, P.Stats.GetHouseQty(ht_School));
  K := 1;
  HS := TKMHouseSchool(P.FindHouse(ht_School, K));
  while HS <> nil do
  begin
    Schools[K-1] := HS;
    for I := 0 to High(HS.Queue) do //Decrease requirement for each unit in training
      if HS.Queue[I] <> ut_None then
        Dec(UnitReq[HS.Queue[I]]); //Can be negative and compensated by e.g. ReqRecruits
    Inc(K);
    HS := TKMHouseSchool(P.FindHouse(ht_School, K));
  end;

  //Order the training
  for K := 0 to High(Schools) do
  begin
    HS := Schools[K];
    if (HS <> nil) and (HS.QueueIsEmpty) then
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
      if HS.QueueIsEmpty then //Still haven't found a match...
        if HasEnoughGold then //If we are low on Gold don't hire more ppl
          if not TryAddToQueue(ut_Serf, Round(fSetup.SerfsPerHouse * (P.Stats.GetHouseQty(ht_Any) + P.Stats.GetUnitQty(ut_Worker)/2))) then
            if not TryAddToQueue(ut_Worker, fSetup.WorkerCount) then
              if fGame.CheckTime(fSetup.RecruitDelay) then //Recruits can only be trained after this time
                if not TryAddToQueue(ut_Recruit, fSetup.RecruitCount * P.Stats.GetHouseQty(ht_Barracks)) then
                  Break; //There's no unit demand at all
    end;
  end;
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
  for I := 0 to fPlayers[fOwner].Houses.Count - 1 do
  begin
    H := fPlayers[fOwner].Houses[I];

    ResOrder := H.ResOrder[1] + H.ResOrder[2] + H.ResOrder[3] + H.ResOrder[4];

    if not H.IsDestroyed and (ResOrder = 0) then
    case H.HouseType of
      ht_ArmorSmithy:     for K := 1 to 4 do
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_MetalShield then
                              H.ResOrder[K] := Round(ShieldNeed * PORTIONS)
                            else
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_MetalArmor then
                              H.ResOrder[K] := Round(ArmorNeed * PORTIONS);
      ht_ArmorWorkshop:   for K := 1 to 4 do
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_Shield then
                              H.ResOrder[K] := Round(ShieldNeed * PORTIONS)
                            else
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_Armor then
                              H.ResOrder[K] := Round(ArmorNeed * PORTIONS);
      ht_WeaponSmithy:    for K := 1 to 4 do
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_Sword then
                              H.ResOrder[K] := Round(AxeNeed * PORTIONS)
                            else
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_Hallebard then
                              H.ResOrder[K] := Round(PikeNeed * PORTIONS)
                            else
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_Arbalet then
                              H.ResOrder[K] := Round(BowNeed * PORTIONS);
      ht_WeaponWorkshop:  for K := 1 to 4 do
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_Axe then
                              H.ResOrder[K] := Round(AxeNeed * PORTIONS)
                            else
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_Pike then
                              H.ResOrder[K] := Round(PikeNeed * PORTIONS)
                            else
                            if fResource.HouseDat[H.HouseType].ResOutput[K] = rt_Bow then
                              H.ResOrder[K] := Round(BowNeed * PORTIONS);
    end;
  end;
end;


//We want to connect to nearest road piece (not necessarily built yet)
function TKMayor.TryConnectToRoad(aLoc: TKMPoint): Boolean;
var
  I: Integer;
  P: TKMPlayer;
  H: TKMHouse;
  LocTo: TKMPoint;
  NodeList: TKMPointList;
  RoadExists: Boolean;
begin
  Result := False;
  P := fPlayers[fOwner];

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
  P: TKMPlayer;
  NodeTagList: TKMPointTagList;
begin
  Result := False;
  P := fPlayers[fOwner];

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
      for I := Min(Loc.Y + 2, fTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, fTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, fTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Corn) then
          NodeTagList.AddEntry(KMPoint(K, I), Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y));

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
      for I := Min(Loc.Y + 1, fTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, fTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, fTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Wine) then
          NodeTagList.AddEntry(KMPoint(K, I), Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y));

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
    for I := Max(Loc.Y - 3, 1) to Min(Loc.Y + 2, fTerrain.MapY - 1) do
    for K := Max(Loc.X - 2, 1) to Min(Loc.X + 2, fTerrain.MapY - 1) do
    if P.CanAddFieldPlan(KMPoint(K, I), ft_Road) then
      P.BuildList.FieldworksList.AddField(KMPoint(K, I), ft_Road);

  Result := True;
end;


procedure TKMayor.CheckHousePlans;
begin
  //
end;


procedure TKMayor.CheckDeliveriesBalance;
var
  I: Integer;
  S: TKMHouseStore;
begin
  //Block stone to store to reduce serf usage
  I := 1;
  S := TKMHouseStore(fPlayers[fOwner].FindHouse(ht_Store, I));
  while S <> nil do
  begin
    S.NotAcceptFlag[rt_Trunk] := S.CheckResIn(rt_Trunk) > 50;
    S.NotAcceptFlag[rt_Stone] := S.CheckResIn(rt_Stone) > 50;

    //Look for next Store
    Inc(I);
    S := TKMHouseStore(fPlayers[fOwner].FindHouse(ht_Store, I));
  end;
end;


procedure TKMayor.CheckExhaustedMines;
var
  I: Integer;
begin
  with fPlayers[fOwner] do
  for I := 0 to Houses.Count - 1 do
  if not Houses[I].IsDestroyed
  and Houses[I].ResourceDepletedMsgIssued then
    Houses[I].DemolishHouse(fOwner);
end;


procedure TKMayor.CheckHouseCount;
var
  P: TKMPlayer;
  H: THouseType;
begin
  P := fPlayers[fOwner];

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

  //todo: Check if planned houses are not building
  //(e.g. worker died while digging or elevation changed to impassable)
  CheckHousePlans;
end;


procedure TKMayor.CheckRoadsCount;
var
  P: TKMPlayer;
  Store: TKMHouse;
  StoreLoc: TKMPoint;
  I, K: Integer;
begin
  P := fPlayers[fOwner];

  //This is one time task to build roads around Store
  //When town becomes larger add road around Store to make traffic smoother
  if not fRoadBelowStore and (P.Stats.GetHouseQty(ht_Any) > 14) then
  begin
    fRoadBelowStore := True;

    Store := P.Houses.FindHouse(ht_Store, 0, 0, 1);
    if Store = nil then Exit;
    StoreLoc := Store.GetEntrance;

    for I := Max(StoreLoc.Y - 3, 1) to Min(StoreLoc.Y + 2, fTerrain.MapY - 1) do
    for K := StoreLoc.X - 2 to StoreLoc.X + 2 do
    if P.CanAddFieldPlan(KMPoint(K, I), ft_Road) then
      P.BuildList.FieldworksList.AddField(KMPoint(K, I), ft_Road);
  end;

  //todo: Check if we need to connect separate branches of road network
  //Town has no plan and usually roadnetwork looks like a tree,
  //where we could improve it by connecting near branches with shortcuts
end;


procedure TKMayor.CheckStrategy;
var
  Store: TKMHouse;
  StoreLoc, T: TKMPoint;
begin
  Store := fPlayers[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1);
  if Store = nil then Exit;
  StoreLoc := Store.GetEntrance;

  //AI will be Wooden if there is no iron/coal nearby
  if (fPlayers[fOwner].PlayerType = pt_Computer) and fSetup.AutoBuild then
  begin
    //@Lewin: Maybe we can choose army type from MapEd
    if (fCityPlanner.FindNearest(StoreLoc, 30, fnIron, T) and fCityPlanner.FindNearest(StoreLoc, 30, fnCoal, T)) then
      fArmyType := atIron
    else
      fArmyType := atLeather;

    //todo: This is to be removed? Most players make both iron and wooden at once, maybe we can make the AI do that later?
    //fArmyType := atLeatherIron;
  end;

  fBalance.ArmyType := fArmyType;

  SetArmyDemand(0.5, 0, 0, 0.5);
end;


procedure TKMayor.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
  fBalance.OwnerUpdate(aPlayer);
  fCityPlanner.OwnerUpdate(aPlayer);
  fPathFindingRoad.OwnerUpdate(aPlayer);
end;


//Tell Mayor what proportions of army is needed
procedure TKMayor.SetArmyDemand(FootmenDemand, PikemenDemand, HorsemenDemand, ArchersDemand: Single);
var
  WarPerMin: Single;
begin
  //todo: normalize input values to sum = 1

  //Store localy in Mayor to place weapon orders
  ShieldNeed := FootmenDemand + HorsemenDemand;
  ArmorNeed := FootmenDemand + PikemenDemand + HorsemenDemand + ArchersDemand;
  AxeNeed := FootmenDemand * 2 + HorsemenDemand;
  PikeNeed := PikemenDemand;
  BowNeed := ArchersDemand;
  HorseNeed := HorsemenDemand;

  //How many warriors we would need to equip per-minute
  WarPerMin := fSetup.WarriorsPerMinute(fArmyType);

  //Update warfare needs accordingly
  fBalance.SetArmyDemand(ShieldNeed * WarPerMin, ArmorNeed * WarPerMin, AxeNeed * WarPerMin, PikeNeed * WarPerMin, BowNeed * WarPerMin, HorseNeed * WarPerMin);
end;


procedure TKMayor.SetAutoRepair(const Value: Boolean);
var
  I: Integer;
begin
  fAutoRepair := Value;

  //Apply to those houses placed by a script before AutoRepair command
  for I := 0 to fPlayers[fOwner].Houses.Count - 1 do
    fPlayers[fOwner].Houses[I].BuildingRepair := fAutoRepair;
end;


function TKMayor.BalanceText: string;
begin
  Result := fBalance.BalanceText;
end;


procedure TKMayor.UpdateState(aTick: Cardinal);
begin
  if (aTick + Byte(fOwner)) mod (MAX_PLAYERS * 10) <> 0 then Exit;

  //Train new units (citizens, serfs, workers and recruits) if needed
  CheckUnitCount;

  CheckWeaponOrderCount;

  if fSetup.AutoBuild then
  begin
    CheckHouseCount;

    //Manage wares ratios and block stone to Store
    CheckDeliveriesBalance;

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
end;


end.

unit KM_AIMayor;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_CityPlanner, KM_PathfindingRoad,
  KM_AISetup;

type
  TKMayor = class
  private
    fOwner: TPlayerIndex;
    fSetup: TKMPlayerAISetup;
    fCityPlanner: TKMCityPlanner;
    fPathFindingRoad: TPathFindingRoad;

    fRoadBelowStore: Boolean;
    fWooden: Boolean;

    function HouseCount(aHouse: THouseType): Integer;
    procedure TryBuildHouse(aHouse: THouseType);
    function TryConnectToRoad(aLoc: TKMPoint): Boolean;

    procedure CheckStrategy;

    procedure CheckUnitCount;
    procedure CheckHouseVital;
    procedure CheckHouseMining;
    procedure CheckHouseMiningAdv;
    procedure CheckHouseFood;
    procedure CheckHouseFoodAdv;
    procedure CheckHouseDefence;
    procedure CheckHouseWeaponry;
    procedure CheckHouseCount;
    procedure CheckHousePlans;
    procedure CheckRoadsCount;
  public
    constructor Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
    destructor Destroy; override;

    property CityPlanner: TKMCityPlanner read fCityPlanner;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer: TPlayerIndex);

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

  WOOD_RAD = 6;


{ TKMayor }
constructor TKMayor.Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
begin
  inherited Create;
  fOwner := aPlayer;
  fPathFindingRoad := TPathFindingRoad.Create(fOwner);
  fCityPlanner := TKMCityPlanner.Create(fOwner);

  fSetup := aSetup;
end;


destructor TKMayor.Destroy;
begin
  fCityPlanner.Free;
  fPathFindingRoad.Free;
  inherited;
end;


procedure TKMayor.AfterMissionInit;
begin
  CheckStrategy;
end;


{ Check existing unit count vs house count and train missing citizens }
procedure TKMayor.CheckUnitCount;
var
  P: TKMPlayer;
  HS: TKMHouseSchool;
  UnitReq: array [TUnitType] of Integer;

  function HasEnoughGold: Boolean;
  begin
    //Producing gold or (Gold > 10)
    Result := (P.Stats.GetGoodsProduced(rt_Gold) > 1)
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
  for H := Low(THouseType) to High(THouseType) do
    if fResource.HouseDat[H].IsValid and (fResource.HouseDat[H].OwnerType <> ut_None) and (H <> ht_Barracks) then
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
        if (UT <> ut_None)
        and (UnitReq[UT] > 0) //Exclude untrainable units like Wolfs
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
          if not TryAddToQueue(ut_Serf, Round((10/fSetup.SerfFactor) * P.Stats.GetHouseQty(ht_Any))) then
            if not TryAddToQueue(ut_Worker, fSetup.WorkerFactor) then
              if fGame.CheckTime(fSetup.RecruitDelay) then //Recruits can only be trained after this time
                if not TryAddToQueue(ut_Recruit, fSetup.RecruitFactor * P.Stats.GetHouseQty(ht_Barracks)) then
                  Break; //There's no unit demand at all
    end;
  end;
end;


//We want to connect to nearest road piece (not necessarily built yet)
function TKMayor.TryConnectToRoad(aLoc: TKMPoint): Boolean;
var
  I: Integer;
  P: TKMPlayer;
  H: TKMHouse;
  LocPlan, LocTo: TKMPoint;
  NodeList: TKMPointList;
  RoadExists: Boolean;
begin
  Result := False;
  P := fPlayers[fOwner];

  //Find nearest wip or ready house
  H := P.Houses.FindHouse(ht_Any, aLoc.X, aLoc.Y, 1, False);
  if H = nil then Exit; //We are screwed, no houses left
  LocTo := KMPointBelow(H.GetEntrance);

  //Check building plans, maybe there's another plan nearby we can connect to with road
  //(avoid parallel connection of several planned house to town)
  if P.BuildList.HousePlanList.FindHousePlan(aLoc, KMPointAbove(aLoc), LocPlan) then
    if KMLengthSqr(aLoc, LocPlan) <= KMLengthSqr(aLoc, LocTo) then
      LocTo := KMPointBelow(LocPlan);

  NodeList := TKMPointList.Create;
  try
    RoadExists := fPathFindingRoad.Route_Make(aLoc, LocTo, NodeList);

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


procedure TKMayor.TryBuildHouse(aHouse: THouseType);
var
  I, K: Integer;
  Loc: TKMPoint;
  P: TKMPlayer;
  NodeTagList: TKMPointTagList;
begin
  P := fPlayers[fOwner];

  //Skip disabled houses
  if not P.Stats.GetCanBuild(aHouse) then Exit;

  //Number of simultaneous WIP houses is limited to 3
  if (P.Stats.GetHouseWip(ht_Any) >= 3) then Exit;

  //Maybe we get more lucky next tick
  if not fCityPlanner.FindPlaceForHouse(aHouse, Loc) then Exit;

  //Place house before road, so that road is made around it
  P.AddHousePlan(aHouse, Loc);

  //Try to connect newly planned house to road network
  //This is final test, if it is passed - we can place the house
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
      for I := Min(Loc.Y + 1, fTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, fTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, fTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Corn) then
          NodeTagList.AddEntry(KMPoint(K, I), Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y), 0);

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
          NodeTagList.AddEntry(KMPoint(K, I), Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y), 0);

      NodeTagList.SortByTag;
      for I := 0 to Min(NodeTagList.Count, 10) - 1 do
        P.BuildList.FieldworksList.AddField(NodeTagList[I], ft_Wine);
    finally
      NodeTagList.Free;
    end;
  end;

  //Block any buildings nearby
  if aHouse = ht_Woodcutters then
    fAIFields.AddAvoidBuilding(Loc.X-1, Loc.Y, WOOD_RAD); //X-1 because entrance is on right

  //Build more roads around 2nd Store
  if aHouse = ht_Store then
    for I := Max(Loc.Y - 3, 1) to Min(Loc.Y + 2, fTerrain.MapY - 1) do
    for K := Max(Loc.X - 2, 1) to Min(Loc.X + 2, fTerrain.MapY - 1) do
    if P.CanAddFieldPlan(KMPoint(K, I), ft_Road) then
      P.BuildList.FieldworksList.AddField(KMPoint(K, I), ft_Road);
end;


function TKMayor.HouseCount(aHouse: THouseType): Integer;
begin
  Result := fPlayers[fOwner].Stats.GetHouseQty(aHouse) + fPlayers[fOwner].Stats.GetHouseWip(aHouse);
end;


procedure TKMayor.CheckHouseVital;
var Req: Integer;
begin
  //Build at least one Store and add one more for each 30 houses
  Req := 1 + (HouseCount(ht_Any) div 30);
  if Req > HouseCount(ht_Store) then
    TryBuildHouse(ht_Store);

  //Build at least one School and one more if Barracks are built
  Req := 1 + HouseCount(ht_Barracks);
  if Req > HouseCount(ht_School) then
    TryBuildHouse(ht_School);

  //Build at least one Inn and one more for every 60 citizens
  Req := 1 + fPlayers[fOwner].Stats.GetCitizensCount div 60;
  if Req > HouseCount(ht_Inn) then
    TryBuildHouse(ht_Inn);
end;


//Basic mining facilities
procedure TKMayor.CheckHouseMining;
var Req: Integer;
begin
  //todo: When building new houses make sure we have enough building materials
  //left to build quary-woodcutters-sawmill. In other words - dont waste stone/wood
  //if it's not producing yet

  //Quary
  //Town needs at least 2 quaries build early and 1 more after Sawmill for competitive AI
  Req := 2 +
         Byte(fSetup.Strong) +
         Byte(fSetup.Strong and (HouseCount(ht_Sawmill) > 0)) * 1;
  if Req > HouseCount(ht_Quary) then
    TryBuildHouse(ht_Quary);

  //Woodcutters
  //Town needs at least 2 woodcutters build early and 2 more after Sawmill
  Req := 2 +
         Byte(fSetup.Strong and (HouseCount(ht_Sawmill) > 0)) * 2 +
         Byte(fWooden) * 2;
  if Req > HouseCount(ht_Woodcutters) then
    TryBuildHouse(ht_Woodcutters);

  //Sawmills count depends only on Woodcutters, build 1 per each two, but at least 1
  Req := Max(1, HouseCount(ht_Woodcutters) div 2);
  if Req > HouseCount(ht_Sawmill) then
    TryBuildHouse(ht_Sawmill);
end;


procedure TKMayor.CheckHouseMiningAdv;
var Req: Integer;
begin
  //Competitive opponent needs at least 2 gold mines and maybe 2 more later on?
  Req := 2 +
         Byte(fSetup.Strong) * 2 +
         Byte(not fWooden) * 2; //For iron production
  if Req > HouseCount(ht_CoalMine) then
    TryBuildHouse(ht_CoalMine);

  //GoldMine
  //Build 2 mines, one before and one after Metallurgists
  Req := 1 + Byte(HouseCount(ht_Metallurgists) > 0);
  if Req > HouseCount(ht_GoldMine) then
    TryBuildHouse(ht_GoldMine);

  //Metallurgists produce gold for Schools and Barracks
  Req := 1 + Byte(HouseCount(ht_Barracks) > 0);
  if Req > HouseCount(ht_Metallurgists) then
    TryBuildHouse(ht_Metallurgists);
end;


procedure TKMayor.CheckHouseFood;
var Req: Integer;
begin
  //Build at least 2 Farms early on
  Req := 1;
  if Req > HouseCount(ht_Farm) then
    TryBuildHouse(ht_Farm);

  //Wineyard
  Req := 1;
  if Req > HouseCount(ht_Wineyard) then
    TryBuildHouse(ht_Wineyard);

  //Mill
  Req := 1;
  if Req > HouseCount(ht_Mill) then
    TryBuildHouse(ht_Mill);

  //Bakery
  Req := 1;
  if Req > HouseCount(ht_Bakery) then
    TryBuildHouse(ht_Bakery);
end;


procedure TKMayor.CheckHouseFoodAdv;
var Req: Integer;
begin
  //Build at least 2 Farms early on
  Req := 1 + Byte(fSetup.Strong) * 2 * Byte(HouseCount(ht_Swine) > 0)
           + fPlayers[fOwner].Stats.GetCitizensCount div 30;
  if Req > HouseCount(ht_Farm) then
    TryBuildHouse(ht_Farm);

  //Wineyard
  Req := 1 + Byte(fSetup.Strong) * 2
           + fPlayers[fOwner].Stats.GetCitizensCount div 30;
  if Req > HouseCount(ht_Wineyard) then
    TryBuildHouse(ht_Wineyard);

  //Mill
  Req := Byte(fSetup.Strong) + HouseCount(ht_Farm) div 4;
  if Req > HouseCount(ht_Mill) then
    TryBuildHouse(ht_Mill);

  //Bakery
  Req := Byte(fSetup.Strong) + HouseCount(ht_Mill) div 2;
  if Req > HouseCount(ht_Bakery) then
    TryBuildHouse(ht_Bakery);

  //Swine
  Req := 1 + Byte(fSetup.Strong) + HouseCount(ht_Farm) div 4;
  if Req > HouseCount(ht_Swine) then
    TryBuildHouse(ht_Swine);

  //Butchers
  Req := 1 + HouseCount(ht_Swine) div 2;
  if Req > HouseCount(ht_Butchers) then
    TryBuildHouse(ht_Butchers);
end;


procedure TKMayor.CheckHouseDefence;
begin

end;


procedure TKMayor.CheckHouseWeaponry;
var
  Req: Integer;
begin
  if fWooden then
  begin
    //
    Req := 1 + Byte(fSetup.Strong);
    if Req > HouseCount(ht_Tannery) then
      TryBuildHouse(ht_Tannery);

    //
    Req := 1 + Byte(fSetup.Strong);
    if Req > HouseCount(ht_ArmorWorkshop) then
      TryBuildHouse(ht_ArmorWorkshop);

    //
    Req := 1 + Byte(fSetup.Strong);
    if Req > HouseCount(ht_WeaponWorkshop) then
      TryBuildHouse(ht_WeaponWorkshop);
  end
  else
  begin
    //
    Req := 1 + Byte(fSetup.Strong);
    if Req > HouseCount(ht_IronMine) then
      TryBuildHouse(ht_IronMine);

    //
    Req := 1 + Byte(fSetup.Strong);
    if Req > HouseCount(ht_IronSmithy) then
      TryBuildHouse(ht_IronSmithy);

    //
    Req := 1 + Byte(fSetup.Strong);
    if Req > HouseCount(ht_WeaponSmithy) then
      TryBuildHouse(ht_WeaponSmithy);

    //
    Req := 1 + Byte(fSetup.Strong);
    if Req > HouseCount(ht_ArmorSmithy) then
      TryBuildHouse(ht_ArmorSmithy);
  end;

  //One Stables is enough
  Req := 1;
  if Req > HouseCount(ht_Stables) then
    TryBuildHouse(ht_Stables);

  //One barracks is enough
  Req := 1;
  if Req > HouseCount(ht_Barracks) then
    TryBuildHouse(ht_Barracks);
end;


procedure TKMayor.CheckHousePlans;
begin
  //
end;


procedure TKMayor.CheckHouseCount;
var
  P: TKMPlayer;
  HT: THouseType;
  HouseWeight: array [HOUSE_MIN..HOUSE_MAX] of Single;
  BestHouse: THouseType;
begin
  P := fPlayers[fOwner];

  //Number of simultaneous WIP houses is limited to 3
  if (P.Stats.GetHouseWip(ht_Any) >= 3) then Exit;

  for HT := HOUSE_MIN to HOUSE_MAX do
    HouseWeight[HT] := 0;

  //AI absolutely needs at least one Store. Each house increases the need by 0.025
  //so at some point second Store is constructed
  HouseWeight[ht_Store] := 1 - P.Stats.GetHouseQty(ht_Store) + P.Stats.GetHouseQty(ht_Any) / 40;
  HouseWeight[ht_School] := 1 - P.Stats.GetHouseQty(ht_School) + P.Stats.GetUnitQty(ut_Any) / 50;
  HouseWeight[ht_Inn] := 1 - P.Stats.GetHouseQty(ht_Inn) + P.Stats.GetUnitQty(ut_Any) / 50;

  HouseWeight[ht_Quary] := 1 - P.Stats.GetHouseQty(ht_Quary) / 5 - P.Stats.GetResourceQty(rt_Stone) / 200;
  HouseWeight[ht_Woodcutters] := 1 - P.Stats.GetHouseQty(ht_Woodcutters) / 4 - P.Stats.GetResourceQty(rt_Wood) / 200;
  HouseWeight[ht_Sawmill] := 1 - P.Stats.GetHouseQty(ht_Sawmill) / 3 - P.Stats.GetResourceQty(rt_Wood) / 200;

  for HT := HOUSE_MIN to HOUSE_MAX do
    HouseWeight[HT] := HouseWeight[HT] * Byte(P.Stats.GetCanBuild(HT));

  BestHouse := HOUSE_MIN;
  for HT := HOUSE_MIN to HOUSE_MAX do
  if HouseWeight[HT] > HouseWeight[BestHouse] then
    BestHouse := HT;

  if HouseWeight[BestHouse] > 0.25 then
    TryBuildHouse(BestHouse);

  Exit;




  //Check if we have Store/Inn/School in adequate counts
  CheckHouseVital;

  CheckHouseMining;
  CheckHouseFood;
  CheckHouseMiningAdv;
  CheckHouseFoodAdv;

  if fSetup.Strong then
  begin
    CheckHouseWeaponry;
    CheckHouseDefence;
  end
  else
  begin
    CheckHouseDefence;
    CheckHouseWeaponry;
  end;

  //Check if we need to demolish depleted mining houses
  //Not sure if AI should do that though

  //Check if planned houses are not building
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
end;


procedure TKMayor.CheckStrategy;
const
  CheckDistance: array [Boolean] of Byte = (25, 32);
var
  P: TKMPlayer;
  Store: TKMHouse;
  StoreLoc, T: TKMPoint;
begin
  Store := fPlayers[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1);
  if Store = nil then Exit;
  StoreLoc := Store.GetEntrance;

  //AI will be Wooden if there no iron/coal nearby
  fWooden := not fCityPlanner.FindNearest(StoreLoc, CheckDistance[fSetup.Strong], fnIron, T)
             or not fCityPlanner.FindNearest(StoreLoc, CheckDistance[fSetup.Strong], fnCoal, T);
end;


procedure TKMayor.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
  fCityPlanner.OwnerUpdate(aPlayer);
  fPathFindingRoad.OwnerUpdate(aPlayer);
end;


procedure TKMayor.UpdateState(aTick: Cardinal);
begin
  if (aTick + Byte(fOwner)) mod (MAX_PLAYERS * 10) <> 0 then Exit;

  //Train new units (citizens, serfs, workers and recruits) if needed
  CheckUnitCount;

  if fSetup.AutoBuild then
  begin
    CheckHouseCount;

    //Build more roads if necessary
    CheckRoadsCount;
  end;
end;


procedure TKMayor.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
  SaveStream.Write(fRoadBelowStore);

  fCityPlanner.Save(SaveStream);
  fPathFindingRoad.Save(SaveStream);
end;


procedure TKMayor.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
  LoadStream.Read(fRoadBelowStore);

  fCityPlanner.Load(LoadStream);
  fPathFindingRoad.Load(LoadStream);
end;


end.


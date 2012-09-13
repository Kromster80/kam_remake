unit KM_AIMayor;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_CityPlanner, KM_PathfindingRoad,
  KM_AISetup;

type
  TKMWareBalanceGold = record
    CoalTheory, GoldOreTheory, GoldTheory: Single;
    Production: Single; //How much do we produce
    Consumption: Single; //How much is used
    Balance: Single; //Resulting balance
    Text: string;
  end;
  TKMWareBalanceFood = record
    Bread: record
      FarmTheory, MillTheory, BakeryTheory: Single;
    end;
    Sausages: record
      FarmTheory, SwineTheory, ButchersTheory: Single;
    end;
    BreadProduction, SausagesProduction, WineProduction, FishProduction: Single;
    Production: Single; //How much food do we produce
    Consumption: Single; //How much food do we use
    Balance: Single; //Resulting balance
    Text: string;
  end;

type
  TKMayor = class
  private
    fOwner: TPlayerIndex;
    fSetup: TKMPlayerAISetup;
    fCityPlanner: TKMCityPlanner;
    fPathFindingRoad: TPathFindingRoad;

    fRoadBelowStore: Boolean;
    fWooden: Boolean;

    fDemandCore: Single;
    fDemandGold: TKMWareBalanceGold;
    fDemandFood: TKMWareBalanceFood;

    function HouseCount(aHouse: THouseType): Integer; overload;
    function HouseCount(aHouse: array of THouseType): Integer; overload;
    procedure TryBuildHouse(aHouse: THouseType);
    function TryConnectToRoad(aLoc: TKMPoint): Boolean;

    procedure CheckStrategy;

    procedure CheckUnitCount;
    procedure BuildCore;
    procedure BuildMoreGold;
    procedure BuildMoreFood;
    procedure BuildMoreDefence;
    procedure BuildMoreWeaponry;
    procedure CheckHouseCount;
    procedure CheckHousePlans;
    procedure CheckRoadsCount;

    procedure UpdateDemands;
  public
    constructor Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
    destructor Destroy; override;

    property CityPlanner: TKMCityPlanner read fCityPlanner;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer: TPlayerIndex);
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


function TKMayor.HouseCount(aHouse: array of THouseType): Integer;
begin
  Result := fPlayers[fOwner].Stats.GetHouseQty(aHouse) + fPlayers[fOwner].Stats.GetHouseWip(aHouse);
end;


procedure TKMayor.BuildCore;
var Req: Integer;
begin
  //Build at least one Store and add one more for each 30 houses
  Req := 1 + (HouseCount(ht_Any) div 30);
  if Req > HouseCount(ht_Store) then
    TryBuildHouse(ht_Store);

  //Build at least one School
  Req := 1;
  if Req > HouseCount(ht_School) then
    TryBuildHouse(ht_School);

  //Build at least one Inn and one more for every 60 citizens
  Req := 1 + fPlayers[fOwner].Stats.GetCitizensCount div 60;
  if Req > HouseCount(ht_Inn) then
    TryBuildHouse(ht_Inn);

  //Quary
  //Town needs at least 2 quaries build early and 1 more after Sawmill
  Req := 2
         + Byte((HouseCount(ht_Sawmill) > 0)
            and (fPlayers[fOwner].Stats.GetResourceQty(rt_Stone) < 500));
  if Req > HouseCount(ht_Quary) then
    TryBuildHouse(ht_Quary);

  //Woodcutters
  //Town needs at least 2 woodcutters build early and 1 more after Sawmill
  Req := 2
         + Byte((HouseCount(ht_Sawmill) > 0)
           and ((fPlayers[fOwner].Stats.GetResourceQty(rt_Trunk) < 150)
                or (fPlayers[fOwner].Stats.GetResourceQty(rt_Wood) < 300)));
  if Req > HouseCount(ht_Woodcutters) then
    TryBuildHouse(ht_Woodcutters);

  //Sawmill
  Req := 1;
  if Req > HouseCount(ht_Sawmill) then
    TryBuildHouse(ht_Sawmill);
end;



//Increase Gold production
procedure TKMayor.BuildMoreGold;
var
  P: TKMPlayer;
begin
  P := fPlayers[fOwner];

  //If all 3 shares 0 we whould pick in that order Gold > Coal > Metallurgists
  with fDemandGold do
  begin
    if CoalTheory <= GoldTheory then
      if CoalTheory <= GoldOreTheory then
        TryBuildHouse(ht_CoalMine)
      else
        TryBuildHouse(ht_GoldMine)
    else
      if GoldOreTheory <= GoldTheory then
        TryBuildHouse(ht_GoldMine)
      else
        TryBuildHouse(ht_Metallurgists);
  end;
end;


procedure TKMayor.BuildMoreFood;
var
  P: TKMPlayer;

  procedure IncProdBread;
  begin
    with fDemandFood.Bread do
    if FarmTheory <= BakeryTheory then
      if FarmTheory <= MillTheory then
        TryBuildHouse(ht_Farm)
      else
        TryBuildHouse(ht_Mill)
    else
      if MillTheory <= BakeryTheory then
        TryBuildHouse(ht_Mill)
      else
        TryBuildHouse(ht_Bakery);
  end;

  procedure IncProdSausages;
  begin
    with fDemandFood.Sausages do
    if FarmTheory <= ButchersTheory then
      if FarmTheory <= SwineTheory then
        TryBuildHouse(ht_Farm)
      else
        TryBuildHouse(ht_Swine)
    else
      if SwineTheory <= ButchersTheory then
        TryBuildHouse(ht_Swine)
      else
        TryBuildHouse(ht_Butchers);
  end;

  procedure IncProdWine;
  begin
    TryBuildHouse(ht_Wineyard);
  end;
begin
  P := fPlayers[fOwner];

  //Pick smallest production and increase it
  with fDemandFood do
  begin
    if BreadProduction <= WineProduction then
      if BreadProduction <= SausagesProduction then
        IncProdBread
      else
        IncProdSausages
    else
      if SausagesProduction <= WineProduction then
        IncProdSausages
      else
        IncProdWine;
  end;
end;


procedure TKMayor.BuildMoreDefence;
begin

end;


procedure TKMayor.BuildMoreWeaponry;
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
type
  TNeedMore = (nmNone, nmGold, nmFood);
var
  P: TKMPlayer;
  HT: THouseType;
  HouseWeight: array [HOUSE_MIN..HOUSE_MAX] of Single;
  BestHouse: THouseType;
  NM: TNeedMore;
begin
  P := fPlayers[fOwner];

  //Number of simultaneous WIP houses is limited to 3
  if (P.Stats.GetHouseWip(ht_Any) >= 3) then Exit;

  {Weights idea seems like too hard to balance
  for HT := HOUSE_MIN to HOUSE_MAX do
    HouseWeight[HT] := 0;

  //When thinking about house weight arguments should be
  //  Base need for a house 0..1
  //    is it a must like an Inn (1), or an option like Stables (0)
  //- What is considered to reduce the need
  //    over 9000 stones at Store make Quary absolutely not needed
  //+ What makes this house more needed
  //    every citizen increases the need for an Inn

  //Range is meant to be like
  //      absolutely not needed
  // 0.0
  //      not needed
  // 0.25 <- after more than this weight Strong AI will build it
  //      averagely needed
  // 0.5  <- after more than this weight Weak AI will build it
  //      quite needed
  // 0.75
  //      really needed
  // 1.0  <- must (e.g. first house of kind)

  //AI absolutely needs at least one Store
  //After 10th house each house increases the need by 0.025
  //0  20-50  60-90  100-130
  HouseWeight[ht_Store]  := 1
                            - HouseCount(ht_Store)
                            + Max(HouseCount(ht_Any) - 10, 0) * 0.025;

  //When we have Barracks we need 2nd School, but it's not very important
  HouseWeight[ht_School] := 1
                            - HouseCount(ht_School)
                            + Byte(HouseCount(ht_Barracks) > 0) * 0.45;

  //Every new citizen increases need by 0.02
  //0 37-75 87-125
  HouseWeight[ht_Inn]         := 1
                                 - HouseCount(ht_Inn)
                                 + Max(P.Stats.GetUnitQty(ut_Any) - 25, 0) * 0.02;

  //Weak QQWWS
  //Strong QQQWWQSW
  HouseWeight[ht_Quary]       := 1
                                 - HouseCount(ht_Quary) * 0.45
                                 - P.Stats.GetResourceQty(rt_Stone) / 300
                                 + Byte(HouseCount(ht_Woodcutters) > 0) * 0.95
                                 + Byte(fSetup.Strong
                                        and (HouseCount(ht_Woodcutters) > 0)) * 0.95;

  HouseWeight[ht_Woodcutters] := 1
                                 - HouseCount(ht_Woodcutters) / 4
                                 - P.Stats.GetResourceQty(rt_Wood) / 300;

  HouseWeight[ht_Sawmill]     := 0.79
                                 - HouseCount(ht_Sawmill) / 3
                                 - P.Stats.GetResourceQty(rt_Wood) / 300;

  //Check ability to build house
  for HT := HOUSE_MIN to HOUSE_MAX do
    HouseWeight[HT] := HouseWeight[HT] * Byte(P.Stats.GetCanBuild(HT));

  //Pick best house
  BestHouse := HOUSE_MIN;
  for HT := HOUSE_MIN to HOUSE_MAX do
  if HouseWeight[HT] > HouseWeight[BestHouse] then
    BestHouse := HT;

  //Everything that is below 0.25 gets ignored for now
  if HouseWeight[BestHouse] > 0.25 then
    TryBuildHouse(BestHouse);

  Exit;}


  //Ensure that we have Store/Inn/School/Quary/Wood in adequate counts
  BuildCore;

  //Try to express needs
  UpdateDemands;

  if fDemandGold.Balance < -0.5 then
    BuildMoreGold
  else
  if fDemandFood.Balance < -0.5 then
    BuildMoreFood;

  //todo: Check if we need to demolish depleted mining houses
  //Not sure if AI should do that though

  //todo: Check if planned houses are not building
  //(e.g. worker died while digging or elevation changed to impassable)
  //CheckHousePlans;
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


//Calculate various demands and save intermediate numbers in case we need them
//in determing what exactly to build to satisfy demand the best
//Production is how much of this resource gets made each minute
// - we evaluate each links theoretical production of end resource (1 Corn = 3/5 Sausages)
// - in chain production the speed is limited by slowest link
// - resource in reserve adds to each production rate a fraction
//Consumption is how much gets consumed
//Balance = Production - Consumption;
procedure TKMayor.UpdateDemands;
var
  P: TKMPlayer;
  CoalProductionRate, CoalConsumptionRate, CoalFlow: Single;
  CornProductionRate, CornConsumptionRate, CornFlow: Single;
begin
  P := fPlayers[fOwner];

  //Core
  //We always need core houses
  fDemandCore := 1;

  //Gold
  with fDemandGold do
  begin
    CoalProductionRate := HouseCount(ht_CoalMine) * ProductionRate[rt_Coal];
    CoalConsumptionRate := HouseCount(ht_ArmorSmithy) * ProductionRate[rt_Shield] //Each operations uses 1 Coal per product
                         + HouseCount(ht_IronSmithy) * ProductionRate[rt_Steel]
                         + HouseCount(ht_Metallurgists) * ProductionRate[rt_Gold]
                         + HouseCount(ht_WeaponSmithy) * ProductionRate[rt_Sword];
    CoalFlow := CoalProductionRate - CoalConsumptionRate;
    CoalTheory := CoalFlow * 2;
    if HouseCount([ht_Metallurgists]) <> 0 then
      CoalTheory := CoalProductionRate / CoalConsumptionRate * (HouseCount(ht_Metallurgists) * ProductionRate[rt_Gold]) * 2;
    GoldOreTheory := HouseCount(ht_GoldMine) * ProductionRate[rt_GoldOre] * 2; //*2 since every Ore becomes 2 Gold
    GoldTheory := HouseCount(ht_Metallurgists) * ProductionRate[rt_Gold];
    //Actual production is minimum of the above
    Production := Min(CoalTheory, GoldOreTheory, GoldTheory);
    Consumption := 1 + Byte(fSetup.Strong); //For now it's a static coef
    Balance := Production - Consumption;
    Text := Format('Gold balance: %.2f - %.2f = %.2f', [Production, Consumption, Balance]);
  end;

  //Food
  with fDemandFood do
  begin
    CornProductionRate := HouseCount(ht_Farm) * ProductionRate[rt_Corn];
    CornConsumptionRate := HouseCount(ht_Mill) * ProductionRate[rt_Flour]
                       + HouseCount(ht_Swine) * ProductionRate[rt_Pig] * 5
                       + HouseCount(ht_Stables) * ProductionRate[rt_Horse] * 5;
    CornFlow := CornProductionRate - CornConsumptionRate;

    //Bread
    //Calculate how much bread each link could possibly produce
    Bread.FarmTheory := CornFlow * 2;
    if HouseCount(ht_Mill) <> 0 then //Take proportional share of all Farms
      Bread.FarmTheory := CornProductionRate / CornConsumptionRate * (HouseCount(ht_Mill) * ProductionRate[rt_Flour]) * 2;
    Bread.MillTheory := HouseCount(ht_Mill) * ProductionRate[rt_Flour] * 2;
    Bread.BakeryTheory := HouseCount(ht_Bakery) * ProductionRate[rt_Bread];
    //Actual production is minimum of the above
    BreadProduction := Min(Bread.FarmTheory, Bread.MillTheory, Bread.BakeryTheory);

    //Sausages
    //Calculate how many sausages each link could possibly produce
    Sausages.FarmTheory := CornFlow / 5 * 3;
    if HouseCount(ht_Swine) <> 0 then
      Sausages.FarmTheory := CornProductionRate / CornConsumptionRate * HouseCount(ht_Swine) * ProductionRate[rt_Pig] / 5 * 3;
    Sausages.SwineTheory := HouseCount(ht_Swine) * ProductionRate[rt_Pig] * 3;
    Sausages.ButchersTheory := HouseCount(ht_Butchers) * ProductionRate[rt_Sausages];
    //Actual production is minimum of the above
    SausagesProduction := Min(Sausages.FarmTheory, Sausages.SwineTheory, Sausages.ButchersTheory);
    //Wine, Fish
    WineProduction := HouseCount(ht_Wineyard) * ProductionRate[rt_Wine];
    FishProduction := HouseCount(ht_FisherHut) * ProductionRate[rt_Fish];

    //Count in "food units per minute"
    Production := BreadProduction * 0.4 +
                  SausagesProduction * 0.6 +
                  WineProduction * 0.3 +
                  FishProduction * 0.5;

    Consumption := P.Stats.GetUnitQty(ut_Any) / 40; //On average unit eats each 40min
    {Available := P.Stats.GetResourceQty(rt_Bread) * 0.4 +
                             P.Stats.GetResourceQty(rt_Sausages) * 0.6 +
                             P.Stats.GetResourceQty(rt_Wine) * 0.3 +
                             P.Stats.GetResourceQty(rt_Fish) * 0.5;}
    Balance := Production - Consumption;
    Text := Format('Food balance: %.2f - %.2f = %.2f', [Production, Consumption, Balance]);
  end;
end;


function TKMayor.BalanceText: string;
begin
  Result := fDemandGold.Text + '|' + fDemandFood.Text;
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


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
  TKMWareBalanceWeaponry = record
    SteelWeapon: record
      CoalTheory, IronTheory, SteelTheory, SmithyTheory: Single;
    end;
    SteelArmor: record
      CoalTheory, IronTheory, SteelTheory, SmithyTheory: Single;
    end;
    WoodenWeapon: record
      TrunkTheory, WoodTheory, WorkshopTheory: Single;
    end;
    WoodenArmor: record
      TrunkTheory, WoodTheory: Single;
      FarmTheory, SwineTheory, TanneryTheory, WorkshopTheory: Single;
    end;
    Horse: record
      FarmTheory, StablesTheory: Single;
    end;
    SteelWeaponProduction, SteelArmorProduction,
    WoodenWeaponProduction, WoodenArmorProduction, WoodenShieldProduction,
    HorseProduction: Single;
    //Let General tell us what proportions of warriors it needs
    SteelWeaponDemand, SteelArmorDemand,
    WoodenWeaponDemand, WoodenArmorDemand, WoodenShieldDemand,
    HorseDemand: Single;
    SteelWeaponBalance, SteelArmorBalance,
    WoodenWeaponBalance, WoodenArmorBalance, WoodenShieldBalance,
    HorseBalance: Single;
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
    fDemandWeaponry: TKMWareBalanceWeaponry;

    function HouseCount(aHouse: THouseType): Integer;
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
    procedure CheckExhaustedMines;

    procedure CalculateBalance;
  public
    constructor Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
    destructor Destroy; override;

    property CityPlanner: TKMCityPlanner read fCityPlanner;

    procedure AfterMissionInit;
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

  WOOD_RAD = 6;


{ TKMayor }
constructor TKMayor.Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
begin
  inherited Create;
  fOwner := aPlayer;
  fPathFindingRoad := TPathFindingRoad.Create(fOwner);
  fCityPlanner := TKMCityPlanner.Create(fOwner);

  fSetup := aSetup;

  SetArmyDemand(1, 0.5, 0.5, 1);
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
  UnitReq: array [CITIZEN_MIN..CITIZEN_MAX] of Integer;

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
          if not TryAddToQueue(ut_Serf, Round((10/fSetup.SerfFactor) * (P.Stats.GetHouseQty(ht_Any) + P.Stats.GetUnitQty(ut_Worker)/2))) then
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


procedure TKMayor.BuildCore;
var
  Demand: Integer;
begin
  Demand := 1 + HouseCount(ht_Any) div 35;
  if Demand > HouseCount(ht_Store) then
    TryBuildHouse(ht_Store);

  Demand := 1;
  if Demand > HouseCount(ht_School) then
    TryBuildHouse(ht_School);

  Demand := 1 + fPlayers[fOwner].Stats.GetCitizensCount div 80;
  if Demand > HouseCount(ht_Inn) then
    TryBuildHouse(ht_Inn);

  Demand := 2
            + Byte((HouseCount(ht_Sawmill) > 0)
            and (fPlayers[fOwner].Stats.GetResourceQty(rt_Stone) < 500));
  if Demand > HouseCount(ht_Quary) then
    TryBuildHouse(ht_Quary);

  //Woodcutters
  //Town needs at least 2 woodcutters build early and 1 more after Sawmill
  Demand := 2
         + Byte((HouseCount(ht_Sawmill) > 0)
           and ((fPlayers[fOwner].Stats.GetResourceQty(rt_Trunk) < 150)
                or (fPlayers[fOwner].Stats.GetResourceQty(rt_Wood) < 300)));
  if Demand > HouseCount(ht_Woodcutters) then
    TryBuildHouse(ht_Woodcutters);

  //Sawmill
  Demand := 2;
  if Demand > HouseCount(ht_Sawmill) then
    TryBuildHouse(ht_Sawmill);

  Demand := Byte(fPlayers[fOwner].Stats.GetWeaponsProduced > 3);
  if Demand > HouseCount(ht_Barracks) then
    TryBuildHouse(ht_Barracks);
end;



//Increase Gold production
procedure TKMayor.BuildMoreGold;
begin
  //If all 3 shares 0 we whould pick in that order Gold > Coal > Metallurgists
  with fDemandGold do
  case PickMin([CoalTheory, GoldOreTheory, GoldTheory]) of
    0:  TryBuildHouse(ht_CoalMine);
    1:  TryBuildHouse(ht_GoldMine);
    2:  TryBuildHouse(ht_Metallurgists);
  end;
end;


procedure TKMayor.BuildMoreFood;
begin
  //Pick smallest production and increase it
  with fDemandFood do
  case PickMin([BreadProduction, SausagesProduction, WineProduction]) of
    0:  with Bread do
        case PickMin([FarmTheory, MillTheory, BakeryTheory]) of
          0:  TryBuildHouse(ht_Farm);
          1:  TryBuildHouse(ht_Mill);
          2:  TryBuildHouse(ht_Bakery);
        end;
    1:  with Sausages do
        case PickMin([FarmTheory, SwineTheory, ButchersTheory]) of
          0:  TryBuildHouse(ht_Farm);
          1:  TryBuildHouse(ht_Swine);
          2:  TryBuildHouse(ht_Butchers);
        end;
    2:  TryBuildHouse(ht_Wineyard);
  end;
end;


procedure TKMayor.BuildMoreDefence;
begin

end;


procedure TKMayor.BuildMoreWeaponry;
begin
  with fDemandWeaponry do
  if fWooden then
  begin
    //Pick best link to improve
    case PickMin([WoodenWeaponBalance, WoodenArmorBalance, WoodenShieldBalance, HorseBalance]) of
      0:  with fDemandWeaponry.WoodenWeapon do
          case PickMin([TrunkTheory, WoodTheory, WorkshopTheory]) of
            0: TryBuildHouse(ht_Woodcutters);
            1: TryBuildHouse(ht_Sawmill);
            2: TryBuildHouse(ht_WeaponWorkshop);
          end;
      1:  with fDemandWeaponry.WoodenArmor do
          case PickMin([FarmTheory, SwineTheory, TanneryTheory, WorkshopTheory]) of
            0: TryBuildHouse(ht_Farm);
            1: TryBuildHouse(ht_Swine);
            2: TryBuildHouse(ht_Tannery);
            3: TryBuildHouse(ht_ArmorWorkshop);
          end;
      2:  with fDemandWeaponry.WoodenArmor do
          case PickMin([TrunkTheory, WoodTheory, WorkshopTheory]) of
            0: TryBuildHouse(ht_Woodcutters);
            1: TryBuildHouse(ht_Sawmill);
            2: TryBuildHouse(ht_WeaponWorkshop);
          end;
      3:  case PickMin([Horse.FarmTheory, Horse.StablesTheory]) of
            0: TryBuildHouse(ht_Farm);
            1: TryBuildHouse(ht_Stables);
          end;
    end;
  end
  else
  begin
    case PickMin([SteelWeaponBalance, SteelArmorBalance, HorseBalance]) of
      0:  with fDemandWeaponry.SteelWeapon do
          case PickMin([CoalTheory, IronTheory, SteelTheory, SmithyTheory]) of
            0: TryBuildHouse(ht_CoalMine);
            1: TryBuildHouse(ht_IronMine);
            2: TryBuildHouse(ht_IronSmithy);
            3: TryBuildHouse(ht_WeaponSmithy);
          end;
      1:  with fDemandWeaponry.SteelArmor do
          case PickMin([CoalTheory, IronTheory, SteelTheory, SmithyTheory]) of
            0: TryBuildHouse(ht_CoalMine);
            1: TryBuildHouse(ht_IronMine);
            2: TryBuildHouse(ht_IronSmithy);
            3: TryBuildHouse(ht_ArmorSmithy);
          end;
      2:  case PickMin([Horse.FarmTheory, Horse.StablesTheory]) of
            0: TryBuildHouse(ht_Farm);
            1: TryBuildHouse(ht_Stables);
          end;
    end;
  end;
end;


procedure TKMayor.CheckHousePlans;
begin
  //
end;


procedure TKMayor.CheckExhaustedMines;
var
  I: Integer;
begin
  with fPlayers[fOwner] do
  for I := 0 to Houses.Count - 1 do
  if not Houses[I].IsDestroyed
  and Houses[I].ResourceDepletedMsgIssued then
    fPlayers[fOwner].RemHouse(Houses[I].GetEntrance, False); //todo: Rework house demolishing to be more sensible (Houses[I].Demolish and call events from it?)
end;


procedure TKMayor.CheckHouseCount;
var
  P: TKMPlayer;
begin
  P := fPlayers[fOwner];

  //Number of simultaneous WIP houses is limited to 3
  if (P.Stats.GetHouseWip(ht_Any) >= 3) then Exit;

  //Ensure that we have Store/Inn/School/Quary/Wood in adequate counts
  BuildCore;

  //Try to express needs
  CalculateBalance;

  if fDemandGold.Balance < 0 then
    BuildMoreGold
  else
  if fDemandFood.Balance < 0 then
    BuildMoreFood
  else
  if fDemandWeaponry.Balance < 0 then
    BuildMoreWeaponry;

  //Check if we need to demolish depleted mining houses
  CheckExhaustedMines;

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
  //P: TKMPlayer;
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
procedure TKMayor.CalculateBalance;
  procedure CoalDistribution;
  var
    CoalProductionRate, CoalConsumptionRate: Single;
  begin
    CoalProductionRate := HouseCount(ht_CoalMine) * ProductionRate[rt_Coal];
    CoalConsumptionRate := HouseCount(ht_ArmorSmithy) * ProductionRate[rt_Shield] //Each operations uses 1 Coal per product
                         + HouseCount(ht_IronSmithy) * ProductionRate[rt_Steel]
                         + HouseCount(ht_Metallurgists) * ProductionRate[rt_Gold]
                         + HouseCount(ht_WeaponSmithy) * ProductionRate[rt_Sword];

    if CoalProductionRate >= CoalConsumptionRate then
    begin
      //Let every industry think the extra belongs to it
      fDemandGold.CoalTheory := (CoalProductionRate - CoalConsumptionRate + HouseCount(ht_Metallurgists) * ProductionRate[rt_Gold]) * 2;
      fDemandWeaponry.SteelWeapon.CoalTheory := CoalProductionRate - CoalConsumptionRate + HouseCount(ht_IronSmithy) * ProductionRate[rt_Steel] + HouseCount(ht_WeaponSmithy) * ProductionRate[rt_Pike];
      fDemandWeaponry.SteelArmor.CoalTheory := CoalProductionRate - CoalConsumptionRate + HouseCount(ht_IronSmithy) * ProductionRate[rt_Steel] + HouseCount(ht_ArmorSmithy) * ProductionRate[rt_MetalArmor];
    end
    else
    begin
      //Share proportionaly
      fDemandGold.CoalTheory := CoalProductionRate / CoalConsumptionRate * (HouseCount(ht_Metallurgists) * ProductionRate[rt_Gold]) * 2;
      fDemandWeaponry.SteelWeapon.CoalTheory := CoalProductionRate / CoalConsumptionRate * (HouseCount(ht_IronSmithy) * ProductionRate[rt_Steel] + HouseCount(ht_WeaponSmithy) * ProductionRate[rt_Pike]);
      fDemandWeaponry.SteelArmor.CoalTheory := CoalProductionRate / CoalConsumptionRate * (HouseCount(ht_IronSmithy) * ProductionRate[rt_Steel] + HouseCount(ht_ArmorSmithy) * ProductionRate[rt_MetalArmor]);
    end;
  end;
  procedure CornDistribution;
  var
    CornProductionRate, CornConsumptionRate: Single;
  begin
    CornProductionRate := HouseCount(ht_Farm) * ProductionRate[rt_Corn];
    CornConsumptionRate := HouseCount(ht_Mill) * ProductionRate[rt_Flour]
                         + HouseCount(ht_Swine) * ProductionRate[rt_Pig] * 5
                         + HouseCount(ht_Stables) * ProductionRate[rt_Horse] * 5;

    if CornProductionRate >= CornConsumptionRate then
    begin
      //Let every industry think the extra belongs to it
      fDemandFood.Bread.FarmTheory := (CornProductionRate - CornConsumptionRate + HouseCount(ht_Mill) * ProductionRate[rt_Flour]) * 2;
      fDemandFood.Sausages.FarmTheory := (CornProductionRate - CornConsumptionRate + HouseCount(ht_Swine) * ProductionRate[rt_Pig] * 5) / 5 * 3;
      fDemandWeaponry.WoodenArmor.FarmTheory := (CornProductionRate - CornConsumptionRate + HouseCount(ht_Swine) * ProductionRate[rt_Skin] * 5) / 5 * 2;
      fDemandWeaponry.Horse.FarmTheory := (CornProductionRate - CornConsumptionRate + HouseCount(ht_Stables) * ProductionRate[rt_Horse] * 5) / 5;
    end
    else
    begin
      //Share proportionaly
      fDemandFood.Bread.FarmTheory := CornProductionRate / CornConsumptionRate * (HouseCount(ht_Mill) * ProductionRate[rt_Flour]) * 2;
      fDemandFood.Sausages.FarmTheory := (CornProductionRate / CornConsumptionRate * HouseCount(ht_Swine) * ProductionRate[rt_Pig]) / 5 * 3;
      fDemandWeaponry.WoodenArmor.FarmTheory := (CornProductionRate / CornConsumptionRate * HouseCount(ht_Swine) * ProductionRate[rt_Skin] * 5) / 5 * 2;
      fDemandWeaponry.Horse.FarmTheory := (CornProductionRate / CornConsumptionRate * HouseCount(ht_Stables) * ProductionRate[rt_Horse]) / 5;
    end;
  end;
  procedure WoodDistribution;
  var
    TrunkProductionRate: Single;
    WoodProductionRate, WoodConsumptionRate: Single;
  begin
    TrunkProductionRate := HouseCount(ht_Woodcutters) * ProductionRate[rt_Trunk];
    WoodProductionRate := HouseCount(ht_Sawmill) * ProductionRate[rt_Wood];
    WoodConsumptionRate := 1.25 //For city building
                         + HouseCount(ht_ArmorWorkshop) * ProductionRate[rt_Armor]
                         + HouseCount(ht_WeaponWorkshop) * ProductionRate[rt_Pike];
    with fDemandWeaponry do
    begin
      if WoodProductionRate >= WoodConsumptionRate then
      begin
        //Let every industry think the extra belongs to it
        WoodenWeapon.WoodTheory := (WoodProductionRate - WoodConsumptionRate + HouseCount(ht_WeaponWorkshop) * ProductionRate[rt_Pike]);
        WoodenArmor.WoodTheory := (WoodProductionRate - WoodConsumptionRate + HouseCount(ht_ArmorWorkshop) * ProductionRate[rt_Armor]);
      end
      else
      begin
        //Share proportionaly
        WoodenWeapon.WoodTheory := WoodProductionRate / WoodConsumptionRate * HouseCount(ht_WeaponWorkshop) * ProductionRate[rt_Pike];
        WoodenArmor.WoodTheory := WoodProductionRate / WoodConsumptionRate * HouseCount(ht_ArmorWorkshop) * ProductionRate[rt_Armor];
      end;
      WoodenWeapon.TrunkTheory := WoodenWeapon.WoodTheory / WoodProductionRate * TrunkProductionRate * 2;
      WoodenArmor.TrunkTheory := WoodenArmor.WoodTheory / WoodProductionRate * TrunkProductionRate * 2;
    end;
  end;
var
  P: TKMPlayer;
begin
  P := fPlayers[fOwner];

  //Core
  //We always need core houses
  fDemandCore := 1;

  CoalDistribution;
  CornDistribution;
  WoodDistribution;

  //Gold
  with fDemandGold do
  begin
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
    //Bread
    //Calculate how much bread each link could possibly produce
    //Bread.FarmTheory calculated above
    Bread.MillTheory := HouseCount(ht_Mill) * ProductionRate[rt_Flour] * 2;
    Bread.BakeryTheory := HouseCount(ht_Bakery) * ProductionRate[rt_Bread];
    //Actual production is minimum of the above
    BreadProduction := Min(Bread.FarmTheory, Bread.MillTheory, Bread.BakeryTheory);

    //Sausages
    //Calculate how many sausages each link could possibly produce
    //Sausages.FarmTheory calculated above
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
    Text := Format('Food balance: %.2f - %.2f = %.2f|', [Production, Consumption, Balance])
          + Format('       Bread: min(F%.2f, M%.2f, B%.2f)|', [Bread.FarmTheory, Bread.MillTheory, Bread.BakeryTheory])
          + Format('    Sausages: min(F%.2f, S%.2f, B%.2f)|', [Sausages.FarmTheory, Sausages.SwineTheory, Sausages.ButchersTheory])
          + Format('  Food value: %.2f + %.2f + %.2f + %.2f|', [BreadProduction * 0.4, SausagesProduction * 0.6, WineProduction * 0.3, FishProduction * 0.5]);
  end;

  //Weaponry
  with fDemandWeaponry do
  begin
    //Weapon
    //Calculate how much Weapon each link could possibly produce
    with SteelWeapon do
    begin
      //Coal calculated above
      IronTheory := HouseCount(ht_IronMine) * ProductionRate[rt_IronOre];
      SteelTheory := HouseCount(ht_IronSmithy) * ProductionRate[rt_Steel];
      SmithyTheory := HouseCount(ht_WeaponSmithy) * ProductionRate[rt_Hallebard]; //All 3 weapons are the same
      SteelWeaponProduction := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) / 3;
      SteelWeaponBalance := SteelWeaponProduction - SteelWeaponDemand;
    end;

    //Armor
    //Calculate how many Armor each link could possibly produce
    with SteelArmor do
    begin
      //Coal calculated above
      IronTheory := HouseCount(ht_IronMine) * ProductionRate[rt_IronOre];
      SteelTheory := HouseCount(ht_IronSmithy) * ProductionRate[rt_Steel];
      SmithyTheory := HouseCount(ht_ArmorSmithy) * ProductionRate[rt_MetalArmor];
      SteelArmorProduction := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) / 2;
      SteelArmorBalance := SteelArmorProduction - SteelArmorDemand;
    end;

    with WoodenWeapon do
    begin
      //Trunk
      //Wood
      WorkshopTheory := HouseCount(ht_WeaponWorkshop) * ProductionRate[rt_Pike];
      WoodenWeaponProduction := Min(TrunkTheory, WoodTheory, WorkshopTheory) / 3;
      WoodenWeaponBalance := WoodenWeaponProduction - WoodenWeaponDemand;
    end;

    with WoodenArmor do
    begin
      //Trunk
      //Wood
      //FarmTheory calculated above
      SwineTheory := HouseCount(ht_Swine) * ProductionRate[rt_Skin] * 2;
      TanneryTheory := HouseCount(ht_Tannery) * ProductionRate[rt_Leather];
      WorkshopTheory := HouseCount(ht_ArmorWorkshop) * ProductionRate[rt_Armor];
      WoodenShieldProduction := Min(TrunkTheory, WoodTheory, WorkshopTheory) / 2;
      WoodenShieldBalance := WoodenShieldProduction - WoodenShieldDemand;
      WoodenArmorProduction := Min(Min(FarmTheory, SwineTheory), Min(TanneryTheory, WorkshopTheory)) / 2;
      WoodenArmorBalance := WoodenArmorProduction - WoodenArmorDemand;
    end;

    //Horse.FarmTheory calculated above
    Horse.StablesTheory := HouseCount(ht_Stables) * ProductionRate[rt_Horse];
    HorseProduction := Min(Horse.FarmTheory, Horse.StablesTheory);
    HorseBalance := HorseProduction - HorseDemand;

    if fWooden then
      Balance := Min([WoodenWeaponBalance, WoodenArmorBalance, WoodenShieldBalance, HorseBalance])
    else
      Balance := Min([SteelWeaponBalance, SteelArmorBalance, HorseBalance]);

    Text := Format('Weaponry Balance: %.2f|', [Balance])
          + Format('  SteelW balance: %.2f - %.2f = %.2f|', [SteelWeaponProduction, SteelWeaponDemand, SteelWeaponBalance])
          + Format('          SteelW: min(C%.2f, I%.2f, S%.2f, W%.2f)|',
                   [SteelWeapon.CoalTheory, SteelWeapon.IronTheory, SteelWeapon.SteelTheory, SteelWeapon.SmithyTheory])

          + Format('  SteelA balance: %.2f - %.2f = %.2f|', [SteelArmorProduction, SteelArmorDemand, SteelArmorBalance])
          + Format('          SteelA: min(C%.2f, I%.2f, S%.2f, W%.2f)|',
                   [SteelArmor.CoalTheory, SteelArmor.IronTheory, SteelArmor.SteelTheory, SteelArmor.SmithyTheory])

          + Format('WoodWeap balance: %.2f - %.2f = %.2f|', [WoodenWeaponProduction, WoodenWeaponDemand, WoodenWeaponBalance])
          + Format('      WoodWeapon: min(T%.2f, W%.2f, W%.2f)|',
                   [WoodenWeapon.TrunkTheory, WoodenWeapon.WoodTheory, WoodenWeapon.WorkshopTheory])

          + Format('WoodShie balance: %.2f - %.2f = %.2f|', [WoodenShieldProduction, WoodenShieldDemand, WoodenShieldBalance])
          + Format('      WoodShield: min(T%.2f, W%.2f, W%.2f)|',
                   [WoodenArmor.TrunkTheory, WoodenArmor.WoodTheory, WoodenArmor.WorkshopTheory])

          + Format('WoodArmo balance: %.2f - %.2f = %.2f|', [WoodenArmorProduction, WoodenArmorDemand, WoodenArmorBalance])
          + Format('       WoodArmor: min(F%.2f, S%.2f, T%.2f, W%.2f)|',
                   [WoodenArmor.FarmTheory, WoodenArmor.SwineTheory, WoodenArmor.TanneryTheory, WoodenArmor.WorkshopTheory])

          + Format('  Horses balance: %.2f - %.2f = %.2f|', [HorseProduction, HorseDemand, HorseBalance])
          + Format('          Horses: min(F%.2f, S%.2f)|',
                   [Horse.FarmTheory, Horse.StablesTheory]);

  end;
end;


//Tell Mayor what proportions of army is needed
procedure TKMayor.SetArmyDemand(FootmenDemand, PikemenDemand, HorsemenDemand, ArchersDemand: Single);
begin
  //Convert army request into how many weapons are needed
  with fDemandWeaponry do
  begin
    SteelWeaponDemand := FootmenDemand + HorsemenDemand + PikemenDemand + ArchersDemand;
    SteelArmorDemand := FootmenDemand * 2 + HorsemenDemand * 2 + PikemenDemand + ArchersDemand;
    WoodenWeaponDemand := FootmenDemand + HorsemenDemand + PikemenDemand + ArchersDemand;
    WoodenArmorDemand := FootmenDemand + HorsemenDemand + PikemenDemand + ArchersDemand;
    WoodenShieldDemand := FootmenDemand + HorsemenDemand;
    HorseDemand := HorsemenDemand;
  end;
end;


function TKMayor.BalanceText: string;
begin
  Result := fDemandGold.Text + '|' + fDemandFood.Text + '|' + fDemandWeaponry.Text;
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


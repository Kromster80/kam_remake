unit KM_Mayor;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_CityPlanner, KM_PathfindingRoad;


type
  TKMayor = class
  private
    fOwner: TPlayerIndex;
    fCityPlanner: TKMCityPlanner;
    fPathFindingRoad: TPathFindingRoad;

    fExpansive: Single;
    //fHouseImportance: array [THouseType] of Single;

    function HouseCount(aHouse: THouseType): Integer;

    procedure CheckVitalCount;
    procedure CheckMiningCount;
    procedure CheckFoodCount;
    procedure CheckDefenceCount;
    procedure CheckWeaponryCount;
    procedure CheckHouseCount;
    function TryBuildHouse(aHouse: THouseType): Boolean;

    //procedure RecalcImportance;
  public
    constructor Create(aPlayer: TPlayerIndex);
    destructor Destroy; override;

    procedure UpdateState;
  end;


implementation
uses KM_Game, KM_Houses, KM_PlayersCollection, KM_Player, KM_Terrain, KM_Resource;


{ TKMayor }
constructor TKMayor.Create(aPlayer: TPlayerIndex);
begin
  inherited Create;
  fOwner := aPlayer;
  fCityPlanner := TKMCityPlanner.Create(fOwner);
  fPathFindingRoad := TPathFindingRoad.Create(fOwner);
end;


destructor TKMayor.Destroy;
begin
  fCityPlanner.Free;
  fPathFindingRoad.Free;
  inherited;
end;


{procedure TKMayor.RecalcImportance;
var
  H: THouseType;
  P: TKMPlayer;
  UnitCount: Integer;
  FoodDemand: Single;
  Expansive: Single;
  StoneSupply: Single;
begin
  P := fPlayers[fOwner];

  FillChar(fHouseImportance, SizeOf(fHouseImportance), #0);

  Expansive := 0.1;
  UnitCount := P.Stats.GetUnitQty(ut_Any);

  //Calculate base importance 0 - means we don't need it. 1 means we need it asap
  fHouseImportance[ht_Inn] := 1 - HouseCount(ht_Inn) + (UnitCount - P.Stats.GetArmyCount) / 50;
  fHouseImportance[ht_School] := 1 - HouseCount(ht_School);// + P.Stats.GetCitizensTrained / 40;

  StoneSupply := Max(1 - P.Stats.GetWareCount(rt_Stone)/200, 1); //0.75
  StoneQuarries := Max(2 - HouseCount(ht_Quary) + Expansive * 1.5, 0);

  //
  fHouseImportance[ht_Quary] := 2 - HouseCount(ht_Quary) + Expansive * 1.5;
  fHouseImportance[ht_Sawmill] := 1.5 - HouseCount(ht_Sawmill) + Expansive * 1.5;
  fHouseImportance[ht_Woodcutters] := fHouseImportance[ht_Sawmill] * 1.1 - HouseCount(ht_Woodcutters);

  //Food demand per-hour
  //Consumtion = (UnitCount * 1.25)
  //Production = (HouseCount(ht_Farm) * 2)
  FoodDemand := (UnitCount * 1.25 - (HouseCount(ht_Farm) * 2)) / 20;
  fHouseImportance[ht_Bakery] := FoodDemand;// + HouseCount(ht_Mill)/2 - HouseCount(ht_Bakery);
  fHouseImportance[ht_Mill] := HouseCount(ht_Bakery) * 1.5 - HouseCount(ht_Mill);//FoodDemand + HouseCount(ht_Farm)/2 - HouseCount(ht_Mill);
  fHouseImportance[ht_Farm] := HouseCount(ht_Mill) * 1.5 - HouseCount(ht_Farm);//FoodDemand;
end;}


function TKMayor.TryBuildHouse(aHouse: THouseType): Boolean;
var
  I: Integer;
  Loc: TKMPoint;
  Store: TKMHouse;
  P: TKMPlayer;
  NodeList: TKMPointList;
  RoadExists: Boolean;
begin
  P := fPlayers[fOwner];
  Result := False;

  if ((P.Stats.GetHouseQty(ht_Any) < 3) and (P.Stats.GetHouseWip(ht_Any) >= 1))
  or (P.Stats.GetHouseWip(ht_Any) >= 3) then
    Exit;

  Result := fCityPlanner.FindPlaceForHouse(aHouse, Loc);
  if not Result then Exit;

  P.AddHousePlan(aHouse, Loc);
  Loc := KMPointBelow(Loc);

  Store := P.Houses.FindHouse(ht_Any, Loc.X, Loc.Y, 1, True);

  NodeList := TKMPointList.Create;
  try
    RoadExists := fPathFindingRoad.Route_Make(Loc, KMPointBelow(Store.GetEntrance), [CanMakeRoads, CanWalkRoad], 0, nil, NodeList, False);

    if RoadExists then
      for I := 0 to NodeList.Count - 1 do
        //We must check if we can add the plan ontop of plans placed earlier in this turn
        if P.CanAddFieldPlan(NodeList[I], ft_Road) then
          P.BuildList.FieldworksList.AddField(NodeList[I], ft_Road);
  finally
    NodeList.Free;
  end;

  //if aHouse = ht_Farm then
  //for I := Loc.Y to Loc.Y do
end;


function TKMayor.HouseCount(aHouse: THouseType): Integer;
begin
  Result := fPlayers[fOwner].Stats.GetHouseQty(aHouse) + fPlayers[fOwner].Stats.GetHouseWip(aHouse);
end;


procedure TKMayor.CheckVitalCount;
begin
  if HouseCount(ht_Store) = 0 then
    TryBuildHouse(ht_Store);

  if HouseCount(ht_School) = 0 then
    TryBuildHouse(ht_School);

  if HouseCount(ht_Inn) = 0 then
    TryBuildHouse(ht_Inn);
end;


procedure TKMayor.CheckMiningCount;
begin
  if (fPlayers[fOwner].Stats.HouseReleased[ht_Quary]) then
  begin
    if (HouseCount(ht_Quary) = 0) then
      TryBuildHouse(ht_Quary);

    if (HouseCount(ht_Sawmill) > 1) and (HouseCount(ht_Quary) < 2) then
      TryBuildHouse(ht_Quary);
  end;

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Woodcutters]) then
  begin
    if (HouseCount(ht_Woodcutters) = 0) then
      TryBuildHouse(ht_Woodcutters);

    if (HouseCount(ht_Sawmill) > 1) and (HouseCount(ht_Woodcutters) < 2) then
      TryBuildHouse(ht_Woodcutters);
  end;

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Sawmill]) and (HouseCount(ht_Sawmill) = 0) then
    TryBuildHouse(ht_Sawmill);
end;


procedure TKMayor.CheckFoodCount;
begin
  if (fPlayers[fOwner].Stats.HouseReleased[ht_Farm]) and (HouseCount(ht_Farm) < 2) then
    TryBuildHouse(ht_Farm);

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Mill]) and (HouseCount(ht_Mill) = 0) then
    TryBuildHouse(ht_Mill);

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Bakery]) and (HouseCount(ht_Bakery) = 0) then
    TryBuildHouse(ht_Bakery);


  if (fPlayers[fOwner].Stats.HouseReleased[ht_Swine]) and (HouseCount(ht_Swine) < 1) then
    TryBuildHouse(ht_Swine);

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Butchers]) and (HouseCount(ht_Butchers) = 0) then
    TryBuildHouse(ht_Butchers);


  if (fPlayers[fOwner].Stats.HouseReleased[ht_Wineyard]) and (HouseCount(ht_Wineyard) < 2) then
    TryBuildHouse(ht_Wineyard);
end;


procedure TKMayor.CheckDefenceCount;
begin

end;


procedure TKMayor.CheckWeaponryCount;
begin
  if (fPlayers[fOwner].Stats.HouseReleased[ht_Tannery]) and (HouseCount(ht_Tannery) = 0) then
    TryBuildHouse(ht_Tannery);
end;


procedure TKMayor.CheckHouseCount;
var
  H: THouseType;
  BestHouse: THouseType;
  P: TKMPlayer;
  Loc: TKMPoint;
begin
  //Check if we have Store/Inn/School in adequate counts
  CheckVitalCount;

  //Check if we have basic resource mining houses (stone, wood, gold)
  CheckMiningCount;

  //Check if produce adequate food supply
  CheckFoodCount;

  //
  CheckDefenceCount;

  //Check if we make enough weaponry
  CheckWeaponryCount;

  //RecalcImportance;

  //Pick best house to build
  {BestHouse := ht_None;
  for H := Low(THouseType) to High(THouseType) do
  begin
    if fHouseImportance[H] > Max(fHouseImportance[BestHouse], 0.5) then
      BestHouse := H;
  end;

  if BestHouse <> ht_None then
    if fCityPlanner.FindPlaceForHouse(BestHouse, Loc) then
      BuildHouse(BestHouse, Loc);}
end;


procedure TKMayor.UpdateState;
begin
  CheckHouseCount;
end;


end.


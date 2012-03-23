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
    fHouseDemand: array [THouseType] of Single;
    fHouseImportance: array [THouseType] of Single;

    procedure CheckHouseCount;
    procedure BuildHouse(aHouse: THouseType; aLoc: TKMPoint);

    procedure RecalcImportance;
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


procedure TKMayor.RecalcImportance;
var
  H: THouseType;
  P: TKMPlayer;
begin
  P := fPlayers[fOwner];

  fHouseDemand[ht_Inn] := 1 + P.Stats.GetCitizensTrained div 50;
  fHouseDemand[ht_School] := 1;

  fHouseDemand[ht_Quary] := 2 + fExpansive;
  fHouseDemand[ht_Sawmill] := 1.5 + fExpansive;
  fHouseDemand[ht_Woodcutters] := (P.Stats.GetHouseQty(ht_Sawmill) + P.Stats.GetHouseWip(ht_Sawmill)) * 2;

  fHouseDemand[ht_Bakery] := 1 + (P.Stats.GetCitizensTrained + P.Stats.GetArmyCount) / 40;
  fHouseDemand[ht_Mill] := 1 + (P.Stats.GetHouseQty(ht_Bakery) + P.Stats.GetHouseWip(ht_Bakery)) * 1.5;
  fHouseDemand[ht_Farm] := 1 + (P.Stats.GetHouseQty(ht_Mill) + P.Stats.GetHouseWip(ht_Mill)) * 1.5;

  for H := Low(THouseType) to High(THouseType) do
    fHouseDemand[H] := fHouseDemand[H] - P.Stats.GetHouseQty(H) - P.Stats.GetHouseWip(H);


  //Calculate base importance 0 - means we don't need it. 1 means we need it asap
  fHouseImportance[ht_Inn] := 1;
  fHouseImportance[ht_School] := 1;

  fHouseImportance[ht_Quary] := 1;
  fHouseImportance[ht_Woodcutters] := 1;
  fHouseImportance[ht_Sawmill] := 1;

  fHouseImportance[ht_Bakery] := 1;
  fHouseImportance[ht_Mill] := 1;
  fHouseImportance[ht_Farm] := 1;
end;


procedure TKMayor.BuildHouse(aHouse: THouseType; aLoc: TKMPoint);
var
  I: Integer;
  Store: TKMHouse;
  P: TKMPlayer;
  NodeList: TKMPointList;
  RoadExists: Boolean;
begin
  P := fPlayers[fOwner];

  P.AddHousePlan(aHouse, aLoc);
  aLoc := KMPointBelow(aLoc);

  Store := P.Houses.FindHouse(ht_Any, aLoc.X, aLoc.Y, 1, True);

  NodeList := TKMPointList.Create;
  try
    RoadExists := fPathFindingRoad.Route_Make(aLoc, KMPointBelow(Store.GetEntrance), CanWalk, 0, nil, NodeList, False);

    if RoadExists then
      for I := 0 to NodeList.Count - 1 do
        //We must check if we can add the plan ontop of plans placed earlier in this turn
        if P.CanAddFieldPlan(NodeList[I], ft_Road) then
          P.BuildList.FieldworksList.AddField(NodeList[I], ft_Road);
  finally
    NodeList.Free;
  end;
end;


procedure TKMayor.CheckHouseCount;
const MandatoryHouses: array [0..2] of THouseType = (ht_Store, ht_School, ht_Inn);
var
  I: Integer;
  H: THouseType;
  P: TKMPlayer;
  Loc: TKMPoint;
begin
  P := fPlayers[fOwner];

  //Top priority houses
  for I := 0 to 2 do
  begin
    H := MandatoryHouses[I];
    if P.Stats.GetHouseQty(H) + P.Stats.GetHouseWip(H) = 0 then
      if fCityPlanner.FindPlaceForHouse(H, Loc) then
        BuildHouse(H, Loc);
  end;
end;


procedure TKMayor.UpdateState;
begin
  CheckHouseCount;
end;


end.


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

    fSerfFactor: Single; //Serfs per house
    procedure CheckHouseCount;
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


procedure TKMayor.CheckHouseCount;
const MandatoryHouses: array [0..2] of THouseType = (ht_Store, ht_School, ht_Inn);
var
  I, K: Integer;
  H: THouseType;
  S: TKMHouse;
  P: TKMPlayer;
  Loc: TKMPoint;
  NodeList: TKMPointList;
  RoadExists: Boolean;
begin
  P := fPlayers[fOwner];

  //Top priority houses
  for I := 0 to 2 do
  begin
    H := MandatoryHouses[I];
    if P.Stats.GetHouseQty(H) + P.Stats.GetHouseWip(H) = 0 then
      if fCityPlanner.FindPlaceForHouse(H, Loc) then
      begin
        P.AddHousePlan(H, Loc);

        //GetEntrance
        //Loc.X := Loc.X + fResource.HouseDat[H].EntranceOffsetX;

        Loc := KMPointBelow(Loc);
        S := P.Houses.FindHouse(ht_Any, Loc.X, Loc.Y, 1, True);

        NodeList := TKMPointList.Create;
        try
          RoadExists := fPathFindingRoad.Route_Make(Loc, KMPointBelow(S.GetEntrance), CanWalk, 0, nil, NodeList, False);

          if RoadExists then
            for K := 0 to NodeList.Count - 1 do
              P.ToggleFieldPlan(NodeList[K], ft_Road, True);
        finally
          FreeAndNil(NodeList);
        end;
      end;
  end;
end;


procedure TKMayor.UpdateState;
begin
  CheckHouseCount;
end;


end.


unit KM_PathFindingRoad;
{$I KaM_Remake.inc}
interface
uses SysUtils, KromUtils,
  KM_CommonClasses, KM_Defaults, KM_PathFindingAStarNew, KM_Points;


type
  //Pathfinding with regard to players plans
  TPathFindingRoad = class(TPathFindingAStarNew)
  private
    fOwner: TPlayerIndex;
  protected
    function CanWalkTo(const aFrom: TKMPoint; bX, bY: SmallInt): Boolean; override;
    function DestinationReached(aX, aY: Word): Boolean; override;
    function IsWalkableTile(aX, aY: Word): Boolean; override;
    function MovementCost(aFromX, aFromY, aToX, aToY: Word): Word; override;
  public
    constructor Create(aOwner: TPlayerIndex);

    procedure OwnerUpdate(aPlayer: TPlayerIndex);
    function Route_Make(aLocA, aLocB: TKMPoint; NodeList: TKMPointList): Boolean; reintroduce;//load;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_Units;


{ TPathFindingRoad }
constructor TPathFindingRoad.Create(aOwner: TPlayerIndex);
begin
  inherited Create;
  fOwner := aOwner;
end;


procedure TPathFindingRoad.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
end;


procedure TPathFindingRoad.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
end;


procedure TPathFindingRoad.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
end;


function TPathFindingRoad.CanWalkTo(const aFrom: TKMPoint; bX, bY: SmallInt): Boolean;
begin
  Result := (bX - aFrom.X = 0) or (bY - aFrom.Y = 0);
end;


function TPathFindingRoad.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
begin
  //Since we don't allow roads to be built diagonally we can assume
  //path is always 1 tile (10 points)
  Result := 10;

  //Building roads over fields is discouraged unless unavoidable
  if fTerrain.TileIsCornField(KMPoint(aToX, aToY))
  or fTerrain.TileIsWineField(KMPoint(aToX, aToY)) then
    Inc(Result, 60); //60 points equals to 6 tiles penalty
end;


function TPathFindingRoad.IsWalkableTile(aX, aY: Word): Boolean;
begin
  Result := ([CanMakeRoads, CanWalkRoad] * fTerrain.Land[aY,aX].Passability <> [])
            and (fPlayers[fOwner].BuildList.FieldworksList.HasField(KMPoint(aX, aY)) in [ft_None, ft_Road])
            and not fPlayers[fOwner].BuildList.HousePlanList.HasPlan(KMPoint(aX, aY));
end;


function TPathFindingRoad.DestinationReached(aX, aY: Word): Boolean;
var
  RoadWorkNear: Boolean;
begin
  //Since we can't step on to WIP tiles we check for them nearby
  RoadWorkNear := ((aX > 1) and (fTerrain.Land[aY, aX-1].TileLock = tlRoadWork) and (TKMUnit(fTerrain.Land[aY, aX-1].IsUnit).Owner = fOwner))
  or ((aX < fTerrain.MapX - 1) and (fTerrain.Land[aY, aX+1].TileLock = tlRoadWork) and (TKMUnit(fTerrain.Land[aY, aX+1].IsUnit).Owner = fOwner))
  or ((aY > 1) and (fTerrain.Land[aY-1, aX].TileLock = tlRoadWork) and (TKMUnit(fTerrain.Land[aY-1, aX].IsUnit).Owner = fOwner))
  or ((aY < fTerrain.MapY - 1) and (fTerrain.Land[aY+1, aX].TileLock = tlRoadWork) and (TKMUnit(fTerrain.Land[aY+1, aX].IsUnit).Owner = fOwner));

  Result := ((aX = fLocB.X) and (aY = fLocB.Y)) //We reached destination point
            or ((fTerrain.Land[aY, aX].TileOverlay = to_Road) and (fTerrain.Land[aY, aX].TileOwner = fOwner)) //We reached own road
            or RoadWorkNear //We reached our roadplan being constructed
            or (fPlayers[fOwner].BuildList.FieldworksList.HasField(KMPoint(aX, aY)) = ft_Road);
end;


function TPathFindingRoad.Route_Make(aLocA, aLocB: TKMPoint; NodeList: TKMPointList): Boolean;
begin
  Result := inherited Route_Make(aLocA, aLocB, [CanMakeRoads, CanWalkRoad], 0, nil, NodeList, False);
end;


end.

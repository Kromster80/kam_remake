unit KM_PathFindingRoad;
{$I KaM_Remake.inc}
interface
uses SysUtils, KromUtils,
  KM_CommonClasses, KM_Defaults, KM_PathFinding, KM_PathFindingAStarNew, KM_Points;


type
  //Pathfinding that finds a route for a road to be built
  //todo: Maybe it is worth trying to make Roadfinder a house-aware algo,
  //to prefer connecting to supply/demand houses
  TPathFindingRoad = class(TPathFindingAStarNew)
  private
    fOwner: TPlayerIndex;
  protected
    function CanWalkTo(const aFrom: TKMPoint; aToX, aToY: SmallInt): Boolean; override;
    function DestinationReached(aX, aY: Word): Boolean; override;
    function IsWalkableTile(aX, aY: Word): Boolean; override;
    function MovementCost(aFromX, aFromY, aToX, aToY: Word): Word; override;
    function EstimateToFinish(aX, aY: Word): Word; override;
  public
    constructor Create(aOwner: TPlayerIndex);

    procedure OwnerUpdate(aPlayer: TPlayerIndex);
    function Route_Make(aLocA, aLocB: TKMPoint; NodeList: TKMPointList): Boolean; reintroduce;
    function Route_ReturnToWalkable(aLocA, aLocB: TKMPoint; NodeList: TKMPointList): Boolean; reintroduce;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Load(LoadStream: TKMemoryStream); override;
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
  inherited;
  SaveStream.Write(fOwner);
end;


procedure TPathFindingRoad.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fOwner);
end;


function TPathFindingRoad.CanWalkTo(const aFrom: TKMPoint; aToX, aToY: SmallInt): Boolean;
begin
  //Roads can't go diagonally, only in 90 turns
  Result := (aToX = aFrom.X) or (aToY = aFrom.Y);
end;


function TPathFindingRoad.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
begin
  //Since we don't allow roads to be built diagonally we can assume
  //path is always 1 tile (10 points)
  Result := 10;

  //Building roads over fields is discouraged unless unavoidable
  if gTerrain.TileIsCornField(KMPoint(aToX, aToY))
  or gTerrain.TileIsWineField(KMPoint(aToX, aToY)) then
    Inc(Result, 60); //60 points equals to 6 tiles penalty
end;


function TPathFindingRoad.EstimateToFinish(aX, aY: Word): Word;
begin
  case fDestination of
    pdLocation:    //Rough estimation
                    Result := (Abs(aX - fLocB.X) + Abs(aY - fLocB.Y)) * 10;

    pdPassability: //Every direction is equaly good
                    Result := 0;
    else            Result := 0;
  end;
end;


function TPathFindingRoad.IsWalkableTile(aX, aY: Word): Boolean;
begin
  Result := ([CanMakeRoads, CanWalkRoad] * gTerrain.Land[aY,aX].Passability <> [])
            and (fPlayers[fOwner].BuildList.FieldworksList.HasField(KMPoint(aX, aY)) in [ft_None, ft_Road])
            and not fPlayers[fOwner].BuildList.HousePlanList.HasPlan(KMPoint(aX, aY));
end;


function TPathFindingRoad.DestinationReached(aX, aY: Word): Boolean;
var
  RoadWorkNear: Boolean;
begin
  //Since we can't step on to WIP tiles we check for them nearby
  RoadWorkNear := ((aX > 1) and (gTerrain.Land[aY, aX-1].TileLock = tlRoadWork) and (TKMUnit(gTerrain.Land[aY, aX-1].IsUnit).Owner = fOwner))
  or ((aX < gTerrain.MapX - 1) and (gTerrain.Land[aY, aX+1].TileLock = tlRoadWork) and (TKMUnit(gTerrain.Land[aY, aX+1].IsUnit).Owner = fOwner))
  or ((aY > 1) and (gTerrain.Land[aY-1, aX].TileLock = tlRoadWork) and (TKMUnit(gTerrain.Land[aY-1, aX].IsUnit).Owner = fOwner))
  or ((aY < gTerrain.MapY - 1) and (gTerrain.Land[aY+1, aX].TileLock = tlRoadWork) and (TKMUnit(gTerrain.Land[aY+1, aX].IsUnit).Owner = fOwner));

  Result := ((aX = fLocB.X) and (aY = fLocB.Y)) //We reached destination point
            or ((gTerrain.Land[aY, aX].TileOverlay = to_Road) and (gTerrain.Land[aY, aX].TileOwner = fOwner)) //We reached own road
            or RoadWorkNear //We reached our roadplan being constructed
            or (fPlayers[fOwner].BuildList.FieldworksList.HasField(KMPoint(aX, aY)) = ft_Road);
end;


function TPathFindingRoad.Route_Make(aLocA, aLocB: TKMPoint; NodeList: TKMPointList): Boolean;
begin
  Result := inherited Route_Make(aLocA, aLocB, [CanMakeRoads, CanWalkRoad], 0, nil, NodeList);
end;


//Even though we are only going to a road network it is useful to know where our target is so we start off in the right direction (makes algorithm faster/work over long distances)
function TPathFindingRoad.Route_ReturnToWalkable(aLocA, aLocB: TKMPoint; NodeList: TKMPointList): Boolean;
begin
  Result := inherited Route_ReturnToWalkable(aLocA, aLocB, wcRoad, 0, [CanMakeRoads, CanWalkRoad], NodeList);
end;


end.
